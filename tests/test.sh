#!/bin/bash
#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

# Usage:
# tests/test.sh [clean]
## clean argument deletes db and removes downloaded files

# Redefine pushd/popd to not print when performing their job
pushd () {
    command pushd "$@" > /dev/null
}

popd () {
    command popd "$@" > /dev/null
}

# Bash needs to quit if a command fails.
set -e

# Remove existing environment set-up as that screws up ydb_env_set
for e in `env | grep -E '^(ydb|gtm)' | cut -d= -f1`; do unset $e; done

# Create db into db directory
script_dir=$(realpath $(dirname "${BASH_SOURCE[0]}"))
ydb_dir=$(realpath "$script_dir/../db/")
export ydb_dir
if [ "$1" = "clean" ]; then
	rm -r $ydb_dir
	rm -r $script_dir/downloads/
fi
source `pkg-config --variable=prefix yottadb`/ydb_env_set

# Remove files we created in prior test runs
rm -f $ydb_tmp/*

# Move our test routines to the database we just created
cp $script_dir/munit-tests/*.m $ydb_dir/r/

# Don't recreate database if it already exists... so that we can re-run faster
if [ ! -f $ydb_dir/r/_ut.m ]; then
	# Load test data.
	# 5200 rows of VistA Data (50.6+VA GENERIC)
	# A small VistA file (vista-mini.zwr)
	# Sample Octo Postgres Tables data (octo-seed.zwr default_user.zwr)
	#
	# Also download M Unit
	mkdir -p $script_dir/downloads
	pushd $script_dir/downloads
	rm -f *
	# Single curl command to take advantage of HTTP/2 and HTTP pipelining
	curl -s \
	 -LO https://raw.githubusercontent.com/WorldVistA/VistA-M/master/Packages/National%20Drug%20File/Globals/50.6%2BVA%20GENERIC.zwr \
	 -LO https://gitlab.com/YottaDB/DBMS/YDBOcto/-/raw/master/tests/fixtures/vista-mini.zwr \
	 -LO https://gitlab.com/YottaDB/DBMS/YDBOcto/-/raw/master/tests/fixtures/octo-seed.zwr \
	 -LO https://gitlab.com/YottaDB/DBMS/YDBOcto/-/raw/master/tests/fixtures/default_user.zwr \
	 -LO https://raw.githubusercontent.com/ChristopherEdwards/M-Unit/master/Routines/_ut.m \
	 -LO https://raw.githubusercontent.com/ChristopherEdwards/M-Unit/master/Routines/_ut1.m
	popd

	# Load the external and internal zwr files
	pushd $script_dir/downloads
	$ydb_dist/mupip load -ignorechset 50.6%2BVA%20GENERIC.zwr &> /dev/null
	$ydb_dist/mupip load -ignorechset vista-mini.zwr &> /dev/null
	$ydb_dist/mupip load -ignorechset octo-seed.zwr &> /dev/null
	$ydb_dist/mupip load -ignorechset default_user.zwr &> /dev/null
	popd

	# Get and Compile M Unit routines
	pushd $ydb_dir/r/
	cp $script_dir/downloads/_ut*.m .
	popd
	object_dir=$(echo $ydb_routines | cut -d'*' -f1)
	pushd $object_dir
	set +e # compiler issues warnings and stops our code; so we need to tell bash to ignore the error.
	for r in $ydb_dir/r/*.m; do $ydb_dist/yottadb -nowarning $r; done
	set -e
	popd
fi

# Run tests
# %YDBAIMTEST spews .mj* files. We don't want that in the repo; put in $ydb_tmp
pushd $ydb_tmp
$ydb_dist/yottadb -r %YDBAIMSAN    | tee -a test_output.txt
$ydb_dist/yottadb -r %YDBAIMTEST   | tee -a test_output.txt
$ydb_dist/yottadb -r %YDBAIMSPEED  | tee -a test_output.txt
popd
