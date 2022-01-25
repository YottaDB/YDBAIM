#!/bin/bash
#################################################################
#                                                               #
# Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries   #
# All rights reserved.                                          #
#                                                               #
#   This source code contains the intellectual property         #
#   of its copyright holder(s), and is made available           #
#   under a license.  If you do not know the terms of           #
#   the license, please stop and do not read further.           #
#                                                               #
#################################################################
set -e
# Centos vs ubuntu have different commands
if [ -x "$(command -v cmake3)" ]; then
  cmakeCommand="cmake3"
else
  cmakeCommand="cmake"
fi
cd "$(dirname "$(readlink -f "$0")")"
rm -rf build
mkdir build && cd build
echo "Installing M Mode Shared Library"
$cmakeCommand -DM_UTF8_MODE=0 ..
make && make install
cd ..
if ([ -d "$ydb_dist/utf8" ] || ( [ -z ${ydb_dist} ] && [ -d "$(pkg-config --variable=prefix yottadb)/utf8" ] )) ; then
	echo ""
	echo "Installing UTF-8 Mode Shared Library"
	rm -rf build
	mkdir build && cd build
	$cmakeCommand -DM_UTF8_MODE=1 ..
	make && make install
	cd ..
fi
