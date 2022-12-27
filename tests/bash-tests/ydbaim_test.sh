#!/bin/bash
#################################################################
#								#
# Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
set +e # don't stop for errors, we generate them.
function run_test() {
	local test_name=$1
	local test_to_run=$2
	echo $test_name $test_to_run
	yottadb -run %XCMD $test_to_run | uniq
	echo ${PIPESTATUS[0]}
	echo "---------------"
	echo
}

# These are tests for error output that are compared later to a reference file
run_test BADINVOCATION 'DO badinvocation^%YDBAIMTEST'
run_test NOSUBS 'write $$XREFDATA^%YDBAIM("^abcd")'
run_test NOTAGBL1 'write $$XREFDATA^%YDBAIM'
run_test NOPIECE 'write $$XREFDATA^%YDBAIM("^abcd",1,"|")'
run_test CANTADDSTAT 'write $$XREFDATA^%YDBAIM("^abcd",1,"|",1),!,$$XREFDATA^%YDBAIM("^abcd",1,"|",1,,,,2)'
run_test NOEXTREF 'write $$XREFDATA^%YDBAIM("^|""x.gld""|x")'
run_test INVPIECE 'set subs(1)="1:2:3" write $$XREFDATA^%YDBAIM("^x",.subs)'
run_test INVPNUMSEP 'kill ^x set ^x(1)="a1|b1",^x(2)="a2|",^x(3)="|b3",subs(1)="*" write $$XREFDATA^%YDBAIM("^x",.subs,"|","1:2:3")'
run_test NOTAGBL2 'write $$UNXREFDATA^%YDBAIM("xy123456789012345678901234567890",1)'
run_test NOTAGBL3 'write $$XREFDATA^%YDBAIM("^xy123456789012345678901234567890",0)'
run_test NOTAGBL4 'write $$XREFDATA^%YDBAIM("^xy123456789012345678901234567890",1)'
run_test NOTAGBL5 'write $$UNXREFDATA^%YDBAIM("^xy123456789012345678901234567890",0)'
run_test NOTAGBL6 'write $$UNXREFDATA^%YDBAIM("^xy123456789012345678901234567890",1)'
run_test GVSUBOFLOW0 'do gvsuboflow^%YDBAIMTEST(0)'	# Test $ZLEVEL=0
run_test GVSUBOFLOW1 'do gvsuboflow^%YDBAIMTEST(1)'	# Test $ZLEVEL=1
run_test BADINVOCATION2 'DO ^%YDBAIM'		# Test of BADINVOCATION via %XCMD

echo "BADINVOCATION3 DO ^%YDBAIM in direct mode (not via %XCMD)"
echo 'DO ^%YDBAIM' | yottadb -direct		# Test of BADINVOCATION in direct mode
echo $?
echo "---------------"
echo
set -e
