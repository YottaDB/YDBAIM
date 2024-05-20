#!/bin/bash
#################################################################
#								#
# Copyright (c) 2021-2025 YottaDB LLC and/or its subsidiaries.	#
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
run_test ALLCONST 'set s(1)=2,s(2)="abc" set x=$$XREFSUB^%YDBAIM("^x",.s,1)'
run_test BADTRANSFORM1 'set x=$$XREFSUB^%YDBAIM("^x",2,1,,,,2,"$$abc^def(a)")'
run_test BADTRANSFORM2 'set x=$$XREFSUB^%YDBAIM("^x",2,1,,,,2)'
run_test NOSUBS1 'write $$XREFDATA^%YDBAIM("^abcd")'
run_test NOSUBS2 'write $$XREFSUB^%YDBAIM("^abcd")'
run_test NOTAGBL1 'write $$XREFDATA^%YDBAIM'
run_test NOTAGBL2 'write $$XREFSUB^%YDBAIM'
run_test NOPIECE 'write $$XREFDATA^%YDBAIM("^abcd",1,"|")'
run_test CANTADDSTAT1 'write $$XREFDATA^%YDBAIM("^abcd",1,"|",1),!,$$XREFDATA^%YDBAIM("^abcd",1,"|",1,,,,2)'
run_test CANTADDSTAT2 'write $$XREFSUB^%YDBAIM("^abcd",1,1,,,1),!,$$XREFSUB^%YDBAIM("^abcd",1,1,,,2)'
run_test NOEXTREF1 'write $$XREFDATA^%YDBAIM("^|""x.gld""|x")'
run_test NOEXTREF2 'write $$XREFSUB^%YDBAIM("^|""x.gld""|x")'
run_test INVPIECE 'set subs(1)="1:2:3" write $$XREFDATA^%YDBAIM("^x",.subs)'
run_test INVPNUMSEP 'kill ^x set ^x(1)="a1|b1",^x(2)="a2|",^x(3)="|b3",subs(1)="*" write $$XREFDATA^%YDBAIM("^x",.subs,"|","1:2:3")'
run_test INVSNUM1 'set x=$$XREFSUB^%YDBAIM("^x",2,0)'
run_test INVSNUM2 'set x=$$XREFSUB^%YDBAIM("^x",2,30)'
run_test INVSNUM3 'set x=$$XREFSUB^%YDBAIM("^x",2,"A")'
run_test INVSNUM4 'set x=$$XREFSUB^%YDBAIM("^x",3,4)'
run_test NOTAGBL2a 'write $$UNXREFDATA^%YDBAIM("xy123456789012345678901234567890",1)'
run_test NOTAGBL2b 'write $$UNXREFSUB^%YDBAIM("xy123456789012345678901234567890",1)'
run_test NOTAGBL3a 'write $$XREFDATA^%YDBAIM("^xy123456789012345678901234567890",0)'
run_test NOTAGBL3b 'write $$XREFSUB^%YDBAIM("^xy123456789012345678901234567890",0)'
run_test NOTAGBL4a 'write $$XREFDATA^%YDBAIM("^xy123456789012345678901234567890",1)'
run_test NOTAGBL4b 'write $$XREFSUB^%YDBAIM("^xy123456789012345678901234567890",1)'
run_test NOTAGBL5a 'write $$UNXREFDATA^%YDBAIM("^xy123456789012345678901234567890",0)'
run_test NOTAGBL5b 'write $$UNXREFSUB^%YDBAIM("^xy123456789012345678901234567890",0)'
run_test NOTAGBL6a 'write $$UNXREFDATA^%YDBAIM("^xy123456789012345678901234567890",1)'
run_test NOTAGBL6b 'write $$UNXREFSUB^%YDBAIM("^xy123456789012345678901234567890",1)'
run_test GVSUBOFLOW0 'do gvsuboflow^%YDBAIMTEST(0)'	# Test $ZLEVEL=0
run_test GVSUBOFLOW1 'do gvsuboflow^%YDBAIMTEST(1)'	# Test $ZLEVEL=1
run_test BADINVOCATION2 'DO ^%YDBAIM'		# Test of BADINVOCATION via %XCMD
run_test BADTRANSFORM1a 'do XREFDATA^%YDBAIM("^abcd",2,,,,,,,2,0)'
run_test BADTRANSFORM1b 'do XREFSUB^%YDBAIM("^abcd",2,2,,,,2,0)'
run_test BADTRANSFORM2a 'do XREFDATA^%YDBAIM("^abcd",2,,,,,,,2,1)'
run_test BADTRANSFORM2b 'do XREFSUB^%YDBAIM("^abcd",2,2,,,,2,1)'
run_test BADTRANSFORM3a 'do XREFDATA^%YDBAIM("^abcd",2,,,,,,,0,"abcd")'
run_test BADTRANSFORM3b 'do XREFSUB^%YDBAIM("^abcd",2,2,,,,2,"abcd")'
run_test BADTRANSFORM4 'do XREFDATA^%YDBAIM("^abcd",2,,,,,,,1,"abcd")'

echo "BADINVOCATION3 DO ^%YDBAIM in direct mode (not via %XCMD)"
echo 'DO ^%YDBAIM' | yottadb -direct		# Test of BADINVOCATION in direct mode
echo $?
echo "---------------"
echo
set -e
