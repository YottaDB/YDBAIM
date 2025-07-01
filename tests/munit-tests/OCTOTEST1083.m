	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;								;
	; Copyright (c) 2025 YottaDB LLC and/or its subsidiaries.	;
	; All rights reserved.						;
	;								;
	;	This source code contains the intellectual property	;
	;	of its copyright holder(s), and is made available	;
	;	under a license.  If you do not know the terms of	;
	;	the license, please stop and do not read further.	;
	;								;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
OCTOTEST1083 ; Special Test Runner for octo1083 which requires a small 
	; ... database size (4 million rows) instead of 20 million
	; Please leave debugging commands in as a hint for the next user
	;zb err
	;zb tinv8+5
	;zb tinv12
	;zb type1t1^%YDBAIMTEST
	;s $zstep="zp @$zpos b"
	;do en^%ut($t(+0),3,1)
	; end debug commands
	;
	do en^%ut($t(+0),3)
	quit
	;
TEARDOWN ; Runs after each test
	D UNXREFDATA^%YDBAIM
	QUIT
	;
octo1083 ; @TEST TRANS2BIG error if AIM xref occupies more than 64Ki blocks
	kill ^names4
	new numrows set numrows=4E6
	for i=1:1:numrows set ^names4(i)="A"_$justify(i,500)_"|B"
	new aim set aim=$$XREFDATA^%YDBAIM("^names4",1,"|",1,0,0,1,2)
	; no asserts as test will fail with $ECODE if TRANS2BIG happened
	quit
