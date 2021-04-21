	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;								;
	; Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	;
	; All rights reserved.						;
	;								;
	;	This source code contains the intellectual property	;
	;	of its copyright holder(s), and is made available	;
	;	under a license.  If you do not know the terms of	;
	;	the license, please stop and do not read further.	;
	;								;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
%YDBAIMSPEED ; Test Speed for YDBAIM
	do en^%ut($t(+0),3)
	quit
	;
STARTUP	; Runs once to create test data
	;
	kill ^names,^names2,^composite
	set numrows=1000
	for i=1:4:numrows set ^names(i)="A|B"
	for i=2:4:numrows set ^names(i)="C|B"
	for i=3:4:numrows set ^names(i)="A|C"
	for i=4:4:numrows set ^names(i)="B|A"
	;
	for i=1:4:numrows set ^names2("fix",i,0)="A|B"
	for i=2:4:numrows set ^names2("fix",i,0)="C|B"
	for i=3:4:numrows set ^names2("fix",i,0)="A|C"
	for i=4:4:numrows set ^names2("fix",i,0)="B|A"
	;
	for i=1:4:numrows set ^composite(1,2,3,4,5,6,7,i)="A|B"
	for i=2:4:numrows set ^composite(1,2,3,4,5,6,7,i)="C|B"
	for i=3:4:numrows set ^composite(1,2,3,4,5,6,7,i)="A|C"
	for i=4:4:numrows set ^composite(1,2,3,4,5,6,7,i)="B|A"
	quit
	;
TEARDOWN	; Runs after each test
	D UNXREFDATA^%YDBAIM
	QUIT
	;
t1	; @TEST Index 1000 rows of ^names(:)="A|B"
	if $$XREFDATA^%YDBAIM("^names",1,"|",2)
	quit
	;
t2	; @TEST Index 1000 rows of ^names2("fix",:,0)="A|B"
 	new subs set subs(1)="""fix""",subs(2)=":",subs(3)=0
 	if $$XREFDATA^%YDBAIM("^names2",.subs,"|",2)
	quit
	;
t3	; @TEST Index 1000 rows of ^composite(1,2,3,4,5,6,7,:)="A|B"
	if $$XREFDATA^%YDBAIM("^composite",8)
	quit
	;
t4	; @TEST Index ~5000 dispersed records in ^PSNDF(50.6,:,"VUID")
 	set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
 	if $$XREFDATA^%YDBAIM("^PSNDF",.subs,"^",1)
	quit
