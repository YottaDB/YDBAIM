	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;								;
	; Copyright (c) 2021-2024 YottaDB LLC and/or its subsidiaries.	;
	; All rights reserved.						;
	;								;
	;	This source code contains the intellectual property	;
	;	of its copyright holder(s), and is made available	;
	;	under a license.  If you do not know the terms of	;
	;	the license, please stop and do not read further.	;
	;								;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
%YDBAIMTEST ; Test Runner for YDBAIM
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
STARTUP	; Runs once to create test data
	; There is a bunch of zwr files that got loaded before we got here.
	set ^customers(1)="George§Washington§gwashington@usa.gov§3200 Mt Vernon Hwy§Mount Vernon§VA§22121"
	set ^customers(2)="John§Adams§jadams@usa.gov§1250 Hancock St§Quincy§MA§02169"
	set ^customers(3)="Thomas§Jefferson§tjefferson@usa.gov§931 Thomas Jefferson Pkwy§Charlottesville§VA§22902"
	set ^customers(4)="James§Madison§jmadison@usa.gov§11350 Constitution Hwy§Orange§VA§22960"
	set ^customers(5)="James§Monroe§jmonroe@usa.gov§2050 James Monroe Parkway§Charlottesville§VA§22902"
	set ^orders(1)="07/04/1776§$234.56§1"
	set ^orders(2)="03/14/1760§$78.50§3"
	set ^orders(3)="05/23/1784§$124.00§2"
	set ^orders(4)="09/03/1790§$65.50§3"
	set ^orders(5)="07/21/1795§$25.50§10"
	set ^orders(6)="11/27/1787§$14.40§9"
	;
	; NB: ^null is in NULLREG which has null enabled. DEFAULT region doesn't. AIM does though by default.
	set ^null("","")="test1"
	set ^null("","A")="test2"
	set ^null("B","")="test3"
	set ^null("",4)="test4"
	set ^null(5,"")="test5"
	;
	; Sets for data in spanning nodes
	; - sameset has null subscripts enabled over all nodes
	; - diffset has null subscripts enabled for numeric first nodes only
	set ^sameset(1,"")="test1"
	set ^sameset("a","")="test2"
	set ^diffset(1,"")="test1"
	set ^diffset("a","a")="test2"
	;
	; The below global variable nodes are needed by the speed test
	kill ^names,^names3,^composite
	new numrows set numrows=1E6
	for i=1:4:numrows set ^names(i)="A|B"
	for i=2:4:numrows set ^names(i)="C|B"
	for i=3:4:numrows set ^names(i)="A|C"
	for i=4:4:numrows set ^names(i)="B|A"
	;
	for i=1:4:numrows set ^names3("fix",i,0)="A|B"
	for i=2:4:numrows set ^names3("fix",i,0)="C|B"
	for i=3:4:numrows set ^names3("fix",i,0)="A|C"
	for i=4:4:numrows set ^names3("fix",i,0)="B|A"
	;
	for i=1:4:numrows set ^composite(1,2,3,4,5,6,7,i)="A|B"
	for i=2:4:numrows set ^composite(1,2,3,4,5,6,7,i)="C|B"
	for i=3:4:numrows set ^composite(1,2,3,4,5,6,7,i)="A|C"
	for i=4:4:numrows set ^composite(1,2,3,4,5,6,7,i)="B|A"
	;
	for i=6000:1:1E6 set ^PSNDF(50.6,i,"VUID")=$select(i#2:"A^B",1:"B^A")
	quit
	;
TEARDOWN ; Runs after each test
	D UNXREFDATA^%YDBAIM
	QUIT
	;
LOADDATA(lab)	; load data after label lab
	new i,line
	for i=1:1 set line=$piece($text(@(lab_"+"_i)),";",2) quit:'$zlength(line)  set @line
	quit:$quit i quit
	;
assert:(boolexpr,msg) ; [private] shortcut to tf^%ut
	if '$data(%ut) do  quit
	. if 'boolexpr set $ecode=",U-ASSERT,"
	;
	if $get(msg)="" set msg="Error from "_$stack($stack-1,"place")_": "_$stack($stack-1,"mcode")
	do tf^%ut(boolexpr,$get(msg))
	quit
	;
trigout:(lines) ; [private] output triggers into .lines
	new f set f="triggers.txt"
	open f:newversion use f
	view "ztrigger_output":1
	if $ztrigger("s")
	use f:rewind
	new i for i=1:1 read lines(i)  quit:$zeof
	kill lines(i) ; last line is empty due to $zeof
	close f:delete
	quit
	;
aimgbls:(array) ; [private] output all AIM globals
	new %,%1 set (%,%1)="^%ydbAIMD"
	for  set %=$order(@%) quit:%=""  quit:%'[%1  if %'="^%ydbAIMDxref" set:$increment(array) array(%)=""
	quit
	;
aimdxref:(array) ; [private] count globals in ^%ydbAIMDxref
	new % set %=""
	for  set %=$order(^%ydbAIMDxref(%)) quit:%=""  set:$increment(array) array(%)=""
	quit
	;
aim72	; @TEST Regression test for YDBAIM#72
	new i,j,s,x
	do assert('$data(^x))
	set s(1)="2:"
	for i=1:1:5 set ^x(i)="abcd|efgh"
	set x=$$XREFDATA^%YDBAIM("^x",.s,"|",1)
	set j="#"
	for i=1:1:5 set j=j_$data(@x@(1,"abcd",i))
	do assert("#01111"=j)
	set x=$$UNXREFDATA^%YDBAIM("^x",.s,"|",1)
	kill ^x
	quit
	;
aim73	; @TEST Test for YDBAIM#73
	; This is a glass box test for the change to prefix cross references with "#" for force=1
	new i,j,nodes,npieces,nsubs,pnum,ref,testval,tvxref,x,y,varpat
	do assert('$data(^x))
	; Default type (0)
	; - Entire node - start by simulating application global data
	set nsubs=1+$random(3)				; No. of subscripts for simulated application global
	set nodes=$$aim73genblk(nsubs,10000,1,1000)	; Generate simulated application global; values are entire nodes
	set testval=$select($random(2):-1-$random(1000),1:1000+$random(1000))	; Application global value not generated above
	set tvxref="#"_testval				; Xref for testval forced to be a string
	; Indirect variable pattern for simulated application global guaranteed to not already exist
	set varpat=$ztranslate($$aim73genpat(nsubs),3,4)
	;   - No statistics
	;     - Initial scan; large number of nodes to hopefully force 2 processes
	set x=$$XREFDATA^%YDBAIM("^x",nsubs,,,,,,,,1)
	set y="" for  set y=$order(@x@(0,y)) quit:'$zlength(y)  do assert("#"=$zextract(y,1))
	;     - Check set trigger by setting a value not previously set
	set @varpat=testval,ref=$reference do assert(10=$data(@x@(0,tvxref)))
	;     - Check kill trigger by killing the value just set
	kill @ref do assert('$data(@x@(0,tvxref)))
	;     - Check complete removal of AIM data
	do UNXREFDATA^%YDBAIM(x),assert('$data(@x))
	;   - Statistics=1
	;     - Initial scan
	set x=$$XREFDATA^%YDBAIM("^x",nsubs,,,,,,1,,1)
	set y="" for  set y=$order(@x@(0,y)) quit:'$zlength(y)  do assert("#"=$zextract(y,1))
	set i=0,y="" for  set y=$order(@x@("",y)) quit:'$zlength(y)  if $increment(i,@x@("",y))
	do assert(nodes=i)    ; Confirm that all nodes are counted
	;     - Check set trigger
	set varpat=$ztranslate(varpat,4,5)
	set @varpat=testval,ref=$reference do assert($data(@x@(0,tvxref))),assert(1=@x@("",tvxref))
	;     - Check kill trigger
	kill @ref do assert('$data(@x@(0,tvxref))),assert('$data(@x@("",tvxref)))
	;     - Check removal
	do UNXREFDATA^%YDBAIM(x),assert('$data(@x))
	;   - Statistics=2
	;     - Initial scan
	set x=$$XREFDATA^%YDBAIM("^x",nsubs,,,,,,2,,1)
	set y="" for j=0:1 set y=$order(@x@(0,y)) quit:'$zlength(y)  do assert("#"=$zextract(y,1))
	set i=0,y="" for  set y=$order(@x@("",y)) quit:'$zlength(y)  if $increment(i,@x@("",y))
	;       Confirm all nodes counted, total count, count of distinct values
	do assert(nodes=i),assert(nodes=@x@(11)),assert(j=@x@(""))
	;     - Check set trigger: additional xref, increase in total statistics, increase in number of distinct values
	set varpat=$ztranslate(varpat,5,6)
	set @varpat=testval,ref=$reference
	do assert(10=$data(@x@(0,tvxref))),assert(1=@x@("",tvxref)),assert(nodes+1=@x@(11)),assert(j+1=@x@(""))
	;     - Check kill trigger: no xref, decrease in total statistics, decrease in number of distinct values
	kill @ref
	do assert('$data(@x@(0,tvxref))),assert('$data(@x@("",tvxref))),assert(nodes=@x@(11)),assert(j=@x@(""))
	;     - Check removal
	do UNXREFDATA^%YDBAIM(x),assert('$data(@x))
	;   Done testing entire node for type=0
	kill ^x
	; - Nodes with pieces
	set npieces=2+$random(5)		; No. of pieces in simulated application data
	set nsubs=1+$random(3)
	set nodes=$$aim73genblk(nsubs,10000,npieces,1000)
	set pnum=1+$random(npieces)		; Choose a piece number to test
	set varpat=$ztranslate($$aim73genpat(nsubs),3,4)
	;   - No statistics
	;     - Initial scan
	set x=$$XREFDATA^%YDBAIM("^x",nsubs,"|",pnum,,,,,,1)
	set y="" for  set y=$order(@x@(pnum,y)) quit:'$zlength(y)  do assert("#"=$zextract(y,1))
	;     - Check set trigger
	set $zpiece(@varpat,"|",pnum)=testval,ref=$reference do assert(10=$data(@x@(pnum,tvxref)))
	;     - Check kill trigger
	kill @ref do assert('$data(@x@(pnum,tvxref)))
	;     - Check removal
	do UNXREFDATA^%YDBAIM(x),assert('$data(@x))
	;   - Statistics=1
	;     - Initial scan
	set x=$$XREFDATA^%YDBAIM("^x",nsubs,"|",pnum,,,,1,,1)
	set y="" for  set y=$order(@x@(pnum,y)) quit:'$zlength(y)  do assert("#"=$zextract(y,1))
	set i=0,y="" for  set y=$order(@x@(-pnum,y)) quit:'$zlength(y)  if $increment(i,@x@(-pnum,y))
	do assert(nodes=i)    ; Confirm that all nodes are counted
	;     - Check set trigger
	set varpat=$ztranslate(varpat,4,5)
	set $zpiece(@varpat,"|",pnum)=testval,ref=$reference
	do assert(10=$data(@x@(pnum,tvxref))),assert(1=@x@(-pnum,tvxref))
	;     - Check kill trigger
	kill @ref do assert('$data(@x@(pnum,tvxref))),assert('$data(@x@(-pnum,tvxref)))
	;     - Check removal
	do UNXREFDATA^%YDBAIM(x),assert('$data(@x))
	;   - Statistics=2
	;     - Initial scan
	set x=$$XREFDATA^%YDBAIM("^x",nsubs,"|",pnum,,,,2,,1)
	set y="" for j=0:1 set y=$order(@x@(pnum,y)) quit:'$zlength(y)  do assert("#"=$zextract(y,1))
	set i=0,y="" for  set y=$order(@x@(-pnum,y)) quit:'$zlength(y)  if $increment(i,@x@(-pnum,y))
	do assert(nodes=i),assert(nodes=@x@(11)),assert(j=@x@(-pnum))
	;     - Check set trigger
	set varpat=$ztranslate(varpat,5,6)
	set $zpiece(@varpat,"|",pnum)=testval,ref=$reference
	do assert(10=$data(@x@(pnum,tvxref))),assert(1=@x@(-pnum,tvxref)),assert(nodes+1=@x@(11)),assert(j+1=@x@(-pnum))
	;     - Check kill trigger
	kill @ref
	do assert('$data(@x@(pnum,tvxref))),assert('$data(@x@(-pnum,tvxref))),assert(nodes=@x@(11)),assert(j=@x@(-pnum))
	;     - Check removal
	do UNXREFDATA^%YDBAIM(x),assert('$data(@x))
	;   Done testing pieces of nodes for type=0
	kill ^x
	; Since type 1 globals are somewhat specialized, the type 1 tests in this suite have been
	; enhanced to test force=0 (the default) or force=1.
	quit
	;
aim73tut003	; @TEST Test fix for TUT003 test failure with AIM#73 code
	new subs,x
	kill ^x
	set subs(1)=":"
	do UNXREFDATA^%YDBAIM("^x",.subs,"","",0,0,1,2,0,1)
	set x=$$XREFDATA^%YDBAIM("^x",.subs,"","",0,0,1,2,0,1)
	set ^x(1)="Old"
	do assert($data(@x@(0,"#Old",1)))
	set ^x(1)="New"
	do assert($data(@x@(0,"#New",1)))
	do assert('$data(@x@(0,"#Old",1)))
	quit
aim73genblk(nsub,nodes,pieces,max,c)	; [private] generate a bunch of data in ^x for the aim73 test
	new i,varpat,x
	set varpat=$$aim73genpat(nsub,$get(c,0))_"="
	for i=1:1:pieces set varpat=varpat_"$random(max)_""|""_"
	set $zextract(varpat,$zlength(varpat)-4,$zlength(varpat))=""
	for i=1:1:nodes set @varpat
	; Count nodes, since it is possible to have generated duplicate subscripts above
	set x="^x("""")" for i=0:1 set x=$query(@x) quit:'$zlength(x)
	quit:$quit i quit
	;
aim73genpat(nsub,c)			; [private] generate a ^x random reference with number of subs & optionally an extra constant subscript
	new i,varpat
	set varpat="^x("
	for i=1:1:nsub set varpat=varpat_"$$^%RANDSTR(3,,""an""),"
	set:$get(c,0) varpat=varpat_($random(1000)/(10**$random(4)))_","
	set $zextract(varpat,$zlength(varpat))=")"
	quit varpat
	;
aim74	; @TEST for YDBAIM#74
	new ret,subs
	kill ^x
	for subs(2)="""/""","""ab/""","""/cd""","""ab/cd""" do
	. set ^x(1,$zwrite(subs(2),1))="abcd",ret=$$XREFDATA^%YDBAIM("^x",.subs,"|",1,0,0,1,2,1,1)
	. do assert(1=@ret@(-1,"#abcd"))
	. do UNXREFDATA^%YDBAIM("^x",.subs,"|",1,0,0,1,2,1,1)
	. do assert('$data(@ret))
	kill ^x
	quit
	;
aim77	; @Test for YDBAIM77
	new force,i,j,aimgbl,stat,sub
	kill ^x
	for i=1:1:100000 set ^x(i,i)=i
	set aimgbl=$$XREFDATA^%YDBAIM("^x",2,,,,,,,2,"$$aim77neg^%YDBAIMTEST()")
	set j=1+$random(i)
	do assert($data(@aimgbl@(0,-j,j,j)))
	zkill ^x(j,j)
	do assert('$data(@aimgbl@(0,-j,j,j)))
	set:$increment(i) ^x(i,i)=i
	do assert($data(@aimgbl@(0,-i,i,i)))
	kill:$increment(j) ^x(j,j)
	do assert('$data(@aimgbl@(0,-j,j,j)))
	do UNXREFDATA^%YDBAIM("^x",2,,,,,,,2,"$$aim77neg^%YDBAIMTEST()")
	do assert('$data(@aimgbl))
	set aimgbl=$$XREFDATA^%YDBAIM("^x",2,,,,,,1,2,"$$aim77neg^%YDBAIMTEST(,1)")
	do assert(1=@aimgbl@("",-i))
	set (^x(j,j),^x(j-1,j-1))=i
	do assert(3=@aimgbl@("",-i))
	zkill ^x(i,i)
	do assert(2=@aimgbl@("",-i))
	do UNXREFDATA^%YDBAIM("^x",2,,,,,,1,2,"$$aim77neg^%YDBAIMTEST(,1)")
	do assert('$data(@aimgbl))
	set aimgbl=$$XREFDATA^%YDBAIM("^x",2,,,,,,2,2,"$$aim77neg^%YDBAIMTEST(,$$FUNC^%DH($system))")
	do assert(99999=@aimgbl@("")),assert(100000=@aimgbl@(11))
	set ^x(0,0)=0
	do assert(100000=@aimgbl@("")),assert(100001=@aimgbl@(11))
	do UNXREFDATA^%YDBAIM("^x",2,,,,,,2,2,"$$aim77neg^%YDBAIMTEST(,$$FUNC^%DH($system))")
	do assert('$data(@aimgbl))
	kill ^x
	for i=1:1:100000 set ^x(i,i)=$random(100000)_"|"_i_"|"_$random(100000)
	set aimgbl=$$XREFDATA^%YDBAIM("^x",2,"|",2,,,,,2,"$$aim77neg^%YDBAIMTEST(,""Some garbage"")")
	set j=1+$random(i)
	do assert($data(@aimgbl@(2,-j,j,j)))
	zkill ^x(j,j)
	do assert('$data(@aimgbl@(2,-j,j,j)))
	set:$increment(i) ^x(i,i)=$random(100000)_"|"_i_"|"_$random(100000)
	do assert($data(@aimgbl@(2,-i,i,i)))
	kill:$increment(j) ^x(j,j)
	do assert('$data(@aimgbl@(2,-j,j,j)))
	do UNXREFDATA^%YDBAIM("^x",2,"|",2,,,,,2,"$$aim77neg^%YDBAIMTEST(,""Some garbage"")")
	do assert('$data(@aimgbl))
	set aimgbl=$$XREFDATA^%YDBAIM("^x",2,"|",2,,,,1,2,"$$aim77neg^%YDBAIMTEST()")
	do assert(1=@aimgbl@(-2,-i))
	set (^x(j,j),^x(j-1,j-1))=$random(100000)_"|"_i_"|"_$random(100000)
	do assert(3=@aimgbl@(-2,-i))
	zkill ^x(j,j)
	do assert(2=@aimgbl@(-2,-i))
	kill ^x(j-1,j-1)
	do assert(1=@aimgbl@(-2,-i))
	do UNXREFDATA^%YDBAIM("^x",2,"|",2,,,,1,2,"$$aim77neg^%YDBAIMTEST()")
	do assert('$data(@aimgbl))
	set aimgbl=$$XREFDATA^%YDBAIM("^x",2,"|",2,,,,2,2,"$$aim77neg^%YDBAIMTEST()")
	do assert(99999=@aimgbl@(11)),assert(99999=@aimgbl@(-2))
	set:$increment(i) ^x(i,i)=$random(100000)_"|"_i_"|"_$random(100000)
	do assert(100000=@aimgbl@(11)),assert(100000=@aimgbl@(-2))
	set (^x(j,j),^x(j-1,j-1))=$random(100000)_"|"_i_"|"_$random(100000)
	do assert(100002=@aimgbl@(11)),assert(100000=@aimgbl@(-2))
	zkill ^x(i,i)
	do assert(100001=@aimgbl@(11)),assert(100000=@aimgbl@(-2))
	kill ^x(j,j),^x(j-1,j-1)
	do assert(99999=@aimgbl@(11)),assert(99999=@aimgbl@(-2))
	do UNXREFDATA^%YDBAIM("^x",2,"|",2,,,,2,2,"$$aim77neg^%YDBAIMTEST()")
	do assert('$data(@aimgbl))
	kill ^x
	set sub(1)="*",sub(2)=2
	; The following are not strictly part of YDBAIM#77, but are included here since
	; the omission was discovered during the code review
	; (https://gitlab.com/YottaDB/Util/YDBAIM/-/merge_requests/76#note_2166696698).
	; No type 1 test because of bug YDBAIM#79.
	for stat=0:1:2 do
	. set ^x(1,2)="a",x=$$XREFDATA^%YDBAIM("^x",.sub,"|",2,,,1,stat,0),^x(7,2)="e||f"
	. do assert($data(@x@(2,"",1))),assert($data(@x@(2,"",7)))
	. if stat do assert(2=@x@(-2,"")) if stat-1 do assert(1=@x@(-2)),assert(2=@x@(11))
	. do UNXREFDATA^%YDBAIM("^x",.sub,"|",2,,,1,stat,0) kill ^x
	set force="$$aim77neg^%YDBAIMTEST()"
	for stat=0:1:2 do
	. set ^x(1,2)="a",x=$$XREFDATA^%YDBAIM("^x",.sub,"|",2,,,1,stat,2,force),^x(7,2)="e||f"
	. do assert($data(@x@(2,"",1))),assert($data(@x@(2,"",7)))
	. if stat do assert(2=@x@(-2,"")) if stat-1 do assert(1=@x@(-2)),assert(2=@x@(11))
	. do UNXREFDATA^%YDBAIM("^x",.sub,"|",2,,,1,stat,2,force) kill ^x
	quit
	;
aim77neg(val,ignore)	quit $select($zlength(val):-val,1:"")	;transformation function negative of value
	;
tinv1	; @TEST Invalid Input: Global without ^
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$T(+0)
	if $$XREFDATA^%YDBAIM("ORD",3,"^",2)
	do assert(ecodetest="U252")
	quit
	;
tinv2	; @TEST Invalid Input: Global including subscripts
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$T(+0)
	if $$XREFDATA^%YDBAIM("^ORD(100.01)",2,"^",2)
	do assert(ecodetest="U252")
	quit
	;
tinv3	; @TEST Invalid Input: Bad Level
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$T(+0)
	if $$XREFDATA^%YDBAIM("^ORD",0,"^",2)
	do assert(ecodetest="U253")
	new ecodetest
	if $$XREFDATA^%YDBAIM("^ORD",-1,"^",2)
	do assert(ecodetest="U253")
	quit
	;
tinv4	; @TEST Invalid Input: Bad Piece Number
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$T(+0)
	if $$XREFDATA^%YDBAIM("^ORD",3,"^",0)
	do assert(ecodetest="U248")
	new ecodetest
	if $$XREFDATA^%YDBAIM("^ORD",3,"^",-1)
	do assert(ecodetest="U248")
	quit
	;
	;
tinv5	; @TEST Invalid Input: More subs than the number in top node
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$T(+0)
	new subs set subs=1,subs(1)="""""",subs(2)="A"
	if $$XREFDATA^%YDBAIM("^null",.subs)
	do assert(ecodetest="U244")
	quit
	;
tinv6	; @TEST Invalid Input: subs contains negative subscripts
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$T(+0)
	new subs set subs=1,subs(-1)="""""",subs(-2)="A"
	if $$XREFDATA^%YDBAIM("^null",.subs)
	do assert(ecodetest="U247")
	quit
	;
tinv7	; @TEST Invalid Input: one of the subs is an empty string
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$T(+0)
	new subs set subs(1)=100.01,subs(2)="",subs(3)=0
	if $$XREFDATA^%YDBAIM("^ORD",.subs,"^",2)
	do assert(ecodetest="U244")
	quit

tinv8	; @TEST Test interaction of separators vs pieces
	; Separator without piece
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$T(+0)
	if $$XREFDATA^%YDBAIM("^customers",1,"§")
	do assert(ecodetest="U250")
	;
	; piece without separator
	new ecodetest
	if $$XREFDATA^%YDBAIM("^customers",1,"",2)
	do assert(ecodetest="U238")
	quit
	;
tinv9	; @TEST Request a higher level of stat after a lower level
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$T(+0)
	if $$XREFDATA^%YDBAIM("^customers",1,"§",1,0,,,1)
	if $$XREFDATA^%YDBAIM("^customers",1,"§",1,0,,,2)
	do assert(ecodetest="U240")
	quit
	;
tinv10	; @TEST invalid stat parameter value
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$text(+0)
	if $$XREFDATA^%YDBAIM("^customers",1,"§",1,0,,,-1)
	do assert(ecodetest="U241")
	kill ecodetest
	if $$XREFDATA^%YDBAIM("^customers",1,"§",1,0,,,5)
	do assert(ecodetest="U241")
	quit
	;
tinv11	; @TEST indexing invalid repeated subscripts
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$text(+0)
	kill ^x
	set ^x(1)="abcd",^x(2)=""
	new subs,xref
	set subs(1)="1:1" set xref=$$XREFDATA^%YDBAIM("^x",.subs)
	do assert(ecodetest="U244")
	quit
	;
tinv12	; @TEST index spanning regions where null setting differs
	; Detailed discussion located at https://gitlab.com/YottaDB/Util/YDBAIM/-/merge_requests/29#note_654447677
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$text(+0)
	; This should not issue an error as the null setting is the same across regions
	new xref set xref=$$XREFDATA^%YDBAIM("^sameset",2)
	do assert($data(@xref@(0,"test1",1,"")))
	; This should issue an error
	new xref set xref=$$XREFDATA^%YDBAIM("^diffset",2)
	do assert(xref="")
	do assert(ecodetest="U251")
	quit
	;
err	; Error trap for tinv* tests
	; Capture $ecode, strip commas, clear
	set ecodetest=$ecode
	set ecodetest=$piece(ecodetest,",",2) ; ,U252, -> U252
	set $ecode=""
	quit:$quit "" quit
	;
tsubs1	; @TEST XREFDATA with numbers & : as subs
	; Index single node in VistA Global; no sub at top node
	new subs set subs(1)=100.01,subs(2)=":",subs(3)=0
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",2)
	do assert($data(@aimgbl@(2,"dc",1)),"subs array works with numeric subscripts,: for varying range")
	; Add data and make sure triggers works
	set ^ORD(100.01,100,0)="JUNK STATUS^junk"
	do assert($data(@aimgbl@(2,"junk",100)),"set trigger works")
	; Delete data and check that kill trigger works
	new keepme merge keepme=^ORD(100.01,99)
	kill ^ORD(100.01,99)
	do assert('$data(@aimgbl@(2,"none",99)),"kill trigger works")
	; Restore data to the way it was for later tests
	merge ^ORD(100.01,99)=keepme
	kill ^ORD(100.01,100)
	quit
	;
tsubs2	; @TEST subs w strings,subs>subs(n)/Index Octo Global
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_namespace""",subs=4
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",1)
	do assert($data(@aimgbl@(1,"public",2200)),"string subs work,subs>sub(n) works")
	do assert('$data(@aimgbl@(1,11)),"unrelated data elsewhere is not indexed")
	; Add data and make sure triggers works
	set ^%ydboctoocto("tables","pg_catalog","pg_namespace",11806)="pg_toast_temp_2|10|"
	do assert($data(@aimgbl@(1,"pg_toast_temp_2",11806)),"New data now exists")
	; kill for later tests
	kill ^%ydboctoocto("tables","pg_catalog","pg_namespace",11806)
	; Delete data ditto
	new %aim set %aim=^%ydboctoocto("tables","pg_catalog","pg_namespace",11)
	kill ^%ydboctoocto("tables","pg_catalog","pg_namespace",11)
	do assert('$data(@aimgbl@(1,"pg_namespace")),"Deleted data removed from index")
	; restore for later tests
	set ^%ydboctoocto("tables","pg_catalog","pg_namespace",11)=%aim
	quit
	;
tsubs3	; @TEST index non-numeric data in last sub;
	; Use * to specify last sub, top level subs not specified
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",12)
	do assert($data(@aimgbl@(12,1231,"numeric")),"non-numeric data properly indexed")
	quit
	;
tsubs4	; @TEST index different subs in same global
	; Test that it produces different aim globals; omit last
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_namespace""",subs=4
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",1)
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",1)
	do assert(aimgbl1'=aimgbl2)
	quit
	;
tsubs5	; @TEST index different pieces in the same global
	; Test that it produces the same aim global
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",12)
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",11)
	do assert(aimgbl1=aimgbl2)
	quit
	;
	;
tallsub	; @TEST index all subscripts at a specific level
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^%ydboctoocto",4)
	do assert($data(@aimgbl@(0,"pg_toast|10|","tables","pg_catalog","pg_namespace",99)))
	; Add data and make sure triggers work
	set ^%ydboctoocto("tables","pg_catalog","pg_namespace",11806)="pg_toast_temp_2|10|"
	do assert($data(@aimgbl@(0,"pg_toast_temp_2|10|","tables","pg_catalog","pg_namespace",11806)))
	kill ^%ydboctoocto("tables","pg_catalog","pg_namespace",11806)
	;
	; Remove data and make sure triggers work
	do assert($data(@aimgbl@(0,"pg_catalog|10|""{postgres=UC/postgres|=U/postgres}""","tables","pg_catalog","pg_namespace",11)))
	new % set %=^%ydboctoocto("tables","pg_catalog","pg_namespace",11)
	kill ^%ydboctoocto("tables","pg_catalog","pg_namespace",11)
	do assert('$data(@aimgbl@(0,"pg_catalog|10|""{postgres=UC/postgres|=U/postgres}""","tables","pg_catalog","pg_namespace",11)))
	set ^%ydboctoocto("tables","pg_catalog","pg_namespace",11)=%
	quit
	;
tnodata1 ; @TEST indexing nodes that don't exist
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",1,"^",2)
	do assert($order(@aimgbl@(0,""))="")
	quit
	;
tnodata2 ; @TEST indexing pieces that don't exist
	; Note, this changed from the original behavior as a result of https://gitlab.com/YottaDB/Util/YDBAIM/-/merge_requests/27
	; Previous behavior is that these pieces were not indexed. New behavior is that the pieces are indexed.
	; 17th piece does not exist
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^customers",1,"§",17)
	do assert($data(@aimgbl@(17,"",1)))
	do assert($data(@aimgbl@(17,"",2)))
	do assert($data(@aimgbl@(17,"",3)))
	quit
	;
tnodata3 ; @TEST indexing fixed nodes that don't exist
	; Default behavior is for nodes that don't exist to not be xrefed. Verify that.
	; Once #51 is fixed, there will be an extra parameter to specify how non-existent fixed nodes
	; need to be handled and at that time this test case can be enhanced to additionally verify
	; the new behavior.
	kill ^tnodata3
	set ^tnodata3(100,1,"const1")="a"
	set ^tnodata3(100,2,"const1","const2")="b"
	set ^tnodata3(100,3,"const1")="c"
	set ^tnodata3(100,3,"const1","const2")="d"
	set ^tnodata3(100,4,"const3")="e"
	set ^tnodata3(100,5,"const1")="f"
	set ^tnodata3(100,5,"const1","const4")="g"
	set ^tnodata3(100,6,"const1","const2","const3")="h"
	new subs,aimgbl
	set subs(1)=100
	set subs(2)=":"
	set subs(3)="""const1"""
	set subs(4)="""const2"""
	do UNXREFDATA^%YDBAIM("^tnodata3",.subs)
	set aimgbl=$$XREFDATA^%YDBAIM("^tnodata3",.subs)
	; Verify the nodes that we expect to exist in the xref
	do assert(""=@aimgbl@(0,"b",2)) kill @aimgbl@(0,"b",2)
	do assert(""=@aimgbl@(0,"d",3)) kill @aimgbl@(0,"d",3)
	; Verify that no other nodes exist in the xref
	do assert(1=$data(@aimgbl@(0)))
	quit
	;
tutf8tp	; @TEST UTF-8 data; seq pieces; updates (kill, set $piece)
	; 3rd piece = emails
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM("^customers",1,"§",3)
	do assert($data(@aimgbl1@(3,"gwashington@usa.gov")))
	; 7rd piece = zip codes
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM("^customers",1,"§",7)
	do assert(aimgbl1=aimgbl2,"AIM global for separate pieces should be the same")
	do assert($data(@aimgbl2@(7,22902,5)))
	;
	; Kill record 2
	do assert($data(@aimgbl1@(3,"jadams@usa.gov",2)))
	do assert($data(@aimgbl1@(7,"02169",2)))
	new % set %=^customers(2)
	kill ^customers(2)
	do assert('$data(@aimgbl1@(3,"jadams@usa.gov",2)))
	do assert('$data(@aimgbl1@(7,"02169",2)))
	set ^customers(2)=%
	;
	; Modify record 1 email address
	do assert($data(@aimgbl1@(3,"gwashington@usa.gov",1)))
	set %=$piece(^customers(1),"§",3)
	set $piece(^customers(1),"§",3)="gwashington@yottadb.com"
	do assert($data(@aimgbl1@(3,"gwashington@yottadb.com",1)))
	set $piece(^customers(1),"§",3)=%
	;
	; Add Record 6 (with a twist, 7th piece is not specified)
	set ^customers(6)="Road§Runner§rr@yottadb.com"
	do assert($data(@aimgbl1@(3,"rr@yottadb.com",6)))
	do assert($data(@aimgbl1@(7,"",6)))
	kill ^customers(6)
	quit
	;
tmpiece	; @TEST multiple pieces requested sequentially
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM("^customers",1,"§","3;7")
	new lines do trigout(.lines)
	do assert(lines(2)'["-delim=""§""")
	do assert($order(@aimgbl1@(3,""))'="")
	do assert($order(@aimgbl1@(7,""))'="")
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM("^customers",1,"§","3:4")
	do assert(aimgbl1=aimgbl2)
	do assert($order(@aimgbl1@(4,""))'="")
	kill lines do trigout(.lines)
	do assert(lines(3)["=3,4,7")
	new aimgbl3 set aimgbl3=$$XREFDATA^%YDBAIM("^customers",1,"§","4:5")
	kill lines do trigout(.lines)
	do assert(lines(3)["=3,4,5,7")
	new aimgbl4 set aimgbl4=$$XREFDATA^%YDBAIM("^customers",1,"§","5:8;10")
	kill lines do trigout(.lines)
	do assert(lines(3)["=3,4,5,6,7,8,10")
	new aimgbl5 set aimgbl5=$$XREFDATA^%YDBAIM("^customers",1,"§","1;2;5")
	kill lines do trigout(.lines)
	do assert(lines(3)["=1,2,3,4,5,6,7,8,10")
	do assert($order(@aimgbl1@(1,""))'="")
	do assert($order(@aimgbl1@(2,""))'="")
	do assert($order(@aimgbl1@(3,""))'="")
	do assert($order(@aimgbl1@(4,""))'="")
	do assert($order(@aimgbl1@(5,""))'="")
	do assert($order(@aimgbl1@(6,""))'="")
	do assert($order(@aimgbl1@(7,""))'="")
	quit
	;
tzpiece1 ; @TEST $zpiece
	; Indexing emails (3rd piece) - should see $ZCH(167), leftover from $ZCH(167,194) = §'
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^customers",1,$ZCHAR(194),3,,1)
	do assert($data(@aimgbl@(3,$ZCH(167)_"gwashington@usa.gov")))
	quit
	;
tzpiece2 ; @TEST $piece and $zpiece concurrently
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM("^customers",1,$ZCHAR(194),3,,1)
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM("^customers",1,"§",3)
	do assert(aimgbl1'=aimgbl2)
	quit
	;
trmindex1 ; #TEST Remove a single piece index (NOT SUPPORTED RIGHT NOW; disabling test)
	; Ensure no globals or triggers exists
	new trigs do trigout(.trigs)
	new aims  do aimgbls(.aims)
	do assert('$data(trigs(1)))
	do assert('$data(aims))
	;
	if $$XREFDATA^%YDBAIM("^customers",1,"§",3)
	if $$XREFDATA^%YDBAIM("^customers",1,"§",7)
	;
	new trigs do trigout(.trigs)
	new aims  do aimgbls(.aims)
	do assert(trigs(2)'["-delim=""§"" -pieces=3;7")
	do assert(aims=1)
	;
	do UNXREFDATA^%YDBAIM("^customers",1,"§",7)
	new trigs do trigout(.trigs)
	new aims  do aimgbls(.aims)
	do assert(trigs(2)'["-delim=""§"" -pieces=3")
	do assert(aims=1)
	quit
	;
trmindex2 ; @TEST Remove indexs on a specific entire global
	; Note: this test depends on the AIM global for ^customers preceding those for
	; ^orders. This is the case with YottaDB r1.32 on x86_64. Depending on the
	; hash used by AIM and the underlying platform, this may need to change for
	; future releases of YottaDB and for other platforms.
	if $$XREFDATA^%YDBAIM("^orders",1,"§",1)
	if $$XREFDATA^%YDBAIM("^customers",1,"§",3)
	if $$XREFDATA^%YDBAIM("^customers",1,"§",7)
	new trigs do trigout(.trigs)
	new aims  do aimgbls(.aims)
	new aimx  do aimdxref(.aimx)
	do assert(aims=2)
	do assert(aimx=2)
	do assert(trigs(3)["=3,7")
	do UNXREFDATA^%YDBAIM("^customers",1,"§")
	kill trigs do trigout(.trigs)
	kill aims  do aimgbls(.aims)
	kill aimx  do aimdxref(.aimx)
	do assert(aims=1)
	do assert(aimx=1)
	do assert(trigs(3)'["=3,7")
	quit
	;
trmindex3 ; @TEST Remove all indexes
	if $$XREFDATA^%YDBAIM("^orders",1,"§",1)
	if $$XREFDATA^%YDBAIM("^customers",1,"§",3)
	if $$XREFDATA^%YDBAIM("^customers",1,"§",7)
	new trigs do trigout(.trigs)
	new aims  do aimgbls(.aims)
	new aimx  do aimdxref(.aimx)
	do assert(aims=2)
	do assert(trigs(3)["=3,7")
	do assert(aimx=2)
	do UNXREFDATA^%YDBAIM
	kill trigs do trigout(.trigs)
	kill aims  do aimgbls(.aims)
	kill aimx  do aimdxref(.aimx)
	do assert('$data(trigs))
	do assert('$data(aims))
	do assert('$data(aimx))
	quit
	;
trmindex4 ; @TEST Remove index on subscripts parts of same global
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",1)
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_namespace""",subs(4)="*"
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",2)
	new aims do aimgbls(.aims)
	new aimx do aimdxref(.aimx)
	do assert(aims=2)
	do assert(aimx=1) ; single global
	kill subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	do UNXREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",1)
	kill aims do aimgbls(.aims)
	kill aimx do aimdxref(.aimx)
	do assert(aims=1)
	do assert(aimx=1)
	kill subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_namespace""",subs(4)="*"
	do UNXREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",2)
	kill aims do aimgbls(.aims)
	kill aimx do aimdxref(.aimx)
	do assert('$data(aims))
	do assert('$data(aimx))
	quit
	;
trmindex5 ; @TEST Remove indexes by %ydbAIMD global name
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM("^orders",1,"§",1)
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM("^customers",1,"§",3)
	new aimgbl3 set aimgbl3=$$XREFDATA^%YDBAIM("^customers",1,"§",7)
	new aims do aimgbls(.aims)
	new aimx  do aimdxref(.aimx)
	do assert(aims=2)
	do assert(aimx=2)
	do UNXREFDATA^%YDBAIM(aimgbl1)
	kill aims do aimgbls(.aims)
	kill aimx do aimdxref(.aimx)
	do assert(aims=1)
	do assert(aimx=1)
	do UNXREFDATA^%YDBAIM(aimgbl2)
	kill aims do aimgbls(.aims)
	kill aimx do aimdxref(.aimx)
	do assert('$data(aims))
	do assert('$data(aimx))
	quit
	;
tresume ; @TEST Resuming an interrupted cross-reference
	; Start job, wait, and interrupt
	new subs set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^PSNDF",.subs,"^",1,1) ; nmonly
	new jobpid job tresumejob:passcurlvn set jobpid=$zjob
	for  quit:$data(^%ydbAIMtmp("%YDBAIM",jobpid,0))  hang .01
	if $ZSIGPROC(jobpid,"SIGUSR1") ; stop the job (job does zgoto 0 in $zint which stops it)
	;
	; Ensure that AIM did not certify this index complete (3 node)
	do assert('$data(@aimgbl@(3)))
	;
	; Ensure that the record count is greater than the index count
	new count,indexcount
	set count=$$tresumecountdata()
	set indexcount=$$tresumecountindex()
	do assert(count>indexcount)
	;
	; Resume building
	if $$XREFDATA^%YDBAIM("^PSNDF",.subs,"^",1)
	;
	; Now verify that AIM says it's complete, and verify index count
	set indexcount=$$tresumecountindex()
	do assert(count=indexcount)
	do assert($data(@aimgbl@(3)))
	quit
	;
tresumejob ; [job target - tresume target]
	set $zinterrupt="if $zjobexam() zgoto 0"
	if $$XREFDATA^%YDBAIM("^PSNDF",.subs,"^",1)
	quit
	;
tresumecountdata:() ; [$$: # of data items in ^PSNDF(50.6)]
	new count set count=0
	new i for i=0:0 set i=$order(^PSNDF(50.6,i)) quit:'i  if $data(^(i,"VUID")) if $increment(count)
	quit count
	;
tresumecountindex:() ; [$$: # of index items in AIM global for ^PSNDF(50.6)]
	new indexcount set indexcount=0
	new i,j set i="" for  set i=$order(@aimgbl@(1,i)) quit:'$zlength(i)  do
	. set j="" for  set j=$order(@aimgbl@(1,i,j)) quit:'$zlength(j)  if $increment(indexcount)
	quit indexcount
	;
trange1 ; @TEST Numeric Subscript Range
	; ^ORD(100.01,1,0)="DISCONTINUED^dc"
	; ^ORD(100.01,2,0)="COMPLETE^comp"
	; ^ORD(100.01,3,0)="HOLD^hold"
	; ^ORD(100.01,4,0)="FLAGGED^flag"
	; ^ORD(100.01,5,0)="PENDING^pend"
	; ^ORD(100.01,6,0)="ACTIVE^actv"
	; ^ORD(100.01,7,0)="EXPIRED^exp"
	; ^ORD(100.01,8,0)="SCHEDULED^schd"
	; ^ORD(100.01,9,0)="PARTIAL RESULTS^part"
	; ^ORD(100.01,10,0)="DELAYED^dlay"
	; ^ORD(100.01,11,0)="UNRELEASED^unr"
	; ^ORD(100.01,12,0)="DISCONTINUED/EDIT^dc/e"
	; ^ORD(100.01,13,0)="CANCELLED^canc"
	; ^ORD(100.01,14,0)="LAPSED^laps"
	; ^ORD(100.01,15,0)="RENEWED^rnew"
	; ^ORD(100.01,99,0)="NO STATUS^none"
	new subs set subs(1)=100.01,subs(2)="1:9",subs(3)=0
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",2)
	do assert($data(@aimgbl@(2,"hold",3)))
	do assert('$data(@aimgbl@(2,"dlay",10)))
	quit
	;
trange2 ; @TEST String Subscript Range
	kill ^sam
	set ^sam("a",0,1)="a1|a2"
	set ^sam("b",0,2)="b1|b2"
	set ^sam("c",0,3)="c1|c2"
	new subs set subs(1)="""a"":""b""",subs(2)=0,subs=3
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^sam",.subs,"|",2)
	do assert($data(@aimgbl@(2,"a2","a",1)))
	do assert($data(@aimgbl@(2,"b2","b",2)))
	do assert('$data(@aimgbl@(2,"c2","c",3)))
	kill ^sam
	quit
	;
trange3	; #TEST indexing subscripts with range symbols embedded
	; This test is disabled until issue YottaDB/DB/YDB#691 is implemented.
	kill ^x
	set ^x("dummy1;:",1)="abcd",^x("dummy1;:",2)=""
	new subs,xref
	set subs(1)="""dummy1;:""",subs(2)="*" set xref=$$XREFDATA^%YDBAIM("^x",.subs)
	do assert($data(@xref@(0,"abcd")))
	quit
	;
trange4	; @TEST indexing subscripts with an open range
	kill ^x
	set ^x(1)="abcd",^x(2)=""
	new subs,xref
	set subs(1)="1:" set xref=$$XREFDATA^%YDBAIM("^x",.subs)
	do assert($data(@xref@(0,"abcd")))
	quit
	;
trange5	; @TEST indexing repeated subscripts 1
	kill ^x
	set ^x(1)="abcd",^x(2)=""
	new subs,xref
	set subs(1)="1;1" set xref=$$XREFDATA^%YDBAIM("^x",.subs)
	do assert($data(@xref@(0,"abcd")))
	quit
	;
tnull	; @TEST Index data with null subscripts
	; Index everything (no fixed subs)
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM("^null",2)
	do assert($data(@aimgbl1@(0,"test1","","")))
	do assert($data(@aimgbl1@(0,"test2","","A")))
	do assert($data(@aimgbl1@(0,"test5",5,"")))
	;
	; Index with null as a fixed sub
	new subs set subs(1)="""""",subs=2
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM("^null",.subs)
	do assert($data(@aimgbl2@(0,"test1","")))
	do assert($data(@aimgbl2@(0,"test2","A")))
	do assert($data(@aimgbl2@(0,"test4",4)))
	quit
	;
tomitfix1 ; @TEST Omit fixed subscripts aka omitfix
	; Default is to omit fixed subscripts.
	; Assert that nodes that are expected are present and those that are not are not.
	new subs set subs(1)="""""",subs=2
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM("^null",.subs)
	; ^%ydbAIMDTeLUKXq5NuulyAqUDqoy1A(0,"test1","")=""
	; ^%ydbAIMDTeLUKXq5NuulyAqUDqoy1A(0,"test2","A")=""
	; ^%ydbAIMDTeLUKXq5NuulyAqUDqoy1A(0,"test4",4)=""
	do assert($data(@aimgbl1@(0,"test1","")))
	do assert($data(@aimgbl1@(0,"test2","A")))
	do assert($data(@aimgbl1@(0,"test4",4)))
	do assert('$data(@aimgbl1@(0,"test1","","")))
	do assert('$data(@aimgbl1@(0,"test2","","A")))
	do assert('$data(@aimgbl1@(0,"test4","",4)))
	;
	; Now create with omitfix=0
	new subs set subs(1)="""""",subs=2
	; params = gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM("^null",.subs,"","","","",0)
	do assert($data(@aimgbl2@(0,"test1","","")))  ; <-- see note below
	do assert($data(@aimgbl2@(0,"test2","","A")))
	do assert($data(@aimgbl2@(0,"test4","",4)))
	do assert($data(@aimgbl2@(0,"test1",""))#2=0) ; This is special as we do have ("","") existing up above
	do assert('$data(@aimgbl2@(0,"test2","A")))
	do assert('$data(@aimgbl2@(0,"test4",4)))
	quit
	;
tomitfix2 ; @TEST Omitfix crash [#35]
	new subs set subs(1)=100.01,subs(2)=":",subs(3)=0
	; params = gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",2,0,0,0,0)
	do assert($data(@aimgbl))
	quit
	;
tnmonly	; @TEST Name only paramater aka nmonly
	; D UNXREFDATA^%YDBAIM
	; write "printing aims:",!
	; new aims  do aimgbls(.aims)
	; zwrite aims
	; write "----------",!
	new subs set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^PSNDF",.subs,"^",1,1) ; nmonly
	; zwrite @aimgbl@(*)
	do assert('$data(@aimgbl),"Shouldn't have been created yet")
	quit
	;
tlsxref1 ; @TEST LSXREFDATA normal tests
	; Test at beginning to assert that nothing exists to begin with
	new xrefs do LSXREFDATA^%YDBAIM(.xrefs)
	do assert('$data(xrefs))
	;
	; Now create indexes on 3 separate globals. 4 xrefs should exist
	new aimgbl1,aimgbl2,aimgbl3,aimgbl4,aimgbl5
	set aimgbl1=$$XREFDATA^%YDBAIM("^orders",1,"§",1)
	set aimgbl2=$$XREFDATA^%YDBAIM("^customers",1,"§",3) ; produces same xref as only piece is different
	set aimgbl3=$$XREFDATA^%YDBAIM("^customers",1,"§",7) ; ditto
	do assert(aimgbl2=aimgbl3)
	new subs set subs(1)="""""",subs=2
	set aimgbl4=$$XREFDATA^%YDBAIM("^null",.subs)         ; different xref as subs is different
	new aimgbl5 set aimgbl5=$$XREFDATA^%YDBAIM("^null",2) ; ditto
	;
	; Test without the second parameter to get all xrefs
	new xrefs do LSXREFDATA^%YDBAIM(.xrefs)
	new xref set xref=""
	new count for  set xref=$order(xrefs(xref)) quit:xref=""  if $increment(count)
	do assert(count=4)
	;
	; Test getting the xrefs by the xreffed global. ^null should have 2
	new xrefs do LSXREFDATA^%YDBAIM(.xrefs,"^null")
	new xref set xref=""
	new count for  set xref=$order(xrefs(xref)) quit:xref=""  if $increment(count)
	do assert(count=2)
	;
	; Test getting the xrefs by the AIM global. Should only return one entry.
	new xrefs do LSXREFDATA^%YDBAIM(.xrefs,aimgbl2)
	new xref set xref=""
	new count for  set xref=$order(xrefs(xref)) quit:xref=""  if $increment(count)
	do assert(count=1)
	quit
	;
tlsxref2 ; @TEST LSXREFDATA when non-AIM ^%ydbAIM* globals exist
	; Test that they do not crash LSXREFDATA
	; https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/898#note_653213876
	new subs set subs(1)="*"
	NEW aimglobal SET aimglobal=$$XREFDATA^%YDBAIM("^names",.subs,"|",2,0,0,1,0)
	set ^%ydbAIMOctoCache("NAMES","LASTNAME","completed?")=1
	set ^%ydbAIMOctoCache("NAMES","LASTNAME","location")=$name(@aimglobal@(2))
	new xrefs do LSXREFDATA^%YDBAIM(.xrefs)
	new xref set xref=""
	new count for  set xref=$order(xrefs(xref)) quit:xref=""  if $increment(count)
	do assert(count=1)
	quit
	;
tstat1	; @TEST stats of 1 and 2 produce the correct output
	; ^customers(1)="George§Washington§gwashington@usa.gov§3200 Mt Vernon Hwy§Mount Vernon§VA§22121"
	; ^customers(2)="John§Adams§jadams@usa.gov§1250 Hancock St§Quincy§MA§02169"
	; ^customers(3)="Thomas§Jefferson§tjefferson@usa.gov§931 Thomas Jefferson Pkwy§Charlottesville§VA§22902"
	; ^customers(4)="James§Madison§jmadison@usa.gov§11350 Constitution Hwy§Orange§VA§22960"
	; ^customers(5)="James§Monroe§jmonroe@usa.gov§2050 James Monroe Parkway§Charlottesville§VA§22902"
	;
	; params = gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat
	; Call with stat=1, which only provides data for the each value, not the total values
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM("^customers",1,"§",1,0,,,1)
	do assert(@aimgbl1@(-1,"George")=1)
	do assert(@aimgbl1@(-1,"James")=2)
	do assert($data(@aimgbl1@(-1))[0) ; Distinct values don't exist
	do assert($data(@aimgbl1@(11))[0) ; Total values don't exist
	;
	; Unxref
	DO UNXREFDATA^%YDBAIM("^customers",1,"§",1,0,,,1)
	do assert('$data(@aimgbl1))
	;
	; Call with stat=2, which provides data for the each value, and total and distinct values
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM("^customers",1,"§",1,0,,,2)
	do assert(@aimgbl2@(-1,"George")=1)
	do assert(@aimgbl2@(-1,"James")=2)
	do assert(@aimgbl2@(-1)=4) ; Distinct values
	do assert(@aimgbl2@(11)=5) ; Total values
	quit
	;
tstat2	; @TEST xref operations with stats=0,1,2
	; Test 3 cases each of which is expected to produce the same output
	; irrespective of whether stat=0, or stat=1 or stat=2.
	;
	new stat for stat=0:1:2 do tstat2run(stat)
	quit
	;
tstat2run(stat) ;
	new case
	; -----------------------------------------------------
	; Case 1
	; ------
	; SET 4 records first and then XREF all records in one shot
	; -----------------------------------------------------
	set case=1
	do UNXREFDATA^%YDBAIM
	kill ^names2
	set ^names2(0)="Zero|Cool"
	set ^names2(1)="Acid|Burn"
	set ^names2(2)="Cereal|Killer"
	set ^names2(3)="Zero|Nikon"
	set xref=$$XREFDATA^%YDBAIM("^names2",1,"|",1,,,,stat)
	do tstat2assertdata(xref,"Error with stat="_stat_" case="_case_" at "_$zpos)
	if stat>0 do tstat2assertstat1(xref,"Error with stat="_stat_" case="_case_" at "_$zpos)
	if stat=2 do tstat2assertstat2(xref,"Error with stat="_stat_" case="_case_" at "_$zpos)
	; -----------------------------------------------------
	; Case 2
	; ------
	; XREF no data first; Then SET 4 records to exercise trigger
	; Final result is same 4 records as Case 1.
	; -----------------------------------------------------
	set case=2
	DO UNXREFDATA^%YDBAIM
	kill ^names2
	set xref=$$XREFDATA^%YDBAIM("^names2",1,"|",1,,,,stat)
	set ^names2(0)="Zero|Cool"
	set ^names2(1)="Acid|Burn"
	set ^names2(2)="Cereal|Killer"
	set ^names2(3)="Zero|Nikon"
	do tstat2assertdata(xref,"Error with stat="_stat_" case="_case_" at "_$zpos)
	if stat>0 do tstat2assertstat1(xref,"Error with stat="_stat_" case="_case_" at "_$zpos)
	if stat=2 do tstat2assertstat2(xref,"Error with stat="_stat_" case="_case_" at "_$zpos)
	; -----------------------------------------------------
	; Case 3
	; ------
	; XREF 6 records first; Then KILL/ ZKILL 3 records and SET 1 record to exercise triggers
	; Final result is same 4 records as Case 1 and Case 2.
	; -----------------------------------------------------
	set case=3
	DO UNXREFDATA^%YDBAIM
	kill ^names2
	set ^names2(0)="Zero|Cool"
	set ^names2(1)="Acid|Burn"
	set ^names2(2)="Cereal|Killer"
	set ^names2(3)="Zero|Nikon"
	set ^names2(4)="Joey|"
	set ^names2(5)="Zero|Cool"
	set xref=$$XREFDATA^%YDBAIM("^names2",1,"|",1,,,,stat)
	zkill ^names2(3)
	kill ^names2(4)
	zkill ^names2(5)
	set ^names2(3)="Zero|Nikon"
	do tstat2assertdata(xref,"Error with stat="_stat_" case="_case_" at "_$zpos)
	if stat>0 do tstat2assertstat1(xref,"Error with stat="_stat_" case="_case_" at "_$zpos)
	if stat=2 do tstat2assertstat2(xref,"Error with stat="_stat_" case="_case_" at "_$zpos)
	quit
	;
tstat2assertdata(xref,message)
	new aims  do aimgbls(.aims)
	do assert(aims=1)
	do assert($data(@xref@(1,"Acid",1)),message)
	do assert($data(@xref@(1,"Cereal",2)),message)
	do assert($data(@xref@(1,"Zero",0)),message)
	do assert($data(@xref@(1,"Zero",3)),message)
	quit
	;
tstat2assertstat1(xref,message)
	new aims  do aimgbls(.aims)
	do assert(aims=1)
	do assert(@xref@(-1,"Acid")=1,message)
	do assert(@xref@(-1,"Cereal")=1,message)
	do assert(@xref@(-1,"Zero")=2,message)
	quit
	;
tstat2assertstat2(xref,message)
	new aims  do aimgbls(.aims)
	; zwrite aims
	do assert(aims=1)
	do assert(@xref@(-1)=3,message)
	do assert(@xref@(11)=4,message)
	quit
	;
tstat3	; @TEST Ensure that stat=2 produces correct triggers
	; https://gitlab.com/YottaDB/Util/YDBAIM/-/issues/47
	kill ^x
	new aim set aim=$$XREFDATA^%YDBAIM("^x",2,"|",1,0,0,1,2)
	do assert(aim'="")
	quit
	;
tstat4	; @TEST More general regression test of #47 than tstat3
	; Test that valid triggers get installed for stats=0,1,2 for random global names and subscript levels
	; #47 (%YDBAIM-F-SETZTRIGGERFAIL error) was seen only in certain global names and subscript levels depending
	; on whether the trigger name (minus the %ydb prefix) started with a decimal number or not. Since this is a
	; hexadecimal hash value, it could be a number (0-9) or (A-F). The latter case is okay as the first letter but
	; the former case is not. Hence the below $random() usage to try various global names and subscript levels
	; and ensure all of those continue to work fine.
	new stat,gblname,subslevel,$etrap
	; In case of an error (like the %YDBAIM-F-SETZTRIGGERFAIL error we saw in #47), display relevant random variables
	; in the error output so one can use that information to recreate the exact test that failed. Hence the zwrite below.
	set $etrap="write ! zwrite $zstatus zwrite:$data(gblname) gblname zwrite:$data(subslevel) subslevel zwrite:$data(stat) stat"
	set gblname=$$randgblname
	set subslevel=1+$random(16)
	for stat=0:1:2 do
	. set xref=$$XREFDATA^%YDBAIM(gblname,subslevel,"|",1,,,,stat)
	. set xref=$$UNXREFDATA^%YDBAIM(gblname,subslevel,"|",1,,,,stat)
	quit
	;
randgblname()	;
	; Returns a random valid global name
	new gblname,alphabet,alphabetlen,gblnamelen,i,alphanumeric,alphanumericlen
	set alphabet="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ%"	; Note: % is last character for a reason
	set alphabetlen=$length(alphabet)
	set numbers="0123456789"
	set alphanumeric=$extract(alphabet,1,alphabetlen-1)_numbers	; -1 to not remove % (not valid after first character)
	set alphanumericlen=$length(alphanumeric)
	set gblname="^"_$extract(alphabet,1+$random(alphabetlen))
	set gblnamelen=$random(31)
	for i=1:1:gblnamelen set gblname=gblname_$extract(alphanumeric,1+$random(alphanumericlen))
	quit gblname
;
tspeed1	; @TEST Index 1E6 rows of ^names(:)="A|B"
	;new reccount,indexcount set (reccount,indexcount)=0
	;new i for i=0:0 set i=$order(^names(i)) quit:'i  if $increment(reccount)
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^names",1,"|",2)
	;new i set i="" for  set i=$order(@aimgbl@(2,i)) quit:i=""  do
	;. new j set j="" for  set j=$order(@aimgbl@(2,i,j)) quit:j=""  if $increment(indexcount)
	;do assert(reccount=indexcount)
	quit
	;
tspeed2	; @TEST Index 1E6 rows of ^names3("fix",:,0)="A|B"
	;new reccount,indexcount set (reccount,indexcount)=0
	;new i for i=0:0 set i=$order(^names3("fix",i)) quit:'i  if $data(^(i,0)),$increment(reccount)
	new subs set subs(1)="""fix""",subs(2)=":",subs(3)=0
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^names3",.subs,"|",2)
	;new i set i="" for  set i=$order(@aimgbl@(2,i)) quit:i=""  do
	;. new j set j="" for  set j=$order(@aimgbl@(2,i,j)) quit:j=""  if $increment(indexcount)
	;do assert(reccount=indexcount)
	quit
	;
tspeed3	; @TEST Index 1E6 rows of ^composite(1,2,3,4,5,6,7,:)="A|B"
	;new reccount,indexcount set (reccount,indexcount)=0
	;new i set i=$name(^composite)
	;for  set i=$query(@i) quit:i=""  if $increment(reccount)
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^composite",8)
	;new i,start set (i,start)=$name(@aimgbl@(0))
	;for  set i=$query(@i) quit:$name(@i,1)'=start  if $increment(indexcount)
	;do assert(reccount=indexcount)
	quit
	;
tspeed4	; @TEST Index ~1E6 dispersed rows in ^PSNDF(50.6,:,"VUID")
	;new reccount,indexcount set (reccount,indexcount)=0
	;new i for i=0:0 set i=$order(^PSNDF(50.6,i)) quit:'i  if $data(^(i,"VUID")) if $increment(reccount)
	new subs set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^PSNDF",.subs,"^",1)
	;new i set i="" for  set i=$order(@aimgbl@(1,i)) quit:i=""  do
	;. new j set j="" for  set j=$order(@aimgbl@(1,i,j)) quit:j=""  if $increment(indexcount)
	;do assert(reccount=indexcount)
	quit
	;
tcon1	; @TEST Concurrent do/undo of the same global
	; First lock prevents the "horses" from running until it is open
	lock +racegate
	job tcon1job
	new job1 set job1=$zjob
	job tcon1job
	new job2 set job2=$zjob
	; Now jobs can run
	lock -racegate
	; Wait till they are done.
	for  quit:'$zgetjpi(job1,"ISPROCALIVE")  hang 0.001
	for  quit:'$zgetjpi(job2,"ISPROCALIVE")  hang 0.001
	;
	; Look for error files
	new files,file set file="tcon1jobet.*.jobexam"
	for  set file=$zsearch(file)  quit:file=""  set files(file)=""
	do assert('$data(files),"There should not be any error files; first error: "_$order(files("")))
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^names",1,"|",2,1) ; nmonly
	do assert('$data(@aimgbl),"Shouldn't exist as the last operation should have been an unxref")
	quit
	;
tcon1job ; [Job target] Concurrent do/undo of the same global
	new aimgbl
	new $etrap set $etrap="goto tcon1jobet^"_$text(+0)
	lock +racegate($J)
	new i for i=1:1:10 do
	. set aimgbl=$$XREFDATA^%YDBAIM("^names",1,"|",2)
	. new % set %=$random(2)
	. if % do
	.. do UNXREFDATA^%YDBAIM("^names",1,"|",2)
	. else  do
	.. do UNXREFDATA^%YDBAIM(aimgbl)
	lock -racegate($J)
	quit
	;
tcon1jobet ; [Error trap] Concurrent do/undo of the same global
	if $zjobexam("tcon1jobet."_$job_".jobexam")
	halt
	;
tcon2	; @TEST Concurrent xref of different globals
	; First lock prevents the "horses" from running until it is open
	lock +racegate
	job tcon2job1
	new job1 set job1=$zjob
	job tcon2job2
	new job2 set job2=$zjob
	; Now jobs can run
	lock -racegate
	; Wait till they are done.
	for  quit:'$zgetjpi(job1,"ISPROCALIVE")  hang 0.001
	for  quit:'$zgetjpi(job2,"ISPROCALIVE")  hang 0.001
	;
	new job1aim set job1aim=$$XREFDATA^%YDBAIM("^names",1,"|",1,1)
	new subs set subs(1)=100.01,subs(2)=":",subs(3)=0
	new job2aim set job2aim=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",2,1)
	do assert($data(@job1aim))
	do assert($data(@job2aim))
	quit
	;
tcon2job1 ; [Job target] Concurrent xref of different globals
	lock +racegate($J)
	if $$XREFDATA^%YDBAIM("^names",1,"|",1)
	lock -racegate($J)
	quit
	;
tcon2job2 ; [Job target] Concurrent xref of different globals
	lock +racegate($J)
	new subs set subs(1)=100.01,subs(2)=":",subs(3)=0
	if $$XREFDATA^%YDBAIM("^ORD",.subs,"^",2)
	lock -racegate($J)
	quit
	;
	;
tcon3	; @TEST Concurrent unxref of different globals
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM("^names",1,"|",1)
	new subs set subs(1)=100.01,subs(2)=":",subs(3)=0
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",2)
	;
	do assert($data(@aimgbl1))
	do assert($data(@aimgbl2))
	;
	lock +racegate
	job tcon3job1
	new job1 set job1=$zjob
	job tcon3job2
	new job2 set job2=$zjob
	; Now jobs can run
	lock -racegate
	; Wait till they are done.
	for  quit:'$zgetjpi(job1,"ISPROCALIVE")  hang 0.001
	for  quit:'$zgetjpi(job2,"ISPROCALIVE")  hang 0.001
	;
	do assert('$data(@aimgbl1))
	do assert('$data(@aimgbl2))
	quit
	;
tcon3job1 ; [Job target] Concurrent unxref of different globals
	lock +racegate($J)
	do UNXREFDATA^%YDBAIM("^names",1,"|",1)
	lock -racegate($J)
	quit
	;
tcon3job2 ; [Job target] Concurrent unxref of different globals
	lock +racegate($J)
	new subs set subs(1)=100.01,subs(2)=":",subs(3)=0
	do UNXREFDATA^%YDBAIM("^ORD",.subs,"^",2)
	lock -racegate($J)
	quit
	;
tcon4	; @TEST Concurrent xref of the same global
	lock +racegate
	job tcon4job1
	new job1 set job1=$zjob
	job tcon4job2
	new job2 set job2=$zjob
	; Now jobs can run
	lock -racegate
	; Wait till they are done.
	for  quit:'$zgetjpi(job1,"ISPROCALIVE")  hang 0.001
	for  quit:'$zgetjpi(job2,"ISPROCALIVE")  hang 0.001
	;
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^names",1,"|",1,1)
	;
	do assert($data(@aimgbl@(3)),"AIM says we completed")
	;
	; verify that all nodes in names are in aimgbl and vice versa
	; ^names -> aimgbl
	new i,v set i=""
	for  set i=$order(^names(i)) quit:i=""  do
	. set v=$piece(^names(i),"|",1)
	. do assert($data(@aimgbl@(1,v,i)),^names(i)_" does not match "_$name(@aimgbl@(1,v,i)))
	;
	; aimgbl -> ^names
	kill i,v
	set (i,v)=""
	for  set v=$order(@aimgbl@(1,v)) quit:v=""  for  set i=$order(@aimgbl@(1,v,i)) quit:i=""  do
	. do assert($piece(^names(i),"|",1)=v,$name(@aimgbl@(1,v,i))_" does not match "_$name(^names(i))_"="_^names(i))
	;
	; Verify stats work properly
	; For stats, we should have these nodes (we have 500 A, 250 of each B and C; total of 1000):
	; ^%ydbAIMDXtoXI8zgdGIqUuCoYeqwF6(-1)=3
	; ^%ydbAIMDXtoXI8zgdGIqUuCoYeqwF6(-1,"A")=500000
	; ^%ydbAIMDXtoXI8zgdGIqUuCoYeqwF6(-1,"B")=250000
	; ^%ydbAIMDXtoXI8zgdGIqUuCoYeqwF6(-1,"C")=250000
	; ^%ydbAIMDXtoXI8zgdGIqUuCoYeqwF6(11)=1000000
	do assert(@aimgbl@(-1)=3)
	do assert(@aimgbl@(-1,"A")=500000)
	do assert(@aimgbl@(-1,"B")=250000)
	do assert(@aimgbl@(-1,"C")=250000)
	do assert(@aimgbl@(11)=1000000)
	;
	set ^names(1000001)="A|Q"
	set ^names(1000002)="Z|F"
	;
	do assert(@aimgbl@(-1)=4)
	do assert(@aimgbl@(-1,"A")=500001)
	do assert(@aimgbl@(-1,"B")=250000)
	do assert(@aimgbl@(-1,"C")=250000)
	do assert(@aimgbl@(-1,"Z")=1)
	do assert(@aimgbl@(11)=1000002)
	;
	; ^names(1)="A|B"
	; ^names(2)="C|B"
	kill ^names(1),^names(2)
	;
	do assert(@aimgbl@(-1)=4)
	do assert(@aimgbl@(-1,"A")=500000)
	do assert(@aimgbl@(-1,"B")=250000)
	do assert(@aimgbl@(-1,"C")=249999)
	do assert(@aimgbl@(-1,"Z")=1)
	do assert(@aimgbl@(11)=1000000)
	quit
	;
tcon4job1 ; [Job target] Concurrent xref of the same global
	lock +racegate($J)
	if $$XREFDATA^%YDBAIM("^names",1,"|",1,"","","",2)
	lock -racegate($J)
	quit
	;
tcon4job2 ; [Job target] Concurrent xref of the same global
	lock +racegate($J)
	if $$XREFDATA^%YDBAIM("^names",1,"|",1,"","","",2)
	lock -racegate($J)
	quit
	;
tcon5	; @TEST Test concurrent lsxref
	new $etrap set $etrap="goto tcon5jobet^"_$text(+0)
	job tcon5job1
	new i
	for i=1:1:100 new xrefs do LSXREFDATA^%YDBAIM(.xrefs) hang $random(100)*0.001
	quit
	;
tcon5job1 ; [Job Target] Test concurrent lsxref
	new $etrap set $etrap="goto tcon5jobet^"_$text(+0)
	kill ^x set ^x(1)=""
	new i,xref,unxref
	for i=1:1:100 do
	. set xref=$$XREFDATA^%YDBAIM("^x",1)
	. set unxref=$$UNXREFDATA^%YDBAIM("^x",1)
	quit
	;
tcon5jobet ; [Error trap] Concurrent do/undo of the same global
	if $zjobexam("tcon5jobet."_$job_".jobexam")
	halt
	;
tcon6	; @TEST Test concurrent xref, unxref, and lsxref together
	set ^stop=0
	set ^njobs=8
	set ^cntr=^njobs
	for i=1:1:^njobs set gbl="^x"_i kill @gbl set @gbl@(1)="",@gbl@(2)="abcd"
	; Start ^njobs child processes
	for i=1:1:^njobs do
	. set jobstr="job tcon6job:(output=""tcon6job.mjo"_i_""":error=""tcon6job.mje"_i_""")"
	. xecute jobstr
	. set ^job(i)=$zjob
	; Let child processes run for a max of 5 seconds. Stop before that if at least one process fails and sets ^stop=1
	for i=1:1:50  quit:^stop=1  hang 0.1
	; Signal child processes to stop
	set ^stop=1
	; Wait for child processes to terminate.
	for i=1:1:^njobs set pid=^job(i) for  quit:'$zgetjpi(pid,"ISPROCALIVE")  hang 0.1
	; Check if all child processes finished cleanly
	do assert(^cntr=0,"^cntr : Expected=0 : Actual="_^cntr)
	if ^cntr do
	. write "[FAIL] [cat tcon6job.mj*] output follows",!
	. zsystem "cat tcon6job.mj* | sed 's/^/tcon6 : [FAIL] /'"
	kill ^stop,^njobs,^cntr,^job
	quit
	;
tcon6job ; [Job Target] Test concurrent xref, unxref, and lsxref together
	set $etrap="zwrite $zstatus  set ^stop=1 halt"
	set njobs=^njobs
	for i=1:1 quit:^stop=1  do
	. for j=1:1:3 do
	. . set gbl="^x"_(1+$random(njobs))
	. . set:j=1 xref=$$XREFDATA^%YDBAIM(gbl,1)
	. . set:j=2 xref=$$UNXREFDATA^%YDBAIM(gbl,1)
	. . if j=3 for k=1:1:8 do
	. . . if $random(2) do LSXREFDATA^%YDBAIM(.xrefs,gbl)
	. . . else  do LSXREFDATA^%YDBAIM(.xrefs)
	; Record that this child finished cleanly
	if $increment(^cntr,-1)
	quit
	;
tsanity	; @TEST Main Sanity YDBAIM tester
	; write !
	do init
	do simappstart(12)
	do xrefprocrun(4)
	; Verify the names of all the cross-references
	new xrefgbl
	new eachxref set eachxref=""
	for  set eachxref=$order(^%ydb674test("xref","aimxref",eachxref)) quit:""=eachxref  do
	. if $data(xrefgbl) do assert(xrefgbl=^%ydb674test("xref","aimxref",eachxref)) if 1
	. else  set xrefgbl=^%ydb674test("xref","aimxref",eachxref)
	; Let the jobs create random data for one second
	hang 1
	; Stop application and check xref
	do simappstop
	do xrefgblchk
	quit
	;
	;
init	new gbl,io,%
	view "jobpid":1,"ztrigger_output":0
	kill ^%ydb674test,^ABC
	set (gbl,%)="^%ydbAIMD" for  set gbl=$order(@gbl) quit:gbl'[%  kill @gbl
	if $ztrigger("item","-*")
	view "ztrigger_output":1
	quit

	; The simapp label simulates an application that randomly sets, modifies, and deletes nodes of global variable, ^ABC. The intent is
	; for simapp processes to be JOB'd off, so that metadata management can be tested with a running application. As The metadata is
	; managed for nodes with three subscripts, the simulated application also generates nodes with two and four subscripts to test that
	; the metadata management only handles nodes with three subscripts and ignores nodes with two and four subscripts. Each node has
	; three "|" separated pieces, consisting of a number, a string, and a number. This allows this simulated application to be used to
	; test both metadata management for entire nodes, as well as for pieces of nodes.
	; The parent sets a count in ^%ydb674test("simapp","start") of the number of application processes it intends to JOB off. Each
	; application process decrements the count by 1, so that when the count reaches 0 the parent knows all application processess are
	; off and running. Before starting application processes, the parent acquires lock in ^%ydb674test("simapp") which it releases when
	; application processes can shut down. When an application proces is able to obtain the lock ^%ydb674test("simapp",$job), that is a
	; signal for it to shut down.
simapp
	new opt,sub1,sub2,sub3,sub4
	if $increment(^%ydb674test("simapp","start"),-1)		; tell parent this JOB'd process is up
	for i=1:1 lock +^%ydb674test("simapp",$job):0 quit:$test  do	; successful lock acquisition is stop signal from parent
	. set sub1=$random(10),sub2=$random(10)     		  	; random subscripts for next node
	. set opt=$random(10)			      		  	; decide on number of subscripts of next node to update
	. if 0=opt do update("^ABC("_sub1_","_sub2_")")			; update 2 subscript nodes 10% of the time
	. else  set sub3=$random(10) if 1=opt do			; update 4 subscript nodes 10% of the time
	. . set sub4=$random(10)
	. . do update("^ABC("_sub1_","_sub2_","_sub3_","_sub4_")")
	. else  do update("^ABC("_sub1_","_sub2_","_sub3_")")		; update 3 subscript nodes 80% of the time
	. ; hang 1+$random(10)/1E3					; random hang if needed to avoid saturating the CPU
	quit


	; Updating a node. If a node does not exist, create it, using odd numbers for the first and third pieces so that it is easy
	; to determine how many times a node was updated. If a node exists, zkill it 20% of the time (since triggers don't work on trees
	; AIM won't handle kill well at present), and the rest of the time, update the node.
update(node)
	new piece
	tstart ()
	if $data(@node)#10 do
	. if $random(5) do						; 80% of the time node will be updated
	. . set piece=$random(2)*2+1					; piece is either 1 or 3
	. . set $zpiece(@node,"|",piece)=0.5*$zpiece(@node,"|",piece)
	. . set $zpiece(@node,"|",2)=$zpiece(@node,"|",2)_$$^%RANDSTR(1,,"AL")
	. else  zkill @node						; and removed 20% of the time
	else  set @node=$random(1E3)*2+1_"|"_$$^%RANDSTR(1,,"AL")_"|"_($random(1E3)*2+1)
	tcommit
	quit

	; Start simulated application
simappstart(nproc)
	new i
	set ^%ydb674test("simapp","start")=nproc			; number of simapp processes
	; write "Starting ",nproc," simulated application processes",!
	lock +^%ydb674test("simapp")					; get lock that tells simapp processes to run
	for i=1:1:nproc job simapp set ^%ydb674test("simapp","jobs",$zjob)=""
	for  quit:'^%ydb674test("simapp","start")  hang .001		; zero ^%ydb674test("simapp","start") means all simapp processes are up
	quit

	; Stop simulated application
simappstop
	lock -^%ydb674test("simapp")					; releasing lock instructs simapp processes to stop
	; write "Stopping simulated application",!
	; Wait for all children to die
	new zjob set zjob=""
	for  set zjob=$order(^%ydb674test("simapp","jobs",zjob)) quit:'zjob  do
	. for  quit:'$zgetjpi(zjob,"isprocalive")  hang .001
	quit

	; Check complete nodes and cross references for Consistency - a cross reference for each node, and a node for each cross reference
xrefgblchk
	new flag,sub1,sub2,sub3,val
	; write "Checking that a node exists for each cross reference ",!
	; Should check whether a $query() loop is faster than the following nested $order() loop
	set flag=0,val="" for  set val=$order(@xrefgbl@(0,val)) quit:""=val  do
	. set sub1="" for  set sub1=$order(@xrefgbl@(0,val,sub1)) quit:""=sub1  do
	. . set sub2="" for  set sub2=$order(@xrefgbl@(0,val,sub1,sub2)) quit:""=sub2  do
	. . . set sub3="" for  set sub3=$order(@xrefgbl@(0,val,sub1,sub2,sub3)) quit:""=sub3  do
	. . . . if '$data(^ABC(sub1,sub2,sub3))#10 write !,xrefgbl,"(",val,",",sub1,",",sub2,",",sub3,") exists but ^ABC node does not" set flag=1
	. . . . else  if val'=^ABC(sub1,sub2,sub3) write !,"^ABC(",sub1,",",sub2,",",sub3,")=",^ABC(sub1,sub2,sub3)," but xref is ",val set flag=1
	do assert('flag)
	; write "Checking that an xref exists for each global node ",!
	set flag=0,sub1=""  for  set sub1=$order(^ABC(sub1)) quit:""=sub1  do:$data(^ABC(sub1))\10
	. set sub2="" for  set sub2=$order(^ABC(sub1,sub2)) quit:""=sub2  do:$data(^ABC(sub1,sub2))\10
	. . set sub3="" for  set sub3=$order(^ABC(sub1,sub2,sub3)) quit:""=sub3  do:$data(^ABC(sub1,sub2,sub3))#10
	. . . set val=^ABC(sub1,sub2,sub3)
	. . . if '$data(@xrefgbl@(0,val,sub1,sub2,sub3)) write !,"^ABC(",sub1,",",sub2,",",sub3,")=",val," has no xref" set flag=1
	do assert('flag)
	quit

	; Run multiple parallel xref processes, to show parallel processes can concurrently cross reference
xrefprocrun(nproc)
	new i
	set ^%ydb674test("xref","start")=nproc				; number of xref processes
	lock ^%ydb674test("xref","start")
	; write "Starting ",nproc," concurrent xrefs",!
	for i=1:1:nproc job xrefproc set ^%ydb674test("xref","jobs",$zjob)=""
	for  quit:'^%ydb674test("xref","start")  hang .001		; zero ^%ydb674test("xref","start") means all xref processes are up
	kill ^%ydb674test("xref","start")
	lock -^%ydb674test("xref","start")
	lock +^%ydb674test("xref","stop")
	; Make sure all xref jobs died off
	new zjob set zjob=""
	for  set zjob=$order(^%ydb674test("xref","jobs",zjob)) quit:'zjob  do
	. for  quit:'$zgetjpi(zjob,"isprocalive")  hang .001
	; write "Cross reference processes complete",!
	quit

xrefproc
	lock +^%ydb674test("xref","stop",$job)
	if $increment(^%ydb674test("xref","start"),-1)
	lock +^%ydb674test("xref","start",$job)				; Gate to ensure all xref processes are up and running
	lock -^%ydb674test("xref","start",$job)
	set ^%ydb674test("xref","aimxref",$job)=$$XREFDATA^%YDBAIM("^ABC",3)
	lock -^%ydb674test("xref","stop",$job)
	quit

tbash	; @TEST Run bash tests through run_bash_tests.sh/ydbaim_test.sh
	zsystem "$script_dir/run_bash_tests.sh"
	do assert(0=$zsystem)
	quit

gvsuboflow(level)	;	Used by tbash
	if level do gvsuboflowhelper quit  ; If level=1, test $ZLEVEL=1 by adding one more DO frame before calling gvsuboflowhelper.
gvsuboflowhelper:	;	Test is meaningful only if key size is less than maximum (i.e., YottaDB defaults pre 2.02)
	if 1019<$$^%PEEKBYNAME("sgmnt_data.max_key_size","YDBAIM") do
	. do UNXREFDATA^%YDBAIM
	. kill ^x
	. for i=1:1 set ^x(i)=$j(2**i,2**i)
	. set $etrap="zwrite $zstatus set $ecode="""" zhalt +$zstatus"
	. set aimglobal=$$XREFDATA^%YDBAIM("^x",1)
	. zwrite aimglobal	; this line should not be reached as GVSUBOFLOW error should transfer control in previous line
	else  do	;	else fake expected error message
	. write "Error occurred: 150372986,xrefdata+137^%YDBAIM,%YDB-E-GVSUBOFLOW, Maximum combined length of subscripts exceeded,%YDB-I-GVIS, ",$c(9,9),"Global variable: ^%ydbAIMDgFr8HZY2gJsda6acj41uCE(0*",!
	. write "aimglobal=""^%ydbAIMDgFr8HZY2gJsda6acj41uCE""",!
	quit

badinvocation	; Used by tbash
	set $etrap="zwrite $zstatus quit"
	for label="","LSXREFDATA","UNXREFDATA","XREFDATA" do
	. set entryref=label_"^%YDBAIM"
	. for tp=0,1 do
	. . write "# Testing "_entryref_" with tp=",tp,!
	. . do badinvocationhelper(entryref,tp)
	quit

badinvocationhelper(entryref,tp)	;
	new $etrap
	set level=$zlevel
	set $etrap="zwrite $zstatus set $ecode="""" trollback:tp  zgoto level-1"
	tstart:tp ():serial
	do @entryref
	trollback:tp
	quit

ttype1	; @TEST test type1 application global variables
	new fstr,i,j,nullxref,s,x,y,z
	for fstr=0,1 do
	. set nullxref=$select(fstr:"#",1:"")
	. do UNXREFDATA^%YDBAIM
	. kill ^USPresidents
	. do assert(46=$$LOADDATA("USPresidents"))
	. do assert(3=$$LOADDATA("NotUSPresidents"))
	. set s(1)=":1900",s(2)=1841
	. ; Entire node: test initial data load & triggers for no statistics
	. zkill ^USPresidents(1837,1841)
	. set x=$$XREFDATA^%YDBAIM("^USPresidents",.s,,,,,,,1,fstr)
	. do assert(x'=$$XREFDATA^%YDBAIM("^USPresidents",.s,,,1,,,,,fstr)),assert(x'=$$XREFDATA^%YDBAIM("^USPresidents",.s,,,1,,,,1,'fstr))
	. set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i),assert(46=$$LOADDATA("USPresidents"))
	. set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(22=i)
	. set y=nullxref for i=1:1 set y=$order(@x@(0,y)) quit:""=y
	. do assert(3=i)
	. zkill ^USPresidents(1837,1841) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i)
	. set y=nullxref for i=1:1 set y=$order(@x@(0,y)) quit:""=y
	. do assert(2=i)
	. zkill ^USPresidents(1837,-42) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i)
	. zkill ^USPresidents(1837,42) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(22=i)
	. do assert(3=$$LOADDATA("NotUSPresidents")) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i)
	. do assert(46=$$LOADDATA("USPresidents")) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(22=i)
	. set y=nullxref for i=1:1 set y=$order(@x@(0,y)) quit:""=y
	. do assert(3=i)
	. do UNXREFDATA^%YDBAIM
	. ; Entire node: test initial data load & triggers for stat=1
	. zkill ^USPresidents(1837,1841)
	. set x=$$XREFDATA^%YDBAIM("^USPresidents",.s,,,,,,1,1,fstr)
	. set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i),assert(46=$$LOADDATA("USPresidents"))
	. set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(22=i)
	. set y=nullxref for i=1:1 set y=$order(@x@(0,y)) quit:""=y
	. do assert(3=i),assert(21=@x@("",nullxref)),assert(1=@x@("",nullxref_"Martin|Van|Buren")),assert(1=@x@("",nullxref_"William|Henry|Harrison"))
	. zkill ^USPresidents(1837,1841) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@x@("",nullxref)),assert('$data(@x@("",nullxref_"Martin|Van|Buren"))),assert(@x@("",nullxref_"William|Henry|Harrison"))
	. set y=nullxref for i=1:1 set y=$order(@x@(0,y)) quit:""=y
	. do assert(2=i)
	. zkill ^USPresidents(1837,-42) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@x@("",nullxref))
	. zkill ^USPresidents(1837,42) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(22=i),assert(21=@x@("",nullxref))
	. do assert(3=$$LOADDATA("NotUSPresidents")) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@x@("",nullxref))
	. do assert(46=$$LOADDATA("USPresidents")) set y=nullxref for i=1:1 set y=$order(@x@(0,y)) quit:""=y
	. do assert(3=i),assert(21=@x@("",nullxref)),assert(1=@x@("",nullxref_"Martin|Van|Buren")),assert(1=@x@("",nullxref_"William|Henry|Harrison"))
	. do UNXREFDATA^%YDBAIM
	. ; Entire node: test initial data load & triggers for stat=2
	. zkill ^USPresidents(1837,1841)
	. set x=$$XREFDATA^%YDBAIM("^USPresidents",.s,,,,,,2,1,fstr)
	. set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i),assert(46=$$LOADDATA("USPresidents"))
	. set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(22=i),assert(3=@x@("")),assert(23=@x@(11))
	. set y=nullxref for i=1:1 set y=$order(@x@(0,y)) quit:""=y
	. do assert(3=i),assert(21=@x@("",nullxref)),assert(1=@x@("",nullxref_"Martin|Van|Buren")),assert(1=@x@("",nullxref_"William|Henry|Harrison"))
	. zkill ^USPresidents(1837,1841) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@x@("",nullxref)),assert('$data(@x@("",nullxref_"Martin|Van|Buren"))),assert(@x@("",nullxref_"William|Henry|Harrison"))
	. set y=nullxref for i=1:1 set y=$order(@x@(0,y)) quit:""=y
	. do assert(2=i),assert(2=@x@("")),assert(23=@x@(11))
	. zkill ^USPresidents(1837,-42) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@x@("",nullxref)),assert(2=@x@("")),assert(23=@x@(11))
	. zkill ^USPresidents(1837,42) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(22=i),assert(21=@x@("",nullxref)),assert(2=@x@("")),assert(22=@x@(11))
	. do assert(3=$$LOADDATA("NotUSPresidents")) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@x@("",nullxref)),assert(2=@x@("")),assert(23=@x@(11))
	. do assert(46=$$LOADDATA("USPresidents")) set y=nullxref for i=1:1 set y=$order(@x@(0,y)) quit:""=y
	. do assert(3=i)
	. do assert(21=@x@("",nullxref)),assert(1=@x@("",nullxref_"Martin|Van|Buren")),assert(1=@x@("",nullxref_"William|Henry|Harrison"))
	. do UNXREFDATA^%YDBAIM
	. ; Pieces
	. zkill ^USPresidents(1837,1841)
	. set z=$$XREFDATA^%YDBAIM("^USPresidents",.s,"|",3,,,,,1,fstr)
	. do assert(z'=x),assert(z'=$$XREFDATA^%YDBAIM("^USPresidents",.s,"|",1,1,,,,fstr))
	. set y="" for i=1:1 set y=$order(@z@(3,nullxref,y)) quit:""=y
	. do assert(23=i),assert(46=$$LOADDATA("USPresidents"))
	. set y="" for i=1:1 set y=$order(@z@(3,nullxref,y)) quit:""=y
	. do assert(22=i)
	. set y=nullxref for i=1:1 set y=$order(@z@(3,y)) quit:""=y
	. do assert(3=i)
	. zkill ^USPresidents(1837,1841) set y="" for i=1:1 set y=$order(@z@(3,nullxref,y)) quit:""=y
	. do assert(23=i)
	. set y=nullxref for i=1:1 set y=$order(@z@(3,y)) quit:""=y
	. do assert(2=i)
	. zkill ^USPresidents(1837,-42) set y="" for i=1:1 set y=$order(@z@(3,nullxref,y)) quit:""=y
	. do assert(23=i)
	. zkill ^USPresidents(1837,42) set y="" for i=1:1 set y=$order(@z@(3,nullxref,y)) quit:""=y
	. do assert(22=i)
	. do assert(3=$$LOADDATA("NotUSPresidents")) set y="" for i=1:1 set y=$order(@z@(3,nullxref,y)) quit:""=y
	. do assert(23=i)
	. do assert(46=$$LOADDATA("USPresidents")) set y="" for i=1:1 set y=$order(@z@(3,nullxref,y)) quit:""=y
	. do assert(22=i)
	. set y=nullxref for i=1:1 set y=$order(@z@(3,y)) quit:""=y
	. do assert(3=i)
	. do UNXREFDATA^%YDBAIM
	. ; Pieces: test initial data load & triggers for stat=1
	. zkill ^USPresidents(1837,1841)
	. set z=$$XREFDATA^%YDBAIM("^USPresidents",.s,"|","1;3",,,,1,1,fstr)
	. set j=2*$random(2)+1,y="" for i=1:1 set y=$order(@z@(j,nullxref,y)) quit:""=y
	. do assert(23=i),assert(46=$$LOADDATA("USPresidents"))
	. set j=2*$random(2)+1,y="" for i=1:1 set y=$order(@z@(j,nullxref,y)) quit:""=y
	. do assert(22=i)
	. set j=2*$random(2)+1,y=nullxref for i=1:1 set y=$order(@z@(j,y)) quit:""=y
	. do assert(3=i),assert(21=@z@(-2*$random(2)-1,nullxref)),assert(1=@z@(-1,nullxref_"Martin")),assert(1=@z@(-3,nullxref_"Buren")),assert(1=@z@(-1,nullxref_"William")),assert(1=@z@(-3,nullxref_"Harrison"))
	. zkill ^USPresidents(1837,1841) set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@z@(-2*$random(2)-1,nullxref)),assert('$data(@z@(-1,nullxref_"Martin"))),assert('$data(@z@(-3,nullxref_"Buren"))),assert(1=@z@(-1,nullxref_"William")),assert(1=@z@(-3,nullxref_"Harrison"))
	. set y=nullxref for i=1:1 set y=$order(@z@(1,y)) quit:""=y
	. do assert(2=i)
	. zkill ^USPresidents(1837,-42) set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@z@(-2*$random(2)-1,nullxref))
	. zkill ^USPresidents(1837,42) set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(22=i),assert(21=@z@(-2*$random(2)-1,nullxref))
	. do assert(3=$$LOADDATA("NotUSPresidents")) set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@z@(-2*$random(2)-1,nullxref))
	. do assert(46=$$LOADDATA("USPresidents")) set y=nullxref for i=1:1 set y=$order(@z@(1,y)) quit:""=y
	. do assert(3=i),assert(21=@z@(-2*$random(2)-1,nullxref))
	. do assert(1=@z@(-1,nullxref_"Martin")),assert(1=@z@(-3,nullxref_"Buren")),assert(1=@z@(-1,nullxref_"William")),assert(1=@z@(-3,nullxref_"Harrison"))
	. do UNXREFDATA^%YDBAIM
	. ; Pieces: test initial data load & triggers for stat=2
	. zkill ^USPresidents(1837,1841)
	. set z=$$XREFDATA^%YDBAIM("^USPresidents",.s,"|","1:3",,,,2,1,fstr)
	. set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(23=i),assert(2=@z@(-2)),assert(69=@z@(11)),assert(46=$$LOADDATA("USPresidents")) quit
	. set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(22=i),assert(3=@z@(-2)),assert(69=@z@(11))
	. set y=nullxref for i=1:1 set y=$order(@z@(1,y)) quit:""=y
	. do assert(3=i),assert(21=@z@(-2*$random(2)-1,nullxref)),assert(1=@z@(-1,nullxref_"Martin")),assert(1=@z@(-3,nullxref_"Buren")),assert(1=@z@(-1,nullxref_"William")),assert(1=@z@(-3,nullxref_"Harrison"))
	. zkill ^USPresidents(1837,1841) set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@z@(-2*$random(2)-1,nullxref)),assert('$data(@z@(-1,nullxref_"Martin"))),assert('$data(@z@(-2,nullxref_"Van"))),assert('$data(@z@(-3,nullxref_"Buren"))),assert(1=@z@(-1,nullxref_"William")),assert(1=@z@(-3,nullxref_"Harrison"))
	. set y=nullxref for i=1:1 set y=$order(@z@(1,y)) quit:""=y
	. do assert(2=i),assert(22=@z@(-2*$random(2)-1,nullxref)),assert(69=@z@(11))
	. zkill ^USPresidents(1837,-42) set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@z@(-2*$random(2)-1,nullxref)),assert(2=@z@(-2)),assert(69=@z@(11))
	. zkill ^USPresidents(1837,42) set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(22=i),assert(21=@z@(-2*$random(2)-1,nullxref)),assert(2=@z@(-1)),assert(66=@z@(11))
	. do assert(3=$$LOADDATA("NotUSPresidents")) set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@z@(-2*$random(2)-1,"")),assert(2=@z@(-2)),assert(69=@z@(11))
	. do assert(46=$$LOADDATA("USPresidents")) set y=nullxref for i=1:1 set y=$order(@z@(1,y)) quit:""=y
	. do assert(3=i),assert(21=@z@(-2*$random(2)-1,nullxref)),assert(1=@z@(-1,nullxref_"Martin")),assert(1=@z@(-3,nullxref_"Buren")),assert(1=@z@(-1,nullxref_"William")),assert(1=@z@(-3,nullxref_"Harrison"))
	. do UNXREFDATA^%YDBAIM
	quit

USPresidents	; former US Presidents with first and last years in office
	;^USPresidents(1789,1797)="George||Washington"
	;^USPresidents(1797,1801)="John||Adams"
	;^USPresidents(1801,1809)="Thomas||Jefferson"
	;^USPresidents(1809,1817)="James||Madison"
	;^USPresidents(1817,1825)="James||Monroe"
	;^USPresidents(1825,1829)="John|Quincy|Adams"
	;^USPresidents(1829,1837)="Andrew||Jackson"
	;^USPresidents(1837,1841)="Martin|Van|Buren"
	;^USPresidents(1841,1841)="William|Henry|Harrison"
	;^USPresidents(1841,1845)="John||Tyler"
	;^USPresidents(1845,1849)="James|K.|Polk"
	;^USPresidents(1849,1850)="Zachary||Taylor"
	;^USPresidents(1850,1853)="Millard||Fillmore"
	;^USPresidents(1853,1857)="Franklin||Pierce"
	;^USPresidents(1857,1861)="James||Buchanan"
	;^USPresidents(1861,1865)="Abraham||Lincoln"
	;^USPresidents(1865,1869)="Andrew||Johnson"
	;^USPresidents(1869,1877)="Ulysses|S.|Grant"
	;^USPresidents(1877,1881)="Rutherford|B.|Hayes"
	;^USPresidents(1881,1881)="James|A.|Garfield"
	;^USPresidents(1881,1885)="Chester|A.|Arthur"
	;^USPresidents(1885,1889)="Grover||Cleveland"
	;^USPresidents(1889,1893)="Benjamin||Harrison"
	;^USPresidents(1893,1897)="Grover||Cleveland"
	;^USPresidents(1897,1901)="William||McKinley"
	;^USPresidents(1901,1909)="Theodore||Roosevelt"
	;^USPresidents(1909,1913)="William|Howard|Taft"
	;^USPresidents(1913,1921)="Woodrow||Wilson"
	;^USPresidents(1921,1923)="Warren|G.|Harding"
	;^USPresidents(1923,1929)="Calvin||Coolidge"
	;^USPresidents(1929,1933)="Herbert||Hoover"
	;^USPresidents(1933,1945)="Franklin|D.|Roosevelt"
	;^USPresidents(1945,1953)="Harry|S.|Truman"
	;^USPresidents(1953,1961)="Dwight|D.|Eisenhower"
	;^USPresidents(1961,1963)="John|F.|Kennedy"
	;^USPresidents(1963,1969)="Lyndon|B.|Johnson"
	;^USPresidents(1969,1974)="Richard||Nixon"
	;^USPresidents(1974,1977)="Gerald||Ford"
	;^USPresidents(1977,1981)="Jimmy||Carter"
	;^USPresidents(1981,1989)="Ronald||Reagan"
	;^USPresidents(1989,1993)="George|H.W.|Bush"
	;^USPresidents(1993,2001)="Bill||Clinton"
	;^USPresidents(2001,2009)="George|W.|Bush"
	;^USPresidents(2009,2017)="Barack||Obama"
	;^USPresidents(2017,2021)="Donald||Trump"

NotUSPresidents	; fake data to test type 1 subscripts
	;^USPresidents(1837,42)="Zaphod||Beeblebrox"
	;^USPresidents(1837,-42)="Ford||Prefect"

v1type1 ; @TEST VistA type1 - Test triggers
	; Index data in ^ORD(100.01,:,.1) first ^ piece
	; ^ORD(100.01,0)="ORDER STATUS^100.01I^99^16"
	; ^ORD(100.01,1,0)="DISCONTINUED^dc"
	; ^ORD(100.01,1,.1)="dc"
	; ^ORD(100.01,1,1,0)="^^1^1^3070625^^^^"
	; ^ORD(100.01,1,1,1,0)="Orders that have been explicitly stopped."
	; ^ORD(100.01,1,"TERMSTATUS",0)="^100.0199DA^1^1"
	; ^ORD(100.01,1,"TERMSTATUS",1,0)="3070607.115705^1"
	; ^ORD(100.01,1,"TERMSTATUS","B",3070607.115705,1)=""
	; ^ORD(100.01,1,"VUID")="4500704^1"
	; ^ORD(100.01,2,0)="COMPLETE^comp"
	; ^ORD(100.01,2,.1)="c"
	; ^ORD(100.01,2,1,0)="^^3^3^3070607"
	; ^ORD(100.01,2,1,1,0)="Orders that require no further action by the ancillary service. "
	; ^ORD(100.01,2,1,2,0)=" e.g., Lab orders are completed when results are available, "
	; ^ORD(100.01,2,1,3,0)="Radiology orders are complete when results are available."
	; ^ORD(100.01,2,"TERMSTATUS",0)="^100.0199DA^1^1"
	; ^ORD(100.01,2,"TERMSTATUS",1,0)="3070607.115705^1"
	; ^ORD(100.01,2,"TERMSTATUS","B",3070607.115705,1)=""
	; ^ORD(100.01,2,"VUID")="4501088^1"
	; ...
	new fstr,nullxref,stat for fstr=0,1 set nullxref=$select(fstr:"#",1:"") for stat=0:1:2 do
	. ; write !,"stat: ",stat	; Uncomment for debugging
	. new subs set subs(1)=100.01,subs(2)=":"" """,subs(3)=.1
	. ;
	. ; gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type
	. ; Get AIM global with name only
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",1,1,0,1,stat,1,fstr)
	. ; Unxref the data
	. do UNXREFDATA^%YDBAIM("^ORD",.subs,"^",1,0,0,1,stat,1,fstr)
	. ; Assert that the data doesn't exist (first time (stat=0) it won't, so the previous UNXREF will be a no-op)
	. do assert('$data(@aimgbl))
	. ; Xref data now
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",1,0,0,1,stat,1,fstr)
	. ;
	. ; Count entries. There are 16 entries, and no entry for zero, which comes out as NULL
	. ; So we expect a total of 17 entries in the index, and 1 NULL.
	. ; The reason we have a NULL for the zero node is a change done for this issue:
	. ; - https://gitlab.com/YottaDB/Util/YDBAIM/-/issues/60
	. ; WE NEED to capture null entries, as they can be significant for non-Fileman-compatible data in VistA
	. do assert($$type1cd(aimgbl)=17)
	. do assert($data(@aimgbl@(1,nullxref,0)))
	. ; do assert($$type1cn(aimgbl)=1)
	. do assert($data(@aimgbl@(1,nullxref_"dc",1)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"dc")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	... do assert(@aimgbl@(11)=17) ; Total entries
	. ;
	. ; Add data and make sure triggers works
	. ; Now set one of the .1 nodes to be non-existent on a new entry
	. set ^ORD(100.01,100,0)="JUNK STATUS^junk"
	. ; There should be now 18 entries, but 2 null entries
	. do assert($$type1cd(aimgbl)=18)
	. do assert($$type1cn(aimgbl)=2)
	. do assert($data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref)=2)
	.. ; Another extra entry to test statistics
	.. set ^ORD(100.01,101,0)="JUNK STATUS2^junk2"
	.. do assert(@aimgbl@(-1,nullxref)=3)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	... do assert(@aimgbl@(11)=19) ; Total entries
	.. kill ^ORD(100.01,101,0)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	... do assert(@aimgbl@(11)=18) ; Total entries
	.. do assert(@aimgbl@(-1,nullxref)=2)
	. ;
	. ; Set the .1 node to some data on entry 100
	. set ^ORD(100.01,100,.1)="ju"
	. ; There should be now 18 entries, but 1 null entry
	. do assert($$type1cd(aimgbl)=18)
	. do assert($$type1cn(aimgbl)=1)
	. do assert($data(@aimgbl@(1,nullxref_"ju",100)))
	. do assert('$data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert(1=@aimgbl@(-1,nullxref))
	.. do assert(@aimgbl@(-1,nullxref_"ju")=1)
	. ;
	. ; Restore data to original
	. kill ^ORD(100.01,100)
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert('$data(@aimgbl@(1,nullxref_"ju",100)))
	. do assert('$data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"ju")))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	... do assert(@aimgbl@(11)=17) ; Total entries
	. ;
	. ; Kill the parent entry node by node
	. new %1,%2 set (%1,%2)=$name(^ORD(100.01,1))
	. new keepme merge keepme=@%2
	. do assert($data(^ORD(100.01,1)))
	. for  set %1=$query(@%1) quit:$name(@%2,$qlength(%2))'=$name(@%1,$qlength(%2))  kill @%1
	. do assert('$data(^ORD(100.01,1)))
	. do assert('$data(@aimgbl@(1,nullxref_"dc",1)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"dc")))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	... do assert(@aimgbl@(11)=16) ; Total entries
	. merge @%2=keepme
	. do assert($data(^ORD(100.01,1)))
	. do assert($data(@aimgbl@(1,nullxref_"dc",1)))
	. if stat=2 do
	.. do assert(@aimgbl@(-1)=17) ; Distinct entries
	.. do assert(@aimgbl@(11)=17) ; Total entries
	. ;
	. ; Kill parent entry all at once
	. new keepme merge keepme=^ORD(100.01,1)
	. kill ^ORD(100.01,1)
	. do assert('$data(^ORD(100.01,1)))
	. do assert('$data(@aimgbl@(1,nullxref_"dc")))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"dc")))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	... do assert(@aimgbl@(11)=16) ; Total entries
	. merge ^ORD(100.01,1)=keepme
	. do assert($data(^ORD(100.01,1)))
	. do assert($data(@aimgbl@(1,nullxref_"dc")))
	. if stat=2 do
	.. do assert(@aimgbl@(-1)=17) ; Distinct entries
	.. do assert(@aimgbl@(11)=17) ; Total entries
	.
	. ; Remove all data in ^ORD(101.01)
	. new keepme merge keepme=^ORD(100.01)
	. kill ^ORD(100.01)
	. do assert($data(@aimgbl@(1))=1)
	. if stat do
	.. do assert($data(@aimgbl@(-1))[0)
	.. if stat=2 do
	... do assert('$data(@aimgbl@(-1))) ; Distinct entries
	... do assert('@aimgbl@(11)) ; Total entries
	. merge ^ORD(100.01)=keepme
	. ;
	. ; Remove an existing sub entry. Should have same number of entries, but 2 null
	. new keepme merge keepme=^ORD(100.01,2,.1)
	. kill ^ORD(100.01,2,.1)
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=2)
	. do assert($data(@aimgbl@(1,nullxref,2)))
	. do assert('$data(@aimgbl@(1,nullxref_"c",2)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"c")))
	.. do assert(@aimgbl@(-1,nullxref)=2)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	... do assert(@aimgbl@(11)=17) ; Total entries
	. ; Put back
	. merge ^ORD(100.01,2,.1)=keepme
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert($data(@aimgbl@(1,nullxref_"c",2)))
	. do assert('$data(@aimgbl@(1,nullxref,2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"c")=1)
	.. do assert(@aimgbl@(-1,nullxref)=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	... do assert(@aimgbl@(11)=17) ; Total entries
	quit

v2type1 ; @TEST VistA type1 - Initial data partly empty
	; Remove an existing entry. Should have same number of entries, but 2 null
	new fstr,nullxref,stat for fstr=0,1 set nullxref=$select(fstr:"#",1:"") for stat=0:1:2 do
	. ; write !,"stat: ",stat	; Uncomment for debugging
	. new keepme merge keepme=^ORD(100.01,2,.1)
	. kill ^ORD(100.01,2,.1)
	. ; gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type
	. ; Get AIM global with name only
	. new subs set subs(1)=100.01,subs(2)=":"" """,subs(3)=.1
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",1,1,0,1,stat,1,fstr)
	. ; Unxref the data
	. do UNXREFDATA^%YDBAIM("^ORD",.subs,"^",1,0,0,1,stat,1,fstr)
	. ; Assert that the data doesn't exist
	. do assert('$data(@aimgbl))
	. ; Initial index
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",1,0,0,1,stat,1,fstr)
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=2)
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"dc")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	... do assert(@aimgbl@(11)=17) ; Total entries
	. ; Check that two null entry exists
	. do assert($data(@aimgbl@(1,nullxref,2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref)=2)
	. ; Put back
	. merge ^ORD(100.01,2,.1)=keepme
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert($data(@aimgbl@(1,nullxref_"c",2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"c")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	... do assert(@aimgbl@(11)=17) ; Total entries
	quit

v1type0 ; @TEST Repeat VistA type 1 tests as type ""
	; Index data in ^ORD(100.01,:,.1) first ^ piece
	new fstr,nullxref,stat for fstr=0,1 set nullxref=$select(fstr:"#",1:"") for stat=0:1:2 do
	. ; write !,"stat: ",stat	; Uncomment for debugging
	. new subs set subs(1)=100.01,subs(2)=":"" """,subs(3)=.1
	. ;
	. ; gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type
	. ; Get AIM global with name only
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",1,1,0,1,stat,"",fstr)
	. ; Unxref the data
	. do UNXREFDATA^%YDBAIM("^ORD",.subs,"^",1,0,0,1,stat,"",fstr)
	. ; Assert that the data doesn't exist (first time (stat=0) it won't, so the previous UNXREF will be a no-op)
	. do assert('$data(@aimgbl))
	. ; Xref data now
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",1,0,0,1,stat,"",fstr)
	. ;
	. ; Count non-null entries. Expect 16
	. ; Expect zero nulls
	. do assert($$type1cd(aimgbl)=16)
	. do assert($$type1cn(aimgbl)=0)
	. do assert($data(@aimgbl@(1,nullxref_"dc",1)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"dc")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	... do assert(@aimgbl@(11)=16) ; Total entries
	. ;
	. ; Add data and make sure triggers works
	. ; Now set one of the .1 nodes to be non-existent on a new entry
	. set ^ORD(100.01,100,0)="JUNK STATUS^junk"
	. ; There should be now 16 entries, as the non-existent entry won't get indexed
	. do assert($$type1cd(aimgbl)=16)
	. do assert($$type1cn(aimgbl)=0)
	. do assert('$data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref)))
	.. ; Another extra entry to test statistics
	.. set ^ORD(100.01,101,0)="JUNK STATUS2^junk2"
	.. do assert('$data(@aimgbl@(-1,nullxref)))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	... do assert(@aimgbl@(11)=16) ; Total entries
	.. kill ^ORD(100.01,101,0)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	... do assert(@aimgbl@(11)=16) ; Total entries
	.. do assert('$data(@aimgbl@(-1,nullxref)))
	. ;
	. ; Set the .1 node to some data on entry 100
	. set ^ORD(100.01,100,.1)="ju"
	. ; There should be now 17 entries, but 0 null entry
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=0)
	. do assert($data(@aimgbl@(1,nullxref_"ju",100)))
	. do assert('$data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref)))
	.. do assert(@aimgbl@(-1,nullxref_"ju")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	... do assert(@aimgbl@(11)=17) ; Total entries
	. ;
	. ; Restore data to original
	. kill ^ORD(100.01,100)
	. do assert($$type1cd(aimgbl)=16)
	. do assert($$type1cn(aimgbl)=0)
	. do assert('$data(@aimgbl@(1,nullxref_"ju",100)))
	. do assert('$data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"ju")))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	... do assert(@aimgbl@(11)=16) ; Total entries
	. ;
	. ; Kill the parent entry node by node
	. new %1,%2 set (%1,%2)=$name(^ORD(100.01,1))
	. new keepme merge keepme=@%2
	. do assert($data(^ORD(100.01,1)))
	. for  set %1=$query(@%1) quit:$name(@%2,$qlength(%2))'=$name(@%1,$qlength(%2))  kill @%1
	. do assert('$data(^ORD(100.01,1)))
	. do assert('$data(@aimgbl@(1,nullxref_"dc")))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"dc")))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=15) ; Distinct entries
	... do assert(@aimgbl@(11)=15) ; Total entries
	. merge @%2=keepme
	. do assert($data(^ORD(100.01,1)))
	. do assert($data(@aimgbl@(1,nullxref_"dc")))
	. if stat=2 do
	.. do assert(@aimgbl@(-1)=16) ; Distinct entries
	.. do assert(@aimgbl@(11)=16) ; Total entries
	. ;
	. ; Kill parent entry all at once
	. new keepme merge keepme=^ORD(100.01,1)
	. kill ^ORD(100.01,1)
	. do assert('$data(^ORD(100.01,1)))
	. do assert('$data(@aimgbl@(1,nullxref_"dc")))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"dc")))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=15) ; Distinct entries
	... do assert(@aimgbl@(11)=15) ; Total entries
	. merge ^ORD(100.01,1)=keepme
	. do assert($data(^ORD(100.01,1)))
	. do assert($data(@aimgbl@(1,nullxref_"dc")))
	. if stat=2 do
	.. do assert(@aimgbl@(-1)=16) ; Distinct entries
	.. do assert(@aimgbl@(11)=16) ; Total entries
	.
	. ; Remove all data in ^ORD(101.01)
	. new keepme merge keepme=^ORD(100.01)
	. kill ^ORD(100.01)
	. do assert($data(@aimgbl@(1))=1)
	. if stat do
	.. do assert($data(@aimgbl@(-1))[0)
	.. if stat=2 do
	... do assert('$data(@aimgbl@(-1))) ; Distinct entries
	... do assert('@aimgbl@(11)) ; Total entries
	. merge ^ORD(100.01)=keepme
	. ;
	. ; Remove an existing sub entry. Should have 1 less entry.
	. new keepme merge keepme=^ORD(100.01,2,.1)
	. kill ^ORD(100.01,2,.1)
	. do assert($$type1cd(aimgbl)=15)
	. do assert($$type1cn(aimgbl)=0)
	. do assert('$data(@aimgbl@(1,nullxref,2)))
	. do assert('$data(@aimgbl@(1,nullxref_"c",2)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"c")))
	.. do assert('$data(@aimgbl@(-1,nullxref)))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=15) ; Distinct entries
	... do assert(@aimgbl@(11)=15) ; Total entries
	. ; Put back
	. merge ^ORD(100.01,2,.1)=keepme
	. do assert($$type1cd(aimgbl)=16)
	. do assert($$type1cn(aimgbl)=0)
	. do assert($data(@aimgbl@(1,nullxref_"c",2)))
	. do assert('$data(@aimgbl@(1,nullxref,2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"c")=1)
	.. do assert('$data(@aimgbl@(-1,nullxref)))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	... do assert(@aimgbl@(11)=16) ; Total entries
	quit
	;
v2type0 ; @TEST VistA type 0 - Initial data partly empty
	; Remove an existing entry. Should have same number of entries, but 1 null
	new fstr,nullxref,stat for fstr=0,1 set nullxref=$select(fstr:"#",1:"") for stat=0:1:2 do
	. ; write !,"stat: ",stat	; Uncomment for debugging
	. new keepme merge keepme=^ORD(100.01,2,.1)
	. kill ^ORD(100.01,2,.1)
	. ; gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type
	. ; Get AIM global with name only
	. new subs set subs(1)=100.01,subs(2)=":"" """,subs(3)=.1
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",1,1,0,1,stat,0,fstr)
	. ; Unxref the data
	. do UNXREFDATA^%YDBAIM("^ORD",.subs,"^",1,0,0,1,stat,"",fstr)
	. ; Assert that the data doesn't exist
	. do assert('$data(@aimgbl))
	. ; Initial index
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",1,0,0,1,stat,0,fstr)
	. do assert($$type1cd(aimgbl)=15)
	. do assert($$type1cn(aimgbl)=0)
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"dc")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=15) ; Distinct entries
	... do assert(@aimgbl@(11)=15) ; Total entries
	. ; Check that null entry doesn't exist
	. do assert('$data(@aimgbl@(1,nullxref,2)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref)))
	. ; Put back
	. merge ^ORD(100.01,2,.1)=keepme
	. do assert($$type1cd(aimgbl)=16)
	. do assert($$type1cn(aimgbl)=0)
	. do assert($data(@aimgbl@(1,nullxref_"c",2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"c")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	... do assert(@aimgbl@(11)=16) ; Total entries
	quit

v1type1s ; @TEST VistA type1 - Test triggers with string subs in xsub
	; https://gitlab.com/YottaDB/Util/YDBAIM/-/issues/58
	; Index data in ^ORD(100.01,:,.1) first ^ piece
	; ^ORD(100.01,0)="ORDER STATUS^100.01I^99^16"
	; ^ORD(100.01,1,0)="DISCONTINUED^dc"
	; ^ORD(100.01,1,.1)="dc"
	; ^ORD(100.01,1,1,0)="^^1^1^3070625^^^^"
	; ^ORD(100.01,1,1,1,0)="Orders that have been explicitly stopped."
	; ^ORD(100.01,1,"TERMSTATUS",0)="^100.0199DA^1^1"
	; ^ORD(100.01,1,"TERMSTATUS",1,0)="3070607.115705^1"
	; ^ORD(100.01,1,"TERMSTATUS","B",3070607.115705,1)=""
	; ^ORD(100.01,1,"VUID")="4500704^1"
	; ^ORD(100.01,2,0)="COMPLETE^comp"
	; ^ORD(100.01,2,.1)="c"
	; ^ORD(100.01,2,1,0)="^^3^3^3070607"
	; ^ORD(100.01,2,1,1,0)="Orders that require no further action by the ancillary service. "
	; ^ORD(100.01,2,1,2,0)=" e.g., Lab orders are completed when results are available, "
	; ^ORD(100.01,2,1,3,0)="Radiology orders are complete when results are available."
	; ^ORD(100.01,2,"TERMSTATUS",0)="^100.0199DA^1^1"
	; ^ORD(100.01,2,"TERMSTATUS",1,0)="3070607.115705^1"
	; ^ORD(100.01,2,"TERMSTATUS","B",3070607.115705,1)=""
	; ^ORD(100.01,2,"VUID")="4501088^1"
	; ...
	new fstr,nullxref,stat for fstr=0,1 set nullxref=$select(fstr:"#",1:"") for stat=0:1:2 do
	. ; write !,"stat: ",stat	; Uncomment for debugging
	. new subs set subs(1)=100.01,subs(2)=$ZWRITE(0)_":"_$ZWRITE($CHAR(0)),subs(3)="""VUID"""
	. ;
	. ; gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type
	. ; Get AIM global with name only
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",1,1,0,1,stat,1,fstr)
	. ; Unxref the data
	. do UNXREFDATA^%YDBAIM("^ORD",.subs,"^",1,0,0,1,stat,1,fstr)
	. ; Assert that the data doesn't exist (first time (stat=0) it won't, so the previous UNXREF will be a no-op)
	. do assert('$data(@aimgbl))
	. ; Xref data now
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",1,0,0,1,stat,1,fstr)
	. ;
	. ; Count entries. There are 16 entries, and no entry for zero, which comes out as NULL
	. ; So we expect a total of 17 entries in the index, and 1 NULL.
	. ; The reason we have a NULL for the zero node is a change done for this issue:
	. ; - https://gitlab.com/YottaDB/Util/YDBAIM/-/issues/60
	. ; WE NEED to capture null entries, as they can be significant for non-Fileman-compatible data in VistA
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert($data(@aimgbl@(1,nullxref_4500704,1)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_4500704)=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	... do assert(@aimgbl@(11)=17) ; Total entries
	. ;
	. ; Add data and make sure triggers works
	. ; Now set one of the .1 nodes to be non-existent on a new entry
	. set ^ORD(100.01,100,0)="JUNK STATUS^junk"
	. ; There should be now 18 entries, and 2 null entries
	. do assert($$type1cd(aimgbl)=18)
	. do assert($$type1cn(aimgbl)=2)
	. do assert($data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref)=2)
	.. ; Another extra entry to test statistics
	.. set ^ORD(100.01,101,0)="JUNK STATUS2^junk2"
	.. do assert(@aimgbl@(-1,nullxref)=3)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	... do assert(@aimgbl@(11)=19) ; Total entries
	.. kill ^ORD(100.01,101,0)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	... do assert(@aimgbl@(11)=18) ; Total entries
	.. do assert(@aimgbl@(-1,nullxref)=2)
	. ;
	. ; Set the VUID node to some data on entry 100
	. set ^ORD(100.01,100,"VUID")="9999999^1"
	. ; There should be now 18 entries, and 1  null entry
	. do assert($$type1cd(aimgbl)=18)
	. do assert($$type1cn(aimgbl)=1)
	. do assert($data(@aimgbl@(1,nullxref_9999999,100)))
	. do assert('$data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert(1=@aimgbl@(-1,nullxref))
	.. do assert(@aimgbl@(-1,nullxref_9999999)=1)
	. ;
	. ; Restore data to original
	. kill ^ORD(100.01,100)
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert('$data(@aimgbl@(1,nullxref_9999999,100)))
	. do assert('$data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_9999999)))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	... do assert(@aimgbl@(11)=17) ; Total entries
	. ;
	. ; Kill the parent entry node by node
	. new %1,%2 set (%1,%2)=$name(^ORD(100.01,1))
	. new keepme merge keepme=@%2
	. do assert($data(^ORD(100.01,1)))
	. for  set %1=$query(@%1) quit:$name(@%2,$qlength(%2))'=$name(@%1,$qlength(%2))  kill @%1
	. do assert('$data(^ORD(100.01,1)))
	. do assert('$data(@aimgbl@(1,nullxref_4500704,1)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_4500704)))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	... do assert(@aimgbl@(11)=16) ; Total entries
	. merge @%2=keepme
	. do assert($data(^ORD(100.01,1)))
	. do assert($data(@aimgbl@(1,nullxref_4500704,1)))
	. if stat=2 do
	.. do assert(@aimgbl@(-1)=17) ; Distinct entries
	.. do assert(@aimgbl@(11)=17) ; Total entries
	. ;
	. ; Kill parent entry all at once
	. new keepme merge keepme=^ORD(100.01,1)
	. kill ^ORD(100.01,1)
	. do assert('$data(^ORD(100.01,1)))
	. do assert('$data(@aimgbl@(1,nullxref_4500704)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_4500704)))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	... do assert(@aimgbl@(11)=16) ; Total entries
	. merge ^ORD(100.01,1)=keepme
	. do assert($data(^ORD(100.01,1)))
	. do assert($data(@aimgbl@(1,nullxref_4500704)))
	. if stat=2 do
	.. do assert(@aimgbl@(-1)=17) ; Distinct entries
	.. do assert(@aimgbl@(11)=17) ; Total entries
	.
	. ; Remove all data in ^ORD(101.01)
	. new keepme merge keepme=^ORD(100.01)
	. kill ^ORD(100.01)
	. do assert($data(@aimgbl@(1))=1)
	. if stat do
	.. do assert($data(@aimgbl@(-1))[0)
	.. if stat=2 do
	... do assert('$data(@aimgbl@(-1))) ; Distinct entries
	... do assert('@aimgbl@(11)) ; Total entries
	. merge ^ORD(100.01)=keepme
	. ;
	. ; Remove an existing sub entry. Should have same number of entries, but 1 null
	. new keepme merge keepme=^ORD(100.01,2,"VUID")
	. kill ^ORD(100.01,2,"VUID")
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=2)
	. do assert($data(@aimgbl@(1,nullxref,2)))
	. do assert('$data(@aimgbl@(1,nullxref_4501088,2)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_4501088)))
	.. do assert(@aimgbl@(-1,nullxref)=2)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	... do assert(@aimgbl@(11)=17) ; Total entries
	. ; Put back
	. merge ^ORD(100.01,2,"VUID")=keepme
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert($data(@aimgbl@(1,nullxref_4501088,2)))
	. do assert('$data(@aimgbl@(1,nullxref,2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_4501088)=1)
	.. do assert(@aimgbl@(-1,nullxref)=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	... do assert(@aimgbl@(11)=17) ; Total entries
	quit
	;
v2type1s ; @TEST VistA type1 - Initial data partly empty
	; Remove an existing entry. Should have same number of entries, but 1 null
	new fstr,nullxref,stat for fstr=0,1 set nullxref=$select(fstr:"#",1:"") for stat=0:1:2 do
        . ; write !,"stat: ",stat       ; Uncomment for debugging
        . new keepme merge keepme=^ORD(100.01,2,.1)
        . kill ^ORD(100.01,2,.1)
	. new subs set subs(1)=100.01,subs(2)=":"" """,subs(3)=.1
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",1,1,0,1,stat,1,fstr)
	. ; Unxref the data
	. do UNXREFDATA^%YDBAIM("^ORD",.subs,"^",1,0,0,1,stat,1,fstr)
	. ; Assert that the data doesn't exist
	. do assert('$data(@aimgbl))
	. ; Initial index
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",1,0,0,1,stat,1,fstr)
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=2)
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"dc")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	... do assert(@aimgbl@(11)=17) ; Total entries
	. ; Check that null entry exists
	. do assert($data(@aimgbl@(1,nullxref,2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref)=2)
	. ; Put back
	. merge ^ORD(100.01,2,.1)=keepme
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert($data(@aimgbl@(1,nullxref_"c",2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"c")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	... do assert(@aimgbl@(11)=17) ; Total entries
	quit
	;
type1cd(aimgbl) ; [type1t1, $$] Count data (including nulls)
	new count set count=0
	new i set i=""
	for  set i=$order(@aimgbl@(1,nullxref,i)) quit:i=""  if $increment(count)
	set i=nullxref for  set i=$order(@aimgbl@(1,i)) quit:i=""  if $increment(count)
	quit count
type1cn(aimgbl) ; [type1t1, $$] Count null
	new count set count=0
	new i set i=""
	for  set i=$order(@aimgbl@(1,nullxref,i)) quit:i=""  if $increment(count)
	quit count
	;
rupdate	; @TEST Range Update Works properly (uses Type 1 index)
	; This tests that if we index a range, updating the range works properly
	; Updating data outside the range does not.
	; ^ORD(100.01,0)="ORDER STATUS^100.01I^99^16"
	; ^ORD(100.01,1,0)="DISCONTINUED^dc"
	; ^ORD(100.01,1,.1)="dc"
	; ...
	; ^ORD(100.01,2,0)="COMPLETE^comp"
	; ^ORD(100.01,2,.1)="c"
	; ...
	; ^ORD(100.01,3,0)="HOLD^hold"
	; ^ORD(100.01,3,.1)="h"
	; ...
	; Index data in ^ORD(100.01,:,.1) first ^ piece
	; Range here is 0:2
	new subs set subs(1)=100.01,subs(2)="0:2",subs(3)=.1
	;
	; gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",1,0,0,1,2,1)
	;
	; Assert that data in 1,2 exists, not 3
	do assert($data(@aimgbl@(1,"c",2)))
	do assert($data(@aimgbl@(1,"dc",1)))
	do assert('$data(@aimgbl@(1,"h",3)))
	;
	; Add data in 1.5, and see that it gets into the index
	set ^ORD(100.01,1.5,.1)="fake"
	do assert($data(@aimgbl@(1,"fake",1.5)))
	; kill it (a node higher just for testing)
	kill ^ORD(100.01,1.5)
	do assert('$data(@aimgbl@(1,"fake",1.5)))
	;
	; Add data in 88, and see that it DOES NOT get into the index
	set ^ORD(100.01,88,.1)="fake2"
	do assert('$data(@aimgbl@(1,"fake2",88)))
	kill ^ORD(100.01,88)
	quit
	;
aim60a	; @TEST For type=1 a non-existent node has metadata created
	; There was an error/omission in the specification for #51 (closed),
	; where non-Fileman compatible data was stored and where NULLs in indexes
	; (represented as an empty string) did not show up in the AIM index,
	; resulting in the IS NULL query returning invalid data.
	kill ^x
	set ^x(1,"const",1)="a",^x(2,"const")="b"
	;
	new subs
	set subs(1)=":"
	set subs(2)="""const"":""constz"""
	set subs(3)=1
	;                                         g  , s   , s ,p,n,z,o,s,t
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^x",.subs,"|",1,0,0,1,0,1)
	do assert($data(@aimgbl@(1,"",2,"const")))
	do assert($data(@aimgbl@(1,"a",1,"const")))
	quit
	;
aim60b	; @TEST Metadata was incorrectly calculated unselected pieces
	; We index the 2 node; we should not see 1 node data present
	new subs set subs(1)=100.01,subs(2)=":"" """,subs(3)=0
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",2,0,0,1,2,1)
	do assert('$data(@aimgbl@(-1)))
	quit
	;
TW27p3	; @TEST Range to empty string doesn't work with AIM#42 changes
	new subs set subs(1)=100.01,subs(2)="0:",subs(3)=0
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",2,0,0,1,0,0)
	do assert($data(@aimgbl@(2,"dc",1)))
	quit
	;
tsigusr1 ; @TEST Interrupt using SIGUSR1
	; This is supposed to only do a job exam, and resume
	; $ZYINTRSIG is supposed to be SIGUSR1
	; Set interrupt code
	view "setenv":"ydb_zinterrupt":"do sigusrint^"_$text(+0)
	;
	; delete old data generated from interrupt (in case of re-runs)
	zsy "rm -f tsigusr1.jobexam"
	kill ^tsigusr
	;
	; Get AIM index name for later use
	new subs set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^PSNDF",.subs,"^",1,1) ; nmonly
	;
	; Job off AIM jobs that will be signaled
	new jobpid job jobsigusr:passcurlvn set jobpid=$zjob
	;
	; Wait till the parallel process children show up
	for  quit:$data(^%ydbAIMtmp("%YDBAIM",jobpid,0))  hang .01
	;
	; give it some time to index
	for  quit:$data(@aimgbl@(1,"A"))  hang .0001
	;
	; Interrupt with USR1
	if $ZSIGPROC(jobpid,"SIGUSR1")
	;
	; Wait till interrupt is processed
	for  quit:$zsearch("tsigusr1.jobexam")'=""  hang .01
	;
	; Count data six times. Each count should be greater than the previous one.
	do sigusrcount(1,aimgbl) h .001
	do sigusrcount(2,aimgbl) h .001
	do sigusrcount(3,aimgbl) h .001
	do sigusrcount(4,aimgbl) h .001
	do sigusrcount(5,aimgbl) h .001
	do sigusrcount(6,aimgbl)
	;
	; Assert that the signal is USR1
	do assert(^tsigusr="SIGUSR1")
	;
	; Wait for AIM job to die (this takes a while as we are still indexing)
	for  quit:'$zgetjpi(jobpid,"ISPROCALIVE")  hang 0.01
	;
	; Reset envionment
	view "unsetenv":"ydb_zinterrupt"
	;
	; Final count
	do sigusrcount(7,aimgbl)
	;
	; ^tsigusr(1,"count","A")=388
	; ^tsigusr(1,"count","B")=410
	; ^tsigusr(2,"count","A")=545
	; ^tsigusr(2,"count","B")=562
	; ^tsigusr(3,"count","A")=693
	; ^tsigusr(3,"count","B")=713
	; ^tsigusr(4,"count","A")=847
	; ^tsigusr(4,"count","B")=873
	; ^tsigusr(5,"count","A")=1014
	; ^tsigusr(5,"count","B")=1047
	; ^tsigusr(6,"count","A")=1192
	; ^tsigusr(6,"count","B")=1225
	; ...
	; ^tsigusr(7,"count","A")=497000
	; ^tsigusr(7,"count","B")=497001
	;
	; Final count should be the full count
	do assert(^tsigusr(7,"count","A")=497000)
	do assert(^tsigusr(7,"count","B")=497001)
	;
	; Previous count should not be zero.
	do assert(^tsigusr(6,"count","A"))
	do assert(^tsigusr(6,"count","B"))
	;
	; The previous count should be much less. Assert that.
	do assert(^tsigusr(6,"count","A")<^tsigusr(7,"count","A"))
	do assert(^tsigusr(6,"count","B")<^tsigusr(7,"count","B"))
	quit
	;
tsigusr2 ; @TEST Interrupt using SIGUSR2
	; This is supposed to only do a job exam, and stop the indexing
	; $ZYINTRSIG is supposed to be SIGUSR2
	; SIGUSR2 processing is only activated when we set ydb_treat_sigusr2_like_sigusr1
	view "setenv":"ydb_treat_sigusr2_like_sigusr1":"1"
	view "setenv":"ydb_zinterrupt":"do sigusrint^"_$text(+0)
	;
	; delete old data generated from interrupt (in case of re-runs)
	zsy "rm -f tsigusr2.jobexam"
	kill ^tsigusr
	;
	; Get AIM index name for later use
	new subs set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^PSNDF",.subs,"^",1,1) ; nmonly
	;
	; Job off AIM jobs that will be signaled
	new jobpid job jobsigusr:passcurlvn set jobpid=$zjob
	;
	; Wait till the parallel process children show up
	for  quit:$data(^%ydbAIMtmp("%YDBAIM",jobpid,0))  hang .01
	;
	; give it some time to index
	for  quit:$data(@aimgbl@(1,"A"))  hang .0001
	;
	; Interrupt with USR2
	if $ZSIGPROC(jobpid,"SIGUSR2")
	;
	; Wait till interrupt is processed
	for  quit:$zsearch("tsigusr2.jobexam")'=""  hang .01
	;
	; Count data six times. The counts should all the same, as we stopped indexing.
	do sigusrcount(1,aimgbl) h .001
	do sigusrcount(2,aimgbl) h .001
	do sigusrcount(3,aimgbl) h .001
	do sigusrcount(4,aimgbl) h .001
	do sigusrcount(5,aimgbl) h .001
	do sigusrcount(6,aimgbl)
	;
	; Assert that the signal is USR2
	do assert(^tsigusr="SIGUSR2")
	;
	; Assert that the zgoto in the interrupt handler actually executed
	do assert($data(^tsigusr("finish")))
	;
	; Check that the count is identical
	do assert(^tsigusr(5,"count","A")=^tsigusr(6,"count","A"))
	do assert(^tsigusr(5,"count","B")=^tsigusr(6,"count","B"))
	;
	; Reset environment
	view "unsetenv":"ydb_zinterrupt"
	view "unsetenv":"ydb_treat_sigusr2_like_sigusr1"
	;
	; Wait for AIM job to die (this is fast as indexing is done)
	for  quit:'$zgetjpi(jobpid,"ISPROCALIVE")  hang 0.001
	quit
	;
jobsigusr ; [job for tsigusr1 and tsigusr2]
	if $$XREFDATA^%YDBAIM("^PSNDF",.subs,"^",1)
	quit
	;
sigusrint ; [interrupt for tsigusr1 and tsigusr2]
	set ^tsigusr=$ZYINTRSIG
	if $ZYINTRSIG="SIGUSR1",$zlength($zjobexam("tsigusr1.jobexam")) quit
	if $ZYINTRSIG="SIGUSR2",$zlength($zjobexam("tsigusr2.jobexam")) goto sigusrjobcancel
	quit
	;
sigusrjobcancel ;
	zgoto 1:sigusr2finish
	;;;
sigusrcount(n,aimgbl) ;
	do sigusrcountA(n,aimgbl)
	do sigusrcountB(n,aimgbl)
	quit
	;
sigusrcountA(n,aimgbl) ;
	new count,i set count=0,i=""
	for  set i=$order(@aimgbl@(1,"A",i)) quit:'i  if $increment(count)
	set ^tsigusr(n,"count","A")=count
	quit
	;
sigusrcountB(n,aimgbl) ;
	new count,i set count=0,i=""
	for  set i=$order(@aimgbl@(1,"B",i)) quit:'i  if $increment(count)
	set ^tsigusr(n,"count","B")=count
	quit
	;
sigusr2finish ;
	set ^tsigusr("finish")=$ZUT
	quit
	;
tintrestore ; @TEST Test that the AIM restores the original interrupt
	kill ^tintrestore
	view "setenv":"ydb_zinterrupt":"do intintrestore^"_$text(+0)
	job jobintrestore
	new myJob set myJob=$zjob
	for  quit:$data(^tintrestore("after"))  hang 0.001
	do assert(^tintrestore("before")=^tintrestore("after"))
	quit
	;
jobintrestore ; [job target for tintrestore]
	set ^tintrestore("before")=$zinterrupt
	if $$XREFDATA^%YDBAIM("^customers",1,"§",3)
	set ^tintrestore("after")=$zinterrupt
	quit
intintrestore ; [no op interrupt; not a typo]
	quit
