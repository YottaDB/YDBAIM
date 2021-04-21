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
%YDBAIMTEST ; Test Runner for YDBAIM
	; Please leave debugging commands in as a hint for the next user
	;zb tinv2
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
	quit
	;
TEARDOWN ; Runs after each test
	D UNXREFDATA^%YDBAIM
	QUIT
	;
assert:(boolexpr,msg) ; [private] shortcut to tf^%ut
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
	close f:delete
	quit
	;
tinv1	; @TEST Invalid Input: Global without ^
	new ecodetest
	new $etrap set $etrap="goto err^"_$T(+0)
	if $$XREFDATA^%YDBAIM("ORD",3,"^",2)
	do assert(ecodetest="U252")
	quit
	;
tinv2	; @TEST Invalid Input: Global including subscripts
	new ecodetest
	new $etrap set $etrap="goto err^"_$T(+0)
	if $$XREFDATA^%YDBAIM("^ORD(100.01)",2,"^",2)
	do assert(ecodetest="U239")
	quit
	;
tinv3	; @TEST Invalid Input: Bad Level
	new ecodetest
	new $etrap set $etrap="goto err^"_$T(+0)
	if $$XREFDATA^%YDBAIM("^ORD",0,"^",2)
	do assert(ecodetest="U253")
	new ecodetest
	if $$XREFDATA^%YDBAIM("^ORD",-1,"^",2)
	do assert(ecodetest="U253")
	quit
	;
tinv4	; @TEST Invalid Input: Bad Piece Number
	new ecodetest
	new $etrap set $etrap="goto err^"_$T(+0)
	if $$XREFDATA^%YDBAIM("^ORD",3,"^",0)
	do assert(ecodetest="U248")
	new ecodetest
	if $$XREFDATA^%YDBAIM("^ORD",3,"^",-1)
	do assert(ecodetest="U248")
	quit
	;
err	; Error trap for tinv* tests
	set ecodetest=$ecode
	set ecodetest=$piece(ecodetest,",",2) ; Change ,U252, -> U252
	set $ecode=""
	quit:$quit "" quit
	;
tsubs1	; @TEST subs w numbers & :/Index single node in VistA Global
	new subs set subs(1)=100.01,subs(2)=":",subs(3)=0
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",2)
	do assert($data(@aimgbl@(2,"dc",1)),"subs array works with numeric subscripts,: for varying range")
	; Add data and make sure triggers works
	set ^ORD(100.01,100,0)="JUNK STATUS^junk"
	do assert($data(@aimgbl@(2,"junk",100)),"set trigger works")
	; Delete data ditto
	; TODO: Currently fails. Tracked at https://gitlab.com/YottaDB/Util/YDBAIM/-/issues/26.
	kill ^ORDER(100.01,99)
	do assert('$data(@aimgbl@(2,"none",99)),"kill trigger works")
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
tsubs3	; @TEST index non-numeric data in last sub; use * to specify last sub, top level subs not specified
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",12)
	do assert($data(@aimgbl@(12,1231,"numeric")),"non-numeric data properly indexed")
	quit
	;
tsubs4	; @TEST indexing different subs in the same global produces different aim globals
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_namespace""",subs=4
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",1)
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",1)
	do assert(aimgbl1'=aimgbl2)
	quit
	;
tsubs5	; @TEST indexing different pieces in the same global produces the same aim global
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",12)
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",11)
	do assert(aimgbl1=aimgbl2)
	quit
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
	; 17th piece does not exist
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^customers",1,"§",17)
	do assert($order(@aimgbl@(17,""))="")
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
	do assert('$data(@aimgbl1@(7,"",6)))
	kill ^customers(6)
	quit
	;
tmpiece	; @TEST multiple pieces requested sequentially
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM("^customers",1,"§","3;7")
	do assert($order(@aimgbl1@(3,""))'="")
	do assert($order(@aimgbl1@(7,""))'="")
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM("^customers",1,"§","3:4")
	do assert(aimgbl1=aimgbl2)
	do assert($order(@aimgbl1@(4,""))'="")
	new lines do trigout(.lines)
	do assert(lines(2)["-delim=""§"" -pieces=3:4;7")
	new aimgbl3 set aimgbl3=$$XREFDATA^%YDBAIM("^customers",1,"§","4:5")
	new lines do trigout(.lines)
	do assert(lines(2)["-delim=""§"" -pieces=3:5;7")
	new aimgbl4 set aimgbl4=$$XREFDATA^%YDBAIM("^customers",1,"§","5:8;10")
	new lines do trigout(.lines)
	do assert(lines(2)["-delim=""§"" -pieces=3:8;10")
	new aimgbl5 set aimgbl5=$$XREFDATA^%YDBAIM("^customers",1,"§","1;2;5")
	new lines do trigout(.lines)
	do assert(lines(2)["-delim=""§"" -pieces=1:8;10")
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
