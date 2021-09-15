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
	;zb err
	;zb tinv8+5
	;zb tinv12
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
	new numrows set numrows=1000
	for i=1:4:numrows set ^names(i)="A|B"
	for i=2:4:numrows set ^names(i)="C|B"
	for i=3:4:numrows set ^names(i)="A|C"
	for i=4:4:numrows set ^names(i)="B|A"
	;
	; The below global variable nodes are needed by the speed test
	kill ^names,^names2,^composite
	new numrows set numrows=1000
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
TEARDOWN ; Runs after each test
	D UNXREFDATA^%YDBAIM
	QUIT
	;
assert:(boolexpr,msg) ; [private] shortcut to tf^%ut
	if '$data(%ut) do  quit
	. if 'boolexpr set $ecode=",U-ASSERT,"
	;
	if $get(msg)="" set msg="Error from "_$stack($stack-1,"place")
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
	; Delete data ditto
	; TODO: Currently fails. Tracked at https://gitlab.com/YottaDB/Util/YDBAIM/-/issues/26.
	;kill ^ORDER(100.01,99)
	;do assert('$data(@aimgbl@(2,"none",99)),"kill trigger works")
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
; Disabling this test, as it is not valid till enhancement for YDBAIM#51 
; tnodata3 ; @TEST indexing fixed nodes that don't exist
; 	; Data looks like this. We will try indexing the .2 node.
; 	; ^ORD(100.01,14,0)="LAPSED^laps"
; 	; ^ORD(100.01,14,.1)="l"
; 	; ^ORD(100.01,14,1,0)="^^3^3^3070607"
; 	; ^ORD(100.01,14,1,1,0)="Orders that remain pending beyond their start date, by a site "
; 	; ^ORD(100.01,14,1,2,0)="defined number of days; unreleased orders that meet this same "
; 	; ^ORD(100.01,14,1,3,0)="criteria will be removed from the system."
; 	; ^ORD(100.01,14,"TERMSTATUS",0)="^100.0199DA^1^1"
; 	; ^ORD(100.01,14,"TERMSTATUS",1,0)="3070607.115705^1"
; 	; ^ORD(100.01,14,"TERMSTATUS","B",3070607.115705,1)=""
; 	; ^ORD(100.01,14,"VUID")="4501099^1"
; 	new subs,aimgbl
; 	set subs(1)=100.01
; 	set subs(2)=":"
; 	set subs(3)=.2
; 	set aimgbl=$$XREFDATA^%YDBAIM("^ORD",.subs,"^",1)
; 	; zwrite @aimgbl
; 	do assert($data(@aimgbl@(1,"",1)))
; 	do assert($data(@aimgbl@(1,"",2)))
;	quit
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
	do assert(aims=2)
	do assert(trigs(3)["=3,7")
	do UNXREFDATA^%YDBAIM("^customers",1,"§")
	kill trigs do trigout(.trigs)
	kill aims  do aimgbls(.aims)
	do assert(aims=1)
	do assert(trigs(3)'["=3,7")
	quit
	;
trmindex3 ; @TEST Remove all indexes
	if $$XREFDATA^%YDBAIM("^orders",1,"§",1)
	if $$XREFDATA^%YDBAIM("^customers",1,"§",3)
	if $$XREFDATA^%YDBAIM("^customers",1,"§",7)
	new trigs do trigout(.trigs)
	new aims  do aimgbls(.aims)
	do assert(aims=2)
	do assert(trigs(3)["=3,7")
	do UNXREFDATA^%YDBAIM
	kill trigs do trigout(.trigs)
	kill aims  do aimgbls(.aims)
	do assert('$data(trigs))
	do assert('$data(aims))
	quit
	;
trmindex4 ; @TEST Remove index on subscripts parts of same global
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",1)
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_namespace""",subs(4)="*"
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",2)
	new aims do aimgbls(.aims)
	do assert(aims=2)
	kill subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	do UNXREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",1)
	kill aims do aimgbls(.aims)
	do assert(aims=1)
	kill subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_namespace""",subs(4)="*"
	do UNXREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",2)
	kill aims do aimgbls(.aims)
	do assert('$data(aims))
	quit
	;
trmindex5 ; @TEST Remove indexes by %ydbAIMD global name
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM("^orders",1,"§",1)
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM("^customers",1,"§",3)
	new aimgbl3 set aimgbl3=$$XREFDATA^%YDBAIM("^customers",1,"§",7)
	new aims do aimgbls(.aims)
	do assert(aims=2)
	do UNXREFDATA^%YDBAIM(aimgbl1)
	kill aims do aimgbls(.aims)
	do assert(aims=1)
	do UNXREFDATA^%YDBAIM(aimgbl2)
	kill aims do aimgbls(.aims)
	do assert('$data(aims))
	quit
	;
tresume ; @TEST Resuming an interrupted cross-reference
	; Start job, wait, and interrupt
	new subs set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^PSNDF",.subs,"^",1,1) ; nmonly
	kill ^tresumejobdone
	job tresumejob:passcurlvn
	for  quit:$get(^tresumejobdone)  hang .0001 ; marker to ensure job started
	kill ^tresumejobdone ; don't need anymore
	hang .01 ; Wait a tiny bit to make sure AIM does something
	if $ZSIGPROC($zjob,"SIGUSR1") ; stop the job (job does zgoto 0 in $zint which stops it)
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
	set ^tresumejobdone=1
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
	new i for i=0:0 set i=$order(@aimgbl@(1,i)) quit:'i  do
	. new j for j=0:0 set j=$order(@aimgbl@(1,i,j)) quit:'j  if $increment(indexcount)
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
	new subs set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^PSNDF",.subs,"^",1,1) ; nmonly
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

tspeed1	; @TEST Index 1000 rows of ^names(:)="A|B"
	if $$XREFDATA^%YDBAIM("^names",1,"|",2)
	quit
	;
tspeed2	; @TEST Index 1000 rows of ^names2("fix",:,0)="A|B"
	new subs set subs(1)="""fix""",subs(2)=":",subs(3)=0
	if $$XREFDATA^%YDBAIM("^names2",.subs,"|",2)
	quit
	;
tspeed3	; @TEST Index 1000 rows of ^composite(1,2,3,4,5,6,7,:)="A|B"
	if $$XREFDATA^%YDBAIM("^composite",8)
	quit
	;
tspeed4	; @TEST Index ~5000 dispersed rows in ^PSNDF(50.6,:,"VUID")
	set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	if $$XREFDATA^%YDBAIM("^PSNDF",.subs,"^",1)
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
	; ^%ydbAIMDXtoXI8zgdGIqUuCoYeqwF6(-1,"A")=500
	; ^%ydbAIMDXtoXI8zgdGIqUuCoYeqwF6(-1,"B")=250
	; ^%ydbAIMDXtoXI8zgdGIqUuCoYeqwF6(-1,"C")=250
	; ^%ydbAIMDXtoXI8zgdGIqUuCoYeqwF6(11)=1000
	do assert(@aimgbl@(-1)=3)
	do assert(@aimgbl@(-1,"A")=500)
	do assert(@aimgbl@(-1,"B")=250)
	do assert(@aimgbl@(-1,"C")=250)
	do assert(@aimgbl@(11)=1000)
	;
	set ^names(1001)="A|Q"
	set ^names(1002)="Z|F"
	;
	do assert(@aimgbl@(-1)=4)
	do assert(@aimgbl@(-1,"A")=501)
	do assert(@aimgbl@(-1,"B")=250)
	do assert(@aimgbl@(-1,"C")=250)
	do assert(@aimgbl@(-1,"Z")=1)
	do assert(@aimgbl@(11)=1002)
	;
	; ^names(1)="A|B"
	; ^names(2)="C|B"
	kill ^names(1),^names(2)
	;
	do assert(@aimgbl@(-1)=4)
	do assert(@aimgbl@(-1,"A")=500)
	do assert(@aimgbl@(-1,"B")=250)
	do assert(@aimgbl@(-1,"C")=249)
	do assert(@aimgbl@(-1,"Z")=1)
	do assert(@aimgbl@(11)=1000)
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
gvsuboflowhelper:    ;
	do UNXREFDATA^%YDBAIM
	kill ^x
	for i=1:1:10 set ^x(i)=$j(2**i,2**i)
	set $etrap="zwrite $zstatus set $ecode="""" zhalt +$zstatus"
	set aimglobal=$$XREFDATA^%YDBAIM("^x",1)
	zwrite aimglobal	; this line should not be reached as GVSUBOFLOW error should transfer control in previous line
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

