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
	;zb tinv10
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
	new numrows set numrows=1000
	for i=1:4:numrows set ^names(i)="A|B"
	for i=2:4:numrows set ^names(i)="C|B"
	for i=3:4:numrows set ^names(i)="A|C"
	for i=4:4:numrows set ^names(i)="B|A"
	;
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
	for  set %=$order(@%) quit:%=""  quit:%'[%1  set:$increment(array) array(%)=""
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
tinv5	; @TEST Invalid Input: More subs than the number we specify at the top node
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
err	; Error trap for tinv* tests
	; ; do before creating any local variables just in case transaction deletes them (TSTART (*))
	trollback:$tlevel
	;
	; Capture $ecode, strip commas, clear
	set ecodetest=$ecode
	set ecodetest=$piece(ecodetest,",",2) ; Change ,U252, -> U252
	set $ecode="" ; Need to do this otherwise M does an emergency trap on the ,U-unwind,
	;
	; Set-up unwinder
	set $etrap="goto errunwind^"_$text(+0)
	set $ecode=",U-unwind,"
	quit:$quit "" quit
	;
errunwind ; Unwinder (if we don't unwind, we can possibly continue inside %YDBAIM [happens in tinv10])
	quit:($estack>1&$quit) ""
	quit:$estack>1
	set $ecode=""
	set $zstatus=""
	set $etrap="goto err^"_$text(+0)
	quit:$quit "" quit
	;
tsubs1	; @TEST subs w numbers & :/Index single node in VistA Global; no sub at top node
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
tsubs3	; @TEST index non-numeric data in last sub; use * to specify last sub, top level subs not specified
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",12)
	do assert($data(@aimgbl@(12,1231,"numeric")),"non-numeric data properly indexed")
	quit
	;
tsubs4	; @TEST indexing different subs in the same global produces different aim globals; omit last
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
	do assert(trigs(2)["-delim=""§"" -pieces=3;7")
	do assert(aims=1)
	;
	do UNXREFDATA^%YDBAIM("^customers",1,"§",7)
	new trigs do trigout(.trigs)
	new aims  do aimgbls(.aims)
	do assert(trigs(2)["-delim=""§"" -pieces=3")
	do assert(aims=1)
	quit
	;
trmindex2 ; @TEST Remove indexs on a specific entire global
	if $$XREFDATA^%YDBAIM("^orders",1,"§",1)
	if $$XREFDATA^%YDBAIM("^customers",1,"§",3)
	if $$XREFDATA^%YDBAIM("^customers",1,"§",7)
	new trigs do trigout(.trigs)
	new aims  do aimgbls(.aims)
	do assert(aims=2)
	do assert(trigs(2)["-delim=""§"" -pieces=3;7")
	do UNXREFDATA^%YDBAIM("^customers",1,"§")
	new trigs do trigout(.trigs)
	new aims  do aimgbls(.aims)
	do assert(aims=1)
	do assert(trigs(2)'["-delim=""§"" -pieces=3;7")
	quit
	;
trmindex3 ; @TEST Remove all indexes
	if $$XREFDATA^%YDBAIM("^orders",1,"§",1)
	if $$XREFDATA^%YDBAIM("^customers",1,"§",3)
	if $$XREFDATA^%YDBAIM("^customers",1,"§",7)
	new trigs do trigout(.trigs)
	new aims  do aimgbls(.aims)
	do assert(aims=2)
	do assert(trigs(2)["-delim=""§"" -pieces=3;7")
	do UNXREFDATA^%YDBAIM
	new trigs do trigout(.trigs)
	new aims  do aimgbls(.aims)
	do assert('$data(trigs))
	do assert('$data(aims))
	quit
	;
trmindex4 ; @TEST Remove indexes on subscripts parts of the same global
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",1)
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_namespace""",subs(4)="*"
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",2)
	new aims do aimgbls(.aims)
	do assert(aims=2)
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	do UNXREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",1)
	new aims do aimgbls(.aims)
	do assert(aims=1)
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_namespace""",subs(4)="*"
	do UNXREFDATA^%YDBAIM("^%ydboctoocto",.subs,"|",2)
	new aims do aimgbls(.aims)
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
	new aims do aimgbls(.aims)
	do assert(aims=1)
	do UNXREFDATA^%YDBAIM(aimgbl2)
	new aims do aimgbls(.aims)
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
tomitfix ; @TEST Omit fixed subscripts aka omitfix
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
tnmonly	; @TEST Name only paramater aka nmonly
 	new subs set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM("^PSNDF",.subs,"^",1,1) ; nmonly
	do assert('$data(@aimgbl),"Shouldn't have been created yet")
	quit
	;
tlsxref	; @TEST $$LSXREFDATA
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
tstat	; @TEST stats
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
