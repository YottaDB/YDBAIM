	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;								;
	; Copyright (c) 2021-2026 YottaDB LLC and/or its subsidiaries.	;
	; All rights reserved.						;
	;								;
	;	This source code contains the intellectual property	;
	;	of its copyright holder(s), and is made available	;
	;	under a license.  If you do not know the terms of	;
	;	the license, please stop and do not read further.	;
	;								;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
%YDBAIMTEST(xdref,aimtype) ; Test Runner for YDBAIM
	; Input: xdref:   If 1, test extended refs, if 0, test normal refs
	;        aimtype: The type every call that does not need a particular type passes:
	;                 0, or 4 to run the whole suite through the record-specification code. Given
	;                 type=4 and no record specification, XREFDATA() takes the whole value
	;                 specification as the record specification, which makes the two agree except on
	;                 a value node that exists with no value but has children.
	;
	; Please leave debugging commands in as a hint for the next user
	;zb err
	;zb tinv8+5
	;zb tinv12
	;zb type1t1^%YDBAIMTEST
	;s $zstep="zp @$zpos b"
	;do en^%ut($t(+0),3,1)
	; end debug commands
	;
	set xdref=+$get(xdref),aimtype=+$get(aimtype)
	set xdrefgld=$zsearch($zdirectory_"../db2/"_$ztrnlnm("ydb_rel")_"/g/yottadb.gld",-1)
	set xdrefloc="|"""_xdrefgld_"""|"
	view "jobpid":1
	do en^%ut($t(+0),3)
	quit
	;
STARTUP	; Runs once to create test data
	; There is a bunch of zwr files that got loaded before we got here.
	if xdref new $zgbldir set $zgbldir=xdrefgld
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
	if xdref new $zgbldir set $zgbldir=xdrefgld
	D UNXREFDATA^%YDBAIM,UNXREFSUB^%YDBAIM
	QUIT
	;
SHUTDOWN ; Runs once, after the whole suite - the counterpart of STARTUP
	; Remove the temporary databases aim93init built - only now, when no test can reach
	; one again. aim93exit explains why not sooner.
	open "pipe":(shell="/bin/sh":command="rm -rf "_$$aim93dir()_"*":writeonly:readonly:independent)::"pipe"
	close "pipe"
	QUIT
	;
LOADDATA(lab)	; load data after label lab
	if xdref new $zgbldir set $zgbldir=xdrefgld
	new i,line
	for i=1:1 set line=$piece($text(@(lab_"+"_i)),";",2) quit:'$zlength(line)  set @line
	quit:$quit i quit
	;
replace:(src,from,to) ; stolen from Octo
	; replace whole character string 'from' with whole character string 'to'
	new i,j,k
	new len,n,res,ch,end,kstr,ich
	; length of src
	SET len=$length(src)
	; length of from
	SET n=$length(from)
	; j is used to iterate `from`
	SET j=1
	; i is used to iterate `src`
	SET i=1
	; res holds the final value to be returned
	SET res=""
	for  do  QUIT:(i>len)
	. set ch=$extract(src,i)
	. if (ch=$extract(from,j)) do
	.. if n=1 set res=res_to,i=i+1 QUIT
	.. set end=0,k=1 ; k is used to iterate over `from`, `end` is set where there is no match to exit loop
	.. set kstr=ch
	.. for  do  QUIT:(end=1)!(k=n)
	... set ich=$extract(src,i+k)
	... if '(ich=$extract(from,j+k)) set end=1
	... else  set k=k+1,kstr=kstr_ich
	.. if (1'=end) set res=res_to,i=i+n ; for iterate over entire `from` so replace it
	.. else  set res=res_kstr,i=i+k
	. else  set res=res_ch,i=i+1 ; no matching char, copy current value to `res`
	QUIT res
assert:(boolexpr,msg) ; [private] shortcut to tf^%ut
	if '$data(%ut) do  quit
	. if 'boolexpr set $ecode=",U-ASSERT,"
	;
	if $get(msg)="" set msg="Error from "_$stack($stack-1,"place")_": "_$stack($stack-1,"mcode")
	do tf^%ut(boolexpr,$get(msg))
	; Only produce jobexam file if the assert failed
	if 'boolexpr,$zjobexam($stack($stack-1,"place")_"."_$zut_".jobexam")
	quit
	;
forline:(lines) ; [private] the index of the trigger code line that holds the piece loop, "for i="; the comment lines ahead of it vary by template
	new i set i="" for  set i=$order(lines(i)) quit:i=""  quit:lines(i)["for i="
	quit +i
	;
trigout:(lines,patt) ; [private] output triggers into .lines
	if xdref new $zgbldir set $zgbldir=xdrefgld
	new f set f="triggers.txt"
	open f:newversion use f
	view "ztrigger_output":1
	if $ztrigger("s",$get(patt,"*"))
	use f:rewind
	new i for i=1:1 read lines(i)  quit:$zeof
	kill lines(i) ; last line is empty due to $zeof
	close f:delete
	quit
	;
aimgbls:(array,type) ; [private] output all AIM globals; type=1 means AIM subscript gbls
	if xdref new $zgbldir set $zgbldir=xdrefgld
	new %,%1,var
	if +$get(type) set (%,%1)="^%ydbAIMS",var="^%ydbAIMSxref"
	else  set (%,%1)="^%ydbAIMD",var="^%ydbAIMDxref"
	for  set %=$order(@%) quit:%=""  quit:%'[%1  if %'=var set:$increment(array) array(%)=""
	quit
	;
aimxref:(array,type) ; [private] count globals in ^%ydbAIMDxref; type=1 means ^%ydbAIMSxref
	if xdref new $zgbldir set $zgbldir=xdrefgld
	new %,var set %="",var=$select(+$get(type):"^%ydbAIMSxref",1:"^%ydbAIMDxref")
	for  set %=$order(@var@(%)) quit:%=""  set:$increment(array) array(%)=""
	quit
	;
aim72	; @TEST YDBAIM#72 - cross referencing continues after first value of lowest level subscript...
	; Test that cross referencing continues after first value of lowest level subscript if xsub
	; has range syntax with empty range-end
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"x"
	new i,j,s,x
	kill @g
	set s(1)="2:"
	for i=1:1:5 set @g@(i)="abcd|efgh"
	set x=$$XREFDATA^%YDBAIM(g,.s,"|",1,,,,,aimtype)
	set j="#"
	for i=1:1:5 set j=j_$data(@x@(1,"abcd",i))
	do assert("#01111"=j)
	; Test XREFSUB()
	set x=$$XREFSUB^%YDBAIM(g,.s,1)
	do assert(@x@(3)=$zbitstr(1,1))
	kill @g
	quit
	;
aim73	; @TEST YDBAIM#73 - glass box test for prefixing cross references with "#" when force=1
	; Test XREFSUB() alongside XREFDATA()
	new i,j,k,l,nodes,npieces,nsubs,pnum,ref,testval,sub,tvxref,varpat,x,xs,xsr,y,ys,z,zs
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"x"
	kill @g
	; Default type (0)
	; - Entire node - start by simulating application global data
	set nsubs=1+$random(3)				; No. of subscripts for simulated application global
	set nodes=$$aim73genblk(nsubs,10000,1,1000)	; Generate simulated application global; values are entire nodes
	set testval=$select($random(2):-1-$random(1000),1:1000+$random(1000))	; Application global value not generated above
	set tvxref="#"_testval				; Xref for testval forced to be a string
	set xsr=1+$random(nsubs)			; Random subscript to xref
	; Indirect variable pattern for simulated application global guaranteed to not already exist
	set varpat=$$replace($$aim73genpat(nsubs),"(3","(4")
	;   - No statistics
	;     - Initial scan; large number of nodes to hopefully force 2 processes
	set x=$$XREFDATA^%YDBAIM(g,nsubs,,,,,,,aimtype,1),xs=$$XREFSUB^%YDBAIM(g,.nsubs,xsr,,,,,1)
	set y="" for  set y=$order(@x@(0,y)) quit:'$zlength(y)  do assert("#"=$zextract(y,1))
	set ys="" for  set ys=$order(@xs@(xsr,ys)) quit:'$zlength(ys)  do assert("#"=$zextract(ys,1))
	;     - Check set trigger by setting a value not previously set
	set @varpat=testval,ref=$reference,sub=$qsubscript(ref,xsr)
	do assert(10=$data(@x@(0,tvxref))),assert($data(@xs@(xsr,"#"_sub))\10)
	;     - Check kill trigger by killing the value just set
	kill @ref do assert('$data(@x@(0,tvxref))),assert('($data(@xs@(xsr,"#"_sub))\10))
	;     - Check complete removal of AIM data
	do UNXREFDATA^%YDBAIM(x),assert('$data(@x)),UNXREFSUB^%YDBAIM(xs),assert('$data(@xs))
	;   - Statistics=1
	;     - Initial scan
	set x=$$XREFDATA^%YDBAIM(g,nsubs,,,,,,1,aimtype,1),xs=$$XREFSUB^%YDBAIM(g,.nsubs,xsr,,,1,,1)
	set y="" for  set y=$order(@x@(0,y)) quit:'$zlength(y)  do assert("#"=$zextract(y,1))
	set ys="" for  set ys=$order(@xs@(xsr,ys)) quit:'$zlength(ys)  do assert("#"=$zextract(ys,1))
	set i=0,y="" for  set y=$order(@x@("",y)) quit:'$zlength(y)  if $increment(i,@x@("",y))
	set j=0,ys="" for  set ys=$order(@xs@(xsr,ys)) quit:'$zlength(ys)  if $increment(j,@xs@(-xsr,ys))
	do assert(nodes=i),assert(nodes=j)    ; Confirm that all nodes are counted
	;     - Check set trigger
	set varpat=$$replace(varpat,"(4","(5"),@varpat=testval,ref=$reference,sub=$qsubscript(ref,xsr)
	do assert($data(@x@(0,tvxref))),assert(1=@x@("",tvxref))
	do assert($data(@xs@(xsr,"#"_sub))\10),assert(1=@xs@(-xsr,"#"_sub))
	;     - Check kill trigger
	kill @ref
	do assert('$data(@x@(0,tvxref))),assert('$data(@x@("",tvxref)))
	do assert('($data(@xs@(xsr,"#"_sub))\10)),assert('$data(@xs@(-xsr,"#"_sub)))
	;     - Check removal
	do UNXREFDATA^%YDBAIM(x),assert('$data(@x)),UNXREFSUB^%YDBAIM(g,.nsubs,xsr,,,1,,1),assert('$data(@xs))
	;   - Statistics=2
	;     - Initial scan
	set x=$$XREFDATA^%YDBAIM(g,nsubs,,,,,,2,aimtype,1),xs=$$XREFSUB^%YDBAIM(g,.nsubs,xsr,,,2,,1)
	set y="" for  set y=$order(@x@(0,y)) quit:'$zlength(y)  do assert("#"=$zextract(y,1))
	set ys="" for  set ys=$order(@xs@(xsr,ys)) quit:'$zlength(ys)  do assert("#"=$zextract(ys,1))
	set i=0,y="" for  set y=$order(@x@("",y)) quit:'$zlength(y)  if $increment(i,@x@("",y))
	set j=0,ys="" for  set ys=$order(@xs@(xsr,ys)) quit:'$zlength(ys)  if $increment(j,@xs@(-xsr,ys))
	set z="" for k=0:1 set z=$order(@x@(0,z)) quit:'$zlength(z)  do assert("#"=$zextract(z,1))
	set zs="" for l=0:1 set zs=$order(@xs@(xsr,zs)) quit:'$zlength(zs)  do assert("#"=$zextract(zs,1))
	;       Confirm all nodes counted, total count, count of distinct values
	do assert(nodes=i),assert(k=@x@(""))
	do assert(nodes=j),assert(l=@xs@(-xsr))
	;     - Check set trigger: additional xref, increase in total statistics, increase in number of distinct values
	set varpat=$$replace(varpat,"(5","(6"),@varpat=testval,ref=$reference,sub=$qsubscript(ref,xsr)
	do assert(10=$data(@x@(0,tvxref))),assert(1=@x@("",tvxref)),assert(k+1=@x@(""))
	do assert($data(@xs@(xsr,"#"_sub))\10),assert(1=@xs@(-xsr,"#"_sub)),assert(l+1=@xs@(-xsr))
	;     - Check kill trigger: no xref, decrease in total statistics, decrease in number of distinct values
	kill @ref
	do assert('$data(@x@(0,tvxref))),assert('$data(@x@("",tvxref))),assert(k=@x@(""))
	do assert('($data(@xs@(xsr,"#"_sub))\10)),assert('$data(@xs@(-xsr,"#"_sub))),assert(l=@xs@(-xsr))
	;     - Check removal
	do UNXREFDATA^%YDBAIM(x),assert('$data(@x)),UNXREFSUB^%YDBAIM(g,.nsubs,xsr,,,2,,1),assert('$data(@xs))
	;   Done testing entire node for type=0
	kill @g
	; - Nodes with pieces
	;   Not meaningful to enhance for subscript cross-referencing, as that would add nothing to the testing above
	set npieces=2+$random(5)		; No. of pieces in simulated application data
	set nsubs=1+$random(3)
	set nodes=$$aim73genblk(nsubs,10000,npieces,1000)
	set pnum=1+$random(npieces)		; Choose a piece number to test
	set varpat=$$replace($$aim73genpat(nsubs),"(3","(4")
	;   - No statistics
	;     - Initial scan
	set x=$$XREFDATA^%YDBAIM(g,nsubs,"|",pnum,,,,,aimtype,1)
	set y="" for  set y=$order(@x@(pnum,y)) quit:'$zlength(y)  do assert("#"=$zextract(y,1))
	;     - Check set trigger
	set $zpiece(@varpat,"|",pnum)=testval,ref=$reference do assert(10=$data(@x@(pnum,tvxref)))
	;     - Check kill trigger
	kill @ref do assert('$data(@x@(pnum,tvxref)))
	;     - Check removal
	do UNXREFDATA^%YDBAIM(x),assert('$data(@x))
	;   - Statistics=1
	;     - Initial scan
	set x=$$XREFDATA^%YDBAIM(g,nsubs,"|",pnum,,,,1,aimtype,1)
	set y="" for  set y=$order(@x@(pnum,y)) quit:'$zlength(y)  do assert("#"=$zextract(y,1))
	set i=0,y="" for  set y=$order(@x@(-pnum,y)) quit:'$zlength(y)  if $increment(i,@x@(-pnum,y))
	do assert(nodes=i)    ; Confirm that all nodes are counted
	;     - Check set trigger
	set varpat=$$replace(varpat,"(4","(5")
	set $zpiece(@varpat,"|",pnum)=testval,ref=$reference
	do assert(10=$data(@x@(pnum,tvxref))),assert(1=@x@(-pnum,tvxref))
	;     - Check kill trigger
	kill @ref do assert('$data(@x@(pnum,tvxref))),assert('$data(@x@(-pnum,tvxref)))
	;     - Check removal
	do UNXREFDATA^%YDBAIM(x),assert('$data(@x))
	;   - Statistics=2
	;     - Initial scan
	set x=$$XREFDATA^%YDBAIM(g,nsubs,"|",pnum,,,,2,aimtype,1)
	set y="" for j=0:1 set y=$order(@x@(pnum,y)) quit:'$zlength(y)  do assert("#"=$zextract(y,1))
	set i=0,y="" for  set y=$order(@x@(-pnum,y)) quit:'$zlength(y)  if $increment(i,@x@(-pnum,y))
	do assert(nodes=i),assert(j=@x@(-pnum))
	;     - Check set trigger
	set varpat=$$replace(varpat,"(5","(6")
	set $zpiece(@varpat,"|",pnum)=testval,ref=$reference
	do assert(10=$data(@x@(pnum,tvxref))),assert(1=@x@(-pnum,tvxref)),assert(j+1=@x@(-pnum))
	;     - Check kill trigger
	kill @ref
	do assert('$data(@x@(pnum,tvxref))),assert('$data(@x@(-pnum,tvxref))),assert(j=@x@(-pnum))
	;     - Check removal
	do UNXREFDATA^%YDBAIM(x),assert('$data(@x))
	;   Done testing pieces of nodes for type=0
	kill @g
	; Since type 1 globals are somewhat specialized, the type 1 tests in this suite have been
	; enhanced to test force=0 (the default) or force=1.
	quit
	;
aim73tut003	; @TEST Test fix for TUT003 test failure with AIM#73 code
	; Not meaningful to enhance for subscript cross-referencing
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"x"
	new subs,x
	kill @g
	set subs(1)=":"
	set x=$$XREFDATA^%YDBAIM(g,.subs,"","",0,0,1,2,aimtype,1)
	set @g@(1)="Old"
	do assert($data(@x@(0,"#Old",1)))
	set @g@(1)="New"
	do assert($data(@x@(0,"#New",1)))
	do assert('$data(@x@(0,"#Old",1)))
	quit
aim73genblk(nsub,nodes,pieces,max,c)	; [private] generate a bunch of data in ^x for the aim73 test
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"x"
	new i,varpat,x
	set varpat=$$aim73genpat(nsub,$get(c,0))_"="
	for i=1:1:pieces set varpat=varpat_"$random(max)_""|""_"
	set $zextract(varpat,$zlength(varpat)-4,$zlength(varpat))=""
	for i=1:1:nodes set @varpat
	; Count nodes, since it is possible to have generated duplicate subscripts above
	set x=g_"("""")" for i=0:1 set x=$query(@x) quit:'$zlength(x)
	quit:$quit i quit
	;
aim73genpat(nsub,c)			; [private] generate a ^x random reference with number of subs & optionally an extra constant subscript
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"x"
	new i,varpat
	set varpat=g_"("
	for i=1:1:nsub set varpat=varpat_"$$^%RANDSTR(3,,""an""),"
	set:$get(c,0) varpat=varpat_($random(1000)/(10**$random(4)))_","
	set $zextract(varpat,$zlength(varpat))=")"
	quit varpat
	;
aim74	; @TEST YDBAIM#74 - literal subscripts that include "/" work
	; Test XREFSUB() alongside XREFATA()
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"x"
	new ret,rets,subs,zsubs2
	kill @g
	for subs(2)="""/""","""ab/""","""/cd""","""ab/cd""" do
	. set zsubs2=$zwrite(subs(2),1),@g@(1,zsubs2)="abcd"
	. set ret=$$XREFDATA^%YDBAIM(g,.subs,"|",1,0,0,1,2,1,1),rets=$$XREFSUB^%YDBAIM(g,.subs,2,0,0,2,0,1)
	. do assert(1=@ret@(-1,"#abcd")),assert(1=@rets@(-2,"#"_zsubs2))
	. do UNXREFDATA^%YDBAIM(g,.subs,"|",1,0,0,1,2,1,1),UNXREFSUB^%YDBAIM(g,.subs,2,0,0,2,0,1)
	. do assert('$data(@ret)),assert('$data(@rets))
	kill @g
	quit
	;
aim77	; @Test for YDBAIM77 - transformation functions
	; Test XREFSUB() alongside XREFDATA()
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"x"
	new force,i,j,k,aimgbl,aimgbls,stat,sub
	kill @g
	for i=1:1:100000 set @g@(i,i)=i
	set aimgbl=$$XREFDATA^%YDBAIM(g,2,,,,,,,2,"$$aim77neg^%YDBAIMTEST()")
	set aimgbls=$$XREFSUB^%YDBAIM(g,2,"1;2",,,,2,"$$aim77neg^%YDBAIMTEST()")
	set j=1+$random(i),k=1+$random(1)
	do assert($data(@aimgbl@(0,-j,j,j))),assert($data(@aimgbls@(k,-j,j,j)))
	zkill @g@(j,j)
	do assert('$data(@aimgbl@(0,-j,j,j))),assert('$data(@aimgbls@(k,-j,j,j)))
	set:$increment(i) @g@(i,i)=i
	do assert($data(@aimgbl@(0,-i,i,i))),assert($data(@aimgbls@(k,-i,i,i)))
	kill:$increment(j) @g@(j,j)
	do assert('$data(@aimgbl@(0,-j,j,j))),assert('$data(@aimgbls@(k,-j,j,j)))
	do UNXREFDATA^%YDBAIM(g,2,,,,,,,2,"$$aim77neg^%YDBAIMTEST()"),UNXREFSUB^%YDBAIM(g,2,"1;2",,,,2,"$$aim77neg^%YDBAIMTEST()")
	do assert('$data(@aimgbl)),assert('$data(@aimgbls))
	set aimgbl=$$XREFDATA^%YDBAIM(g,2,,,,,,1,2,"$$aim77neg^%YDBAIMTEST(,1)")
	set aimgbls=$$XREFSUB^%YDBAIM(g,2,"1;2",,,1,2,"$$aim77neg^%YDBAIMTEST()")
	do assert(1=@aimgbl@("",-i)),assert(1=@aimgbls@(-k,-i))
	set (@g@(j,j),@g@(j-1,j-1))=i ; affects only data at existing nodes, subscripts are unaffected
	do assert(3=@aimgbl@("",-i))
	zkill @g@(i,i)
	do assert(2=@aimgbl@("",-i)),assert('$data(@aimgbls@(-k,-i)))
	do UNXREFDATA^%YDBAIM(g,2,,,,,,1,2,"$$aim77neg^%YDBAIMTEST(,1)"),UNXREFSUB^%YDBAIM(g,2,"1;2",,,1,2,"$$aim77neg^%YDBAIMTEST()")
	do assert('$data(@aimgbl)),assert('$data(@aimgbls))
	set aimgbl=$$XREFDATA^%YDBAIM(g,2,,,,,,2,2,"$$aim77neg^%YDBAIMTEST(,$$FUNC^%DH($system))")
	set aimgbls=$$XREFSUB^%YDBAIM(g,2,"1;2",,,2,2,"$$aim77neg^%YDBAIMTEST(,$$FUNC^%DH($system))")
	do assert(99999=@aimgbl@("")),assert(100000=@aimgbls@(-k)) ; # of distinct subscripts is higher because two nodes have the same data
	set @g@(0,0)=0
	do assert(100000=@aimgbl@("")),assert(100001=@aimgbls@(-k))
	do UNXREFDATA^%YDBAIM(g,2,,,,,,2,2,"$$aim77neg^%YDBAIMTEST(,$$FUNC^%DH($system))")
	do UNXREFSUB^%YDBAIM(g,2,"1;2",,,2,2,"$$aim77neg^%YDBAIMTEST(,$$FUNC^%DH($system))")
	do assert('$data(@aimgbl)),assert('$data(@aimgbls))
	kill @g
	for i=1:1:100000 set @g@(i,i)=$random(100000)_"|"_i_"|"_$random(100000)
	set aimgbl=$$XREFDATA^%YDBAIM(g,2,"|",2,,,,,2,"$$aim77neg^%YDBAIMTEST(,""Some garbage"")")
	set aimgbls=$$XREFSUB^%YDBAIM(g,2,"1;2",,,,2,"$$aim77neg^%YDBAIMTEST(,""Some garbage"")")
	set j=1+$random(i)
	do assert($data(@aimgbl@(2,-j,j,j))),assert($data(@aimgbls@(k,-j,j,j)))
	zkill @g@(j,j)
	do assert('$data(@aimgbl@(2,-j,j,j))),assert('$data(@aimgbls@(k,-j,j,j)))
	set:$increment(i) @g@(i,i)=$random(100000)_"|"_i_"|"_$random(100000)
	do assert($data(@aimgbl@(2,-i,i,i))),assert($data(@aimgbls@(k,-i,i,i)))
	kill:$increment(j) @g@(j,j)
	do assert('$data(@aimgbl@(2,-j,j,j))),assert('$data(@aimgbls@(k,-j,j,j)))
	do UNXREFDATA^%YDBAIM(g,2,"|",2,,,,,2,"$$aim77neg^%YDBAIMTEST(,""Some garbage"")")
	do UNXREFSUB^%YDBAIM(g,2,"1;2",,,,2,"$$aim77neg^%YDBAIMTEST(,""Some garbage"")")
	do assert('$data(@aimgbl)),assert('$data(@aimgbls))
	set aimgbl=$$XREFDATA^%YDBAIM(g,2,"|",2,,,,1,2,"$$aim77neg^%YDBAIMTEST()")
	set aimgbls=$$XREFSUB^%YDBAIM(g,2,"1;2",,,1,2,"$$aim77neg^%YDBAIMTEST()")
	do assert(1=@aimgbl@(-2,-i)),assert(1=@aimgbls@(-k,-i))
	set (@g@(j,j),@g@(j-1,j-1))=$random(100000)_"|"_i_"|"_$random(100000) ; affects only data at existing nodes, subscripts are unaffected
	do assert(3=@aimgbl@(-2,-i))
	zkill @g@(j,j)
	do assert(2=@aimgbl@(-2,-i)),assert('$data(@aimgbls@(-k,-j)))
	kill @g@(j-1,j-1)
	do assert(1=@aimgbl@(-2,-i)),assert('$data(@aimgbls@(-k,1-j)))
	do UNXREFDATA^%YDBAIM(g,2,"|",2,,,,1,2,"$$aim77neg^%YDBAIMTEST()"),UNXREFSUB^%YDBAIM(g,2,"1;2",,,1,2,"$$aim77neg^%YDBAIMTEST()")
	do assert('$data(@aimgbl)),assert('$data(@aimgbls))
	set aimgbl=$$XREFDATA^%YDBAIM(g,2,"|",2,,,,2,2,"$$aim77neg^%YDBAIMTEST()")
	set aimgbls=$$XREFSUB^%YDBAIM(g,2,"1;2",,,2,2,"$$aim77neg^%YDBAIMTEST()")
	do assert(99999=@aimgbl@(-2)),assert(99999=@aimgbls@(-k))
	set:$increment(i) @g@(i,i)=$random(100000)_"|"_i_"|"_$random(100000)
	do assert(100000=@aimgbl@(-2)),assert(100000=@aimgbls@(-k))
	set (@g@(j,j),@g@(j-1,j-1))=$random(100000)_"|"_i_"|"_$random(100000)
	do assert(100000=@aimgbl@(-2))
	zkill @g@(i,i)
	do assert(100000=@aimgbl@(-2)),assert(100001=@aimgbls@(-k))
	kill @g@(j,j),@g@(j-1,j-1)
	do assert(99999=@aimgbl@(-2)),assert(99999=@aimgbls@(-k))
	do UNXREFDATA^%YDBAIM(g,2,"|",2,,,,2,2,"$$aim77neg^%YDBAIMTEST()")
	do UNXREFSUB^%YDBAIM(g,2,"1;2",,,2,2,"$$aim77neg^%YDBAIMTEST()")
	do assert('$data(@aimgbl)),assert('$data(@aimgbls))
	kill @g
	; The following are not relevant to XREFSUB()
	set sub(1)="*",sub(2)=2
	; The following are not strictly part of YDBAIM#77, but are included here since
	; the omission was discovered during the code review
	; (https://gitlab.com/YottaDB/Util/YDBAIM/-/merge_requests/76#note_2166696698).
	; No type 1 test because of bug YDBAIM#79.
	for stat=0:1:2 do
	. set @g@(1,2)="a",x=$$XREFDATA^%YDBAIM(g,.sub,"|",2,,,1,stat,aimtype),@g@(7,2)="e||f"
	. do assert($data(@x@(2,"",1))),assert($data(@x@(2,"",7)))
	. if stat do assert(2=@x@(-2,"")) if stat-1 do assert(1=@x@(-2))
	. do UNXREFDATA^%YDBAIM(g,.sub,"|",2,,,1,stat,aimtype) kill @g
	set force="$$aim77neg^%YDBAIMTEST()"
	for stat=0:1:2 do
	. set @g@(1,2)="a",x=$$XREFDATA^%YDBAIM(g,.sub,"|",2,,,1,stat,2,force),@g@(7,2)="e||f"
	. do assert($data(@x@(2,"",1))),assert($data(@x@(2,"",7)))
	. if stat do assert(2=@x@(-2,"")) if stat-1 do assert(1=@x@(-2))
	. do UNXREFDATA^%YDBAIM(g,.sub,"|",2,,,1,stat,2,force) kill @g
	quit
	;
aim77neg(val,ignore)	quit $select($zlength(val):-val,1:"")	;transformation function negative of value
	;
aim82	; @TEST Test xtratrig for higher level KILLs
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"X"
	new aimgbl
	do aim82init set aimgbl=$$XREFDATA^%YDBAIM(g,2,"|",2,,,,,aimtype)
	do assert($data(@aimgbl@(2,"d",1,2)))
	kill @g@(1) ; kill higer level global
	do assert('$data(@aimgbl@(2,"d",1,2)))
	do assert($data(@aimgbl@(2,"d",2,2)))
	kill @g; kill entire global
	do assert('$data(@aimgbl@(2,"d",2,2)))
	do UNXREFDATA^%YDBAIM(aimgbl)
	do aim82init set aimgbl=$$XREFDATA^%YDBAIM(g,2,,,,,,,aimtype)
	do assert($data(@aimgbl@(0,"c|d",1,2)))
	kill @g@(1)
	do assert('$data(@aimgbl@(0,"c|d",1,2)))
	do assert($data(@aimgbl@(0,"c|d",2,2)))
	kill @g
	do assert('$data(@aimgbl@(0,"c|d",2,2)))
	do aim82init set aimgbl=$$XREFSUB^%YDBAIM(g,2,2)
	do assert($data(@aimgbl@(2,2,1)))
	kill @g@(1)
	do assert('$data(@aimgbl@(2,2,1)))
	do assert($data(@aimgbl@(2,2,2)))
	kill @g
	do assert('$data(@aimgbl@(2,2,2)))
	quit
	;
aim82init	; private for aim82 test
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"X"
	new i,j
	for i=1,2 set @g@(i)="a|b" for j=1,2 set @g@(i,j)="c|d"
	quit
	;
aim89	; @TEST Test comment parameter
	; No need to test absence of comment as all other tests do that
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"USPresidents"
	new aimgbl,aimgbl2,comment,comment2
	; Whole node indexed: slot 11 is a plain scalar.
	set comment=$stack($stack,"mcode"),aimgbl=$$XREFDATA^%YDBAIM(g,2,,,,,,,aimtype,,comment)
	do assert(comment=@aimgbl@(11))
	; Pieces: a cross reference is named by its global, subscripts, delimiter, type, force and
	; record specification - NOT by the piece number - so one cross reference serves every piece of the
	; node. Slot 11 is therefore subscripted by piece, and the scalar is not written.
	set comment=$stack($stack,"mcode"),aimgbl=$$XREFDATA^%YDBAIM(g,2,"|",2,,,,,aimtype,,comment)
	do assert(comment=@aimgbl@(11,2))
	do assert('($data(@aimgbl@(11))#10))
	; A second piece of the same node lands on the same cross reference and adds its comment
	; beside the first rather than over it, which is what a single scalar would have done.
	set comment2=$stack($stack,"mcode"),aimgbl2=$$XREFDATA^%YDBAIM(g,2,"|",3,,,,,aimtype,,comment2)
	do assert(aimgbl2=aimgbl)
	do assert(comment2=@aimgbl@(11,3))
	do assert(comment=@aimgbl@(11,2))
	; XREFSUB cross references subscripts, which have no pieces: scalar again.
	set comment=$stack($stack,"mcode"),aimgbl=$$XREFSUB^%YDBAIM(g,2,1,,,,,,comment)
	do assert(comment=@aimgbl@(11))
	quit
	;
aim93	; @TEST Test Extended Reference
	; Don't run this test if we are in extended reference mode
	if xdref quit
	;
	new aimgbl,currgld,envgld,i,io,lsxref,testdat,testdir,testgbl,testgld,tmp,xrefgbl
	do aim93init
	set testgbl="^|"""_testgld_"""|USPresidents"
	;
	; Test data cross referencing
	set aimgbl=$$XREFDATA^%YDBAIM(testgbl,2,,,,,,,aimtype)
	do assert(aimgbl=$$XREFDATA^%YDBAIM(testgbl,2,,,1,,,,aimtype),"nmonly returns the same name")
	set xrefgbl="^"_$zpiece(aimgbl,"|",3)	; aimgbl without extended reference
	do assert($data(@aimgbl),"Cross created in extended reference database")
	do assert('$data(@xrefgbl),"Cross reference not created in this database")
	do assert($data(@aimgbl@(0,"Jimmy||Carter",1977,1981)),"President Carter record exists")
	set $zgbldir=testgld
	for i=1:1 set tmp=$zpiece($text(NotUSPresidents+i),";",2) quit:""=tmp  set @tmp
	set $zgbldir=currgld
	do assert($data(@aimgbl@(0,"Zaphod||Beeblebrox",1837,42)),"Cross reference created for Zaphod Beeblebrox")
	do assert($data(@aimgbl@(0,"Ford||Prefect",1837,-42)),"Cross reference created for Ford Prefect")
	set $zgbldir=testgld
	zkill @xrefgbl@(0,"Zaphod||Beeblebrox",1837,42) kill @xrefgbl@(0,"Ford||Prefect")
	set $zgbldir=currgld
	do assert('$data(@aimgbl@(0,"Zaphod||Beeblebrox",1837,42)),"Cross reference deleted for Zaphod Beeblebrox")
	do assert('$data(@aimgbl@(0,"Ford||Prefect",1837,-42)),"Cross reference deleted for Ford Prefect")
	do LSXREFDATA^%YDBAIM(.lsxref,testgbl)
	do assert($data(lsxref),"lsxref has data")
	do UNXREFDATA^%YDBAIM(aimgbl)
	do assert('$data(@aimgbl),"UNXREFDATA works as advertised")
	kill lsxref
	;
	; Test subscript cross referencing
	set aimgbl=$$XREFSUB^%YDBAIM(testgbl,2,2)
	do assert(aimgbl=$$XREFSUB^%YDBAIM(testgbl,2,2,1),"nmonly returns the same name")
	set xrefgbl="^"_$zpiece(aimgbl,"|",3)	; aimgbl without extended reference
	do assert($data(@aimgbl),"Cross created in extended reference database")
	do assert('$data(@xrefgbl),"Cross reference not created in this database")
	do assert($data(@aimgbl@(2,1981,1977)),"President Carter record exists")
	set $zgbldir=testgld
	for i=1:1 set tmp=$zpiece($text(NotUSPresidents+i),";",2) quit:""=tmp  set @tmp
	set $zgbldir=currgld
	do assert($data(@aimgbl@(2,42,1837)),"Cross reference created for Zaphod Beeblebrox")
	do assert($data(@aimgbl@(2,-42,1837)),"Cross reference created for Ford Prefect")
	set $zgbldir=testgld
	zkill @xrefgbl@(2,42,1837) kill @xrefgbl@(2,-42)
	set $zgbldir=currgld
	do assert('$data(@aimgbl@(2,42,1837)),"Cross reference deleted for Zaphod Beeblebrox")
	do assert('$data(@aimgbl@(2,-42,1837)),"Cross reference deleted for Ford Prefect")
	do LSXREFSUB^%YDBAIM(.lsxref,testgbl)
	do assert($data(lsxref),"lsxref has data")
	do UNXREFSUB^%YDBAIM(aimgbl)
	do assert('$data(@aimgbl),"UNXREFDATA works as advertised")
	;
	do aim93exit
	quit
	;
aim93init	; private for aim93
	; Create a temporary database to access with extended references
	new gdein,gdeout,i,tmp
	set io=$io
	; Save $zgbldir and relevant environment variables if set
	set currgld=$zgbldir
	set envgld=$ztrnlnm("ydb_gbldir")
	set testdir=$$aim93dir()_$zut
	set gdein=testdir_"/gde.in"
	set gdeout=testdir_"/gde.out"
	set testdat=testdir_"/yottadb.dat"
	set testgld=testdir_"/yottadb.gld"
	view "setenv":"ydb_gbldir":testgld,"setenv":"gtmgbldir":testgld
	zsystem "mkdir -p "_testdir
	open gdein:newversion use gdein
	write "change -region DEFAULT -autodb -key_size=",1019," -record_size=",1048576," -null_subscripts=always",!
	write "change -segment DEFAULT -file_name=""",testdat,"""",!
	write "exit",!
	use io close gdein
	zsystem "bash -c '$ydb_dist/yottadb -run GDE @"_gdein_" &> "_gdeout_"'"
	do assert($zlength($zsearch(testgld,-1)),"Global directory created for test")
	set $zgbldir=testgld
	for i=1:1 set tmp=$zpiece($text(USPresidents+i),";",2) quit:""=tmp  set @tmp
	set $zgbldir=currgld
	quit
	;
aim93exit	; private for aim 93
	; Restore $zgbldir and environment variable if set
	set $zgbldir=currgld
	; The temporary database is NOT deleted here; SHUTDOWN removes it. This process has
	; loaded its global directory, and YottaDB recognises a loaded global directory by the
	; inode of its .gld alone. Deleted now, its inode can be recycled for the next test's
	; new .gld, which this process would then take for the old one: it would read and write
	; the deleted database while every JOB'd process opened the new, empty one. That made
	; t4xdrefr build an empty index and t4xdref deadlock with its own workers. Reported as
	; YDB#1271 (https://gitlab.com/YottaDB/DB/YDB/-/work_items/1271).
	if $zlength(envgld) view "setenv":"ydb_gbldir":envgld,"setenv":"gtmgbldir":envgld
	else  view "unsetenv":"ydb_gbldir","unsetenv":"gtmgbldir"
	quit
	;
aim93dir:()	; [aim93init, SHUTDOWN] Where this process's temporary databases live
	new tmp set tmp=$ztrnlnm("ydb_tmp")
	quit $select($zlength(tmp):tmp,1:"/tmp")_"/"_$text(+0)_"_"_$job_"_"
	;
aim97	; @TEST AIM#97 - the initial scan divides the global variable among several processes
	;
	; AIM divides the subscripts at one subscript level into ranges, and the
	; JOB'd processes scan a range each. The cross reference must come out the
	; same however many processes produced it, so each case below builds one
	; cross reference at several process counts and compares the whole index,
	; node by node, with the index a single process builds. Comparing the whole
	; index rather than sampling it is what tests the division: a range that
	; starts past subscripts, or repeats subscripts and so miscounts at
	; stat=1/2, differs from the single process index.
	;
	; The cases cover where the divided level sits and what its subscripts look
	; like: the last subscript level, with nothing below it to recurse into; an
	; intermediate level, with constant levels above and below it; an explicit
	; range instead of "*", which the ranges must stay inside and whose
	; endpoints they must keep; subscripts holding ":" and ";", at which a range
	; cannot end; a null subscript, which only the first range may scan; levels
	; above the divided one that are "*" but hold one subscript each, which AIM
	; pins so that the division happens at the level that has the subscripts,
	; with and without a constant level in between, and with only constants
	; below, where there is nothing to descend to; and a single node, where the
	; walk cuts no range at all and one process scans everything while the rest
	; find nothing to claim.
	new g,gd,gn,go,i,subs
	set g="^"_$select(xdref:xdrefloc,1:"")_"aim97"
	set gd="^"_$select(xdref:xdrefloc,1:"")_"deepaim97"
	set gn="^"_$select(xdref:xdrefloc,1:"")_"nullaim97"
	set go="^"_$select(xdref:xdrefloc,1:"")_"oneaim97"
	kill @g,@gd,@gn,@go
	; Enough subscripts that the range size doubles at least once
	for i=1:1:3000 set @g@(i)="v"_(i#37)_"|p"_(i#11)
	for i=1:1:800 set @g@("fix",i,0)="n"_(i#23)_"|q"_(i#7)
	; Subscripts holding ":" and ";". Those characters separate a subscript
	; specification into ranges, so a subscript holding either cannot be written
	; into one, and AIM cannot end a range of subscripts to scan at it
	for i=1:1:500 set @g@("s","k"_i_$select(0=i#7:":x",0=i#11:";y",1:""))="s"_(i#13)_"|r"
	; One subscript at each of the outer levels, which are specified as "*"
	for i=1:1:600 set @gd@("only",7,i)="d"_(i#19)_"|e"_(i#5)
	; A null subscript, in the region that permits them
	for i=1:1:200 set @gn@(i)="z"_(i#5)_"|w"
	set @gn@("")="znull|w"
	; A single node
	set @go@(1)="o|p"
	;
	kill subs set subs(1)="*"			; last level, "*"
	do aim97cmp(g,.subs,"last level")
	kill subs set subs(1)="""fix""",subs(2)="*",subs(3)=0	; intermediate level
	do aim97cmp(g,.subs,"intermediate level")
	kill subs set subs(1)="200:1700"		; explicit range
	do aim97cmp(g,.subs,"explicit range")
	kill subs set subs(1)="""s""",subs(2)="*"	; ":" and ";" in subscripts
	do aim97cmp(g,.subs,"delimiters in subscripts")
	kill subs set subs(1)="*",subs(2)="*",subs(3)="*"	; one subscript above
	do aim97cmp(gd,.subs,"single subscript levels above")
	kill subs set subs(1)="*",subs(2)=7,subs(3)="*"	; and with a constant between
	do aim97cmp(gd,.subs,"single subscript level above a constant")
	kill subs set subs(1)="*",subs(2)=7,subs(3)=1	; nothing below to divide
	do aim97cmp(gd,.subs,"single subscript level, constants below")
	kill subs set subs(1)="*"			; null subscript
	do aim97cmp(gn,.subs,"null subscript")
	kill subs set subs(1)="*"			; a single node
	do aim97cmp(go,.subs,"single node")
	;
	; A global variable with no nodes at all. Every process finds nothing to
	; claim, and the cross reference is still created and recorded complete
	; rather than raising SCANINCOMPLETE.
	new c,ge,s,x
	set ge="^"_$select(xdref:xdrefloc,1:"")_"emptyaim97"
	kill @ge
	kill subs set subs(1)="*"
	for i=1,2,9 do
	. view "setenv":"ydb_aim_nproc":i
	. set x=$$XREFDATA^%YDBAIM(ge,.subs,"|",1,,,,,aimtype)
	. do assert($data(@x),"empty global variable: nproc="_i_" created no cross reference")
	. set c=0,s=x
	. for  set s=$query(@s) quit:'$zlength(s)  if 1'=$qlength(s),$increment(c)
	. do assert('c,"empty global variable: nproc="_i_" cross referenced "_c_" nodes")
	. do UNXREFDATA^%YDBAIM(x)
	view "unsetenv":"ydb_aim_nproc"
	;
	; A ydb_aim_nproc that is not a positive integer is rejected, by XREFSUB()
	; as well as XREFDATA(). The rejection has to happen before the cross
	; reference is created, since the scan runs after the triggers are set and
	; the cross reference registered; a rejection during the scan would leave a
	; registered cross reference that was never scanned behind.
	new ecodetest
	set x=$$XREFDATA^%YDBAIM(g,1,"|",1,1,,,,aimtype)		; nmonly, creates nothing
	view "setenv":"ydb_aim_nproc":"two"
	do
	. new $etrap set $etrap="goto err^"_$text(+0)
	. set ecodetest="" if $$XREFDATA^%YDBAIM(g,1,"|",1,,,,,aimtype)
	do assert(ecodetest="U225")
	do assert('$data(@x))
	view "setenv":"ydb_aim_nproc":"0"
	do
	. new $etrap set $etrap="goto err^"_$text(+0)
	. set ecodetest="" if $$XREFDATA^%YDBAIM(g,1,"|",1,,,,,aimtype)
	do assert(ecodetest="U225")
	do assert('$data(@x))
	do
	. new $etrap set $etrap="goto err^"_$text(+0)
	. set ecodetest="" if $$XREFSUB^%YDBAIM(g,1,1)
	do assert(ecodetest="U225")
	view "unsetenv":"ydb_aim_nproc"
	do assert('$data(@$$XREFSUB^%YDBAIM(g,1,1,1)))
	kill @g,@gd,@gn,@go
	quit
	;
aim97cmp(g,subs,what)	; [private] #97 - same cross reference, several process counts
	new n,ref,x
	; 1 is the reference the rest are compared against.
	for n=1,2,3,8,17 do
	. view "setenv":"ydb_aim_nproc":n
	. set x=$$XREFDATA^%YDBAIM(g,.subs,"|",1,,,,,aimtype)
	. do aim97chk(x,.ref,n,what_", XREFDATA()")
	. do UNXREFDATA^%YDBAIM(x)
	kill ref
	for n=1,4,11 do
	. view "setenv":"ydb_aim_nproc":n
	. set x=$$XREFDATA^%YDBAIM(g,.subs,"|",1,,,,2,aimtype)	; also at stat=2
	. do aim97chk(x,.ref,n,what_", XREFDATA() stat=2")
	. do UNXREFDATA^%YDBAIM(x)
	kill ref
	for n=1,2,8 do
	. view "setenv":"ydb_aim_nproc":n
	. set x=$$XREFSUB^%YDBAIM(g,.subs,1)
	. do aim97chk(x,.ref,n,what_", XREFSUB()")
	. do UNXREFSUB^%YDBAIM(x)
	view "unsetenv":"ydb_aim_nproc"
	quit
	;
aim97chk(x,ref,n,what)	; [private] #97 - capture an index, and compare it with the one a single process built
	; The index built at ydb_aim_nproc=n is captured into ref(n) as text, and
	; compared with ref(1), the index the same call built with one process.
	; That the index has the rows the data calls for is what the rest of this
	; test suite covers; what is checked here is that however many processes
	; divided the scan, the index is the one a single process produces.
	new c,s,z
	set (c,z)="",s=x
	; $QUERY() does not leave the global variable it starts in, so the walk ends
	; of its own accord at the last node of this cross reference
	for  set s=$query(@s) quit:'$zlength(s)  do
	. ; AIM's own control nodes are the single subscripted ones; one of them
	. ; holds the time of the scan, which of course differs between scans
	. quit:1=$qlength(s)
	. set c=c+1,z=z_s_"="_@s_$char(10)
	set ref(n)=z
	; Every case must produce a non-empty index, else the comparisons below
	; would pass by comparing nothing
	do assert(0<c)
	; ref(1) is the index a single process built, and every other process count
	; is asserted to have produced exactly that. n=1 is the pass that records
	; it, so it has nothing to compare itself with.
	do:1'=n assert(ref(1)=ref(n),what_": nproc="_n_" index differs from nproc=1")
	quit
	;
	;
	; WHAT #98 CHANGED
	; forceStr is just the built-in string transform x->"#"_x, so #98 makes a type=3 cross reference
	; (transform + index-missing-nodes-as-NULL) represent a missing node's NULL the same way: as the
	; transform applied to "", not as a hardcoded bare "". The transform here is $$aim98pfx, which
	; returns "#"_val -- so transform("")="#", like Octo's strcolval2aimsubs. Every NULL row must
	; therefore sit at the "#" subscript and never at a bare "" (this transform never returns "").
	; omitfix=1 is used (the fully transform-converted path, as Octo uses); the non-omitfix forward
	; index is a follow-up.
	;
	; WHAT EACH stat LEVEL MAINTAINS -- each assertion is gated to the levels whose nodes exist:
	;   stat 0,1,2   index                 @x@(fwd,value,id)   the NULL marker itself
	;   stat 1,2     count node            @x@(cnt,value)      how many rows hold that value
	;   stat 2       distinct-value total  @x@(cnt)            STRUCTURAL -- it counts values rather
	;                                                          than being keyed by one, so it stays
	;                                                          bare and must NOT move to "#"
	;
	; (fwd,cnt) is (0,"") for the whole-node index and (pnum,-pnum) for the piece index. At stat=0
	; the count-node checks are skipped: those nodes do not exist at that level, so asserting their
	; absence would pass vacuously and prove nothing.
	;
	; TEST DATA -- FileMan style: the 0 node always exists, the 9 node is optional. The cross
	; reference is on the (optional) 9 node, so an id whose 9 node is absent is a NULL row, held in
	; the tree by its 0 node:
	;   id 1 -> "Alpha"   id 2 -> "Bravo"   id 3 -> absent (NULL)
	;   id 4 -> "Alpha"   id 5 -> absent (NULL)
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"aim98"
	new m,s,stat,x,xfn
	set xfn="$$aim98pfx^%YDBAIMTEST()"
	set s(1)=":",s(2)=9		; fixed last subscript 9 = the indexed (optional) node
	;
	; ---- whole-node index: index @x@(0,value,id), counts @x@("",value), total @x@("") ----
	for stat=0,1,2 do
	. do aim98dat(g,"")
	. set x=$$XREFDATA^%YDBAIM(g,.s,,,,,1,stat,3,xfn)
	. ;
	. ; initial build:  #Alpha={1,4}  #Bravo={2}  #={3,5}  -- 3 distinct values
	. set m="whole-node stat "_stat_", initial build: "
	. do assert($data(@x@(0,"#Alpha",1)),m_"id 1 is indexed under #Alpha")
	. do assert($data(@x@(0,"#Alpha",4)),m_"id 4 is indexed under #Alpha")
	. do assert($data(@x@(0,"#Bravo",2)),m_"id 2 is indexed under #Bravo")
	. do assert($data(@x@(0,"#",3)),m_"id 3 has no 9 node, so it is indexed as NULL under #")
	. do assert($data(@x@(0,"#",5)),m_"id 5 has no 9 node, so it is indexed as NULL under #")
	. do assert('$data(@x@(0,"")),m_"no NULL row sits at a bare empty subscript -- this is what #98 fixed")
	. do:stat assert(2=@x@("","#Alpha"),m_"count node for #Alpha is 2 (ids 1 and 4)")
	. do:stat assert(1=@x@("","#Bravo"),m_"count node for #Bravo is 1 (id 2)")
	. do:stat assert(2=@x@("","#"),m_"count node for NULL is 2 (ids 3 and 5)")
	. do:stat assert('$data(@x@("","")),m_"no count node sits at a bare empty subscript -- this is what #98 fixed")
	. do:stat=2 assert(3=@x@(""),m_"3 distinct values: #Alpha, #Bravo, #")
	. ;
	. ; KILL trigger -- id 1 loses its 9 node (its 0 node keeps it in the tree) -> becomes a NULL row
	. ;   now:  #Alpha={4}  #Bravo={2}  #={1,3,5}  -- still 3 distinct values
	. zkill @g@(1,9)
	. set m="whole-node stat "_stat_", after KILL of id 1's 9 node: "
	. do assert($data(@x@(0,"#",1)),m_"id 1 is now a NULL row under #")
	. do assert('$data(@x@(0,"#Alpha",1)),m_"id 1 no longer sits under its old value #Alpha")
	. do assert('$data(@x@(0,"")),m_"the KILL trigger did not put the NULL row at a bare empty subscript")
	. do:stat assert(1=@x@("","#Alpha"),m_"count node for #Alpha drops to 1 (only id 4 left)")
	. do:stat assert(3=@x@("","#"),m_"count node for NULL rises to 3 (ids 1, 3 and 5)")
	. do:stat assert('$data(@x@("","")),m_"the KILL trigger did not put a count node at a bare empty subscript")
	. do:stat=2 assert(3=@x@(""),m_"still 3 distinct values -- #Alpha survives on id 4")
	. ;
	. ; SET trigger -- id 3 gains a 9 node -> its NULL row becomes a value row
	. ;   now:  #Alpha={4}  #Bravo={2}  #Charlie={3}  #={1,5}  -- 4 distinct values
	. set @g@(3,9)="Charlie"
	. set m="whole-node stat "_stat_", after SET of id 3's 9 node: "
	. do assert($data(@x@(0,"#Charlie",3)),m_"id 3 is now indexed under #Charlie")
	. do assert('$data(@x@(0,"#",3)),m_"id 3 is no longer a NULL row")
	. do assert('$data(@x@(0,"")),m_"the SET trigger did not leave a NULL row at a bare empty subscript")
	. do:stat assert(1=@x@("","#Charlie"),m_"count node for #Charlie is 1 (id 3)")
	. do:stat assert(2=@x@("","#"),m_"count node for NULL drops to 2 (ids 1 and 5)")
	. do:stat assert('$data(@x@("","")),m_"the SET trigger did not leave a count node at a bare empty subscript")
	. do:stat=2 assert(4=@x@(""),m_"4 distinct values: #Alpha, #Bravo, #, #Charlie")
	. ;
	. do UNXREFDATA^%YDBAIM(x)
	;
	; ---- piece index (pnum 1): index @x@(1,value,id), counts @x@(-1,value), total @x@(-1) ----
	; Same data (with a trailing "|z" piece) and the same three phases; this exercises the
	; tt3Sp0/ZKp0/Sp1/ZKp1/Sp2/ZKp2 templates rather than the whole-node tt3Se*/ZKe* ones.
	for stat=0,1,2 do
	. do aim98dat(g,"|z")
	. set x=$$XREFDATA^%YDBAIM(g,.s,"|",1,,,1,stat,3,xfn)
	. ;
	. ; initial build:  #Alpha={1,4}  #Bravo={2}  #={3,5}  -- 3 distinct values
	. set m="piece stat "_stat_", initial build: "
	. do assert($data(@x@(1,"#Alpha",1)),m_"piece 1 of id 1 is indexed under #Alpha")
	. do assert($data(@x@(1,"#Alpha",4)),m_"piece 1 of id 4 is indexed under #Alpha")
	. do assert($data(@x@(1,"#Bravo",2)),m_"piece 1 of id 2 is indexed under #Bravo")
	. do assert($data(@x@(1,"#",3)),m_"id 3 has no 9 node, so it is indexed as NULL under #")
	. do assert($data(@x@(1,"#",5)),m_"id 5 has no 9 node, so it is indexed as NULL under #")
	. do assert('$data(@x@(1,"")),m_"no NULL row sits at a bare empty subscript -- this is what #98 fixed")
	. do:stat assert(2=@x@(-1,"#Alpha"),m_"count node for #Alpha is 2 (ids 1 and 4)")
	. do:stat assert(1=@x@(-1,"#Bravo"),m_"count node for #Bravo is 1 (id 2)")
	. do:stat assert(2=@x@(-1,"#"),m_"count node for NULL is 2 (ids 3 and 5)")
	. do:stat assert('$data(@x@(-1,"")),m_"no count node sits at a bare empty subscript -- this is what #98 fixed")
	. do:stat=2 assert(3=@x@(-1),m_"3 distinct values: #Alpha, #Bravo, #")
	. ;
	. ; KILL trigger -- id 1 becomes a NULL row
	. ;   now:  #Alpha={4}  #Bravo={2}  #={1,3,5}  -- still 3 distinct values
	. zkill @g@(1,9)
	. set m="piece stat "_stat_", after KILL of id 1's 9 node: "
	. do assert($data(@x@(1,"#",1)),m_"id 1 is now a NULL row under #")
	. do assert('$data(@x@(1,"#Alpha",1)),m_"id 1 no longer sits under its old value #Alpha")
	. do assert('$data(@x@(1,"")),m_"the KILL trigger did not put the NULL row at a bare empty subscript")
	. do:stat assert(1=@x@(-1,"#Alpha"),m_"count node for #Alpha drops to 1 (only id 4 left)")
	. do:stat assert(3=@x@(-1,"#"),m_"count node for NULL rises to 3 (ids 1, 3 and 5)")
	. do:stat assert('$data(@x@(-1,"")),m_"the KILL trigger did not put a count node at a bare empty subscript")
	. do:stat=2 assert(3=@x@(-1),m_"still 3 distinct values -- #Alpha survives on id 4")
	. ;
	. ; SET trigger -- id 3 becomes a value row
	. ;   now:  #Alpha={4}  #Bravo={2}  #Charlie={3}  #={1,5}  -- 4 distinct values
	. set @g@(3,9)="Charlie|z"
	. set m="piece stat "_stat_", after SET of id 3's 9 node: "
	. do assert($data(@x@(1,"#Charlie",3)),m_"piece 1 of id 3 is now indexed under #Charlie")
	. do assert('$data(@x@(1,"#",3)),m_"id 3 is no longer a NULL row")
	. do assert('$data(@x@(1,"")),m_"the SET trigger did not leave a NULL row at a bare empty subscript")
	. do:stat assert(1=@x@(-1,"#Charlie"),m_"count node for #Charlie is 1 (id 3)")
	. do:stat assert(2=@x@(-1,"#"),m_"count node for NULL drops to 2 (ids 1 and 5)")
	. do:stat assert('$data(@x@(-1,"")),m_"the SET trigger did not leave a count node at a bare empty subscript")
	. do:stat=2 assert(4=@x@(-1),m_"4 distinct values: #Alpha, #Bravo, #, #Charlie")
	. ;
	. do UNXREFDATA^%YDBAIM(x)
	;
	; ---- disjoint-namespace transform: prove the NULL marker is EXACTLY transform("") ----
	; aim98pfx above makes NULL ("#") and a value ("#Alpha") share the "#" prefix, so it cannot
	; distinguish "NULL is indexed at transform("")" from the weaker "NULL happens to share a
	; prefix with values". aim98dist maps values to a "regular transform->..." namespace but ""
	; to the unrelated marker "null transform", so the two namespaces are disjoint. A NULL row
	; must sit at exactly transform("") -- not at a bare "" (the old hardcode) and not at the
	; naive value-of-empty subscript "regular transform->". Run at every stat level like the
	; sections above: the index at all stats, the count node at stat=1/2, the distinct total at
	; stat=2. nullmark = transform("") is computed in the test itself and stat-independent.
	new nullmark set nullmark=$$aim98dist^%YDBAIMTEST(""),xfn="$$aim98dist^%YDBAIMTEST()"
	for stat=0,1,2 do
	. do aim98dat(g,"")
	. set x=$$XREFDATA^%YDBAIM(g,.s,,,,,1,stat,3,xfn)
	. ;
	. ; initial build:  regular transform->Alpha={1,4}  regular transform->Bravo={2}  null transform={3,5}
	. set m="disjoint-namespace stat "_stat_", initial build: "
	. do assert($data(@x@(0,nullmark,3)),m_"id 3's NULL is indexed at exactly transform("""") = "_nullmark)
	. do assert($data(@x@(0,nullmark,5)),m_"id 5's NULL is indexed at exactly transform("""")")
	. do assert($data(@x@(0,"regular transform->Alpha",1)),m_"id 1's value sits in the regular namespace")
	. do assert('$data(@x@(0,"")),m_"no NULL sits at a bare empty subscript -- the old hardcode")
	. do assert('$data(@x@(0,"regular transform->")),m_"no NULL landed at the naive value-of-empty subscript")
	. do:stat assert(2=@x@("",nullmark),m_"count node for NULL is keyed by transform("""") (ids 3 and 5)")
	. do:stat=2 assert(3=@x@(""),m_"3 distinct markers: two regular + one null")
	. ;
	. ; KILL -- id 1 loses its 9 node -> its value row must migrate into the disjoint null namespace
	. zkill @g@(1,9)
	. set m="disjoint-namespace stat "_stat_", after KILL of id 1's 9 node: "
	. do assert($data(@x@(0,nullmark,1)),m_"id 1 now sits at transform("""")")
	. do assert('$data(@x@(0,"regular transform->Alpha",1)),m_"id 1 left its regular-namespace marker")
	. do:stat assert(3=@x@("",nullmark),m_"NULL count rises to 3 (ids 1, 3 and 5)")
	. ;
	. ; SET -- id 3 gains a 9 node -> its NULL row must leave the disjoint null namespace
	. set @g@(3,9)="Charlie"
	. set m="disjoint-namespace stat "_stat_", after SET of id 3's 9 node: "
	. do assert($data(@x@(0,"regular transform->Charlie",3)),m_"id 3 now sits in the regular namespace")
	. do assert('$data(@x@(0,nullmark,3)),m_"id 3 left the disjoint null marker")
	. do:stat assert(2=@x@("",nullmark),m_"NULL count drops to 2 (ids 1 and 5)")
	. ;
	. do UNXREFDATA^%YDBAIM(x)
	kill @g
	quit
	;
aim98dat(g,sfx)	; [private] #98 - (re)load the test global for one iteration
	; FileMan style: the 0 node always exists; the indexed 9 node is optional and is absent for
	; ids 3 and 5, which are therefore the NULL rows.
	kill @g
	set @g@(1,0)="anchor",@g@(1,9)="Alpha"_sfx
	set @g@(2,0)="anchor",@g@(2,9)="Bravo"_sfx
	set @g@(3,0)="anchor"			; 9 node absent -> NULL row
	set @g@(4,0)="anchor",@g@(4,9)="Alpha"_sfx
	set @g@(5,0)="anchor"			; 9 node absent -> NULL row
	quit
	;
aim98pfx(val)	; [private] #98 transformation function: force-string ("#") prefix, like Octo's strcolval2aimsubs
	quit "#"_val
	;
aim98dist(val)	; [private] #98 distinguishing transform: value -> "regular transform->"_val, "" -> "null transform"
	; Unlike aim98pfx (where NULL "#" and a value "#Alpha" share the "#" prefix), here transform("")
	; shares no prefix with any value's transform, so a NULL row and a value row land in completely
	; disjoint subscript namespaces. This proves a NULL is indexed at exactly transform(""), and is
	; neither a bare "" (the old hardcode) nor anything derivable from a value.
	quit $select(val'="":"regular transform->"_val,1:"null transform")
	;
aim102	; @TEST AIM#102 - a fatal worker error is reported, not discarded
	; The parent reads each worker's error file and raises JOBERR for a message
	; that is neither informational (-I-) nor success (-S-). YottaDB writes GVIS
	; on the same line as the message that produced it, and that trailing
	; "%YDB-I-GVIS" once made the whole line read as informational: the error was
	; discarded, its file deleted, and the parent reported SCANINCOMPLETE, which
	; names neither the error nor what caused it.
	;
	; The value indexed below is longer than a key of the region that holds the
	; cross reference, so the worker that reaches that node dies with GVSUBOFLOW,
	; to which YottaDB appends GVIS.
	new ecodetest,errfile,g,gtmetrap,i,jobstat,keysize,line,ydbetrap
	set g="^"_$select(xdref:xdrefloc,1:"")_"aim102"
	kill @g
	do
	. if xdref new $zgbldir set $zgbldir=xdrefgld
	. set keysize=$$^%PEEKBYNAME("sgmnt_data.max_key_size","YDBAIM")
	for i=1:1:100 set @g@(i,0)="v"_(i#7)_"|x"
	set @g@(50,0)=$justify("",keysize+100)_"|x"
	; A JOB'd process writes an error to its error file only when it has no
	; $ETRAP of its own, and ydb_env_set puts one in the environment that writes
	; the error to stdout instead.
	set ydbetrap=$ztrnlnm("ydb_etrap"),gtmetrap=$ztrnlnm("gtm_etrap")
	view "unsetenv":"ydb_etrap"
	view "unsetenv":"gtm_etrap"
	do
	. new $etrap set $etrap="set jobstat=$zstatus goto err^"_$text(+0)
	. set (ecodetest,jobstat)="" if $$XREFDATA^%YDBAIM(g,2,"|",1)
	if $zlength(ydbetrap) view "setenv":"ydb_etrap":ydbetrap
	if $zlength(gtmetrap) view "setenv":"gtm_etrap":gtmetrap
	do assert("U233"=ecodetest,"the worker's GVSUBOFLOW is reported as JOBERR; $ecode was ,"_ecodetest_",")
	; JOBERR names the worker's error file, which is left behind to be read
	set errfile=$zpiece(jobstat,"JOB'd process in ",2)
	do assert($zlength(errfile),"JOBERR names the error file: "_jobstat)
	do:$zlength(errfile)
	. new io set io=$io
	. ; XREFDATA() left the device open where it stopped reading, hence the rewind
	. open errfile use errfile:rewind read line use io close errfile:delete
	. do assert(line["GVSUBOFLOW","the error file holds the message that stopped the worker: "_line)
	; The incomplete cross reference has to go before the global variable does:
	; a KILL of the node with the long value fires the cross reference's KILL
	; trigger, which raises GVSUBOFLOW in this process.
	do
	. if xdref new $zgbldir set $zgbldir=xdrefgld
	. do UNXREFDATA^%YDBAIM
	kill @g
	quit
	;
tjobsym	; @TEST workers receive AIM's own variables, not the caller's symbol table
	; Builds an XREFDATA() and an XREFSUB() cross reference with four workers while
	; the caller holds a large local tjobsymjunk and its own name. The
	; transformation function runs in the workers and records their pids, and
	; whether they can see tjobsymjunk. Asserts that the workers ran, that none saw
	; tjobsymjunk, and that the caller's name is unchanged.
	new g,gx,i,j,name,subs,tjobsymjunk,x
	set g="^"_$select(xdref:xdrefloc,1:"")_"tjobsym",subs(1)="*"
	;
	; The workers write ^tjobsymx through the global directory of g
	set gx="^"_$select(xdref:xdrefloc,1:"")_"tjobsymx"
	kill @g,@gx
	for i=1:1:3000 set @g@(i)="v"_(i#37)_"|p"_(i#11)
	;
	; A caller with its own variable named like AIM's, and a large symbol table
	set name="caller's"
	for i=1:1:50000 set tjobsymjunk(i)="junk"_i
	;
	view "setenv":"ydb_aim_nproc":4
	;
	for j="XREFDATA","XREFSUB" do
	. kill @gx
	. set x=$select("XREFDATA"=j:$$XREFDATA^%YDBAIM(g,.subs,"|",1,,,1,0,2,"$$tjobsymxf^%YDBAIMTEST()"),1:$$XREFSUB^%YDBAIM(g,.subs,1,,1,0,2,"$$tjobsymxf^%YDBAIMTEST()"))
	. do assert($data(@gx@("pid")),j_": the transformation function never ran in a worker")
	. do assert('$data(@gx@("leak")),j_": a worker saw the caller's tjobsymjunk")
	. do:"XREFDATA"=j UNXREFDATA^%YDBAIM(g,.subs,"|",1,,,1,0,2,"$$tjobsymxf^%YDBAIMTEST()")
	. do:"XREFSUB"=j UNXREFSUB^%YDBAIM(g,.subs,1,,1,0,2,"$$tjobsymxf^%YDBAIMTEST()")
	view "unsetenv":"ydb_aim_nproc"
	;
	; The caller's name is unchanged
	do assert("caller's"=name,"the caller's name changed")
	;
	kill @g,@gx
	quit
	;
tjobsymxf(val,ignore)	; [private] transformation function for tjobsym: records what the worker sees
	set ^tjobsymx("pid",$job)=""
	set:$data(tjobsymjunk) ^tjobsymx("leak",$job)=""
	quit val
	;
tinv1	; @TEST Invalid Input: Global without ^
	new g set g=""_$select(xdref:xdrefloc,1:"")_"ORD"
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$T(+0)
	do
	. set ecodetest="" if $$XREFDATA^%YDBAIM(g,3,"^",2,,,,,aimtype)
	do assert(ecodetest="U252")
	; Test XREFSUB()
	do
	. set ecodetest="" if $$XREFSUB^%YDBAIM(g,3,1+$random(3))
	do assert(ecodetest="U252")
	quit
	;
tinv2	; @TEST Invalid Input: Global including subscripts
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD(100.01)"
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$T(+0)
	do
	. set ecodetest="" if $$XREFDATA^%YDBAIM(g,2,"^",2,,,,,aimtype)
	do assert(ecodetest="U252")
	; Test XREFSUB()
	do
	. set ecodetest="" if $$XREFSUB^%YDBAIM(g,2,"1:2")
	do assert(ecodetest="U252")
	quit
	;
tinv3	; @TEST Invalid Input: Bad Level
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$T(+0)
	do
	. set ecodetest="" if $$XREFDATA^%YDBAIM(g,0,"^",2,,,,,aimtype)
	do assert(ecodetest="U253")
	do
	. set ecodetest="" if $$XREFDATA^%YDBAIM(g,-1,"^",2,,,,,aimtype)
	do assert(ecodetest="U253")
	; Test XREFSUB()
	do
	. set ecodetest="" if $$XREFSUB^%YDBAIM(g,0)
	do assert(ecodetest="U253")
	do
	. set ecodetest="" if $$XREFSUB^%YDBAIM(g,-1,1)
	do assert(ecodetest="U253")
	quit
	;
tinv4	; @TEST Invalid Input: Bad Piece Number
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$T(+0)
	do
	. set ecodetest="" if $$XREFDATA^%YDBAIM(g,3,"^",0,,,,,aimtype)
	do assert(ecodetest="U248")
	do
	. set ecodetest="" if $$XREFSUB^%YDBAIM(g,3,0)
	do assert(ecodetest="U228")
	; Test XREFSUB()
	do
	. set ecodetest="" if $$XREFDATA^%YDBAIM(g,3,"^",-1,,,,,aimtype)
	do assert(ecodetest="U248")
	do
	. set ecodetest="" if $$XREFSUB^%YDBAIM(g,3,-1)
	do assert(ecodetest="U228")
	quit
	;
	;
tinv5	; @TEST Invalid Input: More subs than the number in top node
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"null"
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$T(+0)
	new subs set subs=1,subs(1)="""""",subs(2)="A"
	do
	. set ecodetest="" if $$XREFDATA^%YDBAIM(g,.subs,,,,,,,aimtype)
	do assert(ecodetest="U244")
	; Test XREFSUB()
	do
	. set ecodetest="" if $$XREFSUB^%YDBAIM(g,.subs)
	do assert(ecodetest="U230")
	quit
	;
tinv6	; @TEST Invalid Input: subs contains negative subscripts
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"null"
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$T(+0)
	new subs set subs=1,subs(-1)="""""",subs(-2)="A"
	do
	. set ecodetest="" if $$XREFDATA^%YDBAIM(g,.subs,,,,,,,aimtype)
	do assert(ecodetest="U247")
	; Test XREFSUB()
	do
	. set ecodetest="" if $$XREFSUB^%YDBAIM(g,.subs)
	do assert(ecodetest="U247")
	quit
	;
tinv7	; @TEST Invalid Input: one of the subs is an empty string
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$T(+0)
	new subs set subs(1)=100.01,subs(2)="",subs(3)=0
	do
	. set ecodetest="" if $$XREFDATA^%YDBAIM(g,.subs,"^",2,,,,,aimtype)
	do assert(ecodetest="U244")
	; Test XREFSUB(). Same underlying condition (no varying subscript left to
	; index), but XREFSUB() reports it as U230/ALLCONST rather than U244/NEEDSUB.
	do
	. set ecodetest="" if $$XREFSUB^%YDBAIM(g,.subs)
	do assert(ecodetest="U230")
	quit
	;
tinv8	; @TEST Test interaction of separators vs pieces
	; Separator without piece
	; Not meaningful for XREFSUB()
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"customers"
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$T(+0)
	do
	. set ecodetest="" if $$XREFDATA^%YDBAIM(g,1,"§",,,,,,aimtype)
	do assert(ecodetest="U250")
	;
	; piece without separator
	do
	. set ecodetest="" if $$XREFDATA^%YDBAIM(g,1,"",2,,,,,aimtype)
	do assert(ecodetest="U238")
	quit
	;
tinv9	; @TEST Request a higher level of stat after a lower level
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"customers"
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$T(+0)
	if $$XREFDATA^%YDBAIM(g,1,"§",1,0,,,1,aimtype)
	do
	. set ecodetest="" if $$XREFDATA^%YDBAIM(g,1,"§",1,0,,,2,aimtype)
	do assert(ecodetest="U240")
	; Test XREFSUB()
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"names3"
	if $$XREFSUB^%YDBAIM(g,2,1,,,1)
	do
	. set ecodetest="" if $$XREFSUB^%YDBAIM(g,2,1,,,2)
	do assert(ecodetest="U240")
	quit
	;
tinv10	; @TEST invalid stat parameter value
	; Test XREFSUB() alongside XREFDATA()
	new gc set gc="^"_$select(xdref:xdrefloc,1:"")_"customers"
	new gn set gn="^"_$select(xdref:xdrefloc,1:"")_"names3"
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$text(+0)
	do
	. set ecodetest="" if $$XREFDATA^%YDBAIM(gc,1,"§",1,0,,,-1,aimtype)
	do assert(ecodetest="U241")
	do
	. set ecodetest="" if $$XREFSUB^%YDBAIM(gn,2,1,,,-1)
	do assert(ecodetest="U241")
	do
	. set ecodetest="" if $$XREFDATA^%YDBAIM(gc,1,"§",1,0,,,5,aimtype)
	do assert(ecodetest="U241")
	do
	. set ecodetest="" if $$XREFSUB^%YDBAIM(gn,2,1,,,5)
	do assert(ecodetest="U241")
	quit
	;
tinv11	; @TEST indexing invalid repeated subscripts
	new ecodetest
	new $etrap,$estack set $etrap="goto err^"_$text(+0)
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"x"
	kill @g
	set @g@(1)="abcd",@g@(2)=""
	new subs,xref
	set subs(1)="1:1"
	do
	. set ecodetest="",xref="" set xref=$$XREFDATA^%YDBAIM(g,.subs,,,,,,,aimtype)
	do assert(ecodetest="U244")
	; Test XREFSUB(). "1:1" is a degenerate range, so every subscript is constant;
	; XREFSUB() reports that as U230/ALLCONST rather than U244/NEEDSUB.
	do
	. set ecodetest="",xref="" set xref=$$XREFSUB^%YDBAIM(g,.subs,1)
	do assert(ecodetest="U230")
	quit
	;
tinv12	; @TEST index spanning regions where null setting differs
	; Detailed discussion located at https://gitlab.com/YottaDB/Util/YDBAIM/-/merge_requests/29#note_654447677
	; Test XREFSUB() alongside XREFDATA()
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"sameset"
	new ecodetest,xref
	new $etrap,$estack set $etrap="goto err^"_$text(+0)
	; This should not issue an error as the null setting is the same across regions
	do
	. set ecodetest="",xref="" set xref=$$XREFDATA^%YDBAIM(g,2,,,,,,,aimtype)
	do assert($data(@xref@(0,"test1",1,"")))
	do
	. set ecodetest="",xref="" set xref=$$XREFSUB^%YDBAIM(g,2,1)
	do assert(10=$data(@xref@(1,1)))
	; This should issue an error
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"diffset"
	do
	. set ecodetest="",xref="" set xref=$$XREFDATA^%YDBAIM(g,2,,,,,,,aimtype)
	do assert(xref="")
	do assert(ecodetest="U251")
	do
	. set ecodetest="",xref="" set xref=$$XREFSUB^%YDBAIM(g,2,1)
	do assert(xref="")
	do assert(ecodetest="U251")
	quit
	;
tinv13	; @TEST AIM#99 - omitfix=0 with type 1 or 3 is rejected (U254), not silently built as an empty index
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"aim99"
	new s,x,ecodetest,xfn
	set xfn="$$aim98pfx^%YDBAIMTEST()"
	set s(1)=":",s(2)=9
	kill @g
	set @g@(1,0)="anchor",@g@(1,9)="Alpha"	; present value
	set @g@(2,0)="anchor"			; 9 node absent -> NULL row
	new $etrap,$estack set $etrap="goto err^"_$T(+0)
	;
	; type=1, omitfix=0 -> U254
	do
	. set ecodetest="" if $$XREFDATA^%YDBAIM(g,.s,,,,,0,0,1,1)
	do assert(ecodetest="U254","type=1 omitfix=0 must be rejected with U254")
	;
	; type=3, omitfix=0 -> U254
	do
	. set ecodetest="" if $$XREFDATA^%YDBAIM(g,.s,,,,,0,0,3,xfn)
	do assert(ecodetest="U254","type=3 omitfix=0 must be rejected with U254")
	;
	; control: type=1, omitfix=1 still builds and indexes (value and missing-node NULL)
	do
	. set ecodetest="",x=$$XREFDATA^%YDBAIM(g,.s,,,,,1,0,1,1)
	do assert(ecodetest="","type=1 omitfix=1 must not error")
	do assert($data(@x@(0,"#Alpha",1)),"type=1 omitfix=1 indexes id1 under #Alpha")
	do assert($data(@x@(0,"#",2)),"type=1 omitfix=1 indexes id2's missing 9 node as NULL under #")
	;
	do UNXREFDATA^%YDBAIM(x)
	;
	; control: type=3, omitfix=1 still builds
	do
	. set ecodetest="",x=$$XREFDATA^%YDBAIM(g,.s,,,,,1,0,3,xfn)
	do assert(ecodetest="","type=3 omitfix=1 must not error")
	do assert($data(@x@(0,"#Alpha",1)),"type=3 omitfix=1 indexes id1 under #Alpha")
	do assert($data(@x@(0,"#",2)),"type=3 omitfix=1 indexes id2's missing 9 node as NULL under #")
	do UNXREFDATA^%YDBAIM(x)
	kill @g
	quit
	;
	;
err	; Error trap for tinv* tests
	; Capture $ecode, strip commas, clear
	set ecodetest=$ecode
	set ecodetest=$piece(ecodetest,",",2) ; ,U252, -> U252
	set $ecode=""
	quit:$quit "" quit
	;
tsubs1	; @TEST XREFDATA & XREFSUB with numbers & : as subs
	; Index single node in VistA Global; no sub at top node
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new subs set subs(1)=100.01,subs(2)=":",subs(3)=0
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",2,,,,,aimtype)
	do assert($data(@aimgbl@(2,"dc",1)),"subs array works with numeric subscripts,: for varying range")
	; Add data and make sure triggers works
	set @g@(100.01,100,0)="JUNK STATUS^junk"
	do assert($data(@aimgbl@(2,"junk",100)),"set trigger works")
	; Delete data and check that kill trigger works
	new keepme merge keepme=@g@(100.01,99)
	kill @g@(100.01,99)
	do assert('$data(@aimgbl@(2,"none",99)),"kill trigger works")
	; Restore data to the way it was for later tests
	merge @g@(100.01,99)=keepme
	kill @g@(100.01,100)
	; Test XREFSUB()
	new aimgbl set aimgbl=$$XREFSUB^%YDBAIM(g,.subs,"2:3")
	do assert($data(@aimgbl@(3,0,1)),"subs array works with numeric subscripts,: for varying range")
	; Add data and make sure triggers works
	set @g@(100.01,100,0)="JUNK STATUS^junk"
	do assert($data(@aimgbl@(3,0,100)),"set trigger works")
	; Delete data and check that kill trigger works
	kill @g@(100.01,100,0)
	do assert('$data(@aimgbl@(3,0,100)),"kill trigger works")
	do UNXREFSUB^%YDBAIM(g,.subs,"2:3")
	quit
	;
tsubs2	; @TEST subs w strings,subs>subs(n)/Index Octo Global
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"%ydboctoocto"
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_namespace""",subs=4
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"|",1,,,,,aimtype)
	do assert($data(@aimgbl@(1,"public",2200)),"string subs work,subs>sub(n) works")
	do assert('$data(@aimgbl@(1,11)),"unrelated data elsewhere is not indexed")
	; Add data and make sure triggers works
	set @g@("tables","pg_catalog","pg_namespace",11806)="pg_toast_temp_2|10|"
	do assert($data(@aimgbl@(1,"pg_toast_temp_2",11806)),"New data now exists")
	; kill for later tests
	kill @g@("tables","pg_catalog","pg_namespace",11806)
	; Delete data ditto
	new %aim set %aim=@g@("tables","pg_catalog","pg_namespace",11)
	kill @g@("tables","pg_catalog","pg_namespace",11)
	do assert('$data(@aimgbl@(1,"pg_namespace")),"Deleted data removed from index")
	; restore for later tests
	set @g@("tables","pg_catalog","pg_namespace",11)=%aim
	; Test XREFSUB()
	set subs(2)=":",subs(4)=$ZWRITE(0)_":"_$ZWRITE($CHAR(0))
	set @g@("tables","pg_catalog","pg_namespace","STRING")="Ignore me"
	set aimgbl=$$XREFSUB^%YDBAIM(g,.subs,4)
	do assert(11=$data(@aimgbl@(4)))
	new i,x set x="" for  set x=$order(@aimgbl@(4,x)) quit:""=x  if $increment(i)
	do assert(6=i)
	do assert($data(@aimgbl@(4,11805,"pg_catalog")))
	do assert('$data(@aimgbl@(4,"STRING","pg_catalog")))
	; Add data and make sure SET trigger works
	set @g@("tables","pg_catalog","pg_namespace",11806)="pg_toast_temp_2|10|"
	do assert($data(@aimgbl@(4,11806,"pg_catalog")))
	zkill @g@("tables","pg_catalog","pg_namespace",11806)
	do assert('$data(@aimgbl@(4,11806,"pg_catalog")))
	kill %aim merge %aim=@g@("tables","pg_catalog","pg_namespace",11)
	kill @g@("tables","pg_catalog","pg_namespace",11)
	do assert('$data(@aimgbl@(4,11,"pg_catalog")))
	merge @g@("tables","pg_catalog","pg_namespace",11)=%aim
	do UNXREFSUB^%YDBAIM(g,.subs,4)
	do assert('$data(@aimgbl))
	kill @g@("tables","pg_catalog","pg_namespace","STRING")
	quit
	;
tsubs3	; @TEST index non-numeric data in last sub;
	; Use * to specify last sub, top level subs not specified
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"%ydboctoocto"
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"|",12,,,,,aimtype)
	do assert($data(@aimgbl@(12,1231,"numeric")),"non-numeric data properly indexed")
	; Test XREFSUB()
	set aimgbl=$$XREFSUB^%YDBAIM(g,.subs,4)
	do assert($data(@aimgbl@(4,"numeric")))
	do UNXREFSUB^%YDBAIM(g,.subs,4)
	quit
	;
tsubs4	; @TEST index different subs in same global
	; Test that it produces different aim globals; omit last
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"%ydboctoocto"
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_namespace""",subs=4
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM(g,.subs,"|",1,,,,,aimtype)
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM(g,.subs,"|",1,,,,,aimtype)
	do assert(aimgbl1'=aimgbl2)
	; Test XREFSUB()
	set subs(1)="*",subs(2)="""pg_catalog""",subs(3)="""pg_namespace""",subs=4
	set aimgbl1=$$XREFSUB^%YDBAIM(g,.subs,1)
	set subs(1)="*",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs=4
	set aimgbl2=$$XREFSUB^%YDBAIM(g,.subs,1)
	do assert(aimgbl1'=aimgbl2)
	; needed so that UNXREFSUB can remove remote database xrefs
	if xdref new $zgbldir set $zgbldir=xdrefgld
	do UNXREFSUB^%YDBAIM
	do assert('$data(@aimgbl1)),assert('$data(@aimgbl2))
	quit
	;
tsubs5	; @TEST index different pieces / subscripts with the same global specifications
	; Test that it produces the same aim global
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"%ydboctoocto"
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM(g,.subs,"|",12,,,,,aimtype)
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM(g,.subs,"|",11,,,,,aimtype)
	do assert(aimgbl1=aimgbl2)
	; Test XREFSUB()
	set aimgbl1=$$XREFSUB^%YDBAIM(g,.subs,2)
	set aimgbl2=$$XREFSUB^%YDBAIM(g,.subs,4)
	do assert(aimgbl1=aimgbl2)
	quit
	;
	;
tallsub	; @TEST index all subscripts at a specific level
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"%ydboctoocto"
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,4,,,,,,,aimtype)
	do assert($data(@aimgbl@(0,"pg_toast|10|","tables","pg_catalog","pg_namespace",99)))
	; Add data and make sure triggers work
	set @g@("tables","pg_catalog","pg_namespace",11806)="pg_toast_temp_2|10|"
	do assert($data(@aimgbl@(0,"pg_toast_temp_2|10|","tables","pg_catalog","pg_namespace",11806)))
	kill @g@("tables","pg_catalog","pg_namespace",11806)
	;
	; Remove data and make sure triggers work
	do assert($data(@aimgbl@(0,"pg_catalog|10|""{postgres=UC/postgres|=U/postgres}""","tables","pg_catalog","pg_namespace",11)))
	new % set %=@g@("tables","pg_catalog","pg_namespace",11)
	kill @g@("tables","pg_catalog","pg_namespace",11)
	do assert('$data(@aimgbl@(0,"pg_catalog|10|""{postgres=UC/postgres|=U/postgres}""","tables","pg_catalog","pg_namespace",11)))
	set @g@("tables","pg_catalog","pg_namespace",11)=%
	;
	; Test XREFSUB()
	set aimgbl=$$XREFSUB^%YDBAIM(g,4,"1:4")
	do assert($data(@aimgbl@(4,99,"tables","pg_catalog","pg_namespace")))
	set @g@("tables","pg_catalog","pg_namespace",11806)="pg_toast_temp_2|10|"
	do assert($data(@aimgbl@(4,11806,"tables","pg_catalog","pg_namespace")))
	;
	; Remove data and make sure triggers work
	do assert($data(@aimgbl@(4,11,"tables","pg_catalog","pg_namespace")))
	set %=@g@("tables","pg_catalog","pg_namespace",11)
	kill @g@("tables","pg_catalog","pg_namespace",11)
	do assert('$data(@aimgbl@(4,11,"tables","pg_catalog","pg_namespace")))
	set @g@("tables","pg_catalog","pg_namespace",11)=%
	quit
	;
tnodata1 ; @TEST indexing nodes that don't exist
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,1,"^",2,,,,,aimtype)
	do assert($order(@aimgbl@(0,""))="")
	; Test XREFSUB()
	set aimgbl=$$XREFSUB^%YDBAIM(g,1,1)
	do assert('$data(@aimgbl@(1,.5)))
	quit
	;
tnodata2 ; @TEST indexing pieces that don't exist
	; Note, this changed from the original behavior as a result of https://gitlab.com/YottaDB/Util/YDBAIM/-/merge_requests/27
	; Previous behavior is that these pieces were not indexed. New behavior is that the pieces are indexed.
	; 17th piece does not exist
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"customers"
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,1,"§",17,,,,,aimtype)
	do assert($data(@aimgbl@(17,"",1)))
	do assert($data(@aimgbl@(17,"",2)))
	do assert($data(@aimgbl@(17,"",3)))
	; This is not a useful test to adapt for XREFSUB()
	quit
	;
tnodata3 ; @TEST indexing fixed nodes that don't exist
	; Default behavior is for nodes that don't exist to not be xrefed. Verify that.
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"tnodata3"
	kill @g
	set @g@(100,1,"const1")="a"
	set @g@(100,2,"const1","const2")="b"
	set @g@(100,3,"const1")="c"
	set @g@(100,3,"const1","const2")="d"
	set @g@(100,4,"const3")="e"
	set @g@(100,5,"const1")="f"
	set @g@(100,5,"const1","const4")="g"
	set @g@(100,6,"const1","const2","const3")="h"
	new subs,aimgbl
	set subs(1)=100
	set subs(2)=":"
	set subs(3)="""const1"""
	set subs(4)="""const2"""
	do UNXREFDATA^%YDBAIM(g,.subs,,,,,,,aimtype)
	set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,,,,,,,aimtype)
	; Verify the nodes that we expect to exist in the xref
	do assert(""=@aimgbl@(0,"b",2)) kill @aimgbl@(0,"b",2)
	do assert(""=@aimgbl@(0,"d",3)) kill @aimgbl@(0,"d",3)
	; Record 6's value node exists with no value of its own but with a child. In the
	; type=4 pass the record specification is the whole value specification, so the
	; depth is the value node's own level and the child lies below it: not a record for
	; type 4 either, and the two passes index the same nodes.
	; Verify that no other nodes exist in the xref
	do assert(1=$data(@aimgbl@(0)))
	;
	; Test XREFSUB() - there are no other nodes indexed
	set aimgbl=$$XREFSUB^%YDBAIM(g,.subs,"1")
	do assert(""=@aimgbl@(1,100,2)) kill @aimgbl@(1,100,2)
	do assert(""=@aimgbl@(1,100,3)) kill @aimgbl@(1,100,3)
	do assert(1=$data(@aimgbl@(1)))
	quit
	;
tutf8tp	; @TEST UTF-8 data; seq pieces; updates (kill, set $piece)
	; 3rd piece = emails
	; Test XREFSUB() alongside XREFDATA
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"customers"
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM(g,1,"§",3,,,,,aimtype)
	do assert($data(@aimgbl1@(3,"gwashington@usa.gov")))
	; 7rd piece = zip codes
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM(g,1,"§",7,,,,,aimtype)
	do assert(aimgbl1=aimgbl2,"AIM global for separate pieces should be the same")
	do assert($data(@aimgbl2@(7,22902,5)))
	new aimgbl3 set aimgbl3=$$XREFSUB^%YDBAIM(g,1,1)
	new i,j set i="" for j=1:1 set i=$order(@aimgbl3@(1,i)) quit:""=i  do assert(i=j)
	do assert(6=j)
	;
	; Kill record 2
	do assert($data(@aimgbl1@(3,"jadams@usa.gov",2)))
	do assert($data(@aimgbl1@(7,"02169",2)))
	new % set %=@g@(2)
	kill @g@(2)
	do assert('$data(@aimgbl1@(3,"jadams@usa.gov",2)))
	do assert('$data(@aimgbl1@(7,"02169",2)))
	do assert('$data(@aimgbl3@(1,2)))
	set @g@(2)=%
	do assert($data(@aimgbl3@(1,2)))
	do UNXREFSUB^%YDBAIM(g,1,1)
	; The rest of this test is not relevant to XREFSUB()
	;
	; Modify record 1 email address
	do assert($data(@aimgbl1@(3,"gwashington@usa.gov",1)))
	set %=$piece(@g@(1),"§",3)
	set $piece(@g@(1),"§",3)="gwashington@yottadb.com"
	do assert($data(@aimgbl1@(3,"gwashington@yottadb.com",1)))
	set $piece(@g@(1),"§",3)=%
	;
	; Add Record 6 (with a twist, 7th piece is not specified)
	set @g@(6)="Road§Runner§rr@yottadb.com"
	do assert($data(@aimgbl1@(3,"rr@yottadb.com",6)))
	do assert($data(@aimgbl1@(7,"",6)))
	kill @g@(6)
	quit
	;
tmpiece	; @TEST multiple pieces requested sequentially
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"customers"
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM(g,1,"§","3;7",,,,,aimtype)
	new lines
	do
	. if xdref new $zgbldir set $zgbldir=xdrefgld
	. do trigout(.lines,"%ydb"_$zpiece(aimgbl1,"AIMD",2)_"S")
	do assert(lines(2)'["-delim=""§""")
	do assert($order(@aimgbl1@(3,""))'="")
	do assert($order(@aimgbl1@(7,""))'="")
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM(g,1,"§","3:4",,,,,aimtype)
	do assert(aimgbl1=aimgbl2)
	do assert($order(@aimgbl1@(4,""))'="")
	do
	. if xdref new $zgbldir set $zgbldir=xdrefgld
	. kill lines do trigout(.lines,"%ydb"_$zpiece(aimgbl2,"AIMD",2)_"S")
	do assert(lines($$forline(.lines))["=3,4,7")
	new aimgbl3 set aimgbl3=$$XREFDATA^%YDBAIM(g,1,"§","4:5",,,,,aimtype)
	do
	. if xdref new $zgbldir set $zgbldir=xdrefgld
	. kill lines do trigout(.lines,"%ydb"_$zpiece(aimgbl3,"AIMD",2)_"S")
	do assert(lines($$forline(.lines))["=3,4,5,7")
	new aimgbl4 set aimgbl4=$$XREFDATA^%YDBAIM(g,1,"§","5:8;10",,,,,aimtype)
	do
	. if xdref new $zgbldir set $zgbldir=xdrefgld
	. kill lines do trigout(.lines,"%ydb"_$zpiece(aimgbl4,"AIMD",2)_"S")
	do assert(lines($$forline(.lines))["=3,4,5,6,7,8,10")
	new aimgbl5 set aimgbl5=$$XREFDATA^%YDBAIM(g,1,"§","1;2;5",,,,,aimtype)
	do
	. if xdref new $zgbldir set $zgbldir=xdrefgld
	. kill lines do trigout(.lines,"%ydb"_$zpiece(aimgbl5,"AIMD",2)_"S")
	do assert(lines($$forline(.lines))["=1,2,3,4,5,6,7,8,10")
	do assert($order(@aimgbl1@(1,""))'="")
	do assert($order(@aimgbl1@(2,""))'="")
	do assert($order(@aimgbl1@(3,""))'="")
	do assert($order(@aimgbl1@(4,""))'="")
	do assert($order(@aimgbl1@(5,""))'="")
	do assert($order(@aimgbl1@(6,""))'="")
	do assert($order(@aimgbl1@(7,""))'="")
	; This is not a useful test to adapt for XREFSUB()
	quit
	;
tzpiece1 ; @TEST $zpiece
	; Indexing emails (3rd piece) - should see $ZCH(167), leftover from $ZCH(167,194) = §'
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"customers"
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,1,$ZCHAR(194),3,,1,,,aimtype)
	do assert($data(@aimgbl@(3,$ZCH(167)_"gwashington@usa.gov")))
	; This is not a useful test to adapt for XREFSUB()
	quit
	;
tzpiece2 ; @TEST $piece and $zpiece concurrently
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"customers"
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM(g,1,$ZCHAR(194),3,,1,,,aimtype)
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM(g,1,"§",3,,,,,aimtype)
	do assert(aimgbl1'=aimgbl2)
	; This is not a useful test to adapt for XREFSUB()
	quit
	;
trmindex1 ; #TEST Remove a single piece index (NOT SUPPORTED RIGHT NOW; disabling test)
	; Ensure no globals or triggers exists
	new trigs do trigout(.trigs)
	new aims  do aimgbls(.aims)
	do assert('$data(trigs(1)))
	do assert('$data(aims))
	;
	if $$XREFDATA^%YDBAIM("^customers",1,"§",3,,,,,aimtype)
	if $$XREFDATA^%YDBAIM("^customers",1,"§",7,,,,,aimtype)
	;
	new trigs do trigout(.trigs)
	new aims  do aimgbls(.aims)
	do assert(trigs(2)'["-delim=""§"" -pieces=3;7")
	do assert(aims=1)
	;
	do UNXREFDATA^%YDBAIM("^customers",1,"§",7,,,,,aimtype)
	new trigs do trigout(.trigs)
	new aims  do aimgbls(.aims)
	do assert(trigs(2)'["-delim=""§"" -pieces=3")
	do assert(aims=1)
	; This is not a useful test to adapt for XREFSUB()
	quit
	;
trmindex2 ; @TEST Remove indexes on a specific entire global
	; Note: this test depends on the AIM global for ^customers preceding those for
	; ^orders. This is the case with YottaDB r1.32 on x86_64. Depending on the
	; hash used by AIM and the underlying platform, this may need to change for
	; future releases of YottaDB and for other platforms.
	new gcustomers set gcustomers="^"_$select(xdref:xdrefloc,1:"")_"customers"
	new gorders set gorders="^"_$select(xdref:xdrefloc,1:"")_"orders"
	new aimgblcust,aimgblorders
	if $$XREFDATA^%YDBAIM(gorders,1,"§",1,,,,,aimtype)
	if $$XREFDATA^%YDBAIM(gcustomers,1,"§",3,,,,,aimtype)
	set aimgblcust=$$XREFDATA^%YDBAIM(gcustomers,1,"§",7,,,,,aimtype)
	new trigs do trigout(.trigs,"%ydb"_$zpiece(aimgblcust,"AIMD",2)_"S")
	new aims do aimgbls(.aims)
	new aimx do aimxref(.aimx)
	do assert(aims=2)
	do assert(aimx=2)
	do assert(trigs($$forline(.trigs))["=3,7")
	do UNXREFDATA^%YDBAIM(gcustomers,1,"§",,,,,,aimtype)
	kill trigs do trigout(.trigs,"%ydb"_$zpiece(aimgblcust,"AIMD",2)_"S")
	kill aims  do aimgbls(.aims)
	kill aimx  do aimxref(.aimx)
	do assert(aims=1)
	do assert(aimx=1)
	do assert('$data(trigs(3)))
	do UNXREFDATA^%YDBAIM(gorders,1,"§",1,,,,,aimtype)
	; Test XREFSUB()
	set aimgblorders=$$XREFSUB^%YDBAIM(gorders,1,1)
	set aimgblcust=$$XREFSUB^%YDBAIM(gcustomers,1,1)
	kill trigs do trigout(.trigs,"%ydb"_$zpiece(aimgblcust,"AIMS",2)_"S")
	kill aims do aimgbls(.aims,1)
	kill aimx do aimxref(.aimx,1)
	do assert(aims=2)
	do assert(aimx=2)
	do assert(trigs(2)["+^customers(sub1=*)")
	do UNXREFSUB^%YDBAIM(gcustomers,1,1)
	kill trigs do trigout(.trigs,"%ydb"_$zpiece(aimgblcust,"AIMS",2)_"S")
	kill aims do aimgbls(.aims,1)
	kill aimx do aimxref(.aimx,1)
	do assert(aims=1)
	do assert(aimx=1)
	do assert('$data(trigs))
	do UNXREFSUB^%YDBAIM(gorders,1,1)
	quit
	;
trmindex3 ; @TEST Remove all indexes
	new gcustomers set gcustomers="^"_$select(xdref:xdrefloc,1:"")_"customers"
	new gorders set gorders="^"_$select(xdref:xdrefloc,1:"")_"orders"
	if $$XREFDATA^%YDBAIM(gorders,1,"§",1,,,,,aimtype)
	if $$XREFDATA^%YDBAIM(gcustomers,1,"§",3,,,,,aimtype)
	new aimgblcust set aimgblcust=$$XREFDATA^%YDBAIM(gcustomers,1,"§",7,,,,,aimtype)
	new trigs do trigout(.trigs,"%ydb"_$zpiece(aimgblcust,"AIMD",2)_"S")
	new aims do aimgbls(.aims)
	new aimx do aimxref(.aimx)
	do assert(aims=2)
	do assert(trigs($$forline(.trigs))["=3,7")
	do assert(aimx=2)
	if xdref do
	. new $zgbldir set $zgbldir=xdrefgld
	. do UNXREFDATA^%YDBAIM
	else  do UNXREFDATA^%YDBAIM
	kill trigs do trigout(.trigs)
	kill aims  do aimgbls(.aims)
	kill aimx  do aimxref(.aimx)
	do assert('$data(trigs))
	do assert('$data(aims))
	do assert('$data(aimx))
	; Test XREFSUB()
	if $$XREFSUB^%YDBAIM(gorders,1,1)
	set aimgblcust=$$XREFSUB^%YDBAIM(gcustomers,1,1)
	kill trigs do trigout(.trigs,"%ydb"_$zpiece(aimgblcust,"AIMS",2)_"S")
	kill aims do aimgbls(.aims,1)
	kill aimx do aimxref(.aimx,1)
	do assert(aims=2)
	do assert(trigs(2)["+^customers(sub1=*)")
	do assert(aimx=2)
	if xdref do
	. new $zgbldir set $zgbldir=xdrefgld
	. do UNXREFSUB^%YDBAIM
	else  do UNXREFSUB^%YDBAIM
	kill trigs do trigout(.trigs)
	kill aims do aimgbls(.aims,1)
	kill aimx do aimxref(.aimx,1)
	do assert('$data(trigs))
	do assert('$data(aims))
	do assert('$data(aimx))
	quit
	;
trmindex4 ; @TEST Remove index on subscripts parts of same global
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"%ydboctoocto"
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM(g,.subs,"|",1,,,,,aimtype)
	new subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_namespace""",subs(4)="*"
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM(g,.subs,"|",2,,,,,aimtype)
	new aims do aimgbls(.aims)
	new aimx do aimxref(.aimx)
	do assert(aims=2)
	do assert(aimx=1) ; single global
	kill subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	do UNXREFDATA^%YDBAIM(g,.subs,"|",1,,,,,aimtype)
	kill aims do aimgbls(.aims)
	kill aimx do aimxref(.aimx)
	do assert(aims=1)
	do assert(aimx=1)
	kill subs set subs(1)="""tables""",subs(2)="""pg_catalog""",subs(3)="""pg_namespace""",subs(4)="*"
	do UNXREFDATA^%YDBAIM(g,.subs,"|",2,,,,,aimtype)
	kill aims do aimgbls(.aims)
	kill aimx do aimxref(.aimx)
	do assert('$data(aims))
	do assert('$data(aimx))
	; Test XREFSUB()
	set subs(1)="*",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	set aimgbl1=$$XREFSUB^%YDBAIM(g,.subs,"3:4")
	set subs(1)="*",subs(2)="""pg_catalog""",subs(3)="""pg_namespace""",subs(4)="*"
	set aimgbl2=$$XREFSUB^%YDBAIM(g,.subs,"3:4")
	kill aims do aimgbls(.aims,1)
	kill aimx do aimxref(.aimx,1)
	do assert(aims=2)
	do assert(aimx=1) ; single global
	set subs(1)="*",subs(2)="""pg_catalog""",subs(3)="""pg_type""",subs(4)="*"
	do UNXREFSUB^%YDBAIM(g,.subs,"3:4")
	kill aims do aimgbls(.aims,1)
	kill aimx do aimxref(.aimx,1)
	do assert(aims=1)
	do assert(aimx=1)
	kill subs set subs(1)="*",subs(2)="""pg_catalog""",subs(3)="""pg_namespace""",subs(4)="*"
	do UNXREFSUB^%YDBAIM(g,.subs,"3:4")
	kill aims do aimgbls(.aims,1)
	kill aimx do aimxref(.aimx,1)
	do assert('$data(aims))
	do assert('$data(aimx))
	quit
	;
trmindex5 ; @TEST Remove indexes by %ydbAIMD & %ydbAIMS global names
	new gorders set gorders="^"_$select(xdref:xdrefloc,1:"")_"orders"
	new gcustomers set gcustomers="^"_$select(xdref:xdrefloc,1:"")_"customers"
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM(gorders,1,"§",1,,,,,aimtype)
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM(gcustomers,1,"§",3,,,,,aimtype)
	new aimgbl3 set aimgbl3=$$XREFDATA^%YDBAIM(gcustomers,1,"§",7,,,,,aimtype)
	new aims do aimgbls(.aims)
	new aimx  do aimxref(.aimx)
	do assert(aims=2)
	do assert(aimx=2)
	do UNXREFDATA^%YDBAIM(aimgbl1)
	kill aims do aimgbls(.aims)
	kill aimx do aimxref(.aimx)
	do assert(aims=1)
	do assert(aimx=1)
	do UNXREFDATA^%YDBAIM(aimgbl2)
	kill aims do aimgbls(.aims)
	kill aimx do aimxref(.aimx)
	do assert('$data(aims))
	do assert('$data(aimx))
	; Test XREFSUB()
	set aimgbl1=$$XREFSUB^%YDBAIM(gorders,1,1)
	set aimgbl2=$$XREFSUB^%YDBAIM(gcustomers,1,1)
	kill aims do aimgbls(.aims,1)
	kill aimx do aimxref(.aimx,1)
	do assert(aims=2)
	do assert(aimx=2)
	do UNXREFSUB^%YDBAIM(aimgbl1)
	kill aims do aimgbls(.aims,1)
	kill aimx do aimxref(.aimx,1)
	do assert(aims=1)
	do assert(aimx=1)
	do UNXREFSUB^%YDBAIM(aimgbl2)
	kill aims do aimgbls(.aims,1)
	kill aimx do aimxref(.aimx,1)
	do assert('$data(aims))
	do assert('$data(aimx))
	quit
	;
tresume ; @TEST Resuming an interrupted cross-reference
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"PSNDF"
	; Start job, wait, and interrupt
	new subs set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,1,,,,aimtype) ; nmonly
	new jobpid job tresumejob:passcurlvn set jobpid=$zjob
	for  quit:$data(^%ydbAIMtmp("%YDBAIM",jobpid,0))  hang .01
	if $ZSIGPROC(jobpid,"SIGUSR1") ; stop the job (job does zgoto 0 in $zint which stops it)
	do tresumestopchk(jobpid)
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
	if $$XREFDATA^%YDBAIM(g,.subs,"^",1,,,,,aimtype)
	;
	; Now verify that AIM says it's complete, and verify index count
	set indexcount=$$tresumecountindex()
	do assert(count=indexcount)
	do assert($data(@aimgbl@(3)))
	;
	; Test XREFSUB()
	set aimgbl=$$XREFSUB^%YDBAIM(g,.subs,2,1) ; nmonly
	job tresumejob(1):passcurlvn set jobpid=$zjob
	for  quit:$data(^%ydbAIMtmp("%YDBAIM",jobpid,0))  hang .01
	if $ZSIGPROC(jobpid,"SIGUSR1") ; stop the job (job does zgoto 0 in $zint which stops it)
	do tresumestopchk(jobpid)
	;
	; Ensure that AIM did not certify this index complete (0 node)
	; As the (0) node is not set and the (3) node is not updated until cross-referencing
	; is complete, either one is a sufficient test for non-completion.
	do assert('$data(@aimgbl@(0)))
	;
	; Ensure that the record count is greater than the index count
	set count=$$tresumecountdata()
	set indexcount=$$tresumecountindex(1)
	do assert(count>indexcount)
	;
	; Resume building
	if $$XREFSUB^%YDBAIM(g,.subs,2)
	; Now verify that AIM says it's complete, and verify index count
	set indexcount=$$tresumecountindex(1)
	do assert(count=indexcount)
	do assert($data(@aimgbl@(0)))
	quit
	;
tresumejob(type) ; [job target - tresume target] - type=1 means XREFSUB()
	; $ZLENGTH() is what makes the ZGOTO run. $ZJOBEXAM() returns the name of the
	; file it wrote, and a file name is 0 as a truth value, so this line written
	; as "if $zjobexam() zgoto 0" halted nothing: the process wrote its dump and
	; went on scanning, and tresume, whose whole subject is resuming an
	; interrupted scan, interrupted nothing and raced the scan it started. Same
	; idiom as sigusrint below.
	set $zinterrupt="if $zlength($zjobexam()) zgoto 0"
	if +$get(type) do XREFSUB^%YDBAIM("^PSNDF",.subs,2)
	else  do XREFDATA^%YDBAIM("^PSNDF",.subs,"^",1,,,,,aimtype)
	quit
	;
	; Wait for a signaled scan, and every process it JOB'd, to stop.
	;
	; Halting the process that started the scan does not end the scan. Its
	; $ZINTERRUPT handler halts it without running any AIM code, so the processes
	; it JOB'd are left to scan the ranges already queued for them, and since
	; AIM#97 there is one of those per CPU. Left alone they can index the whole
	; global variable while this test counts what they have indexed, which makes
	; the counts below equal and leaves nothing partial for the resumption below
	; to resume. So wait for the signaled process to stop, terminate the processes
	; it JOB'd, and wait for those to stop, before counting anything. AIM stops them
	; itself once their parent is gone, but only at the end of the range each is
	; scanning, and this test does not wait for that.
	;
	; The pids are read from the same node the wait above waits on, which the
	; halted process leaves behind, and both waits are bounded, so that a process
	; that never exits fails this test and says which wait it was in rather than
	; hanging the whole test suite.
tresumestopchk:(pid)
	new deadline,proc,waiting
	set deadline=$zut+60E6
	for  quit:'$zgetjpi(pid,"isprocalive")!($zut>deadline)  hang .01
	do assert('$zgetjpi(pid,"isprocalive"),"the signaled process did not exit within 60 seconds")
	set proc="" for  set proc=$order(^%ydbAIMtmp("%YDBAIM",pid,0,proc)) quit:'$zlength(proc)  if $zsigproc(proc,"TERM")
	set deadline=$zut+60E6,waiting=1
	for  do  quit:'waiting!($zut>deadline)
	. set waiting=0,proc=""
	. for  set proc=$order(^%ydbAIMtmp("%YDBAIM",pid,0,proc)) quit:'$zlength(proc)  set:$zgetjpi(proc,"isprocalive") waiting=1
	. hang:waiting .01
	do assert('waiting,"the scanning processes did not exit within 60 seconds")
	quit
	;
tresumecountdata:() ; [$$: # of data items in ^PSNDF(50.6)]
	new count set count=0
	new i for i=0:0 set i=$order(^PSNDF(50.6,i)) quit:'i  if $data(^(i,"VUID")) if $increment(count)
	quit count
	;
tresumecountindex:(type) ; [$$: # of index items in AIM global for ^PSNDF(50.6)] - type=1 means subscript test
	new indexcount set indexcount=0
	new i,j
	if +$get(type) do
	. set i="" for  set i=$order(@aimgbl@(2,i)) quit:'$zlength(i)  if $increment(indexcount)
	else  do
	. set i="" for  set i=$order(@aimgbl@(1,i)) quit:'$zlength(i)  do
	.. set j="" for  set j=$order(@aimgbl@(1,i,j)) quit:'$zlength(j)  if $increment(indexcount)
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
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new subs set subs(1)=100.01,subs(2)="1:9",subs(3)=0
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",2,,,,,aimtype)
	do assert($data(@aimgbl@(2,"hold",3)))
	do assert('$data(@aimgbl@(2,"dlay",10)))
	; Test XREFSUB()
	set aimgbl=$$XREFSUB^%YDBAIM(g,.subs,3)
	do assert($data(@aimgbl@(3,0,9)))
	do assert('$data(@aimgbl@(3,0,10)))
	quit
	;
trange2 ; @TEST String Subscript Range
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"sam"
	kill @g
	set @g@("a",0,1)="a1|a2"
	set @g@("b",0,2)="b1|b2"
	set @g@("c",0,3)="c1|c2"
	new subs set subs(1)="""a"":""b""",subs(2)=0,subs=3
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"|",2,,,,,aimtype)
	do assert($data(@aimgbl@(2,"a2","a",1)))
	do assert($data(@aimgbl@(2,"b2","b",2)))
	do assert('$data(@aimgbl@(2,"c2","c",3)))
	; Test XREFSUB()
	set aimgbl=$$XREFSUB^%YDBAIM(g,.subs,1)
	do assert($data(@aimgbl@(1,"a",1)))
	do assert($data(@aimgbl@(1,"b",2)))
	do assert('$data(@aimgbl@(1,"c",3)))
	kill @g
	quit
	;
trange3	; #TEST indexing subscripts with range symbols embedded
	; This test is disabled until issue YottaDB/DB/YDB#691 is implemented.
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"x"
	kill @g
	set @g@("dummy1;:",1)="abcd",@g@("dummy1;:",2)=""
	new subs,xref
	set subs(1)="""dummy1;:""",subs(2)="*" set xref=$$XREFDATA^%YDBAIM(g,.subs,,,,,,,aimtype)
	do assert($data(@xref@(0,"abcd")))
	; Once this is enabled for XREFDATA(), add a test for XREFSUB()
	quit
	;
trange4	; @TEST indexing subscripts with an open range
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"x"
	kill @g
	set @g@(1)="abcd",@g@(2)=""
	new subs,xref
	set subs(1)="1:" set xref=$$XREFDATA^%YDBAIM(g,.subs,,,,,,,aimtype)
	do assert($data(@xref@(0,"abcd")))
	; Test XREFSUB()
	set xref=$$XREFSUB^%YDBAIM(g,.subs,1)
	do assert($data(@xref@(1,1)))
	quit
	;
trange5	; @TEST indexing repeated subscripts 1
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"x"
	kill @g
	set @g@(1)="abcd",@g@(2)=""
	new subs,xref
	set subs(1)="1;1" set xref=$$XREFDATA^%YDBAIM(g,.subs,,,,,,,aimtype)
	do assert($data(@xref@(0,"abcd")))
	; Test XREFSUB()
	set xref=$$XREFSUB^%YDBAIM(g,.subs,1)
	do assert($data(@xref@(1,1))),assert('$data(@xref@(1,2)))
	quit
	;
tnull	; @TEST Index data with null subscripts
	; Index everything (no fixed subs)
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"null"
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM(g,2,,,,,,,aimtype)
	do assert($data(@aimgbl1@(0,"test1","","")))
	do assert($data(@aimgbl1@(0,"test2","","A")))
	do assert($data(@aimgbl1@(0,"test5",5,"")))
	; Test XREFSUB()
	set aimgbl1=$$XREFSUB^%YDBAIM(g,2,2)
	do assert($data(@aimgbl1@(2,"","")))
	do assert($data(@aimgbl1@(2,"A","")))
	do assert($data(@aimgbl1@(2,"",5)))
	;
	; Index with null as a fixed sub
	new subs set subs(1)="""""",subs=2
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM(g,.subs,,,,,,,aimtype)
	do assert($data(@aimgbl2@(0,"test1","")))
	do assert($data(@aimgbl2@(0,"test2","A")))
	do assert($data(@aimgbl2@(0,"test4",4)))
	;
	set aimgbl2=$$XREFSUB^%YDBAIM(g,.subs,2)
	do assert($data(@aimgbl2@(2,"")))
	do assert($data(@aimgbl2@(2,"A")))
	do assert('$data(@aimgbl2@(2,5)))
	quit
	;
tomitfix1 ; @TEST Omit fixed subscripts aka omitfix
	; Default is to omit fixed subscripts.
	; Assert that nodes that are expected are present and those that are not are not.
	; Test XREFSUB() alongside XREFDATA()
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"null"
	new subs set subs(1)="""""",subs=2
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM(g,.subs,,,,,,,aimtype)
	; ^%ydbAIMDTeLUKXq5NuulyAqUDqoy1A(0,"test1","")=""
	; ^%ydbAIMDTeLUKXq5NuulyAqUDqoy1A(0,"test2","A")=""
	; ^%ydbAIMDTeLUKXq5NuulyAqUDqoy1A(0,"test4",4)=""
	do assert($data(@aimgbl1@(0,"test1","")))
	do assert($data(@aimgbl1@(0,"test2","A")))
	do assert($data(@aimgbl1@(0,"test4",4)))
	do assert('$data(@aimgbl1@(0,"test1","","")))
	do assert('$data(@aimgbl1@(0,"test2","","A")))
	do assert('$data(@aimgbl1@(0,"test4","",4)))
	set aimgbl1=$$XREFSUB^%YDBAIM(g,.subs,1)
	do assert($data(@aimgbl1@(1,"",4)))
	do assert($data(@aimgbl1@(1,"","A")))
	;
	; Now create with omitfix=0
	new subs set subs(1)="""""",subs=2
	; params = gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM(g,.subs,"","","","",0,,aimtype)
	do assert($data(@aimgbl2@(0,"test1","","")))  ; <-- see note below
	do assert($data(@aimgbl2@(0,"test2","","A")))
	do assert($data(@aimgbl2@(0,"test4","",4)))
	do assert($data(@aimgbl2@(0,"test1",""))#2=0) ; This is special as we do have ("","") existing up above
	do assert('$data(@aimgbl2@(0,"test2","A")))
	do assert('$data(@aimgbl2@(0,"test4",4)))
	set aimgbl2=$$XREFSUB^%YDBAIM(g,.subs,1,,0)
	do assert($data(@aimgbl2@(1,"","")))
	do assert($data(@aimgbl2@(1,"","A")))
	do assert($data(@aimgbl2@(1,"",4)))
	quit
	;
tomitfix2 ; @TEST Omitfix crash [#35]
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new subs set subs(1)=100.01,subs(2)=":",subs(3)=0
	; params = gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",2,0,0,0,0,aimtype)
	do assert($data(@aimgbl))
	; params = gbl,xsub,snum,nmonly,omitfix,stat
	; Test XREFSUB()
	set aimgbl=$$XREFSUB^%YDBAIM(g,.subs,"1:3",0,0,0)
	do assert($data(aimgbl))
	quit
	;
tnmonly	; @TEST Name only paramater aka nmonly
	; D UNXREFDATA^%YDBAIM
	; write "printing aims:",!
	; new aims  do aimgbls(.aims)
	; zwrite aims
	; write "----------",!
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"PSNDF"
	new subs set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,1,,,,aimtype) ; nmonly
	; zwrite @aimgbl@(*)
	do assert('$data(@aimgbl),"Shouldn't have been created yet")
	; Test XREFSUB()
	set aimgbl=$$XREFSUB^%YDBAIM(g,.subs,2,1)
	do assert('$data(@aimgbl),"Shouldn't have been created yet")
	quit
	;
tlsxref1 ; @TEST LSXREFDATA normal tests
	; Test at beginning to assert that nothing exists to begin with
	new gorders set gorders="^"_$select(xdref:xdrefloc,1:"")_"orders"
	new gcustomers set gcustomers="^"_$select(xdref:xdrefloc,1:"")_"customers"
	new gnull set gnull="^"_$select(xdref:xdrefloc,1:"")_"null"
	new xrefs
	if xdref do
	. new $zgbldir set $zgbldir=xdrefgld
	. do LSXREFDATA^%YDBAIM(.xrefs)
	else  do LSXREFDATA^%YDBAIM(.xrefs)
	do assert('$data(xrefs))
	;
	; Now create indexes on 3 separate globals. 4 xrefs should exist
	new aimgbl1,aimgbl2,aimgbl3,aimgbl4,aimgbl5
	set aimgbl1=$$XREFDATA^%YDBAIM(gorders,1,"§",1,,,,,aimtype)
	set aimgbl2=$$XREFDATA^%YDBAIM(gcustomers,1,"§",3,,,,,aimtype) ; produces same xref as only piece is different
	set aimgbl3=$$XREFDATA^%YDBAIM(gcustomers,1,"§",7,,,,,aimtype) ; ditto
	do assert(aimgbl2=aimgbl3)
	new subs set subs(1)="""""",subs=2
	set aimgbl4=$$XREFDATA^%YDBAIM(gnull,.subs,,,,,,,aimtype)         ; different xref as subs is different
	new aimgbl5 set aimgbl5=$$XREFDATA^%YDBAIM(gnull,2,,,,,,,aimtype) ; ditto
	;
	; Test without the second parameter to get all xrefs
	new xrefs
	if xdref do
	. new $zgbldir set $zgbldir=xdrefgld
	. do LSXREFDATA^%YDBAIM(.xrefs)
	else  do LSXREFDATA^%YDBAIM(.xrefs)
	new xref set xref=""
	new count for  set xref=$order(xrefs(xref)) quit:xref=""  if $increment(count)
	do assert(count=4)
	;
	; Test getting the xrefs by the xreffed global. ^null should have 2
	new xrefs do LSXREFDATA^%YDBAIM(.xrefs,gnull)
	new xref set xref=""
	new count for  set xref=$order(xrefs(xref)) quit:xref=""  if $increment(count)
	do assert(count=2)
	;
	; Test getting the xrefs by the AIM global. Should only return one entry.
	new xrefs do LSXREFDATA^%YDBAIM(.xrefs,aimgbl2)
	new xref set xref=""
	new count for  set xref=$order(xrefs(xref)) quit:xref=""  if $increment(count)
	do assert(count=1)
	;
	; Test XREFSUB()
	kill xrefs
	if xdref do
	. new $zgbldir set $zgbldir=xdrefgld
	. do LSXREFSUB^%YDBAIM(.xrefs)
	else  do LSXREFSUB^%YDBAIM(.xrefs)
	do assert('$data(xrefs))
	set aimgbl1=$$XREFSUB^%YDBAIM(gorders,1,1)
	set aimgbl2=$$XREFSUB^%YDBAIM(gcustomers,1,1)
	set subs(1)="""""",subs=2
	set aimgbl4=$$XREFSUB^%YDBAIM(gnull,.subs,1)
	set aimgbl5=$$XREFSUB^%YDBAIM(gnull,2,1)
	if xdref do
	. new $zgbldir set $zgbldir=xdrefgld
	. do LSXREFSUB^%YDBAIM(.xrefs)
	else  do LSXREFSUB^%YDBAIM(.xrefs)
	set xref="" for count=0:1 set xref=$order(xrefs(xref)) quit:'$zlength(xref)
	do assert(count=4)
	kill xrefs do LSXREFSUB^%YDBAIM(.xrefs,gnull)
	set xref="" for count=0:1 set xref=$order(xrefs(xref)) quit:'$zlength(xref)
	do assert(count=2)
	kill xrefs do LSXREFSUB^%YDBAIM(.xrefs,aimgbl2)
	set xref="" for count=0:1 set xref=$order(xrefs(xref)) quit:'$zlength(xref)
	do assert(count=1)
	quit
	;
tlsxref2 ; @TEST LSXREFDATA when non-AIM ^%ydbAIM* globals exist
	; Test that they do not crash LSXREFDATA
	; https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/898#note_653213876
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"names"
	new subs set subs(1)="*"
	NEW aimglobal SET aimglobal=$$XREFDATA^%YDBAIM(g,.subs,"|",2,0,0,1,0,aimtype)
	set ^%ydbAIMOctoCache("NAMES","LASTNAME","completed?")=1
	set ^%ydbAIMOctoCache("NAMES","LASTNAME","location")=$name(@aimglobal@(2))
	new xrefs
	if xdref do
	. new $zgbldir set $zgbldir=xdrefgld
	. do LSXREFDATA^%YDBAIM(.xrefs)
	else  do LSXREFDATA^%YDBAIM(.xrefs)
	new xref set xref=""
	new count for  set xref=$order(xrefs(xref)) quit:xref=""  if $increment(count)
	do assert(count=1)
	; Test XREFSUB()
	SET aimglobal=$$XREFSUB^%YDBAIM(g,.subs,1)
	kill count,xrefs set xref=""
	if xdref do
	. new $zgbldir set $zgbldir=xdrefgld
	. do LSXREFSUB^%YDBAIM(.xrefs)
	else  do LSXREFSUB^%YDBAIM(.xrefs)
	for count=0:1 set xref=$order(xrefs(xref)) quit:'$zlength(xref)
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
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"customers"
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM(g,1,"§",1,0,,,1,aimtype)
	do assert(@aimgbl1@(-1,"George")=1)
	do assert(@aimgbl1@(-1,"James")=2)
	do assert($data(@aimgbl1@(-1))[0) ; Distinct values don't exist
	;
	; Unxref
	DO UNXREFDATA^%YDBAIM(g,1,"§",1,0,,,1,aimtype)
	do assert('$data(@aimgbl1))
	;
	; Call with stat=2, which provides data for the each value, and total and distinct values
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM(g,1,"§",1,0,,,2,aimtype)
	do assert(@aimgbl2@(-1,"George")=1)
	do assert(@aimgbl2@(-1,"James")=2)
	do assert(@aimgbl2@(-1)=4) ; Distinct values
	;
	; Test XREFSUB()
	; params = gbl,xsub,snum,nmonly,omitfix,stat
	; Call with stat=1, which only provides data for the each value, not the total values
	set aimgbl1=$$XREFSUB^%YDBAIM(g,1,1,,,1)
	do assert(@aimgbl1@(-1,1)=1)
	do assert(@aimgbl1@(-1,5)=1)
	do assert($data(@aimgbl1@(-1))[0) ; Distinct values don't exist
	;
	; Unxref
	DO UNXREFSUB^%YDBAIM(g,1,1,,,1)
	do assert('$data(@aimgbl1))
	;
	; Call with stat=2, which provides data for the each value, and total and distinct values
	set aimgbl2=$$XREFSUB^%YDBAIM(g,1,1,,,2)
	do assert(@aimgbl2@(-1,1)=1)
	do assert(@aimgbl2@(-1,5)=1)
	do assert(@aimgbl2@(-1)=5) ; Distinct values
	do UNXREFSUB^%YDBAIM(aimgbl2)
	quit
	;
tstat2	; @TEST xref operations with stats=0,1,2
	; Test 3 cases each of which is expected to produce the same output
	; irrespective of whether stat=0, or stat=1 or stat=2.
	; Test XREFSUB() alongside XREFDATA()
	;
	new stat for stat=0:1:2 do tstat2run(stat)
	quit
	;
tstat2run(stat) ;
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"names2"
	new case,xrefdata,xrefsub
	; -----------------------------------------------------
	; Case 1
	; ------
	; SET 4 records first and then XREF all records in one shot
	; -----------------------------------------------------
	set case=1
	kill @g
	set @g@(0)="Zero|Cool"
	set @g@(1)="Acid|Burn"
	set @g@(2)="Cereal|Killer"
	set @g@(3)="Zero|Nikon"
	set xrefdata=$$XREFDATA^%YDBAIM(g,1,"|",1,,,,stat,aimtype)
	do tstat2assertdata(xrefdata,"Error with stat="_stat_" case="_case_" at "_$zpos)
	if stat>0 do tstat2assertstat1(xrefdata,"Error with stat="_stat_" case="_case_" at "_$zpos)
	if stat=2 do tstat2assertstat2(xrefdata,"Error with stat="_stat_" case="_case_" at "_$zpos)
	set xrefsub=$$XREFSUB^%YDBAIM(g,1,1,,,stat)
	do tstat2assertsub(xrefsub,"Error with stat="_stat_" case="_case_" at "_$zpos)
	if stat>0 do tstat2assertsubstat1(xrefsub,"Error with stat="_stat_" case="_case_" at "_$zpos)
	if stat=2 do tstat2assertsubstat2(xrefsub,"Error with stat="_stat_" case="_case_" at "_$zpos)
	; -----------------------------------------------------
	; Case 2
	; ------
	; XREF no data first; Then SET 4 records to exercise trigger
	; Final result is same 4 records as Case 1.
	; -----------------------------------------------------
	set case=2
	kill @g
	set xrefdata=$$XREFDATA^%YDBAIM(g,1,"|",1,,,,stat,aimtype)
	set xrefsub=$$XREFSUB^%YDBAIM(g,1,1,,,stat)
	set @g@(0)="Zero|Cool"
	set @g@(1)="Acid|Burn"
	set @g@(2)="Cereal|Killer"
	set @g@(3)="Zero|Nikon"
	do tstat2assertdata(xrefdata,"Error with stat="_stat_" case="_case_" at "_$zpos)
	if stat>0 do tstat2assertstat1(xrefdata,"Error with stat="_stat_" case="_case_" at "_$zpos)
	if stat=2 do tstat2assertstat2(xrefdata,"Error with stat="_stat_" case="_case_" at "_$zpos)
	do tstat2assertsub(xrefsub,"Error with stat="_stat_" case="_case_" at "_$zpos)
	if stat>0 do tstat2assertsubstat1(xrefsub,"Error with stat="_stat_" case="_case_" at "_$zpos)
	if stat=2 do tstat2assertsubstat2(xrefsub,"Error with stat="_stat_" case="_case_" at "_$zpos)
	; -----------------------------------------------------
	; Case 3
	; ------
	; XREF 6 records first; Then KILL/ ZKILL 3 records and SET 1 record to exercise triggers
	; Final result is same 4 records as Case 1 and Case 2.
	; -----------------------------------------------------
	set case=3
	kill @g
	set @g@(0)="Zero|Cool"
	set @g@(1)="Acid|Burn"
	set @g@(2)="Cereal|Killer"
	set @g@(3)="Zero|Nikon"
	set @g@(4)="Joey|"
	set @g@(5)="Zero|Cool"
	set xrefdata=$$XREFDATA^%YDBAIM(g,1,"|",1,,,,stat,aimtype)
	set xrefsub=$$XREFSUB^%YDBAIM(g,1,1,,,stat)
	zkill @g@(3)
	kill @g@(4)
	zkill @g@(5)
	set @g@(3)="Zero|Nikon"
	do tstat2assertdata(xrefdata,"Error with stat="_stat_" case="_case_" at "_$zpos)
	if stat>0 do tstat2assertstat1(xrefdata,"Error with stat="_stat_" case="_case_" at "_$zpos)
	if stat=2 do tstat2assertstat2(xrefdata,"Error with stat="_stat_" case="_case_" at "_$zpos)
	do tstat2assertsub(xrefsub,"Error with stat="_stat_" case="_case_" at "_$zpos)
	if stat>0 do tstat2assertsubstat1(xrefsub,"Error with stat="_stat_" case="_case_" at "_$zpos)
	if stat=2 do tstat2assertsubstat2(xrefsub,"Error with stat="_stat_" case="_case_" at "_$zpos)
	if xdref do
	. new $zgbldir set $zgbldir=xdrefgld
	. DO UNXREFDATA^%YDBAIM,UNXREFSUB^%YDBAIM
	else  DO UNXREFDATA^%YDBAIM,UNXREFSUB^%YDBAIM
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
tstat2assertsub(xref,message)
	new aims  do aimgbls(.aims,1)
	do assert(aims=1)
	do assert($data(@xref@(1,1)),message)
	do assert($data(@xref@(1,2)),message)
	do assert($data(@xref@(1,0)),message)
	do assert($data(@xref@(1,3)),message)
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
tstat2assertsubstat1(xref,message)
	new aims  do aimgbls(.aims,1)
	do assert(aims=1)
	do assert(@xref@(-1,1)=1,message)
	do assert(@xref@(-1,2)=1,message)
	do assert(@xref@(-1,0)=1,message)
	quit
	;
tstat2assertstat2(xref,message)
	new aims  do aimgbls(.aims)
	; zwrite aims
	do assert(aims=1)
	do assert(@xref@(-1)=3,message)
	quit
	;
tstat2assertsubstat2(xref,message)
	new aims  do aimgbls(.aims,1)
	; zwrite aims
	do assert(aims=1)
	do assert(@xref@(-1)=4,message)
	quit
	;
tstat3	; @TEST Ensure that stat=2 produces correct triggers
	; https://gitlab.com/YottaDB/Util/YDBAIM/-/issues/47
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"x"
	kill @g
	new aim set aim=$$XREFDATA^%YDBAIM(g,2,"|",1,0,0,1,2,aimtype)
	do assert(aim'="")
	set aim=$$XREFSUB^%YDBAIM(g,2,"1,2",,2)
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
	; Test XREFSUB() alongside XREFDATA()
	new stat,gblname,subslevel,subnum,$etrap
	; In case of an error (like the %YDBAIM-F-SETZTRIGGERFAIL error we saw in #47), display relevant random variables
	; in the error output so one can use that information to recreate the exact test that failed. Hence the zwrite below.
	set $etrap="write ! zwrite $zstatus zwrite:$data(gblname) gblname zwrite:$data(subslevel) subslevel zwrite:$data(stat) stat"
	set gblname=$$randgblname
	set subslevel=1+$random(16),subnum="1:"_(1+$random(subslevel))
	for stat=0:1:2 do
	. set xref=$$XREFDATA^%YDBAIM(gblname,subslevel,"|",1,,,,stat,aimtype)
	. set xref=$$UNXREFDATA^%YDBAIM(gblname,subslevel,"|",1,,,,stat,aimtype)
	. set xref=$$XREFSUB^%YDBAIM(gblname,subslevel,subnum,,,stat)
	. set xref=$$UNXREFSUB^%YDBAIM(gblname,subslevel,subnum,,,stat)
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
	set gblname="^"_$select(xdref:xdrefloc,1:"")_$extract(alphabet,1+$random(alphabetlen))
	set gblnamelen=$random(31)
	for i=1:1:gblnamelen set gblname=gblname_$extract(alphanumeric,1+$random(alphanumericlen))
	quit gblname
	;
tspeeddata1	; @TEST Data Index 1E6 rows of ^names(:)="A|B"
	;new reccount,indexcount set (reccount,indexcount)=0
	;new i for i=0:0 set i=$order(^names(i)) quit:'i  if $increment(reccount)
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"names"
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,1,"|",2,,,,,aimtype)
	;new i set i="" for  set i=$order(@aimgbl@(2,i)) quit:i=""  do
	;. new j set j="" for  set j=$order(@aimgbl@(2,i,j)) quit:j=""  if $increment(indexcount)
	;do assert(reccount=indexcount)
	quit
	;
tspeeddata2	; @TEST Data Index 1E6 rows of ^names3("fix",:,0)="A|B"
	;new reccount,indexcount set (reccount,indexcount)=0
	;new i for i=0:0 set i=$order(^names3("fix",i)) quit:'i  if $data(^(i,0)),$increment(reccount)
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"names3"
	new subs set subs(1)="""fix""",subs(2)=":",subs(3)=0
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"|",2,,,,,aimtype)
	;new i set i="" for  set i=$order(@aimgbl@(2,i)) quit:i=""  do
	;. new j set j="" for  set j=$order(@aimgbl@(2,i,j)) quit:j=""  if $increment(indexcount)
	;do assert(reccount=indexcount)
	quit
	;
tspeeddata3	; @TEST Data Index 1E6 rows of ^composite(1,2,3,4,5,6,7,:)="A|B"
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"composite"
	;new reccount,indexcount set (reccount,indexcount)=0
	;new i set i=$name(^composite)
	;for  set i=$query(@i) quit:i=""  if $increment(reccount)
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,8,,,,,,,aimtype)
	;new i,start set (i,start)=$name(@aimgbl@(0))
	;for  set i=$query(@i) quit:$name(@i,1)'=start  if $increment(indexcount)
	;do assert(reccount=indexcount)
	quit
	;
tspeeddata4	; @TEST Data Index ~1E6 dispersed rows in ^PSNDF(50.6,:,"VUID")
	;new reccount,indexcount set (reccount,indexcount)=0
	;new i for i=0:0 set i=$order(^PSNDF(50.6,i)) quit:'i  if $data(^(i,"VUID")) if $increment(reccount)
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"PSNDF"
	new subs set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,,,,,aimtype)
	;new i set i="" for  set i=$order(@aimgbl@(1,i)) quit:i=""  do
	;. new j set j="" for  set j=$order(@aimgbl@(1,i,j)) quit:j=""  if $increment(indexcount)
	;do assert(reccount=indexcount)
	quit
	;
t4speeddata1	; @TEST type=4 Data Index 1E6 nodes of ^names(:)="A|B"
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	; tspeeddata1 with a record specification. The record walk is divided among JOB'd
	; processes just as the value walk is, so this times that division over the
	; same data.
	;
	; The counting that would check the divided walk missed nothing is commented
	; out, as it is in tspeeddata1: it is two 1E6 $ORDER() walks in this process
	; and it dominates the measurement, hiding the thing being measured. It was
	; run before being commented out, at 1, 2, 3, 5, 8 and 15 processes, and the
	; whole suite passes at 3 as well as at the default.
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"names"
	new recsub set recsub(1)=":"		; the value node IS the record
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,1,"|",2,,,,,4,,,g,.recsub)
	;new reccount,indexcount set (reccount,indexcount)=0
	;new i for i=0:0 set i=$order(@g@(i)) quit:'i  if $increment(reccount)
	;set i="" for  set i=$order(@aimgbl@(2,i)) quit:i=""  do
	;. new j set j="" for  set j=$order(@aimgbl@(2,i,j)) quit:j=""  if $increment(indexcount)
	;do assert(reccount=indexcount,"divided record walk indexed "_indexcount_" of "_reccount)
	do UNXREFDATA^%YDBAIM(aimgbl)
	quit
	;
t4speeddata4	; @TEST type=4 Data Index ~1E6 dispersed rows in ^PSNDF(50.6,:,"VUID")
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	; tspeeddata4 with a record specification, and the shape Octo generates: the column
	; one level below the row, so the record specification runs one level past its deepest named
	; subscript and the chunk level is the ien.
	;
	; The commented-out count differs from tspeeddata4's on purpose. The value walk
	; visits only the records that HAVE a "VUID" node; the record walk visits every
	; record and indexes the rest under NULL, so the index holds one entry per
	; ^PSNDF(50.6,ien) with no $data() test. Commented out for the reason given in
	; t4speeddata1, and run the same way before it was.
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"PSNDF"
	new subs   set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new recsub set recsub(1)=50.6,recsub(2)=":",recsub=3	; depth 3: level 3 varies and is watched, not a key column
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,,,,,4,,,g,.recsub)
	;new reccount,indexcount set (reccount,indexcount)=0
	;new i for i=0:0 set i=$order(@g@(50.6,i)) quit:'i  if $increment(reccount)
	;set i="" for  set i=$order(@aimgbl@(1,i)) quit:i=""  do
	;. new j set j="" for  set j=$order(@aimgbl@(1,i,j)) quit:j=""  if $increment(indexcount)
	;do assert(reccount=indexcount,"divided record walk indexed "_indexcount_" of "_reccount)
	do UNXREFDATA^%YDBAIM(aimgbl)
	quit
	;
tspeeddata4solo	; @TEST 1 process: type=0 Data Index ~1E6 dispersed rows in ^PSNDF(50.6,:,"VUID")
	; tspeeddata4 pinned to a single scanning process. This is the INSTRUMENT for
	; changes to xrefdata(), the type=0 walk, and its partner t4speeddata4solo is
	; the instrument for the decoupled walk.
	;
	; Why pinning matters. xrefjobmore() JOBs one worker at the outset and recruits
	; more only once the scan has been running longer than jobby, 50ms. A build of
	; this fixture takes about a second, so it always crosses that line, and how many
	; workers it then recruits and when depends on what else the machine is doing.
	; That is the dominant term in the run-to-run spread of tspeeddata4, which was
	; measured at about 25% of the mean - far wider than any change to the walk
	; itself is likely to be, so those tests can detect only a gross regression.
	; With ydb_aim_nproc=1 there is one worker, no recruitment, and the number left
	; is the cost of the walk.
	;
	; It does NOT measure the parallel scan; tspeeddata4 still does that.
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"PSNDF"
	new subs set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new aimgbl,sav
	set sav=$ztrnlnm("ydb_aim_nproc")
	view "setenv":"ydb_aim_nproc":"1"
	set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,,,,,aimtype)
	do speednproc(sav)
	do UNXREFDATA^%YDBAIM(aimgbl)
	quit
	;
t4speeddata4solo	; @TEST 1 process: type=4 Data Index ~1E6 dispersed rows in ^PSNDF(50.6,:,"VUID")
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	; t4speeddata4 pinned to a single scanning process; see tspeeddata4solo for why.
	; This one times the decoupled record walk, xr4walk/xr4rng, over the same rows,
	; so the pair brackets both walkers that step C proposes to merge into one.
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"PSNDF"
	new subs   set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new recsub set recsub(1)=50.6,recsub(2)=":",recsub=3
	new aimgbl,sav
	set sav=$ztrnlnm("ydb_aim_nproc")
	view "setenv":"ydb_aim_nproc":"1"
	set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,,,,,4,,,g,.recsub)
	do speednproc(sav)
	do UNXREFDATA^%YDBAIM(aimgbl)
	quit
	;
t6speeddata4	; @TEST type=6 Data Index ~1E6 dispersed rows in ^PSNDF(50.6,:,"VUID")
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	; t4speeddata4 with a transformation function, on the same data, so the pair isolates
	; what a transformation costs per indexed value: types 2 and 6 run the caller's
	; function in the scan and in the trigger bodies for EVERY value indexed.
	;
	; aim77neg is deliberately trivial - it negates the value - so that what is timed
	; is AIM's per-value call, not somebody else's function.
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"PSNDF"
	new subs   set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new recsub set recsub(1)=50.6,recsub(2)=":",recsub=3	; depth 3: level 3 varies and is watched, not a key column
	; gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type,force,comment,recgbl,recsub
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,0,0,,,6,"$$aim77neg^%YDBAIMTEST()",,g,.recsub)
	do UNXREFDATA^%YDBAIM(aimgbl)
	quit
	;
speednproc(sav)	; [tspeeddata4solo, t4speeddata4solo] Put ydb_aim_nproc back as it was
	; Restoring rather than unsetting: the suite may be run with a process count of
	; its own, and a speed test must not be what changes it for everything after.
	if $zlength(sav) view "setenv":"ydb_aim_nproc":sav
	else  view "unsetenv":"ydb_aim_nproc"
	quit
	;
tspeedtrig1	; @TEST Triggers: 3E5 SET/ZKILL/SET of the value node, 1E5 rows of ^trgspd(:)="A|B"
	; The trigger counterpart of tspeeddata1: the time is the triggers', not the scan's.
	; A private global of 1E5 rows is built, indexed, and then every row's value node
	; is SET to a new value, ZKILLed, and SET back, so each of the value node's SET and
	; ZKILL bodies runs 1E5 times and the ZKILL body sees a row that survives. The
	; index is built first so that the build is a small part of the time, and the
	; global is the test's own, so it is left as it was found: absent.
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"trgspd"
	kill @g
	new i for i=1:1:1E5 set @g@(i)="A|B"
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,1,"|",2,,,,,aimtype)
	for i=1:1:1E5 set @g@(i)="C|D"
	for i=1:1:1E5 zkill @g@(i)
	for i=1:1:1E5 set @g@(i)="A|B"
	do assert($data(@aimgbl@(2,"B",1E5)),"the last row is indexed under its value again")
	do assert('$data(@aimgbl@(2,"D",1E5)),"...and not under the value it had in between")
	do UNXREFDATA^%YDBAIM(aimgbl)
	kill @g
	quit
	;
t4speedtrig1	; @TEST type=4 Triggers: 3E5 SET/ZKILL/SET of the value node, 1E5 rows of ^trgspd(:)="A|B"
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	; tspeedtrig1 with a record specification: the value node is the row, so the
	; bodies test the node itself and walk nothing.
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"trgspd"
	kill @g
	new i for i=1:1:1E5 set @g@(i)="A|B"
	new recsub set recsub(1)=":"		; the value node IS the row
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,1,"|",2,,,,,4,,,g,.recsub)
	for i=1:1:1E5 set @g@(i)="C|D"
	for i=1:1:1E5 zkill @g@(i)
	for i=1:1:1E5 set @g@(i)="A|B"
	do assert($data(@aimgbl@(2,"B",1E5)),"the last row is indexed under its value again")
	do assert('$data(@aimgbl@(2,"D",1E5)),"...and not under the value it had in between")
	do UNXREFDATA^%YDBAIM(aimgbl)
	kill @g
	quit
	;
tspeedtrig4	; @TEST Triggers: 6E5 writes to 1E5 rows of ^trgspd4(50.6,ien,"VUID") and their records
	; The trigger counterpart of tspeeddata4: the Octo shape, the column one level below
	; the row, with a sibling node ^(ien,0) that keeps the row alive when the column is
	; ZKILLed. Six writes per row: the column SET to a new value, ZKILLed and SET back,
	; then the record KILLed and made again, its 0 node first and the column after. For
	; type=0 the KILL fires the tree's KILL trigger and the 0 node fires nothing.
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"trgspd4"
	kill @g
	new i for i=1:1:1E5 set @g@(50.6,i,0)="name"_i,@g@(50.6,i,"VUID")="v"_i_"^1"
	new subs set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,,,,,aimtype)
	for i=1:1:1E5 set @g@(50.6,i,"VUID")="w"_i_"^1"
	for i=1:1:1E5 zkill @g@(50.6,i,"VUID")
	for i=1:1:1E5 set @g@(50.6,i,"VUID")="v"_i_"^1"
	for i=1:1:1E5 kill @g@(50.6,i)
	for i=1:1:1E5 set @g@(50.6,i,0)="name"_i
	for i=1:1:1E5 set @g@(50.6,i,"VUID")="v"_i_"^1"
	do assert($data(@aimgbl@(1,"v"_1E5,1E5)),"the last row is indexed under its value again")
	do assert('$data(@aimgbl@(1,"w"_1E5,1E5)),"...and not under the value it had in between")
	do UNXREFDATA^%YDBAIM(aimgbl)
	kill @g
	quit
	;
t4speedtrig4	; @TEST type=4 Triggers: 6E5 writes to 1E5 rows of ^trgspd4(50.6,ien,"VUID") and their records
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	; tspeedtrig4 with the record specification Octo generates, depth 3. This is the
	; shape whose bodies walk: the column's ZKILL and the record side's ZKILL ask
	; $QUERY whether a node with a value survives under ^(50.6,ien), and the 0 node is
	; what they find. The record's KILL fires the record side's KILL trigger, and the 0
	; node's SET fires the record side's SET trigger, which indexes the row as NULL
	; until the column's SET moves it.
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"trgspd4"
	kill @g
	new i for i=1:1:1E5 set @g@(50.6,i,0)="name"_i,@g@(50.6,i,"VUID")="v"_i_"^1"
	new subs   set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new recsub set recsub(1)=50.6,recsub(2)=":",recsub=3	; depth 3: level 3 varies and is watched, not a key column
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,,,,,4,,,g,.recsub)
	for i=1:1:1E5 set @g@(50.6,i,"VUID")="w"_i_"^1"
	for i=1:1:1E5 zkill @g@(50.6,i,"VUID")
	for i=1:1:1E5 set @g@(50.6,i,"VUID")="v"_i_"^1"
	for i=1:1:1E5 kill @g@(50.6,i)
	for i=1:1:1E5 set @g@(50.6,i,0)="name"_i
	for i=1:1:1E5 set @g@(50.6,i,"VUID")="v"_i_"^1"
	do assert($data(@aimgbl@(1,"v"_1E5,1E5)),"the last record is indexed under its value again")
	do assert('$data(@aimgbl@(1,"w"_1E5,1E5)),"...and not under the value it had in between")
	do assert('$data(@aimgbl@(1,"",1E5)),"...nor under the empty string, which it was while its record was being made")
	do UNXREFDATA^%YDBAIM(aimgbl)
	kill @g
	quit
	;
tspeedsubs1 ; @TEST Subs Index 1E6 rows of ^names(:)="A|B"
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"names"
	new aimgbl set aimgbl=$$XREFSUB^%YDBAIM(g,1,1)
	quit
	;
tspeedsubs2 ; @TEST Subs Index 1E6 rows of ^names3("fix",:,0)="A|B"
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"names3"
	new subs set subs(1)="""fix""",subs(2)=":",subs(3)=0
	new aimgbl set aimgbl=$$XREFSUB^%YDBAIM(g,.subs,1+$random(3))
	quit
	;
tspeedsubs3 ; @TEST Subs Index 1E6 rows of ^composite(1,2,3,4,5,6,7,:)="A|B"
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"composite"
	new aimgbl set aimgbl=$$XREFSUB^%YDBAIM(g,8,1+$random(8))
	quit
	;
tspeedsubs3a ; @TEST Multiple Subs Index 1E6 rows of ^composite(1,2,3,4,5,6,7,:)="A|B"
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"composite"
	new aimgbl set aimgbl=$$XREFSUB^%YDBAIM(g,8,"8;;6;1:3")
	quit
	;
tspeedsubs4 ; @TEST Subs Index ~1E6 dispersed rows in ^PSNDF(50.6,:,"VUID")
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"PSNDF"
	new subs set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new aimgbl set aimgbl=$$XREFSUB^%YDBAIM(g,.subs,"1;3;2")
	quit
	;
tcon1	; @TEST Concurrent do/undo of the same global
	; First lock prevents the "horses" from running until it is open
	lock +racegate
	job tcon1job:pass
	new job1 set job1=$zjob
	job tcon1job:pass
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
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"names"
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,1,"|",2,1,,,,aimtype) ; nmonly
	do assert('$data(@aimgbl),"Shouldn't exist as the last operation should have been an unxref")
	;
	; Test XREFSUB()
	lock +racegate
	job tcon1subjob
	set job1=$zjob
	job tcon1subjob
	set job2=$zjob
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
	set aimgbl=$$XREFSUB^%YDBAIM(g,1,1,1) ; nmonly
	do assert('$data(@aimgbl),"Shouldn't exist as the last operation should have been an unxref")
	quit
	;
tcon1job ; [Job target] Concurrent do/undo of the same global
	new aimgbl
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"names"
	new $etrap set $etrap="goto tcon1jobet^"_$text(+0)
	lock +racegate($J)
	new i for i=1:1:10 do
	. set aimgbl=$$XREFDATA^%YDBAIM(g,1,"|",2,,,,,aimtype)
	. new % set %=$random(2)
	. if % do
	.. do UNXREFDATA^%YDBAIM(g,1,"|",2,,,,,aimtype)
	. else  do
	.. do UNXREFDATA^%YDBAIM(aimgbl)
	lock -racegate($J)
	quit
	;
tcon1subjob ; [Job target] Concurrent do/undo of the same global
	new aimgbl
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"names"
	new $etrap set $etrap="goto tcon1jobet^"_$text(+0)
	lock +racegate($J)
	new i for i=1:1:10 do
	. set aimgbl=$$XREFSUB^%YDBAIM(g,1,1)
	. new % set %=$random(2)
	. if % do
	.. do UNXREFSUB^%YDBAIM(g,1,1)
	. else  do
	.. do UNXREFSUB^%YDBAIM(aimgbl)
	lock -racegate($J)
	quit
	;
tcon1jobet ; [Error trap] Concurrent do/undo of the same global
	if $zjobexam("tcon1jobet."_$job_".jobexam")
	halt
	;
tcon2	; @TEST Concurrent xref of different globals
	; First lock prevents the "horses" from running until it is open
	new gnames set gnames="^"_$select(xdref:xdrefloc,1:"")_"names"
	new gord set gord="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	lock +racegate
	job tcon2job1:pass
	new job1 set job1=$zjob
	job tcon2job2:pass
	new job2 set job2=$zjob
	; Now jobs can run
	lock -racegate
	; Wait till they are done.
	for  quit:'$zgetjpi(job1,"ISPROCALIVE")  hang 0.001
	for  quit:'$zgetjpi(job2,"ISPROCALIVE")  hang 0.001
	;
	new job1aim set job1aim=$$XREFDATA^%YDBAIM(gnames,1,"|",1,1,,,,aimtype)
	new subs set subs(1)=100.01,subs(2)=":",subs(3)=0
	new job2aim set job2aim=$$XREFDATA^%YDBAIM(gord,.subs,"^",2,1,,,,aimtype)
	do assert($data(@job1aim))
	do assert($data(@job2aim))
	;
	; Test XREFSUB()
	lock +racegate
	job tcon2job1(1):pass
	set job1=$zjob
	job tcon2job2(1):pass
	set job2=$zjob
	; Now jobs can run
	lock -racegate
	; Wait till they are done.
	for  quit:'($zgetjpi(job1,"ISPROCALIVE")!$zgetjpi(job2,"ISPROCALIVE"))  hang 0.001
	;
	set job1aim=$$XREFSUB^%YDBAIM(gnames,1,1)
	set job2aim=$$XREFSUB^%YDBAIM(gord,.subs,1)
	do assert($data(@job1aim))
	do assert($data(@job2aim))
	quit
	;
tcon2job1(type) ; [Job target] Concurrent xref of different globals
	lock +racegate($J)
	if +$get(type) do XREFSUB^%YDBAIM(gnames,1,1)
	else  if $$XREFDATA^%YDBAIM(gnames,1,"|",1,,,,,aimtype)
	lock -racegate($J)
	quit
	;
tcon2job2(type) ; [Job target] Concurrent xref of different globals
	lock +racegate($J)
	new subs set subs(1)=100.01,subs(2)=":",subs(3)=0
	if +$get(type) do XREFSUB^%YDBAIM(gord,.subs,1)
	else  if $$XREFDATA^%YDBAIM(gord,.subs,"^",2,,,,,aimtype)
	lock -racegate($J)
	quit
	;
	;
tcon3	; @TEST Concurrent unxref of different globals
	new gnames set gnames="^"_$select(xdref:xdrefloc,1:"")_"names"
	new gord set gord="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new aimgbl1 set aimgbl1=$$XREFDATA^%YDBAIM(gnames,1,"|",1,,,,,aimtype)
	new subs set subs(1)=100.01,subs(2)=":",subs(3)=0
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM(gord,.subs,"^",2,,,,,aimtype)
	;
	do assert($data(@aimgbl1))
	do assert($data(@aimgbl2))
	;
	lock +racegate
	job tcon3job1:pass
	new job1 set job1=$zjob
	job tcon3job2:pass
	new job2 set job2=$zjob
	; Now jobs can run
	lock -racegate
	; Wait till they are done.
	for  quit:'$zgetjpi(job1,"ISPROCALIVE")  hang 0.001
	for  quit:'$zgetjpi(job2,"ISPROCALIVE")  hang 0.001
	;
	do assert('$data(@aimgbl1))
	do assert('$data(@aimgbl2))
	;
	set aimgbl1=$$XREFSUB^%YDBAIM(gnames,1,1)
	set aimgbl2=$$XREFSUB^%YDBAIM(gord,.subs,"1:3")
	;
	do assert($data(@aimgbl1))
	do assert($data(@aimgbl2))
	;
	; Test XREFSUB()
	lock +racegate
	job tcon3job1(1):pass
	set job1=$zjob
	job tcon3job2(1):pass
	set job2=$zjob
	; Now jobs can run
	lock -racegate
	; Wait till they are done.
	for  quit:'($zgetjpi(job1,"ISPROCALIVE")!$zgetjpi(job2,"ISPROCALIVE"))  hang 0.001
	;
	do assert('$data(@aimgbl1))
	do assert('$data(@aimgbl2))
	quit
	;
tcon3job1(type) ; [Job target] Concurrent unxref of different globals
	lock +racegate($J)
	if +$get(type) do UNXREFSUB^%YDBAIM(gnames,1,1)
	else  do UNXREFDATA^%YDBAIM(gnames,1,"|",1,,,,,aimtype)
	lock -racegate($J)
	quit
	;
tcon3job2(type) ; [Job target] Concurrent unxref of different globals
	lock +racegate($J)
	new subs set subs(1)=100.01,subs(2)=":",subs(3)=0
	if +$get(type) do UNXREFSUB^%YDBAIM(gord,.subs,"1:3")
	else  do UNXREFDATA^%YDBAIM(gord,.subs,"^",2,,,,,aimtype)
	lock -racegate($J)
	quit
	;
tcon4	; @TEST Concurrent xref of the same global
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"names"
	lock +racegate
	job tcon4job:pass
	new job1 set job1=$zjob
	job tcon4job:pass
	new job2 set job2=$zjob
	; Now jobs can run
	lock -racegate
	; Wait till they are done.
	for  quit:'$zgetjpi(job1,"ISPROCALIVE")  hang 0.001
	for  quit:'$zgetjpi(job2,"ISPROCALIVE")  hang 0.001
	;
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,1,"|",1,1,,,,aimtype)
	;
	do assert($data(@aimgbl@(3)),"AIM says we completed")
	;
	; verify that all nodes in names are in aimgbl and vice versa
	; ^names -> aimgbl
	new i,v set i=""
	for  set i=$order(@g@(i)) quit:i=""  do
	. set v=$piece(@g@(i),"|",1)
	. do assert($data(@aimgbl@(1,v,i)),@g@(i)_" does not match "_$name(@aimgbl@(1,v,i)))
	;
	; aimgbl -> ^names
	kill i,v
	set (i,v)=""
	for  set v=$order(@aimgbl@(1,v)) quit:v=""  for  set i=$order(@aimgbl@(1,v,i)) quit:i=""  do
	. do assert($piece(@g@(i),"|",1)=v,$name(@aimgbl@(1,v,i))_" does not match "_$name(@g@(i))_"="_@g@(i))
	;
	; Verify stats work properly
	; For stats, we should have these nodes (we have 500 A, 250 of each B and C; total of 1000):
	; ^%ydbAIMDXtoXI8zgdGIqUuCoYeqwF6(-1)=3
	; ^%ydbAIMDXtoXI8zgdGIqUuCoYeqwF6(-1,"A")=500000
	; ^%ydbAIMDXtoXI8zgdGIqUuCoYeqwF6(-1,"B")=250000
	; ^%ydbAIMDXtoXI8zgdGIqUuCoYeqwF6(-1,"C")=250000
	do assert(@aimgbl@(-1)=3)
	do assert(@aimgbl@(-1,"A")=500000)
	do assert(@aimgbl@(-1,"B")=250000)
	do assert(@aimgbl@(-1,"C")=250000)
	;
	set @g@(1000001)="A|Q"
	set @g@(1000002)="Z|F"
	;
	do assert(@aimgbl@(-1)=4)
	do assert(@aimgbl@(-1,"A")=500001)
	do assert(@aimgbl@(-1,"B")=250000)
	do assert(@aimgbl@(-1,"C")=250000)
	do assert(@aimgbl@(-1,"Z")=1)
	;
	; @g@(1)="A|B"
	; @g@(2)="C|B"
	kill @g@(1),@g@(2)
	;
	do assert(@aimgbl@(-1)=4)
	do assert(@aimgbl@(-1,"A")=500000)
	do assert(@aimgbl@(-1,"B")=250000)
	do assert(@aimgbl@(-1,"C")=249999)
	do assert(@aimgbl@(-1,"Z")=1)
	;
	do UNXREFDATA^%YDBAIM(aimgbl)
	set @g@(1)="A|B",@g@(2)="C|B"
	kill @g@(1000001),@g@(1000002)
	;
	; Test XREFSUB()
	lock +racegate
	job tcon4job(1):pass
	new job1 set job1=$zjob
	job tcon4job(1):pass
	new job2 set job2=$zjob
	; Now jobs can run
	lock -racegate
	; Wait till they are done.
	for  quit:'($zgetjpi(job1,"ISPROCALIVE")!$zgetjpi(job2,"ISPROCALIVE"))  hang .001
	;
	set aimgbl=$$XREFSUB^%YDBAIM(g,1,1)
	;
	do assert($data(@aimgbl@(0)),"AIM says we completed")
	;
	; verify that all nodes in names are in aimgbl and vice versa
	; ^names -> aimgbl
	new i,v set i=""
	for  set i=$order(@g@(i)) quit:i=""  do
	. do assert($data(@aimgbl@(1,i)),@g@(i)_" does not match "_$name(@aimgbl@(1,i)))
	;
	; aimgbl -> ^names
	kill i,v
	set (i,v)=""
	for  set v=$order(@aimgbl@(1,v)) quit:v=""  do
	. do assert($data(@g@(v)))
	;
	; Verify stats work properly
	; @aimgbl@(-1)=1000000
	; @aimgbl@(1,1 through 1000000) should exist
	; @aimgbl@(-1,1 through 1000000) should exist
	new tmp set tmp=1+$random(1000000) do assert($data(@aimgbl@(1,tmp)))
	do assert($data(@aimgbl@(-1,1+$random(1000000))))
	do assert(@aimgbl@(-1)=1000000)
	;
	set @g@(1000001)="A|Q"
	set @g@(1000002)="Z|F"
	;
	set tmp=1000001+$random(2) do assert($data(@aimgbl@(1,tmp)))
	do assert($data(@aimgbl@(-1,1000001+$random(2))))
	do assert(@aimgbl@(-1)=1000002)
	;
	; @g@(1)="A|B"
	; @g@(2)="C|B"
	kill @g@(1),@g@(2)
	;
	set tmp=3+$random(1000000) do assert($data(@aimgbl@(1,tmp)))
	do assert($data(@aimgbl@(-1,3+$random(1000000))))
	do assert(@aimgbl@(-1)=1000000)
	quit
	;
tcon4job(type) ; [Job target] Concurrent xref of the same global
	; type 0 XREFDATA, 1 XREFSUB.
	; Postconditionals rather than IF/ELSE: XREFDATA()/XREFSUB() contain IF
	; commands, so an ELSE after one of them tests whatever $TEST it left.
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"names"
	lock +racegate($J)
	do:1=+$get(type) XREFSUB^%YDBAIM(g,1,1,,,2)
	if 0=+$get(type),$$XREFDATA^%YDBAIM(g,1,"|",1,"","","",2,aimtype)
	lock -racegate($J)
	quit
	;
tconp	; @TEST concurrent xref of two DIFFERENT pieces of one node
	; tcon4 races two callers on the SAME piece, where the piece bit-map union is a
	; no-op. This races them on different pieces, which is what the union is for.
	;
	; A cross reference is named by its global, subscripts, delimiter, type, force and
	; record specification - NOT by the piece number - so both callers land on one cross
	; reference, and each has to ADD its piece to whatever the other left.
	;
	; The merge is a read of ^(4) followed by a write of it, and it is atomic only
	; while the read sits in the same transaction as the write. The lock XREFDATA()
	; takes is subscripted by $JOB on purpose and does not serialize two callers, so
	; the transaction is the only thing that can. With the read outside it, both
	; callers see an absent bit-map, each writes its own piece alone, and the last
	; commit wins: the bit-map names one piece, the triggers maintain one piece, and
	; the other caller's index entries are left behind with nothing maintaining them.
	;
	; Were the union not atomic, the bit-map would name one piece, the triggers would
	; maintain one piece, and the other caller's index entries would be left with nothing
	; maintaining them. The last assertions make that visible: a record written after the
	; race has to reach both pieces.
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"names"
	;
	; Start from nothing, so both callers race to CREATE the cross reference rather
	; than one of them finding the other's already built.
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,1,"|",1,1,,,,aimtype)
	do:$data(@aimgbl@(0))#10 UNXREFDATA^%YDBAIM(aimgbl)
	;
	lock +racegate
	job tconpjob(1):pass
	new job1 set job1=$zjob
	job tconpjob(2):pass
	new job2 set job2=$zjob
	; Now jobs can run
	lock -racegate
	; Wait till they are done.
	for  quit:'$zgetjpi(job1,"ISPROCALIVE")  hang 0.001
	for  quit:'$zgetjpi(job2,"ISPROCALIVE")  hang 0.001
	;
	; Both pieces are in the bit-map. It is "#" followed by one position per piece,
	; so piece n is position n+1.
	new pstr set pstr=$get(@aimgbl@(4))
	do assert(1=+$zextract(pstr,2),"piece 1 is in the bit-map "_$zwrite(pstr))
	do assert(1=+$zextract(pstr,3),"piece 2 is in the bit-map "_$zwrite(pstr))
	;
	; Both pieces of the records that were already there are indexed. A sample rather
	; than the whole global: ^names holds 1E6 records, and the bit-map assertion above
	; is what detects the race - this only confirms the index agrees with it.
	new i,n set i="" for n=1:1:8 set i=$order(@g@(i)) quit:i=""  do
	. do assert($data(@aimgbl@(1,$zpiece(@g@(i),"|",1),i)),"record "_i_" indexed under piece 1")
	. do assert($data(@aimgbl@(2,$zpiece(@g@(i),"|",2),i)),"record "_i_" indexed under piece 2")
	;
	; A record written AFTER the race has to reach both pieces, which happens only if
	; the surviving trigger trigger code carry both. This is the half a bit-map naming one
	; piece would silently stop maintaining.
	new newid set newid=$order(@g@(""),-1)+1
	set @g@(newid)="Y|Z"
	do assert($data(@aimgbl@(1,"Y",newid)),"record written after the race is indexed under piece 1")
	do assert($data(@aimgbl@(2,"Z",newid)),"record written after the race is indexed under piece 2")
	kill @g@(newid)
	do assert('$data(@aimgbl@(1,"Y",newid)),"and its piece 1 entry goes when it does")
	do assert('$data(@aimgbl@(2,"Z",newid)),"and its piece 2 entry goes when it does")
	;
	do UNXREFDATA^%YDBAIM(aimgbl)
	quit
	;
tconpjob(pnum)	; [Job target] one caller of the tconp race, cross referencing piece pnum
	; Postconditionals rather than IF/ELSE: XREFDATA() contains IF commands, so an
	; ELSE after one of them tests whatever $TEST it left.
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"names"
	lock +racegate($J)
	if $$XREFDATA^%YDBAIM(g,1,"|",pnum,"","","",2,aimtype)
	lock -racegate($J)
	quit
	;
tcon5	; @TEST Test concurrent lsxref
	new $etrap set $etrap="goto tcon5jobet^"_$text(+0)
	new i,job1,job2,xrefs
	job tcon5job1:pass set job1=$zjob
	if xdref do
	. new $zgbldir set $zgbldir=xdrefgld
	. for i=1:1:100 kill xrefs do LSXREFDATA^%YDBAIM(.xrefs) hang $random(100)*.001
	else  do
	. for i=1:1:100 kill xrefs do LSXREFDATA^%YDBAIM(.xrefs) hang $random(100)*.001
	;
	; Test concurrent DATA & SUB
	job tcon5job1(1):pass set job2=$zjob
	if xdref do
	. new $zgbldir set $zgbldir=xdrefgld
	. for i=1:1:100 kill xrefs do LSXREFSUB^%YDBAIM(.xrefs) hang $random(100)*.001
	else  do
	. for i=1:1:100 kill xrefs do LSXREFSUB^%YDBAIM(.xrefs) hang $random(100)*.001
	;
	for  quit:'($zgetjpi(job1,"ISPROCALIVE")!$zgetjpi(job2,"ISPROCALIVE"))  hang .001
	quit
	;
tcon5job1(type) ; [Job Target] Test concurrent lsxref
	new $etrap set $etrap="goto tcon5jobet^"_$text(+0)
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"x"
	kill @g set @g@(1)=""
	new i,xref,unxref
	if +$get(type) for i=1:1:100 do
	. set ref=$$XREFSUB^%YDBAIM(g,1,1)
	. set unxref=$$UNXREFSUB^%YDBAIM(g,1)
	else  for i=1:1:100 do
	. set xref=$$XREFDATA^%YDBAIM(g,1,,,,,,,aimtype)
	. set unxref=$$UNXREFDATA^%YDBAIM(g,1,,,,,,,aimtype)
	quit
	;
tcon5jobet ; [Error trap] Concurrent do/undo of the same global
	if $zjobexam("tcon5jobet."_$job_".jobexam")
	halt
	;
tcon6	; @TEST Test concurrent xref, unxref, and lsxref together
	new gprefix set gprefix="^"_$select(xdref:xdrefloc,1:"")
	set ^stop=0
	set ^njobs=8
	set ^cntr=^njobs
	for i=1:1:^njobs set gbl=gprefix_"x"_i kill @gbl set @gbl@(1)="",@gbl@(2)="abcd"
	; Start ^njobs child processes
	for i=1:1:^njobs do
	. set jobstr="job tcon6job:(output=""tcon6job.mjo"_i_""":error=""tcon6job.mje"_i_""":pass)"
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
	;
	; Test XREFSUB()
	set ^stop=0
	set ^njobs=8
	set ^cntr=^njobs
	for i=1:1:^njobs set gbl=gprefix_"x"_i kill @gbl set @gbl@(1)="",@gbl@(2)="abcd"
	; Start ^njobs child processes
	for i=1:1:^njobs do
	. set jobstr="job tcon6job(1):(output=""tcon6job.mjo"_i_""":error=""tcon6job.mje"_i_""":pass)"
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
tcon6job(type) ; [Job Target] Test concurrent xref, unxref, and lsxref together
	new gprefix set gprefix="^"_$select(xdref:xdrefloc,1:"")
	set $etrap="zwrite $zstatus  set ^stop=1 halt"
	set njobs=^njobs
	if +$get(type) for i=1:1 quit:^stop=1  do
	. for j=1:1:3 do
	.. set gbl=gprefix_"x"_(1+$random(njobs))
	.. set:j=1 xref=$$XREFSUB^%YDBAIM(gbl,1,1)
	.. set:j=2 xref=$$UNXREFSUB^%YDBAIM(gbl,1,1)
	.. if j=3 for k=1:1:8 do
	... if $random(2) do LSXREFSUB^%YDBAIM(.xrefs,gbl)
	... else  do LSXREFSUB^%YDBAIM(.xrefs)
	else  for i=1:1 quit:^stop=1  do
	. for j=1:1:3 do
	.. set gbl=gprefix_"x"_(1+$random(njobs))
	.. set:j=1 xref=$$XREFDATA^%YDBAIM(gbl,1,,,,,,,aimtype)
	.. set:j=2 xref=$$UNXREFDATA^%YDBAIM(gbl,1,,,,,,,aimtype)
	.. if j=3 for k=1:1:8 do
	... if $random(2) do LSXREFDATA^%YDBAIM(.xrefs,gbl)
	... else  do LSXREFDATA^%YDBAIM(.xrefs)
	; Record that this child finished cleanly
	if $increment(^cntr,-1)
	quit
	;
tsanity	; @TEST Main Sanity YDBAIM tester
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ABC"
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
	;
	do init(1)
	do simappstart(12)
	do xrefprocrun(4,1)
	; Verify the names of all the cross-references
	kill xrefgbl
	set eachxref=""
	for  set eachxref=$order(^%ydb674test("xref","aimxref",eachxref)) quit:""=eachxref  do
	. if $data(xrefgbl) do assert(xrefgbl=^%ydb674test("xref","aimxref",eachxref)) if 1
	. else  set xrefgbl=^%ydb674test("xref","aimxref",eachxref)
	; Let the jobs create random data for one second
	hang 1
	; Stop application and check xref
	do simappstop
	do xrefgblchk(1)
	quit
	;
	;
init(type)	new gbl,io,%
	view "jobpid":1,"ztrigger_output":0
	kill ^%ydb674test,@g
	if xdref new $zgbldir set $zgbldir=xdrefgld
	set (gbl,%)=$select(+$get(type):"^%ydbAIMS",1:"^%ydbAIMD")
	for  set gbl=$order(@gbl) quit:gbl'[%  kill @gbl
	if $ztrigger("item","-*")
	view "ztrigger_output":1
	quit
	;
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
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ABC"
	new opt,sub1,sub2,sub3,sub4
	if $increment(^%ydb674test("simapp","start"),-1)		; tell parent this JOB'd process is up
	for i=1:1 lock +^%ydb674test("simapp",$job):0 quit:$test  do	; successful lock acquisition is stop signal from parent
	. set sub1=$random(10),sub2=$random(10)     		  	; random subscripts for next node
	. set opt=$random(10)			      		  	; decide on number of subscripts of next node to update
	. if 0=opt do update(g_"("_sub1_","_sub2_")")			; update 2 subscript nodes 10% of the time
	. else  set sub3=$random(10) if 1=opt do			; update 4 subscript nodes 10% of the time
	.. set sub4=$random(10)
	.. do update(g_"("_sub1_","_sub2_","_sub3_","_sub4_")")
	. else  do update(g_"("_sub1_","_sub2_","_sub3_")")		; update 3 subscript nodes 80% of the time
	. ; hang 1+$random(10)/1E3					; random hang if needed to avoid saturating the CPU
	quit
	;
	; Updating a node. If a node does not exist, create it, using odd numbers for the first and third pieces so that it is easy
	; to determine how many times a node was updated. If a node exists, zkill it 20% of the time (since triggers don't work on trees
	; AIM won't handle kill well at present), and the rest of the time, update the node.
update(node)
	new piece
	tstart ():transactionid="batch"
	if $data(@node)#10 do
	. if $random(5) do						; 80% of the time node will be updated
	.. set piece=$random(2)*2+1					; piece is either 1 or 3
	.. set $zpiece(@node,"|",piece)=0.5*$zpiece(@node,"|",piece)
	.. set $zpiece(@node,"|",2)=$zpiece(@node,"|",2)_$$^%RANDSTR(1,,"AL")
	. else  zkill @node
	else  set @node=$random(1E3)*2+1_"|"_$$^%RANDSTR(1,,"AL")_"|"_($random(1E3)*2+1)
	tcommit
	quit
	;
	; Start simulated application
simappstart(nproc)
	new i
	set ^%ydb674test("simapp","start")=nproc			; number of simapp processes
	; write "Starting ",nproc," simulated application processes",!
	lock +^%ydb674test("simapp")					; get lock that tells simapp processes to run
	for i=1:1:nproc job simapp:pass set ^%ydb674test("simapp","jobs",$zjob)=""
	for  quit:'^%ydb674test("simapp","start")  hang .001		; zero ^%ydb674test("simapp","start") means all simapp processes are up
	quit
	;
	; Stop simulated application
simappstop
	lock -^%ydb674test("simapp")					; releasing lock instructs simapp processes to stop
	; write "Stopping simulated application",!
	; Wait for all children to die
	new zjob set zjob=""
	for  set zjob=$order(^%ydb674test("simapp","jobs",zjob)) quit:'zjob  do
	. for  quit:'$zgetjpi(zjob,"isprocalive")  hang .001
	quit
	;
	; Check complete nodes and cross references for Consistency - a cross reference for each node, and a node for each cross reference
xrefgblchk(type)
	new flag,ref,sub1,sub2,sub3,val
	; Should check whether a $query() loop is faster than the following nested $order() loop
	if +$get(type) do
	. ; write "Checking that a node exists for each cross reference ",!
	. set flag=0,ref=$query(@xrefgbl@(3,"")) for  quit:3'=$qsubscript(ref,1)!'$zlength(ref)  do  set ref=$query(@ref)
	.. set sub1=$qsubscript(ref,3),sub2=$qsubscript(ref,4),sub3=$qsubscript(ref,2)
	.. if '($data(@g@(sub1,sub2,sub3))#10) write !,xrefgbl,"(3,",sub3,",",sub1,",",sub2,",",") exists but ^ABC node does not" set flag=1
	. do assert('flag)
	. ; write "checking that an xref exists for each global node",!
	. set flag=0,sub1="" for  set sub1=$order(@g@(sub1)) quit:""=sub1  do
	.. set sub2="" for  set sub2=$order(@g@(sub1,sub2)) quit:""=sub2  do
	... set sub3="" for  set sub3=$order(@g@(sub1,sub2,sub3)) quit:""=sub3  do:10'=$data(@g@(sub1,sub2,sub3))
	.... if '$data(@xrefgbl@(3,sub3,sub1,sub2)) write !,"^ABC(",sub1,",",sub2,",",sub3,") exists but xref does not" set flag=1
	. do assert('flag)
	else  do
	. ; write "Checking that a node exists for each cross reference ",!
	. set flag=0,val="" for  set val=$order(@xrefgbl@(0,val)) quit:""=val  do
	.. set sub1="" for  set sub1=$order(@xrefgbl@(0,val,sub1)) quit:""=sub1  do
	... set sub2="" for  set sub2=$order(@xrefgbl@(0,val,sub1,sub2)) quit:""=sub2  do
	.... set sub3="" for  set sub3=$order(@xrefgbl@(0,val,sub1,sub2,sub3)) quit:""=sub3  do
	..... if '$data(@g@(sub1,sub2,sub3))#10 write !,xrefgbl,"(",val,",",sub1,",",sub2,",",sub3,") exists but ^ABC node does not" set flag=1
	..... else  if val'=@g@(sub1,sub2,sub3) write !,"^ABC(",sub1,",",sub2,",",sub3,")=",@g@(sub1,sub2,sub3)," but xref is ",val set flag=1
	. do assert('flag)
	. ; write "Checking that an xref exists for each global node ",!
	. set flag=0,sub1=""  for  set sub1=$order(@g@(sub1)) quit:""=sub1  do:$data(@g@(sub1))\10
	.. set sub2="" for  set sub2=$order(@g@(sub1,sub2)) quit:""=sub2  do:$data(@g@(sub1,sub2))\10
	... set sub3="" for  set sub3=$order(@g@(sub1,sub2,sub3)) quit:""=sub3  do:$data(@g@(sub1,sub2,sub3))#10
	.... set val=@g@(sub1,sub2,sub3)
	.... if '$data(@xrefgbl@(0,val,sub1,sub2,sub3)) write !,"^ABC(",sub1,",",sub2,",",sub3,")=",val," has no xref" set flag=1
	. do assert('flag)
	;
	quit
	;
	; Run multiple parallel xref processes, to show parallel processes can concurrently cross reference
xrefprocrun(nproc,type)
	new i
	set ^%ydb674test("xref","start")=nproc				; number of xref processes
	lock ^%ydb674test("xref","start")
	; write "Starting ",nproc," concurrent xrefs",!
	for i=1:1:nproc job xrefproc(+$get(type)):pass set ^%ydb674test("xref","jobs",$zjob)=""
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
	;
xrefproc(type)
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ABC"
	lock +^%ydb674test("xref","stop",$job)
	if $increment(^%ydb674test("xref","start"),-1)
	lock +^%ydb674test("xref","start",$job)				; Gate to ensure all xref processes are up and running
	lock -^%ydb674test("xref","start",$job)
	set ^%ydb674test("xref","aimxref",$job)=$select(+$get(type):$$XREFSUB^%YDBAIM(g,3,3),1:$$XREFDATA^%YDBAIM(g,3,,,,,,,aimtype))
	lock -^%ydb674test("xref","stop",$job)
	quit
	;
tbash	; @TEST Run bash tests through run_bash_tests.sh/ydbaim_test.sh
	zsystem "$script_dir/run_bash_tests.sh "_$ztrnlnm("ydb_dist")
	do assert(0=$zsystem)
	quit
	;
gvsuboflow(level)	;	Used by tbash
	if level do gvsuboflowhelper quit  ; If level=1, test $ZLEVEL=1 by adding one more DO frame before calling gvsuboflowhelper.
gvsuboflowhelper:	;	Test is meaningful only if key size is less than maximum (i.e., YottaDB defaults pre 2.02)
	if 1019<$$^%PEEKBYNAME("sgmnt_data.max_key_size","YDBAIM") do
	. do UNXREFDATA^%YDBAIM
	. kill ^x
	. for i=1:1 set ^x(i)=$j(2**i,2**i)
	. set $etrap="zwrite $zstatus set $ecode="""" zhalt +$zstatus"
	. set aimglobal=$$XREFDATA^%YDBAIM("^x",1,,,,,,,aimtype)
	. zwrite aimglobal	; this line should not be reached as GVSUBOFLOW error should transfer control in previous line
	else  do	;	else fake expected error message
	. write "Error occurred: 150372986,xrefdata+122^%YDBAIM,%YDB-E-GVSUBOFLOW, Maximum combined length of subscripts exceeded,%YDB-I-GVIS, ",$c(9,9),"Global variable: ^%ydbAIMDgFr8HZY2gJsda6acj41uCE(0*",!
	. write "aimglobal=""^%ydbAIMDgFr8HZY2gJsda6acj41uCE""",!
	quit
	;
badinvocation	; Used by tbash
	set $etrap="zwrite $zstatus quit"
	for label="","LSXREFDATA","UNXREFDATA","XREFDATA" do
	. set entryref=label_"^%YDBAIM"
	. for tp=0,1 do
	.. write "# Testing "_entryref_" with tp=",tp,!
	.. do badinvocationhelper(entryref,tp)
	quit
	;
badinvocationhelper(entryref,tp)	;
	new $etrap
	set level=$zlevel
	set $etrap="zwrite $zstatus set $ecode="""" trollback:tp  zgoto level-1"
	tstart:tp ():serial
	do @entryref
	trollback:tp
	quit
	;
ttype1	; @TEST test type1 application global variables
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"USPresidents"
	new fstr,i,j,nullxref,s,x,y,z
	for fstr=0,1 do
	. set nullxref=$select(fstr:"#",1:"")
	. if xdref do
	.. new $zgbldir set $zgbldir=xdrefgld
	.. do UNXREFDATA^%YDBAIM
	. else  do UNXREFDATA^%YDBAIM
	. kill @g
	. do assert(46=$$LOADDATA("USPresidents"))
	. do assert(3=$$LOADDATA("NotUSPresidents"))
	. set s(1)=":1900",s(2)=1841
	. ; Entire node: test initial data load & triggers for no statistics
	. zkill @g@(1837,1841)
	. set x=$$XREFDATA^%YDBAIM(g,.s,,,,,,,1,fstr)
	. do assert(x'=$$XREFDATA^%YDBAIM(g,.s,,,1,,,,aimtype,fstr)),assert(x'=$$XREFDATA^%YDBAIM(g,.s,,,1,,,,1,'fstr))
	. set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i),assert(46=$$LOADDATA("USPresidents"))
	. set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(22=i)
	. set y=nullxref for i=1:1 set y=$order(@x@(0,y)) quit:""=y
	. do assert(3=i)
	. zkill @g@(1837,1841) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i)
	. set y=nullxref for i=1:1 set y=$order(@x@(0,y)) quit:""=y
	. do assert(2=i)
	. zkill @g@(1837,-42) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i)
	. zkill @g@(1837,42) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(22=i)
	. do assert(3=$$LOADDATA("NotUSPresidents")) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i)
	. do assert(46=$$LOADDATA("USPresidents")) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(22=i)
	. set y=nullxref for i=1:1 set y=$order(@x@(0,y)) quit:""=y
	. do assert(3=i)
	. if xdref do
	.. new $zgbldir set $zgbldir=xdrefgld
	.. do UNXREFDATA^%YDBAIM
	. else  do UNXREFDATA^%YDBAIM
	. ; Entire node: test initial data load & triggers for stat=1
	. zkill @g@(1837,1841)
	. set x=$$XREFDATA^%YDBAIM(g,.s,,,,,,1,1,fstr)
	. set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i),assert(46=$$LOADDATA("USPresidents"))
	. set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(22=i)
	. set y=nullxref for i=1:1 set y=$order(@x@(0,y)) quit:""=y
	. do assert(3=i),assert(21=@x@("",nullxref)),assert(1=@x@("",nullxref_"Martin|Van|Buren")),assert(1=@x@("",nullxref_"William|Henry|Harrison"))
	. zkill @g@(1837,1841) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@x@("",nullxref)),assert('$data(@x@("",nullxref_"Martin|Van|Buren"))),assert(@x@("",nullxref_"William|Henry|Harrison"))
	. set y=nullxref for i=1:1 set y=$order(@x@(0,y)) quit:""=y
	. do assert(2=i)
	. zkill @g@(1837,-42) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@x@("",nullxref))
	. zkill @g@(1837,42) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(22=i),assert(21=@x@("",nullxref))
	. do assert(3=$$LOADDATA("NotUSPresidents")) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@x@("",nullxref))
	. do assert(46=$$LOADDATA("USPresidents")) set y=nullxref for i=1:1 set y=$order(@x@(0,y)) quit:""=y
	. do assert(3=i),assert(21=@x@("",nullxref)),assert(1=@x@("",nullxref_"Martin|Van|Buren")),assert(1=@x@("",nullxref_"William|Henry|Harrison"))
	. if xdref do
	.. new $zgbldir set $zgbldir=xdrefgld
	.. do UNXREFDATA^%YDBAIM
	. else  do UNXREFDATA^%YDBAIM
	. ; Entire node: test initial data load & triggers for stat=2
	. zkill @g@(1837,1841)
	. set x=$$XREFDATA^%YDBAIM(g,.s,,,,,,2,1,fstr)
	. set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i),assert(46=$$LOADDATA("USPresidents"))
	. set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(22=i),assert(3=@x@(""))
	. set y=nullxref for i=1:1 set y=$order(@x@(0,y)) quit:""=y
	. do assert(3=i),assert(21=@x@("",nullxref)),assert(1=@x@("",nullxref_"Martin|Van|Buren")),assert(1=@x@("",nullxref_"William|Henry|Harrison"))
	. zkill @g@(1837,1841) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@x@("",nullxref)),assert('$data(@x@("",nullxref_"Martin|Van|Buren"))),assert(@x@("",nullxref_"William|Henry|Harrison"))
	. set y=nullxref for i=1:1 set y=$order(@x@(0,y)) quit:""=y
	. do assert(2=i),assert(2=@x@(""))
	. zkill @g@(1837,-42) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@x@("",nullxref)),assert(2=@x@(""))
	. zkill @g@(1837,42) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(22=i),assert(21=@x@("",nullxref)),assert(2=@x@(""))
	. do assert(3=$$LOADDATA("NotUSPresidents")) set y="" for i=1:1 set y=$order(@x@(0,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@x@("",nullxref)),assert(2=@x@(""))
	. do assert(46=$$LOADDATA("USPresidents")) set y=nullxref for i=1:1 set y=$order(@x@(0,y)) quit:""=y
	. do assert(3=i)
	. do assert(21=@x@("",nullxref)),assert(1=@x@("",nullxref_"Martin|Van|Buren")),assert(1=@x@("",nullxref_"William|Henry|Harrison"))
	. if xdref do
	.. new $zgbldir set $zgbldir=xdrefgld
	.. do UNXREFDATA^%YDBAIM
	. else  do UNXREFDATA^%YDBAIM
	. ; Pieces
	. zkill @g@(1837,1841)
	. set z=$$XREFDATA^%YDBAIM(g,.s,"|",3,,,,,1,fstr)
	. do assert(z'=x),assert(z'=$$XREFDATA^%YDBAIM(g,.s,"|",1,1,,,,fstr))
	. set y="" for i=1:1 set y=$order(@z@(3,nullxref,y)) quit:""=y
	. do assert(23=i),assert(46=$$LOADDATA("USPresidents"))
	. set y="" for i=1:1 set y=$order(@z@(3,nullxref,y)) quit:""=y
	. do assert(22=i)
	. set y=nullxref for i=1:1 set y=$order(@z@(3,y)) quit:""=y
	. do assert(3=i)
	. zkill @g@(1837,1841) set y="" for i=1:1 set y=$order(@z@(3,nullxref,y)) quit:""=y
	. do assert(23=i)
	. set y=nullxref for i=1:1 set y=$order(@z@(3,y)) quit:""=y
	. do assert(2=i)
	. zkill @g@(1837,-42) set y="" for i=1:1 set y=$order(@z@(3,nullxref,y)) quit:""=y
	. do assert(23=i)
	. zkill @g@(1837,42) set y="" for i=1:1 set y=$order(@z@(3,nullxref,y)) quit:""=y
	. do assert(22=i)
	. do assert(3=$$LOADDATA("NotUSPresidents")) set y="" for i=1:1 set y=$order(@z@(3,nullxref,y)) quit:""=y
	. do assert(23=i)
	. do assert(46=$$LOADDATA("USPresidents")) set y="" for i=1:1 set y=$order(@z@(3,nullxref,y)) quit:""=y
	. do assert(22=i)
	. set y=nullxref for i=1:1 set y=$order(@z@(3,y)) quit:""=y
	. do assert(3=i)
	. if xdref do
	.. new $zgbldir set $zgbldir=xdrefgld
	.. do UNXREFDATA^%YDBAIM
	. else  do UNXREFDATA^%YDBAIM
	. ; Pieces: test initial data load & triggers for stat=1
	. zkill @g@(1837,1841)
	. set z=$$XREFDATA^%YDBAIM(g,.s,"|","1;3",,,,1,1,fstr)
	. set j=2*$random(2)+1,y="" for i=1:1 set y=$order(@z@(j,nullxref,y)) quit:""=y
	. do assert(23=i),assert(46=$$LOADDATA("USPresidents"))
	. set j=2*$random(2)+1,y="" for i=1:1 set y=$order(@z@(j,nullxref,y)) quit:""=y
	. do assert(22=i)
	. set j=2*$random(2)+1,y=nullxref for i=1:1 set y=$order(@z@(j,y)) quit:""=y
	. do assert(3=i),assert(21=@z@(-2*$random(2)-1,nullxref)),assert(1=@z@(-1,nullxref_"Martin")),assert(1=@z@(-3,nullxref_"Buren")),assert(1=@z@(-1,nullxref_"William")),assert(1=@z@(-3,nullxref_"Harrison"))
	. zkill @g@(1837,1841) set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@z@(-2*$random(2)-1,nullxref)),assert('$data(@z@(-1,nullxref_"Martin"))),assert('$data(@z@(-3,nullxref_"Buren"))),assert(1=@z@(-1,nullxref_"William")),assert(1=@z@(-3,nullxref_"Harrison"))
	. set y=nullxref for i=1:1 set y=$order(@z@(1,y)) quit:""=y
	. do assert(2=i)
	. zkill @g@(1837,-42) set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@z@(-2*$random(2)-1,nullxref))
	. zkill @g@(1837,42) set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(22=i),assert(21=@z@(-2*$random(2)-1,nullxref))
	. do assert(3=$$LOADDATA("NotUSPresidents")) set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@z@(-2*$random(2)-1,nullxref))
	. do assert(46=$$LOADDATA("USPresidents")) set y=nullxref for i=1:1 set y=$order(@z@(1,y)) quit:""=y
	. do assert(3=i),assert(21=@z@(-2*$random(2)-1,nullxref))
	. do assert(1=@z@(-1,nullxref_"Martin")),assert(1=@z@(-3,nullxref_"Buren")),assert(1=@z@(-1,nullxref_"William")),assert(1=@z@(-3,nullxref_"Harrison"))
	. if xdref do
	.. new $zgbldir set $zgbldir=xdrefgld
	.. do UNXREFDATA^%YDBAIM
	. else  do UNXREFDATA^%YDBAIM
	. ; Pieces: test initial data load & triggers for stat=2
	. zkill @g@(1837,1841)
	. set z=$$XREFDATA^%YDBAIM(g,.s,"|","1:3",,,,2,1,fstr)
	. set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(23=i),assert(2=@z@(-2)),assert(46=$$LOADDATA("USPresidents")) quit
	. set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(22=i),assert(3=@z@(-2))
	. set y=nullxref for i=1:1 set y=$order(@z@(1,y)) quit:""=y
	. do assert(3=i),assert(21=@z@(-2*$random(2)-1,nullxref)),assert(1=@z@(-1,nullxref_"Martin")),assert(1=@z@(-3,nullxref_"Buren")),assert(1=@z@(-1,nullxref_"William")),assert(1=@z@(-3,nullxref_"Harrison"))
	. zkill @g@(1837,1841) set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@z@(-2*$random(2)-1,nullxref)),assert('$data(@z@(-1,nullxref_"Martin"))),assert('$data(@z@(-2,nullxref_"Van"))),assert('$data(@z@(-3,nullxref_"Buren"))),assert(1=@z@(-1,nullxref_"William")),assert(1=@z@(-3,nullxref_"Harrison"))
	. set y=nullxref for i=1:1 set y=$order(@z@(1,y)) quit:""=y
	. do assert(2=i),assert(22=@z@(-2*$random(2)-1,nullxref))
	. zkill @g@(1837,-42) set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@z@(-2*$random(2)-1,nullxref)),assert(2=@z@(-2))
	. zkill @g@(1837,42) set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(22=i),assert(21=@z@(-2*$random(2)-1,nullxref)),assert(2=@z@(-1))
	. do assert(3=$$LOADDATA("NotUSPresidents")) set y="" for i=1:1 set y=$order(@z@(1,nullxref,y)) quit:""=y
	. do assert(23=i),assert(22=@z@(-2*$random(2)-1,"")),assert(2=@z@(-2))
	. do assert(46=$$LOADDATA("USPresidents")) set y=nullxref for i=1:1 set y=$order(@z@(1,y)) quit:""=y
	. do assert(3=i),assert(21=@z@(-2*$random(2)-1,nullxref)),assert(1=@z@(-1,nullxref_"Martin")),assert(1=@z@(-3,nullxref_"Buren")),assert(1=@z@(-1,nullxref_"William")),assert(1=@z@(-3,nullxref_"Harrison"))
	. if xdref do
	.. new $zgbldir set $zgbldir=xdrefgld
	.. do UNXREFDATA^%YDBAIM
	. else  do UNXREFDATA^%YDBAIM
	quit
	;
; If you update this list of US Presidents to make it current, you must also
; update various statistics used to test for correctness. It's best to leave
; this reference dataset unmodified.
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
	;
NotUSPresidents	; fake data to test type 1 subscripts
	;^USPresidents(1837,42)="Zaphod||Beeblebrox"
	;^USPresidents(1837,-42)="Ford||Prefect"
	;
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
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new fstr,nullxref,stat for fstr=0,1 set nullxref=$select(fstr:"#",1:"") for stat=0:1:2 do
	. ; write !,"stat: ",stat	; Uncomment for debugging
	. new subs set subs(1)=100.01,subs(2)=":"" """,subs(3)=.1
	. ;
	. ; gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type
	. ; Get AIM global with name only
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,1,0,1,stat,1,fstr)
	. ; Unxref the data
	. do UNXREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,1,fstr)
	. ; Assert that the data doesn't exist (first time (stat=0) it won't, so the previous UNXREF will be a no-op)
	. do assert('$data(@aimgbl))
	. ; Xref data now
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,1,fstr)
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
	. ;
	. ; Add data and make sure triggers works
	. ; Now set one of the .1 nodes to be non-existent on a new entry
	. set @g@(100.01,100,0)="JUNK STATUS^junk"
	. ; There should be now 18 entries, but 2 null entries
	. do assert($$type1cd(aimgbl)=18)
	. do assert($$type1cn(aimgbl)=2)
	. do assert($data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref)=2)
	.. ; Another extra entry to test statistics
	.. set @g@(100.01,101,0)="JUNK STATUS2^junk2"
	.. do assert(@aimgbl@(-1,nullxref)=3)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	.. kill @g@(100.01,101,0)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	.. do assert(@aimgbl@(-1,nullxref)=2)
	. ;
	. ; Set the .1 node to some data on entry 100
	. set @g@(100.01,100,.1)="ju"
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
	. kill @g@(100.01,100)
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert('$data(@aimgbl@(1,nullxref_"ju",100)))
	. do assert('$data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"ju")))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	. ;
	. ; Kill the parent entry node by node
	. new %1,%2 set (%1,%2)=$name(@g@(100.01,1))
	. new keepme merge keepme=@%2
	. do assert($data(@g@(100.01,1)))
	. for  set %1=$query(@%1) quit:$name(@%2,$qlength(%2))'=$name(@%1,$qlength(%2))  kill @%1
	. do assert('$data(@g@(100.01,1)))
	. do assert('$data(@aimgbl@(1,nullxref_"dc",1)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"dc")))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	. merge @%2=keepme
	. do assert($data(@g@(100.01,1)))
	. do assert($data(@aimgbl@(1,nullxref_"dc",1)))
	. if stat=2 do
	.. do assert(@aimgbl@(-1)=17) ; Distinct entries
	. ;
	. ; Kill parent entry all at once
	. new keepme merge keepme=@g@(100.01,1)
	. kill @g@(100.01,1)
	. do assert('$data(@g@(100.01,1)))
	. do assert('$data(@aimgbl@(1,nullxref_"dc")))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"dc")))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	. merge @g@(100.01,1)=keepme
	. do assert($data(@g@(100.01,1)))
	. do assert($data(@aimgbl@(1,nullxref_"dc")))
	. if stat=2 do
	.. do assert(@aimgbl@(-1)=17) ; Distinct entries
	. ;
	. ; Remove all data in ^ORD(101.01)
	. new keepme merge keepme=@g@(100.01)
	. kill @g@(100.01)
	. do assert($data(@aimgbl@(1))=1)
	. if stat do
	.. do assert($data(@aimgbl@(-1))[0)
	.. if stat=2 do
	... do assert('$data(@aimgbl@(-1))) ; Distinct entries
	. merge @g@(100.01)=keepme
	. ;
	. ; Remove an existing sub entry. Should have same number of entries, but 2 null
	. new keepme merge keepme=@g@(100.01,2,.1)
	. kill @g@(100.01,2,.1)
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=2)
	. do assert($data(@aimgbl@(1,nullxref,2)))
	. do assert('$data(@aimgbl@(1,nullxref_"c",2)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"c")))
	.. do assert(@aimgbl@(-1,nullxref)=2)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	. ; Put back
	. merge @g@(100.01,2,.1)=keepme
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert($data(@aimgbl@(1,nullxref_"c",2)))
	. do assert('$data(@aimgbl@(1,nullxref,2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"c")=1)
	.. do assert(@aimgbl@(-1,nullxref)=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	quit
	;
v1type3 ; @TEST VistA type3 - Test triggers
	; This is modeled on v1type1; look at sample data there.
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new fstr,nullxref,stat set fstr="$reverse()",nullxref="" for stat=0:1:2 do
	. ; write !,"stat: ",stat	; Uncomment for debugging
	. new subs set subs(1)=100.01,subs(2)=":"" """,subs(3)=.1
	. ;
	. ; gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type
	. ; Get AIM global with name only
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,1,0,1,stat,3,fstr)
	. ; Unxref the data
	. do UNXREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,3,fstr)
	. ; Assert that the data doesn't exist (first time (stat=0) it won't, so the previous UNXREF will be a no-op)
	. do assert('$data(@aimgbl))
	. ; Xref data now
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,3,fstr)
	. ;
	. ; Count entries. There are 16 entries, and no entry for zero, which comes out as NULL
	. ; So we expect a total of 17 entries in the index, and 1 NULL.
	. ; The reason we have a NULL for the zero node is a change done for this issue:
	. ; - https://gitlab.com/YottaDB/Util/YDBAIM/-/issues/60
	. ; WE NEED to capture null entries, as they can be significant for non-Fileman-compatible data in VistA
	. do assert($$type1cd(aimgbl)=17)
	. do assert($data(@aimgbl@(1,nullxref,0)))
	. ; do assert($$type1cn(aimgbl)=1)
	. do assert($data(@aimgbl@(1,nullxref_"cd",1)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"cd")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	. ;
	. ; Add data and make sure triggers works
	. ; Now set one of the .1 nodes to be non-existent on a new entry
	. set @g@(100.01,100,0)="JUNK STATUS^junk"
	. ; There should be now 18 entries, but 2 null entries
	. do assert($$type1cd(aimgbl)=18)
	. do assert($$type1cn(aimgbl)=2)
	. do assert($data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref)=2)
	.. ; Another extra entry to test statistics
	.. set @g@(100.01,101,0)="JUNK STATUS2^junk2"
	.. do assert(@aimgbl@(-1,nullxref)=3)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	.. kill @g@(100.01,101,0)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	.. do assert(@aimgbl@(-1,nullxref)=2)
	. ;
	. ; Set the .1 node to some data on entry 100
	. set @g@(100.01,100,.1)="ju"
	. ; There should be now 18 entries, but 1 null entry
	. do assert($$type1cd(aimgbl)=18)
	. do assert($$type1cn(aimgbl)=1)
	. do assert($data(@aimgbl@(1,nullxref_"uj",100)))
	. do assert('$data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert(1=@aimgbl@(-1,nullxref))
	.. do assert(@aimgbl@(-1,nullxref_"uj")=1)
	. ;
	. ; Restore data to original
	. kill @g@(100.01,100)
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert('$data(@aimgbl@(1,nullxref_"ju",100)))
	. do assert('$data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"ju")))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	. ;
	. ; Kill the parent entry node by node
	. new %1,%2 set (%1,%2)=$name(@g@(100.01,1))
	. new keepme merge keepme=@%2
	. do assert($data(@g@(100.01,1)))
	. for  set %1=$query(@%1) quit:$name(@%2,$qlength(%2))'=$name(@%1,$qlength(%2))  kill @%1
	. do assert('$data(@g@(100.01,1)))
	. do assert('$data(@aimgbl@(1,nullxref_"cd",1)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"cd")))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	. merge @%2=keepme
	. do assert($data(@g@(100.01,1)))
	. do assert($data(@aimgbl@(1,nullxref_"cd",1)))
	. if stat=2 do
	.. do assert(@aimgbl@(-1)=17) ; Distinct entries
	. ;
	. ; Kill parent entry all at once
	. new keepme merge keepme=@g@(100.01,1)
	. kill @g@(100.01,1)
	. do assert('$data(@g@(100.01,1)))
	. do assert('$data(@aimgbl@(1,nullxref_"cd")))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"cd")))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	. merge @g@(100.01,1)=keepme
	. do assert($data(@g@(100.01,1)))
	. do assert($data(@aimgbl@(1,nullxref_"cd")))
	. if stat=2 do
	.. do assert(@aimgbl@(-1)=17) ; Distinct entries
	. ;
	. ; Remove all data in ^ORD(101.01)
	. new keepme merge keepme=@g@(100.01)
	. kill @g@(100.01)
	. do assert($data(@aimgbl@(1))=1)
	. if stat do
	.. do assert($data(@aimgbl@(-1))[0)
	.. if stat=2 do
	... do assert('$data(@aimgbl@(-1))) ; Distinct entries
	. merge @g@(100.01)=keepme
	. ;
	. ; Remove an existing sub entry. Should have same number of entries, but 2 null
	. new keepme merge keepme=@g@(100.01,2,.1)
	. kill @g@(100.01,2,.1)
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=2)
	. do assert($data(@aimgbl@(1,nullxref,2)))
	. do assert('$data(@aimgbl@(1,nullxref_"c",2)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"c")))
	.. do assert(@aimgbl@(-1,nullxref)=2)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	. ; Put back
	. merge @g@(100.01,2,.1)=keepme
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert($data(@aimgbl@(1,nullxref_"c",2)))
	. do assert('$data(@aimgbl@(1,nullxref,2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"c")=1)
	.. do assert(@aimgbl@(-1,nullxref)=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	quit
	;
v2type1 ; @TEST VistA type1 - Initial data partly empty
	; Remove an existing entry. Should have same number of entries, but 2 null
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new fstr,nullxref,stat for fstr=0,1 set nullxref=$select(fstr:"#",1:"") for stat=0:1:2 do
	. ; write !,"stat: ",stat	; Uncomment for debugging
	. new keepme merge keepme=@g@(100.01,2,.1)
	. kill @g@(100.01,2,.1)
	. ; gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type
	. ; Get AIM global with name only
	. new subs set subs(1)=100.01,subs(2)=":"" """,subs(3)=.1
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,1,0,1,stat,1,fstr)
	. ; Unxref the data
	. do UNXREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,1,fstr)
	. ; Assert that the data doesn't exist
	. do assert('$data(@aimgbl))
	. ; Initial index
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,1,fstr)
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=2)
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"dc")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	. ; Check that two null entry exists
	. do assert($data(@aimgbl@(1,nullxref,2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref)=2)
	. ; Put back
	. merge @g@(100.01,2,.1)=keepme
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert($data(@aimgbl@(1,nullxref_"c",2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"c")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	quit
	;
v2type3 ; @TEST VistA type3 - Initial data partly empty
	; Remove an existing entry. Should have same number of entries, but 2 null
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new fstr,nullxref,stat set fstr="$reverse()",nullxref="" for stat=0:1:2 do
	. ; write !,"stat: ",stat	; Uncomment for debugging
	. new keepme merge keepme=@g@(100.01,2,.1)
	. kill @g@(100.01,2,.1)
	. ; gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type
	. ; Get AIM global with name only
	. new subs set subs(1)=100.01,subs(2)=":"" """,subs(3)=.1
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,1,0,1,stat,3,fstr)
	. ; Unxref the data
	. do UNXREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,3,fstr)
	. ; Assert that the data doesn't exist
	. do assert('$data(@aimgbl))
	. ; Initial index
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,3,fstr)
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=2)
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"cd")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	. ; Check that two null entry exists
	. do assert($data(@aimgbl@(1,nullxref,2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref)=2)
	. ; Put back
	. merge @g@(100.01,2,.1)=keepme
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert($data(@aimgbl@(1,nullxref_"c",2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"c")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	quit
	;
v1type0 ; @TEST Repeat VistA type 1 tests as type ""
	; Index data in ^ORD(100.01,:,.1) first ^ piece
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new fstr,nullxref,stat for fstr=0,1 set nullxref=$select(fstr:"#",1:"") for stat=0:1:2 do
	. ; write !,"stat: ",stat	; Uncomment for debugging
	. new subs set subs(1)=100.01,subs(2)=":"" """,subs(3)=.1
	. ;
	. ; gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type
	. ; Get AIM global with name only
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,1,0,1,stat,aimtype,fstr)
	. ; Unxref the data
	. do UNXREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,aimtype,fstr)
	. ; Assert that the data doesn't exist (first time (stat=0) it won't, so the previous UNXREF will be a no-op)
	. do assert('$data(@aimgbl))
	. ; Xref data now
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,aimtype,fstr)
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
	. ;
	. ; Add data and make sure triggers works
	. ; Now set one of the .1 nodes to be non-existent on a new entry
	. set @g@(100.01,100,0)="JUNK STATUS^junk"
	. ; There should be now 16 entries, as the non-existent entry won't get indexed
	. do assert($$type1cd(aimgbl)=16)
	. do assert($$type1cn(aimgbl)=0)
	. do assert('$data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref)))
	.. ; Another extra entry to test statistics
	.. set @g@(100.01,101,0)="JUNK STATUS2^junk2"
	.. do assert('$data(@aimgbl@(-1,nullxref)))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	.. kill @g@(100.01,101,0)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	.. do assert('$data(@aimgbl@(-1,nullxref)))
	. ;
	. ; Set the .1 node to some data on entry 100
	. set @g@(100.01,100,.1)="ju"
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
	. ;
	. ; Restore data to original
	. kill @g@(100.01,100)
	. do assert($$type1cd(aimgbl)=16)
	. do assert($$type1cn(aimgbl)=0)
	. do assert('$data(@aimgbl@(1,nullxref_"ju",100)))
	. do assert('$data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"ju")))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	. ;
	. ; Kill the parent entry node by node
	. new %1,%2 set (%1,%2)=$name(@g@(100.01,1))
	. new keepme merge keepme=@%2
	. do assert($data(@g@(100.01,1)))
	. for  set %1=$query(@%1) quit:$name(@%2,$qlength(%2))'=$name(@%1,$qlength(%2))  kill @%1
	. do assert('$data(@g@(100.01,1)))
	. do assert('$data(@aimgbl@(1,nullxref_"dc")))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"dc")))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=15) ; Distinct entries
	. merge @%2=keepme
	. do assert($data(@g@(100.01,1)))
	. do assert($data(@aimgbl@(1,nullxref_"dc")))
	. if stat=2 do
	.. do assert(@aimgbl@(-1)=16) ; Distinct entries
	. ;
	. ; Kill parent entry all at once
	. new keepme merge keepme=@g@(100.01,1)
	. kill @g@(100.01,1)
	. do assert('$data(@g@(100.01,1)))
	. do assert('$data(@aimgbl@(1,nullxref_"dc")))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"dc")))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=15) ; Distinct entries
	. merge @g@(100.01,1)=keepme
	. do assert($data(@g@(100.01,1)))
	. do assert($data(@aimgbl@(1,nullxref_"dc")))
	. if stat=2 do
	.. do assert(@aimgbl@(-1)=16) ; Distinct entries
	. ;
	. ; Remove all data in ^ORD(101.01)
	. new keepme merge keepme=@g@(100.01)
	. kill @g@(100.01)
	. do assert($data(@aimgbl@(1))=1)
	. if stat do
	.. do assert($data(@aimgbl@(-1))[0)
	.. if stat=2 do
	... do assert('$data(@aimgbl@(-1))) ; Distinct entries
	. merge @g@(100.01)=keepme
	. ;
	. ; Remove an existing sub entry. Should have 1 less entry.
	. new keepme merge keepme=@g@(100.01,2,.1)
	. kill @g@(100.01,2,.1)
	. do assert($$type1cd(aimgbl)=15)
	. do assert($$type1cn(aimgbl)=0)
	. do assert('$data(@aimgbl@(1,nullxref,2)))
	. do assert('$data(@aimgbl@(1,nullxref_"c",2)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_"c")))
	.. do assert('$data(@aimgbl@(-1,nullxref)))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=15) ; Distinct entries
	. ; Put back
	. merge @g@(100.01,2,.1)=keepme
	. do assert($$type1cd(aimgbl)=16)
	. do assert($$type1cn(aimgbl)=0)
	. do assert($data(@aimgbl@(1,nullxref_"c",2)))
	. do assert('$data(@aimgbl@(1,nullxref,2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"c")=1)
	.. do assert('$data(@aimgbl@(-1,nullxref)))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	quit
	;
v2type0 ; @TEST VistA type 0 - Initial data partly empty
	; Remove an existing entry. Should have same number of entries, but 1 null
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new fstr,nullxref,stat for fstr=0,1 set nullxref=$select(fstr:"#",1:"") for stat=0:1:2 do
	. ; write !,"stat: ",stat	; Uncomment for debugging
	. new keepme merge keepme=@g@(100.01,2,.1)
	. kill @g@(100.01,2,.1)
	. ; gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type
	. ; Get AIM global with name only
	. new subs set subs(1)=100.01,subs(2)=":"" """,subs(3)=.1
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,1,0,1,stat,aimtype,fstr)
	. ; Unxref the data
	. do UNXREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,aimtype,fstr)
	. ; Assert that the data doesn't exist
	. do assert('$data(@aimgbl))
	. ; Initial index
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,aimtype,fstr)
	. do assert($$type1cd(aimgbl)=15)
	. do assert($$type1cn(aimgbl)=0)
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"dc")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=15) ; Distinct entries
	. ; Check that null entry doesn't exist
	. do assert('$data(@aimgbl@(1,nullxref,2)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref)))
	. ; Put back
	. merge @g@(100.01,2,.1)=keepme
	. do assert($$type1cd(aimgbl)=16)
	. do assert($$type1cn(aimgbl)=0)
	. do assert($data(@aimgbl@(1,nullxref_"c",2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"c")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	quit
	;
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
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new fstr,nullxref,stat for fstr=0,1 set nullxref=$select(fstr:"#",1:"") for stat=0:1:2 do
	. ; write !,"stat: ",stat	; Uncomment for debugging
	. new subs set subs(1)=100.01,subs(2)=$ZWRITE(0)_":"_$ZWRITE($CHAR(0)),subs(3)="""VUID"""
	. ;
	. ; gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type
	. ; Get AIM global with name only
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,1,0,1,stat,1,fstr)
	. ; Unxref the data
	. do UNXREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,1,fstr)
	. ; Assert that the data doesn't exist (first time (stat=0) it won't, so the previous UNXREF will be a no-op)
	. do assert('$data(@aimgbl))
	. ; Xref data now
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,1,fstr)
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
	. ;
	. ; Add data and make sure triggers works
	. ; Now set one of the .1 nodes to be non-existent on a new entry
	. set @g@(100.01,100,0)="JUNK STATUS^junk"
	. ; There should be now 18 entries, and 2 null entries
	. do assert($$type1cd(aimgbl)=18)
	. do assert($$type1cn(aimgbl)=2)
	. do assert($data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref)=2)
	.. ; Another extra entry to test statistics
	.. set @g@(100.01,101,0)="JUNK STATUS2^junk2"
	.. do assert(@aimgbl@(-1,nullxref)=3)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	.. kill @g@(100.01,101,0)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	.. do assert(@aimgbl@(-1,nullxref)=2)
	. ;
	. ; Set the VUID node to some data on entry 100
	. set @g@(100.01,100,"VUID")="9999999^1"
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
	. kill @g@(100.01,100)
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert('$data(@aimgbl@(1,nullxref_9999999,100)))
	. do assert('$data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_9999999)))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	. ;
	. ; Kill the parent entry node by node
	. new %1,%2 set (%1,%2)=$name(@g@(100.01,1))
	. new keepme merge keepme=@%2
	. do assert($data(@g@(100.01,1)))
	. for  set %1=$query(@%1) quit:$name(@%2,$qlength(%2))'=$name(@%1,$qlength(%2))  kill @%1
	. do assert('$data(@g@(100.01,1)))
	. do assert('$data(@aimgbl@(1,nullxref_4500704,1)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_4500704)))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	. merge @%2=keepme
	. do assert($data(@g@(100.01,1)))
	. do assert($data(@aimgbl@(1,nullxref_4500704,1)))
	. if stat=2 do
	.. do assert(@aimgbl@(-1)=17) ; Distinct entries
	. ;
	. ; Kill parent entry all at once
	. new keepme merge keepme=@g@(100.01,1)
	. kill @g@(100.01,1)
	. do assert('$data(@g@(100.01,1)))
	. do assert('$data(@aimgbl@(1,nullxref_4500704)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_4500704)))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	. merge @g@(100.01,1)=keepme
	. do assert($data(@g@(100.01,1)))
	. do assert($data(@aimgbl@(1,nullxref_4500704)))
	. if stat=2 do
	.. do assert(@aimgbl@(-1)=17) ; Distinct entries
	. ;
	. ; Remove all data in ^ORD(101.01)
	. new keepme merge keepme=@g@(100.01)
	. kill @g@(100.01)
	. do assert($data(@aimgbl@(1))=1)
	. if stat do
	.. do assert($data(@aimgbl@(-1))[0)
	.. if stat=2 do
	... do assert('$data(@aimgbl@(-1))) ; Distinct entries
	. merge @g@(100.01)=keepme
	. ;
	. ; Remove an existing sub entry. Should have same number of entries, but 1 null
	. new keepme merge keepme=@g@(100.01,2,"VUID")
	. kill @g@(100.01,2,"VUID")
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=2)
	. do assert($data(@aimgbl@(1,nullxref,2)))
	. do assert('$data(@aimgbl@(1,nullxref_4501088,2)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_4501088)))
	.. do assert(@aimgbl@(-1,nullxref)=2)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	. ; Put back
	. merge @g@(100.01,2,"VUID")=keepme
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert($data(@aimgbl@(1,nullxref_4501088,2)))
	. do assert('$data(@aimgbl@(1,nullxref,2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_4501088)=1)
	.. do assert(@aimgbl@(-1,nullxref)=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	quit
	;
v1type3s ; @TEST VistA type3 - Test triggers with string subs in xsub
	; This is modeled on v1type1s; look at sample data there.
	; ...
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new fstr,nullxref,stat set fstr="$reverse()",nullxref="" for stat=0:1:2 do
	. ; write !,"stat: ",stat	; Uncomment for debugging
	. new subs set subs(1)=100.01,subs(2)=$ZWRITE(0)_":"_$ZWRITE($CHAR(0)),subs(3)="""VUID"""
	. ;
	. ; gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type
	. ; Get AIM global with name only
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,1,0,1,stat,3,fstr)
	. ; Unxref the data
	. do UNXREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,3,fstr)
	. ; Assert that the data doesn't exist (first time (stat=0) it won't, so the previous UNXREF will be a no-op)
	. do assert('$data(@aimgbl))
	. ; Xref data now
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,3,fstr)
	. ;
	. ; Count entries. There are 16 entries, and no entry for zero, which comes out as NULL
	. ; So we expect a total of 17 entries in the index, and 1 NULL.
	. ; The reason we have a NULL for the zero node is a change done for this issue:
	. ; - https://gitlab.com/YottaDB/Util/YDBAIM/-/issues/60
	. ; WE NEED to capture null entries, as they can be significant for non-Fileman-compatible data in VistA
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert($data(@aimgbl@(1,4070054,1)))
	. if stat do
	.. do assert(@aimgbl@(-1,4070054)=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	. ;
	. ; Add data and make sure triggers works
	. ; Now set one of the .1 nodes to be non-existent on a new entry
	. set @g@(100.01,100,0)="JUNK STATUS^junk"
	. ; There should be now 18 entries, and 2 null entries
	. do assert($$type1cd(aimgbl)=18)
	. do assert($$type1cn(aimgbl)=2)
	. do assert($data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref)=2)
	.. ; Another extra entry to test statistics
	.. set @g@(100.01,101,0)="JUNK STATUS2^junk2"
	.. do assert(@aimgbl@(-1,nullxref)=3)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	.. kill @g@(100.01,101,0)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	.. do assert(@aimgbl@(-1,nullxref)=2)
	. ;
	. ; Set the VUID node to some data on entry 100
	. set @g@(100.01,100,"VUID")="9999999^1"
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
	. kill @g@(100.01,100)
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert('$data(@aimgbl@(1,nullxref_9999999,100)))
	. do assert('$data(@aimgbl@(1,nullxref,100)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,nullxref_9999999)))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	. ;
	. ; Kill the parent entry node by node
	. new %1,%2 set (%1,%2)=$name(@g@(100.01,1))
	. new keepme merge keepme=@%2
	. do assert($data(@g@(100.01,1)))
	. for  set %1=$query(@%1) quit:$name(@%2,$qlength(%2))'=$name(@%1,$qlength(%2))  kill @%1
	. do assert('$data(@g@(100.01,1)))
	. do assert('$data(@aimgbl@(1,4070054,1)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,4070054)))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	. merge @%2=keepme
	. do assert($data(@g@(100.01,1)))
	. do assert($data(@aimgbl@(1,4070054,1)))
	. if stat=2 do
	.. do assert(@aimgbl@(-1)=17) ; Distinct entries
	. ;
	. ; Kill parent entry all at once
	. new keepme merge keepme=@g@(100.01,1)
	. kill @g@(100.01,1)
	. do assert('$data(@g@(100.01,1)))
	. do assert('$data(@aimgbl@(1,4070054)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,4070054)))
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	. merge @g@(100.01,1)=keepme
	. do assert($data(@g@(100.01,1)))
	. do assert($data(@aimgbl@(1,4070054)))
	. if stat=2 do
	.. do assert(@aimgbl@(-1)=17) ; Distinct entries
	. ;
	. ; Remove all data in ^ORD(101.01)
	. new keepme merge keepme=@g@(100.01)
	. kill @g@(100.01)
	. do assert($data(@aimgbl@(1))=1)
	. if stat do
	.. do assert($data(@aimgbl@(-1))[0)
	.. if stat=2 do
	... do assert('$data(@aimgbl@(-1))) ; Distinct entries
	. merge @g@(100.01)=keepme
	. ;
	. ; Remove an existing sub entry. Should have same number of entries, but 1 null
	. new keepme merge keepme=@g@(100.01,2,"VUID")
	. kill @g@(100.01,2,"VUID")
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=2)
	. do assert($data(@aimgbl@(1,nullxref,2)))
	. do assert('$data(@aimgbl@(1,8801054,2)))
	. if stat do
	.. do assert('$data(@aimgbl@(-1,8801054)))
	.. do assert(@aimgbl@(-1,nullxref)=2)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	. ; Put back
	. merge @g@(100.01,2,"VUID")=keepme
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert($data(@aimgbl@(1,8801054,2)))
	. do assert('$data(@aimgbl@(1,nullxref,2)))
	. if stat do
	.. do assert(@aimgbl@(-1,8801054)=1)
	.. do assert(@aimgbl@(-1,nullxref)=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	quit
	;
v2type1s ; @TEST VistA type1 - Initial data partly empty
	; Remove an existing entry. Should have same number of entries, but 1 null
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new fstr,nullxref,stat for fstr=0,1 set nullxref=$select(fstr:"#",1:"") for stat=0:1:2 do
	. ; write !,"stat: ",stat       ; Uncomment for debugging
	. new keepme merge keepme=@g@(100.01,2,.1)
	. kill @g@(100.01,2,.1)
	. new subs set subs(1)=100.01,subs(2)=":"" """,subs(3)=.1
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,1,0,1,stat,1,fstr)
	. ; Unxref the data
	. do UNXREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,1,fstr)
	. ; Assert that the data doesn't exist
	. do assert('$data(@aimgbl))
	. ; Initial index
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,1,fstr)
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=2)
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"dc")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	. ; Check that null entry exists
	. do assert($data(@aimgbl@(1,nullxref,2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref)=2)
	. ; Put back
	. merge @g@(100.01,2,.1)=keepme
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert($data(@aimgbl@(1,nullxref_"c",2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"c")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
	quit
	;
v2type3s ; @TEST VistA type3 - Initial data partly empty
	; Remove an existing entry. Should have same number of entries, but 1 null
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new fstr,nullxref,stat set fstr="$reverse()",nullxref="" for stat=0:1:2 do
	. ; write !,"stat: ",stat       ; Uncomment for debugging
	. new keepme merge keepme=@g@(100.01,2,.1)
	. kill @g@(100.01,2,.1)
	. new subs set subs(1)=100.01,subs(2)=":"" """,subs(3)=.1
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,1,0,1,stat,3,fstr)
	. ; Unxref the data
	. do UNXREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,3,fstr)
	. ; Assert that the data doesn't exist
	. do assert('$data(@aimgbl))
	. ; Initial index
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,stat,3,fstr)
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=2)
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"cd")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=16) ; Distinct entries
	. ; Check that null entry exists
	. do assert($data(@aimgbl@(1,nullxref,2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref)=2)
	. ; Put back
	. merge @g@(100.01,2,.1)=keepme
	. do assert($$type1cd(aimgbl)=17)
	. do assert($$type1cn(aimgbl)=1)
	. do assert($data(@aimgbl@(1,"c",2)))
	. if stat do
	.. do assert(@aimgbl@(-1,nullxref_"c")=1)
	.. if stat=2 do
	... do assert(@aimgbl@(-1)=17) ; Distinct entries
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
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new subs set subs(1)=100.01,subs(2)="0:2",subs(3)=.1
	;
	; gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,2,1)
	;
	; Assert that data in 1,2 exists, not 3
	do assert($data(@aimgbl@(1,"c",2)))
	do assert($data(@aimgbl@(1,"dc",1)))
	do assert('$data(@aimgbl@(1,"h",3)))
	;
	; Add data in 1.5, and see that it gets into the index
	set @g@(100.01,1.5,.1)="fake"
	do assert($data(@aimgbl@(1,"fake",1.5)))
	; kill it (a node higher just for testing)
	kill @g@(100.01,1.5)
	do assert('$data(@aimgbl@(1,"fake",1.5)))
	;
	; Add data in 88, and see that it DOES NOT get into the index
	set @g@(100.01,88,.1)="fake2"
	do assert('$data(@aimgbl@(1,"fake2",88)))
	kill @g@(100.01,88)
	quit
	;
aim60a	; @TEST For type=1 a non-existent node has metadata created
	; There was an error/omission in the specification for #51 (closed),
	; where non-Fileman compatible data was stored and where NULLs in indexes
	; (represented as an empty string) did not show up in the AIM index,
	; resulting in the IS NULL query returning invalid data.
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"x"
	kill @g
	set @g@(1,"const",1)="a",@g@(2,"const")="b"
	;
	new subs
	set subs(1)=":"
	set subs(2)="""const"":""constz"""
	set subs(3)=1
	;                                         g  , s   , s ,p,n,z,o,s,t
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"|",1,0,0,1,0,1)
	do assert($data(@aimgbl@(1,"",2,"const")))
	do assert($data(@aimgbl@(1,"a",1,"const")))
	quit
	;
aim60b	; @TEST Metadata was incorrectly calculated unselected pieces
	; We index the 2 node; we should not see 1 node data present
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new subs set subs(1)=100.01,subs(2)=":"" """,subs(3)=0
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",2,0,0,1,2,1)
	do assert('$data(@aimgbl@(-1)))
	quit
	;
TW27p3	; @TEST Range to empty string doesn't work with AIM#42 changes
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"ORD"
	new subs set subs(1)=100.01,subs(2)="0:",subs(3)=0
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",2,0,0,1,0,aimtype)
	do assert($data(@aimgbl@(2,"dc",1)))
	; Test XREFSUB()
	set aimgbl=$$XREFSUB^%YDBAIM(g,.subs,2,0,1)
	do assert($data(@aimgbl@(2,1)))
	quit
	;
tsigusr1 ; @TEST Interrupt using SIGUSR1
	; This is supposed to only do a job exam, and resume
	; $ZYINTRSIG is supposed to be SIGUSR1
	; Set interrupt code
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"PSNDF"
	new aimtmp set aimtmp="^"_$select(xdref:xdrefloc,1:"")_"%ydbAIMtmp"
	view "setenv":"ydb_zinterrupt":"do sigusrint^"_$text(+0),"jobpid":1
	; This test needs the scan to still be running when the signal below is
	; delivered: it waits for the interrupt to be processed, and then requires
	; the index to grow between six samples. Since AIM#97 the scan of this
	; global variable is several times faster, so the number of processes is
	; pinned here rather than left to the environment, which on a machine with
	; many CPUs would leave nothing to interrupt. Three rather than one, so that
	; the interrupt is delivered to a scan that is actually divided. tsigusr2
	; and tresume interrupt a scan that runs with as many processes as the
	; environment gives it.
	view "setenv":"ydb_aim_nproc":"3"
	;
	; delete old data generated from interrupt (in case of re-runs)
	zsy "rm -f tsigusr1.jobexam"
	kill ^tsigusr,@aimtmp
	;
	; Get AIM index name for later use
	new subs set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,1,,,,aimtype) ; nmonly
	;
	; Job off AIM jobs that will be signaled
	new jobpid job jobsigusr:passcurlvn set jobpid=$zjob
	;
	; Every wait below is bounded. Each of them is waiting on the JOB'd process
	; to reach a particular point, and if it never does, an unbounded wait hangs
	; the whole test suite with no indication of what it was waiting for. A
	; bounded one fails the test and says so.
	new deadline
	;
	; Wait till the parallel process children show up
	set deadline=$zut+60E6
	for  quit:$data(@aimtmp@("%YDBAIM",jobpid,0))!($zut>deadline)  hang .01
	do assert($data(@aimtmp@("%YDBAIM",jobpid,0)),"the JOB'd process recorded no scanning processes within 60 seconds")
	;
	; give it some time to index
	set deadline=$zut+60E6
	for  quit:$data(@aimgbl@(1,"A"))!($zut>deadline)  hang .0001
	do assert($data(@aimgbl@(1,"A")),"nothing was indexed within 60 seconds; there was nothing to interrupt")
	;
	; Interrupt with USR1
	if $ZSIGPROC(jobpid,"SIGUSR1")
	;
	; Wait till interrupt is processed. $ZSEARCH returns the successive matches
	; for a search argument and then the empty string, so the match is captured
	; here rather than searched for a second time afterwards, which would report
	; the file as absent when it exists.
	new jobexam set jobexam="",deadline=$zut+60E6
	for  set jobexam=$zsearch("tsigusr1.jobexam") quit:$zlength(jobexam)!($zut>deadline)  hang .01
	do assert($zlength(jobexam),"the interrupt was not processed within 60 seconds; the scan may have finished before the signal arrived")
	quit:'$zlength(jobexam)		; nothing below can succeed without it
	;
	; Setting ^tsigusr happens inside of a transaction. In rare cases the interrupt
	; may successfully run (creating tsigusr1.jobexam), but ^tsigusr may be
	; still uncommitted at that point. That's why we check for it separately.
	set deadline=$zut+60E6
	for  quit:($get(^tsigusr)'="")!($zut>deadline)  hang .01
	do assert($get(^tsigusr)'="","the interrupt handler did not commit ^tsigusr within 60 seconds")
	;
	; Count once as a baseline and then recount until the index grows, bounded as
	; every other wait in this test is, rather than counting a fixed number of
	; times a fixed interval apart. Since AIM#97 the signaled process no longer
	; scans: it cuts the ranges the processes it JOB'd scan, and while it is in
	; its interrupt handler writing the jobexam dump it queues no ranges, so a
	; worker that has drained the queue writes nothing either. This test starts
	; counting inside that gap, and six counts a millisecond apart can all fall in
	; it and come out equal, which is how this test failed in a pipeline with a
	; complete and correct index. Growth at any point after the interrupt is what
	; proves the scan outlived it, and no window is too narrow to see it.
	do sigusrcountA(1,aimgbl)
	do assert(^tsigusr(1,"count","A"),"nothing was indexed at the first count")
	set deadline=$zut+60E6
	for  do  quit:(^tsigusr(2,"count","A")>^tsigusr(1,"count","A"))!($zut>deadline)
	. hang .001
	. do sigusrcountA(2,aimgbl)
	do assert(^tsigusr(2,"count","A")>^tsigusr(1,"count","A"),"the index did not grow within 60 seconds of the interrupt")
	;
	; Assert that the signal is USR1
	do assert(^tsigusr="SIGUSR1")
	;
	; Wait for AIM job to die (this takes a while as we are still indexing)
	set deadline=$zut+120E6
	for  quit:'$zgetjpi(jobpid,"ISPROCALIVE")!($zut>deadline)  hang 0.01
	do assert('$zgetjpi(jobpid,"ISPROCALIVE"),"the JOB'd process was still running 120 seconds after being interrupted")
	;
	; Final count
	do sigusrcount(7,aimgbl)
	;
	; ^tsigusr(1,"count","A")=388		; the baseline, taken after the interrupt
	; ^tsigusr(2,"count","A")=545		; the recount that saw it grow
	; ^tsigusr(7,"count","A")=497000	; after the scan ended
	; ^tsigusr(7,"count","B")=497001
	;
	; Final count should be the full count
	do assert(^tsigusr(7,"count","A")=497000)
	do assert(^tsigusr(7,"count","B")=497001)
	;
	; Test XREFSUB()
	; delete old data generated from interrupt (in case of re-runs)
	zsy "rm -f tsigusr1.jobexam"
	kill ^tsigusr,@aimtmp
	;
	; Get AIM index name for later use
	set aimgbl=$$XREFSUB^%YDBAIM(g,.subs,2,1) ; nmonly
	;
	; Job off AIM jobs that will be signaled
	job jobsigusr(1):passcurlvn set jobpid=$zjob
	;
	; Wait till the parallel process children show up
	for  quit:$data(@aimtmp@("%YDBAIM",jobpid,0))  hang .01
	;
	; give it some time to index
	for  quit:$data(@aimgbl@(2,200000))  hang .0001
	;
	; Interrupt with USR1
	if $ZSIGPROC(jobpid,"SIGUSR1")
	;
	; Wait till interrupt is processed
	for  quit:$zsearch("tsigusr1.jobexam")'=""  hang .01
	;
	; Setting ^tsigusr happens inside of a transaction. In rare cases the interrupt
	; may successfully run (creating tsigusr1.jobexam), but ^tsigusr may be
	; still uncommitted at that point. That's why we check for it separately.
	for  quit:$get(^tsigusr)'=""  hang .01
	;
	; Count data six times. Each count should be greater than the previous one.
	do sigusrcount(1,aimgbl,1) h .001
	do sigusrcount(2,aimgbl,1) h .001
	do sigusrcount(3,aimgbl,1) h .001
	do sigusrcount(4,aimgbl,1) h .001
	do sigusrcount(5,aimgbl,1) h .001
	do sigusrcount(6,aimgbl,1)
	;
	; Assert that the signal is USR1
	do assert(^tsigusr="SIGUSR1")
	;
	; Wait for AIM job to die (this takes a while as we are still indexing)
	set deadline=$zut+120E6
	for  quit:'$zgetjpi(jobpid,"ISPROCALIVE")!($zut>deadline)  hang 0.01
	do assert('$zgetjpi(jobpid,"ISPROCALIVE"),"the JOB'd process was still running 120 seconds after being interrupted")
	;
	; Reset envionment
	view "unsetenv":"ydb_zinterrupt"
	view "unsetenv":"ydb_aim_nproc"
	;
	; Final count
	do sigusrcount(7,aimgbl,1)
	;
	; Final count should be the full count
	do assert(^tsigusr(7)=999378)
	;
	; Counts should be non-zero and increasing monotonically
	;
	; In AIM jobs we have seen though that sometimes the last 3 counts are
	; the same which means that we finished early. So assert instead that
	; 3 counts of 6 are increasing monotonically.
	new i,increasedCount set increasedCount=0
	for i=1:1:6 do
	. do assert(^tsigusr(i))
	. if ^tsigusr(i)<^tsigusr(i+1) set increasedCount=increasedCount+1
	do assert(increasedCount>2)
	quit
	;
tsigusr2 ; @TEST Interrupt using SIGUSR2
	; This is supposed to only do a job exam, and stop the indexing
	; $ZYINTRSIG is supposed to be SIGUSR2
	; SIGUSR2 processing is only activated when we set ydb_treat_sigusr2_like_sigusr1
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"PSNDF"
	new aimtmp set aimtmp="^"_$select(xdref:xdrefloc,1:"")_"%ydbAIMtmp"
	view "setenv":"ydb_treat_sigusr2_like_sigusr1":"1"
	view "setenv":"ydb_zinterrupt":"do sigusrint^"_$text(+0),"jobpid":1
	;
	; delete old data generated from interrupt (in case of re-runs)
	zsy "rm -f tsigusr2.jobexam"
	kill ^tsigusr,@aimtmp
	;
	; Get AIM index name for later use
	new subs set subs(1)=50.6,subs(2)=":",subs(3)="""VUID"""
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,1,,,,aimtype) ; nmonly
	;
	; Job off AIM jobs that will be signaled
	new jobpid job jobsigusr:passcurlvn set jobpid=$zjob
	;
	; Wait till the parallel process children show up
	for  quit:$data(@aimtmp@("%YDBAIM",jobpid,0))  hang .01
	;
	; give it some time to index
	for  quit:$data(@aimgbl@(1,"A"))  hang .001
	;
	; Interrupt with USR2
	if $ZSIGPROC(jobpid,"SIGUSR2")
	;
	; Wait till interrupt is processed
	for  quit:$zsearch("tsigusr2.jobexam")'=""  hang .01
	; See comments in tsigusr1 test for why the next line is necessary
	for  quit:$get(^tsigusr)'=""  hang .01
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
	; Wait for AIM job to die (this is fast as indexing is done)
	for  quit:'$zgetjpi(jobpid,"ISPROCALIVE")  hang 0.001
	;
	; Test XREFSUB()
	; delete old data generated from interrupt (in case of re-runs)
	zsy "rm -f tsigusr2.jobexam"
	kill ^tsigusr,@aimtmp
	do UNXREFSUB^%YDBAIM(g) ; for repeated runs
	;
	; Get AIM index name for later use
	set aimgbl=$$XREFSUB^%YDBAIM(g,.subs,2,1) ; nmonly
	;
	; Job off AIM jobs that will be signaled
	new jobpid job jobsigusr(1):passcurlvn set jobpid=$zjob
	;
	; Wait till the parallel process children show up
	for  quit:$data(@aimtmp@("%YDBAIM",jobpid,0))  hang .001
	;
	; give it some time to index
	for  quit:$data(@aimgbl@(2))  hang .0001
	;
	; Interrupt with USR2
	if $ZSIGPROC(jobpid,"SIGUSR2")
	;
	; Wait till interrupt is processed
	for  quit:$zsearch("tsigusr2.jobexam")'=""  hang .01
	;
	; Count data six times. The counts should all the same, as we stopped indexing.
	do sigusrcount(1,aimgbl,1) h .001
	do sigusrcount(2,aimgbl,1) h .001
	do sigusrcount(3,aimgbl,1) h .001
	do sigusrcount(4,aimgbl,1) h .001
	do sigusrcount(5,aimgbl,1) h .001
	do sigusrcount(6,aimgbl,1)
	;
	; Assert that the signal is USR2
	do assert(^tsigusr="SIGUSR2")
	;
	; Assert that the zgoto in the interrupt handler actually executed
	do assert($data(^tsigusr("finish")))
	;
	; Counts should be non-zero and identical
	for i=1:1:5 do assert(^tsigusr(i)),assert(^tsigusr(i)=^tsigusr(i+1))
	;
	; Reset environment
	view "unsetenv":"ydb_zinterrupt"
	view "unsetenv":"ydb_treat_sigusr2_like_sigusr1"
	;
	; Wait for AIM job to die (this is fast as indexing is done)
	for  quit:'$zgetjpi(jobpid,"ISPROCALIVE")  hang 0.001
	quit
	;
jobsigusr(type) ; [job for tsigusr1 and tsigusr2] - type=1 means XREFSUB()
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"PSNDF"
	if +$get(type) do XREFSUB^%YDBAIM(g,.subs,2)
	else  do XREFDATA^%YDBAIM(g,.subs,"^",1,,,,,aimtype)
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
sigusrcount(n,aimgbl,type) ; type=1 is XREFSUB() test
	if +$get(type) do
	. new count,i set count=0,i=""
	. for  set i=$order(@aimgbl@(2,i)) quit:'i  if $increment(count)
	. set ^tsigusr(n)=count
	else  do sigusrcountA(n,aimgbl),sigusrcountB(n,aimgbl)
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
	job jobintrestore:pass
	for  quit:$data(^tintrestore("after"))  hang 0.001
	do assert(^tintrestore("before")=^tintrestore("after"))
	kill ^tintrestore
	job jobintrestore(1):pass
	for  quit:$data(^tintrestore("after"))  hang 0.001
	do assert(^tintrestore("before")=^tintrestore("after"))
	; Every process JOB'd after this - every scan worker for the rest of the pass - would
	; otherwise start with this handler as its $zinterrupt.
	view "unsetenv":"ydb_zinterrupt"
	quit
	;
jobintrestore(type) ; [job target for tintrestore] - type is XREFSUB() test
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"customers"
	set ^tintrestore("before")=$zinterrupt
	if +$get(type) do XREFSUB^%YDBAIM(g,1,1)
	else  do XREFDATA^%YDBAIM(g,1,"§",3,,,,,aimtype)
	set ^tintrestore("after")=$zinterrupt
	quit
intintrestore ; [no op interrupt; not a typo]
	quit
	;
version ; @TEST Test that VERSION() returns correct versions
	; DATA went 5 -> 6 when types 1 and 3 became a record specification: the cross
	; reference NAME changes for them, since type and the record specification both feed the hash.
	do assert(5.3=$$VERSION^%YDBAIM("DATA"))
	do assert(5.3=$$VERSION^%YDBAIM("data"))
	do assert(2.3=$$VERSION^%YDBAIM("SUB"))
	do assert(2.3=$$VERSION^%YDBAIM("sub"))
	do assert(7.3=$$VERSION^%YDBAIM)
	do assert(7.3=$$VERSION^%YDBAIM())
	do assert(7.3=$$VERSION^%YDBAIM($$^%RANDSTR(5,,"AN")))
	quit
	;
t4valid	; @TEST type=4 (record specifications): parameter validation
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	; The validation and naming of the type=4 parameters recgbl and recsub. Every XREFDATA
	; call below passes nmonly=1, so no cross reference, trigger or global node is created.
	;
	; Two globals are named (and, because nmonly=1, never read):
	;   value  global  ^t4val(A)            -- where a value would live
	;   record global  ^t4rec(A,"fixed",B)  -- where a record's existence is defined
	; The value subscripts (A) is a left-prefix of the record subscripts (A,B).
	;
	new vgbl set vgbl="^"_$select(xdref:xdrefloc,1:"")_"t4val"
	new rgbl set rgbl="^"_$select(xdref:xdrefloc,1:"")_"t4rec"
	;
	new subs   set subs(1)=":"							; value  ^t4val(A)
	new recsub set recsub(1)=":",recsub(2)="fixed",recsub(3)=":"	; record ^t4rec(A,"fixed",B)
	new name1  set name1=$$XREFDATA^%YDBAIM(vgbl,.subs,"^",23,1,,,,4,,,rgbl,.recsub)
	;
	;---------------------------------------------------------------------------
	; 1. The record specification participates in the AIM name: same value spec but a
	;    different record global must yield a different name.
	;---------------------------------------------------------------------------
	new rgbl2 set rgbl2="^"_$select(xdref:xdrefloc,1:"")_"t4rec2"
	new name2 set name2=$$XREFDATA^%YDBAIM(vgbl,.subs,"^",23,1,,,,4,,,rgbl2,.recsub)
	do assert(name1'=name2,"a different record specification must give a different name")
	;
	;---------------------------------------------------------------------------
	; 2. type=4 WITHOUT a record specification is given one: the whole value
	;    specification, so the record is the value node itself. It names the same
	;    cross reference as that specification written out - recsub stated, recgbl left
	;    to default to gbl, which is how AIM writes the one it derives.
	;---------------------------------------------------------------------------
	new recsubv set recsubv(1)=":",recsubv=1
	new name4 set name4=$$XREFDATA^%YDBAIM(vgbl,.subs,"^",23,1,,,,4)
	do assert($zlength(name4),"type=4 with no specification is given one")
	do assert(name4=$$XREFDATA^%YDBAIM(vgbl,.subs,"^",23,1,,,,4,,,,.recsubv),"...and it is the whole value specification")
	;
	; Only a record specification in the value global itself can be derived. Naming another
	; record global without saying where in it the record is, is still rejected.
	new ecodetest
	do
	. new $etrap set $etrap="goto err^"_$text(+0)
	. set ecodetest="" if $$XREFDATA^%YDBAIM(vgbl,.subs,"^",23,1,,,,4,,,rgbl)
	do assert(ecodetest="U219","a record global other than gbl needs recsub")
	;
	;---------------------------------------------------------------------------
	; 3. A record specification WITHOUT type=4 is rejected with U219.
	;---------------------------------------------------------------------------
	new ecodetest
	do
	. new $etrap set $etrap="goto err^"_$text(+0)
	. set ecodetest="" if $$XREFDATA^%YDBAIM(vgbl,.subs,"^",23,1,,,,,,,rgbl,.recsub)
	do assert(ecodetest="U219","recgbl/recsub are only allowed with type=4")
	; ...and type=1, whose specification AIM derives, may not be given one either
	new subs1 set subs1(1)=":",subs1(2)="""X"""
	new ecodetest
	do
	. new $etrap set $etrap="goto err^"_$text(+0)
	. set ecodetest="" if $$XREFDATA^%YDBAIM(vgbl,.subs1,"^",23,1,,,,1,,,rgbl,.recsub)
	do assert(ecodetest="U219","type=1 derives its specification and may not be given one")
	;
	;---------------------------------------------------------------------------
	; 4. The value subscripts must be a left-prefix of the record subscripts (U220).
	;    value subscripts A,B (two ":") but record subscripts A only (one ":").
	;---------------------------------------------------------------------------
	new subs2   set subs2(1)=":",subs2(2)=":"
	new recsub2 set recsub2(1)=":"
	new ecodetest
	do
	. new $etrap set $etrap="goto err^"_$text(+0)
	. set ecodetest="" if $$XREFDATA^%YDBAIM(vgbl,.subs2,"^",23,1,,,,4,,,rgbl,.recsub2)
	do assert(ecodetest="U220","value subscripts must be a left-prefix of the record subscripts")
	;
	;---------------------------------------------------------------------------
	; 5. A record subscript may not sit below a level that varies and names nothing (U221).
	;    Level 2 is left unnamed, and level 3 is a record subscript.
	;---------------------------------------------------------------------------
	new recsub3 set recsub3(1)=":",recsub3(3)=":"
	new ecodetest
	do
	. new $etrap set $etrap="goto err^"_$text(+0)
	. set ecodetest="" if $$XREFDATA^%YDBAIM(vgbl,.subs,"^",23,1,,,,4,,,rgbl,.recsub3)
	do assert(ecodetest="U221","a record subscript below an unnamed level is rejected")
	;
	;---------------------------------------------------------------------------
	; 6. "*" is a synonym of ":" in a record subscript specification, as it is in xsub:
	;    both spell a record subscript, and the two spellings name the same cross reference.
	;---------------------------------------------------------------------------
	new recsub4 set recsub4(1)=":",recsub4(2)="*"
	new recsub4c set recsub4c(1)=":",recsub4c(2)=":"
	do assert($$XREFDATA^%YDBAIM(vgbl,.subs,"^",23,1,,,,4,,,rgbl,.recsub4)=$$XREFDATA^%YDBAIM(vgbl,.subs,"^",23,1,,,,4,,,rgbl,.recsub4c),"""*"" and "":"" in recsub name the same cross reference")
	;
	;---------------------------------------------------------------------------
	; 7. The depth must cover every named subscript and be an integer 1 through 29 (U223).
	;---------------------------------------------------------------------------
	new recsub5 set recsub5(1)=":",recsub5(2)=":",recsub5=1
	new ecodetest
	do
	. new $etrap set $etrap="goto err^"_$text(+0)
	. set ecodetest="" if $$XREFDATA^%YDBAIM(vgbl,.subs,"^",23,1,,,,4,,,rgbl,.recsub5)
	do assert(ecodetest="U223","a depth above the deepest named subscript is rejected")
	new recsub6 set recsub6(1)=":",recsub6=30
	new ecodetest
	do
	. new $etrap set $etrap="goto err^"_$text(+0)
	. set ecodetest="" if $$XREFDATA^%YDBAIM(vgbl,.subs,"^",23,1,,,,4,,,rgbl,.recsub6)
	do assert(ecodetest="U223","a depth beyond 29 is rejected")
	;
	quit
	;
t4shape1	; @TEST type=4 structure 1: the value node on a parent subscript of the record, same global (YDBAIM#96)
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	; The four structures of a value node relative to its record, by global and by key length (k
	; value subscripts against n record subscripts):
	;
	;   structure 1   value node ^G(A)       record ^G(A,B,...)   same global        k<n   this test
	;   structure 2   value node ^G(A,B,c)   record ^G(A,B)       same global        k=n   t4shape2, the Fileman structure
	;   structure 3   value node ^H(A,B)     record ^G(A,B,...)   different global   k=n   t4shape3
	;   structure 4   value node ^H(A)       record ^G(A,B,...)   different global   k<n   t4shape4
	;
	; Minimal %YDBAIM-level reproduction of YDBAIM#96, with the structure of the data of
	; YDBOcto's test TX21.
	;
	; Orders have lines: a record is a line, and lives at the deeper node
	; ^t4base(order_id,line_id,...). The customer is a value node at the PARENT subscript,
	; ^t4base(order_id), held once per order and shared by the order's lines:
	;
	;   ^t4base(1)="Alice"          ; customer for order 1 (parent node, ONE subscript)
	;   ^t4base(1,1,0)="Widget"     ; order 1, line 1 (a record)
	;   ^t4base(1,2,0)="Gadget"     ; order 1, line 2 (a record)
	;   ^t4base(2)="Bob"            ; customer for order 2
	;   ^t4base(2,1,0)="Gizmo"      ; order 2, line 1 (a record)
	;   ^t4base(3,1,0)="Sprocket"   ; order 3, line 1 -- but ^t4base(3) is ABSENT, so customer is ""
	;
	; Cross referencing the customer takes a record specification (type=4): the value lives
	; at the PARENT node ^t4base(order_id), while a record's existence is the RECORD node
	; ^t4base(order_id,line_id,0). AIM cross references each order's customer and, for
	; order 3 -- whose ^t4base(3) is ABSENT but which still has a record ^t4base(3,1,0) --
	; makes an empty-string entry (a missing node, exactly what the record specification is
	; for). The call passes:
	;
	;   value  spec: base="^t4base", subs(1)=":"                    (value subscripts = order_id)
	;   record spec: recgbl="^t4base", recsub(1)=":",(2)=":",(3)=0  (record subscripts = order_id,line_id)
	;   SET aimglobal=$$XREFDATA^%YDBAIM(base,.subs,"^",1,0,0,1,0,4,,,base,.recsub)
	;                                              type=4 is the 9th arg ^  ^ recgbl,recsub
	;
	new base set base="^"_$select(xdref:xdrefloc,1:"")_"t4base"
	;
	kill @base
	set @base@(1)="Alice",@base@(1,1,0)="Widget",@base@(1,2,0)="Gadget"
	set @base@(2)="Bob",@base@(2,1,0)="Gizmo"
	set @base@(3,1,0)="Sprocket"
	;
	new subs   set subs(1)=":"                              ; value node  ^t4base(order_id)
	new recsub set recsub(1)=":",recsub(2)=":",recsub(3)=0  ; record node ^t4base(order_id,line_id,0)
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(base,.subs,"^",1,0,0,1,0,4,,,base,.recsub)
	;
	; Orders 1 and 2 cross reference their customer; order 3 (^t4base(3) absent) cross references an empty-string entry.
	do assert($data(@aimgbl@(1,"Alice",1)),"order 1 customer Alice indexed")
	do assert($data(@aimgbl@(1,"Bob",2)),"order 2 customer Bob indexed")
	do assert($data(@aimgbl@(1,"",3)),"order 3 (record without a value node) cross referenced under the empty string")
	;
	; Exactly three value entries under piece 1 -- nothing spurious. In a
	; null-subscripts region $order seeded with "" skips the null-subscript node,
	; so count the empty-string entry (value subscript "") directly, then $order the
	; non-null value entries (this is the idiom used by the ttype1 test).
	new cnt,s,vk set cnt=0
	set vk="" for  set vk=$order(@aimgbl@(1,"",vk)) quit:'$zlength(vk)  set cnt=cnt+1
	set s="" for  set s=$order(@aimgbl@(1,s)) quit:'$zlength(s)  set vk="" for  set vk=$order(@aimgbl@(1,s,vk)) quit:'$zlength(vk)  set cnt=cnt+1
	do assert(cnt=3,"exactly three indexed orders (Alice, Bob, and order 3's "")")
	;
	do UNXREFDATA^%YDBAIM(base,.subs,"^",1,0,0,1,0,4,,base,.recsub)
	kill @base
	quit
	;
t4shape2	; @TEST type=4 structure 2: the Fileman structure, with a RANGE record subscript and a watched level below the record
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	; The record is a VALUELESS container node, kept in existence by whichever of its children
	; happen to exist:
	;
	;   ^t4pres4(1837)        the record      - no value of its own, $DATA is 10
	;   ^t4pres4(1837,1841)   the value node   - the indexed value node
	;   ^t4pres4(1837,42)     another field - not indexed, but it keeps the record in existence
	;
	; The record subscript is a RANGE, ":1900", the one range in a record specification in the
	; suite; the record specification is what xrefrng() parses for the record-specification types.
	; recsub(1)=":1900" with recsub=2 says "a record exists as long as ANY ^t4pres4(year,x)
	; exists". Level 1 is the record SUBSCRIPT (it pairs with the ":" in subs); level 2 varies
	; and names nothing.
	;
	; Note step 2: the record dies without the value node ever being touched, which is the
	; case a record specification pointing at ^t4pres4(year) alone cannot see, because a
	; valueless node is never the target of a SET or a ZKILL.
	;
	new g4 set g4="^"_$select(xdref:xdrefloc,1:"")_"t4pres4"
	kill @g4
	;
	new subs   set subs(1)=":1900",subs(2)=1841	; value node ^t4pres4(year,1841)
	new recsub set recsub(1)=":1900",recsub=2	; a record is any ^t4pres4(year,x)
	;
	set @g4@(1837,1841)="Van Buren",@g4@(1837,42)="other field"
	new x4 set x4=$$XREFDATA^%YDBAIM(g4,.subs,,,,,,,4,,,g4,.recsub)
	;
	do assert($data(@x4@(0,"Van Buren",1837)),"built -> indexed under the value")
	do assert('$data(@x4@(0,"",1837)),"built -> cross referenced under the value: not """)
	;
	; The value node goes, but the other field keeps the record in existence -> "", not gone.
	zkill @g4@(1837,1841)
	do assert($data(@x4@(0,"",1837)),"value node killed, other field remains")
	do assert('$data(@x4@(0,"Van Buren",1837)),"value node killed, other field remains: value entry gone")
	;
	; The last field goes: the record itself no longer exists, so it leaves the index.
	; Nothing touched the value node here -- only a sibling of it.
	zkill @g4@(1837,42)
	do assert('$data(@x4@(0,"",1837))&'$data(@x4@(0,"Van Buren",1837)),"last field killed -> record gone")
	;
	; A KILL of the whole record, rather than a field at a time.
	set @g4@(1837,1841)="Van Buren",@g4@(1837,42)="other field"
	do assert($data(@x4@(0,"Van Buren",1837)),"record restored")
	do assert('$data(@x4@(0,"",1837)),"record restored: not """)
	kill @g4@(1837)
	do assert('$data(@x4@(0,"",1837))&'$data(@x4@(0,"Van Buren",1837)),"record KILLed outright -> record gone")
	;
	; Removing a field that is NOT the value node leaves the value node indexed.
	set @g4@(1837,1841)="Van Buren",@g4@(1837,42)="other field"
	zkill @g4@(1837,42)
	do assert($data(@x4@(0,"Van Buren",1837)),"other field killed, value node intact")
	do assert('$data(@x4@(0,"",1837)),"other field killed, value node intact: not """)
	;
	; A record created by setting some field other than the value node -> "". (1885 is inside
	; the ":1900" key range; a year past 1900 would not be indexed by either type.)
	set @g4@(1885,42)="new record"
	do assert($data(@x4@(0,"",1885)),"record created via another field -> """)
	;
	;---------------------------------------------------------------------------
	; The key range applies to the record specification as well: a record outside ":1900" is
	; not indexed, whether it was already there when the cross reference was built
	; or arrives afterwards. The build walks the RECORD global, so it has to apply
	; the range itself -- the triggers get it for free, because YottaDB applies the
	; subscript specification to the trigger.
	;---------------------------------------------------------------------------
	set @g4@(1950,1841)="Out|Of|Range"
	do assert('$data(@x4@(0,"Out|Of|Range",1950)),"year past the range is not indexed")
	;
	; ...and the same on a rebuild, which is the path that walks the record global.
	do UNXREFDATA^%YDBAIM(g4,.subs,,,,,,,4,,g4,.recsub)
	new x4r set x4r=$$XREFDATA^%YDBAIM(g4,.subs,,,,,,,4,,,g4,.recsub)
	do assert('$data(@x4r@(0,"Out|Of|Range",1950)),"rebuild: year past the range is not indexed")
	do assert($data(@x4r@(0,"Van Buren",1837)),"rebuild: in-range record still indexed")
	;
	;---------------------------------------------------------------------------
	; A record can also be a record node carrying a VALUE and no children at all --
	; ^ORD(100.01,0)="ORDER STATUS^..." is the Fileman example. The watched level says a
	; record is any ^g(year,x), so on its own it would miss this one; the trigger pair on the
	; record node itself is what sees it. Both types must cross reference it under the empty string, since
	; it has no ^g(year,1841) value.
	;---------------------------------------------------------------------------
	set @g4@(1860)="record with a value and no fields"
	do assert($data(@x4r@(0,"",1860)),"record node with only a value -> """)
	;
	; Give it a value: it moves out of the empty-string entry like any other record.
	set @g4@(1860,1841)="Late|Comer"
	do assert($data(@x4r@(0,"Late|Comer",1860)),"value node added to that record")
	;
	; ZKILL the record node's own value. The value node is still there, so the record lives.
	zkill @g4@(1860)
	do assert($data(@x4r@(0,"Late|Comer",1860)),"record node value gone, value node remains")
	;
	; Now remove the value node too: nothing is left, so the record leaves the index.
	zkill @g4@(1860,1841)
	do assert('$data(@x4r@(0,"",1860)),"nothing left -> record gone")
	;
	; And a record that is ONLY a valued record node disappears when that value goes.
	set @g4@(1870)="only a value"
	do assert($data(@x4r@(0,"",1870)),"valued record node -> """)
	zkill @g4@(1870)
	do assert('$data(@x4r@(0,"",1870)),"its value removed -> record gone")
	;
	do UNXREFDATA^%YDBAIM(g4,.subs,,,,,,,4,,g4,.recsub)
	kill @g4
	quit
	;
t4shape3	; @TEST type=4 structure 3: the value node in a different global, value subscripts = full record subscripts
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	;   value node  ^cust(order_id,line_id)     <- the value node, one per record
	;   record node ^ord(order_id,line_id,0)    <- a record
	;
	; Here the value subscripts IS the record subscripts, so one record corresponds to each value subscripts,
	; and "is this the last record for the key" is just "does this record still exist".
	;
	new vgbl set vgbl="^"_$select(xdref:xdrefloc,1:"")_"cust"
	new rgbl set rgbl="^"_$select(xdref:xdrefloc,1:"")_"ord"
	kill @vgbl,@rgbl
	;
	set @vgbl@(1,1)="Alice",@vgbl@(1,2)="Bob"
	set @rgbl@(1,1,0)="Widget",@rgbl@(1,2,0)="Gadget"
	set @rgbl@(1,3,0)="Sprocket"		; no ^cust(1,3) -> ""
	;
	new subs   set subs(1)=":",subs(2)=":"                  ; value node  ^cust(order_id,line_id)
	new recsub set recsub(1)=":",recsub(2)=":",recsub(3)=0  ; record node ^ord(order_id,line_id,0)
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(vgbl,.subs,"^",1,0,0,1,0,4,,,rgbl,.recsub)
	;
	do assert($data(@aimgbl@(1,"Alice",1,1)),"record (1,1) = Alice")
	do assert($data(@aimgbl@(1,"Bob",1,2)),"record (1,2) = Bob")
	do assert('$data(@aimgbl@(1,"",1,1))&'$data(@aimgbl@(1,"",1,2)),"records (1,1) and (1,2) are not in the empty-string entry")
	do assert($data(@aimgbl@(1,"",1,3)),"record (1,3) has no value -> """)
	;
	;---------------------------------------------------------------------------
	; With k=n each record stands alone: killing one record removes exactly its entry.
	;---------------------------------------------------------------------------
	zkill @rgbl@(1,2,0)
	do assert('$data(@aimgbl@(1,"Bob",1,2)),"record (1,2) killed -> its entry gone")
	do assert('$data(@aimgbl@(1,"",1,2)),"...and no empty-string entry: the record is gone")
	do assert($data(@aimgbl@(1,"Alice",1,1)),"...and record (1,1) is untouched")
	;
	;---------------------------------------------------------------------------
	; A value change for one record does not disturb its neighbours.
	;---------------------------------------------------------------------------
	set @vgbl@(1,1)="Carol"
	do assert($data(@aimgbl@(1,"Carol",1,1)),"record (1,1) value changed -> Carol")
	do assert('$data(@aimgbl@(1,"",1,1)),"...and not under the empty string")
	do assert('$data(@aimgbl@(1,"Alice",1,1)),"...and Alice is gone")
	;
	;---------------------------------------------------------------------------
	; A new record with no value joins the empty-string entry.
	;---------------------------------------------------------------------------
	set @rgbl@(1,4,0)="Widget4"
	do assert($data(@aimgbl@(1,"",1,4)),"new record (1,4), no value -> """)
	;
	do UNXREFDATA^%YDBAIM(vgbl,.subs,"^",1,0,0,1,0,4,,rgbl,.recsub)
	kill @vgbl,@rgbl
	quit
	;
t4shape4	; @TEST type=4 structure 4: the value node in a DIFFERENT global, at the parent level
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	; The hardest of the four structures (see t4shape1): different global AND fewer record subscripts.
	;
	;   value node  ^cust(order_id)             <- the value node, in its own global
	;   record node ^ord(order_id,line_id,0)    <- a record, in another global
	;
	; Everything the same-global tests cover has to keep working when the two specifications
	; have nothing in common: the build must be driven by ^ord (walking ^cust would
	; both miss orders that have records but no customer, and invent orders that have a
	; customer but no records), and each side's triggers must reach across to the other
	; global to decide what the index should say.
	;
	new vgbl set vgbl="^"_$select(xdref:xdrefloc,1:"")_"cust"
	new rgbl set rgbl="^"_$select(xdref:xdrefloc,1:"")_"ord"
	kill @vgbl,@rgbl
	;
	; Orders 1 and 2 have a customer; order 3 has records but NO customer; customer 9
	; has no records at all and so must not appear in the index.
	set @vgbl@(1)="Alice",@vgbl@(2)="Bob",@vgbl@(9)="Nobody"
	set @rgbl@(1,1,0)="Widget",@rgbl@(1,2,0)="Gadget"
	set @rgbl@(2,1,0)="Gizmo"
	set @rgbl@(3,1,0)="Sprocket"
	;
	new subs   set subs(1)=":"                              ; value node  ^cust(order_id)
	new recsub set recsub(1)=":",recsub(2)=":",recsub(3)=0  ; record node ^ord(order_id,line_id,0)
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(vgbl,.subs,"^",1,0,0,1,0,4,,,rgbl,.recsub)
	;
	;---------------------------------------------------------------------------
	; The build is driven by the RECORD global, so it indexes exactly the orders
	; that have records -- including order 3 with no customer, and excluding customer 9.
	;---------------------------------------------------------------------------
	do assert($data(@aimgbl@(1,"Alice",1)),"order 1 = Alice")
	do assert($data(@aimgbl@(1,"Bob",2)),"order 2 = Bob")
	do assert('$data(@aimgbl@(1,"",1))&'$data(@aimgbl@(1,"",2)),"orders 1 and 2 are not in the empty-string entry")
	do assert($data(@aimgbl@(1,"",3)),"order 3 has records but no customer -> """)
	do assert('$data(@aimgbl@(1,"Nobody",9)),"customer 9 has no records -> not indexed")
	do assert('$data(@aimgbl@(1,"",9)),"...nor under the empty string")
	;
	;---------------------------------------------------------------------------
	; SET of the value node in the other global moves order 3 out of the empty-string entry.
	;---------------------------------------------------------------------------
	set @vgbl@(3)="Eve"
	do assert($data(@aimgbl@(1,"Eve",3)),"customer set -> order 3 = Eve")
	do assert('$data(@aimgbl@(1,"",3)),"...and order 3 left the empty-string entry")
	;
	;---------------------------------------------------------------------------
	; SET of a value node for a key with NO records must not invent an entry. The build
	; above already refuses customer 9; the SET trigger has to refuse it too, or the
	; two disagree and the index holds a value subscripts that no record backs. Writing the
	; value global cannot bring a record into existence when the record lives in
	; another global, so there is nothing here for the entry to describe.
	;---------------------------------------------------------------------------
	set @vgbl@(8)="Ghost"
	do assert('$data(@aimgbl@(1,"Ghost",8)),"customer 8 has no records -> SET must not index it")
	do assert('$data(@aimgbl@(1,"",8)),"...and no empty-string entry for it either")
	;
	;---------------------------------------------------------------------------
	; ZKILL of the value node: order 1 still has records in the OTHER global, so it
	; becomes an empty-string entry rather than disappearing. This is the cross-global @recex probe.
	;---------------------------------------------------------------------------
	zkill @vgbl@(1)
	do assert($data(@aimgbl@(1,"",1)),"customer killed but records remain -> order 1 = """)
	do assert('$data(@aimgbl@(1,"Alice",1)),"...and Alice is gone")
	;
	;---------------------------------------------------------------------------
	; ZKILL of the last record: order 2 leaves the index even though ^cust(2) is still
	; there. This is the cross-global read from the record side.
	;---------------------------------------------------------------------------
	zkill @rgbl@(2,1,0)
	do assert('$data(@aimgbl@(1,"Bob",2)),"last record gone -> order 2 not indexed")
	do assert('$data(@aimgbl@(1,"",2)),"...and no empty-string entry: order 2 has no record left")
	do assert($data(@vgbl@(2)),"...even though the customer node still exists")
	;
	;---------------------------------------------------------------------------
	; A record created for a customer that already exists picks up that customer's value.
	;---------------------------------------------------------------------------
	set @rgbl@(9,1,0)="Widget9"
	do assert($data(@aimgbl@(1,"Nobody",9)),"record created -> order 9 = Nobody")
	do assert('$data(@aimgbl@(1,"",9)),"...and not under the empty string")
	;
	;---------------------------------------------------------------------------
	; Recursive KILL up the RECORD tree removes the order; the customer is untouched.
	;---------------------------------------------------------------------------
	kill @rgbl@(1)
	do assert('$data(@aimgbl@(1,"",1)),"KILL up the record tree -> order 1 gone")
	;
	;---------------------------------------------------------------------------
	; Recursive KILL up the VALUE tree leaves the orders indexed, as NULLs, because
	; their records are in the other global and are still there.
	;---------------------------------------------------------------------------
	kill @vgbl
	do assert($data(@aimgbl@(1,"",3)),"KILL of the value global -> order 3 = """)
	do assert($data(@aimgbl@(1,"",9)),"KILL of the value global -> order 9 = """)
	do assert('$data(@aimgbl@(1,"Eve",3))&'$data(@aimgbl@(1,"Nobody",9)),"...and their value entries are gone")
	;
	do UNXREFDATA^%YDBAIM(vgbl,.subs,"^",1,0,0,1,0,4,,rgbl,.recsub)
	kill @vgbl,@rgbl
	quit
	;
t4vtrig	; @TEST type=4 value-side triggers: SET / ZKILL of the value node
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	; Builds the ^t4vtrig type=4 cross reference (value ^t4vtrig(order_id), record
	; ^t4vtrig(order_id,line_id,0)), then mutates the VALUE node and checks the index
	; tracks the change via triggers:
	;   - value ZKILL -> value entry removed, and an empty-string entry re-added iff a record survives
	;   - value SET on a key previously under the empty string -> the empty string replaced by the new value
	;   - value change -> old value entry replaced by the new one
	;
	new base set base="^"_$select(xdref:xdrefloc,1:"")_"t4vtrig"
	;
	kill @base
	set @base@(1)="Alice",@base@(1,1,0)="Widget",@base@(1,2,0)="Gadget"
	set @base@(2)="Bob",@base@(2,1,0)="Gizmo"
	set @base@(3,1,0)="Sprocket"
	;
	new subs   set subs(1)=":"                              ; value node  ^t4vtrig(order_id)
	new recsub set recsub(1)=":",recsub(2)=":",recsub(3)=0  ; record node ^t4vtrig(order_id,line_id,0)
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(base,.subs,"^",1,0,0,1,0,4,,,base,.recsub)
	;
	;---------------------------------------------------------------------------
	; Baseline from the bulk build: orders 1 and 2 valued, order 3 under the empty string.
	;---------------------------------------------------------------------------
	do assert($data(@aimgbl@(1,"Alice",1)),"baseline: order 1 = Alice")
	do assert($data(@aimgbl@(1,"Bob",2)),"baseline: order 2 = Bob")
	do assert('$data(@aimgbl@(1,"",1))&'$data(@aimgbl@(1,"",2)),"baseline: orders 1 and 2 are not in the empty-string entry")
	do assert($data(@aimgbl@(1,"",3)),"baseline: order 3 = """)
	;
	;---------------------------------------------------------------------------
	; ZKILL order 1's customer value. Record ^t4vtrig(1,1,0) still exists, so the value
	; entry is removed and order 1 becomes an empty-string entry.
	;---------------------------------------------------------------------------
	zkill @base@(1)
	do assert('$data(@aimgbl@(1,"Alice",1)),"after zkill: Alice entry removed")
	do assert($data(@aimgbl@(1,"",1)),"after zkill: order 1 now "" (record survives)")
	;
	;---------------------------------------------------------------------------
	; SET order 3's customer value. It was ""; now it has a value.
	;---------------------------------------------------------------------------
	set @base@(3)="Carol"
	do assert($data(@aimgbl@(1,"Carol",3)),"after set: order 3 = Carol")
	do assert('$data(@aimgbl@(1,"",3)),"after set: order 3 empty-string entry removed")
	;
	;---------------------------------------------------------------------------
	; Change order 2's customer value. Old value entry replaced by the new one.
	;---------------------------------------------------------------------------
	set @base@(2)="Bobby"
	do assert('$data(@aimgbl@(1,"Bob",2)),"after change: old Bob entry removed")
	do assert($data(@aimgbl@(1,"Bobby",2)),"after change: order 2 = Bobby")
	do assert('$data(@aimgbl@(1,"",2)),"after change: order 2 is not in the empty-string entry")
	;
	do UNXREFDATA^%YDBAIM(base,.subs,"^",1,0,0,1,0,4,,base,.recsub)
	kill @base
	quit
	;
t4rtrig	; @TEST type=4 record-side triggers: record creation / removal maintains the index
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	; Builds the ^t4rtrig type=4 cross reference, then writes and removes RECORD nodes
	; (^t4rtrig(order_id,line_id,0)) and checks the index tracks record creation and removal:
	;   - record created without a value node     -> its value subscripts cross referenced under the empty string
	;   - record created whose value node is present -> its value subscripts cross referenced under the value
	;   - last record with given value subscripts removed -> the entry for those value subscripts removed
	;   - a non-last record removed               -> the entry is retained
	;
	new base set base="^"_$select(xdref:xdrefloc,1:"")_"t4rtrig"
	;
	kill @base
	set @base@(1)="Alice",@base@(1,1,0)="Widget",@base@(1,2,0)="Gadget",@base@(1,3,0)="Doodad"
	set @base@(2)="Bob",@base@(2,1,0)="Gizmo"
	set @base@(3,1,0)="Sprocket"
	;
	new subs   set subs(1)=":"
	new recsub set recsub(1)=":",recsub(2)=":",recsub(3)=0
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(base,.subs,"^",1,0,0,1,0,4,,,base,.recsub)
	;
	;---------------------------------------------------------------------------
	; A new record without a value node (^t4rtrig(4,1,0), no ^t4rtrig(4) value) -> order 4 cross referenced under the empty string.
	;---------------------------------------------------------------------------
	set @base@(4,1,0)="Sprocket2"
	do assert($data(@aimgbl@(1,"",4)),"record without a value node born -> order 4 = """)
	;
	;---------------------------------------------------------------------------
	; A new record whose value node is present -> indexed under that value. (The value
	; is set first -- its own trigger indexes it; the record trigger confirms it.)
	;---------------------------------------------------------------------------
	set @base@(6)="Eve"
	do assert('$data(@aimgbl@(1,"Eve",6)),"a value with no record is not indexed")
	do assert('$data(@aimgbl@(1,"",6)),"...nor is it an empty-string entry: order 6 has no record yet")
	set @base@(6,1,0)="Widget6"
	do assert($data(@aimgbl@(1,"Eve",6)),"record created with value -> order 6 = Eve")
	do assert('$data(@aimgbl@(1,"",6)),"...and not under the empty string")
	;
	;---------------------------------------------------------------------------
	; Remove order 2's only record -> its value entry is removed.
	;---------------------------------------------------------------------------
	zkill @base@(2,1,0)
	do assert('$data(@aimgbl@(1,"Bob",2)),"last record of order 2 removed -> Bob entry gone")
	do assert('$data(@aimgbl@(1,"",2)),"...and no empty-string entry: order 2 has no record left")
	;
	;---------------------------------------------------------------------------
	; Order 1 has three records, so "is this the last one?" is exercised in all three
	; directions. That question is answered by looking for a surviving SIBLING (the
	; killed record still counts as present while its own trigger runs), so a record is
	; removed with siblings on both sides, then with a sibling only BEFORE it, and
	; only then the last one. The middle case is the one that catches a sibling
	; check that looks in just one direction.
	;---------------------------------------------------------------------------
	zkill @base@(1,2,0)
	do assert($data(@aimgbl@(1,"Alice",1)),"records 1 and 3 remain -> Alice retained")
	do assert('$data(@aimgbl@(1,"",1)),"...and order 1 is not in the empty-string entry")
	zkill @base@(1,3,0)
	do assert($data(@aimgbl@(1,"Alice",1)),"record 1 remains, before the killed record -> Alice retained")
	do assert('$data(@aimgbl@(1,"",1)),"...and order 1 is not in the empty-string entry")
	zkill @base@(1,1,0)
	do assert('$data(@aimgbl@(1,"Alice",1)),"order 1 last record removed -> Alice entry gone")
	do assert('$data(@aimgbl@(1,"",1)),"...and no empty-string entry: order 1 has no record left")
	;
	do UNXREFDATA^%YDBAIM(base,.subs,"^",1,0,0,1,0,4,,base,.recsub)
	kill @base
	quit
	;
t4zknull	; @TEST type=4 ZKILL: an empty-string entry is left behind only when the record really survives
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	; The value-side ZKILL template ends by putting the key in the empty-string entry IF a record
	; still exists for it once the node being killed has lost its value. A record is a record
	; node that has a value, or a node with a value below it within the depth.
	;
	; A YottaDB ZKILL trigger runs BEFORE the node is removed, so the node being killed is
	; still there while the trigger looks, and must not be counted. Where the value node
	; IS the record node - any value node that is a piece of the record node, which is the structure
	; a global variable of delimited nodes produces - the ZKILL takes the record's own value, and
	; the record survives only through nodes below it within the depth. With the depth at
	; the record node itself, nothing below it counts, and every such ZKILL ends the record.
	;
	; Case A and case B are that structure, through the two different templates - pieces
	; (tt4ZKp0) and the whole node (tt4ZKe0). Case C is the contrasting structure, the value
	; node BELOW the record node, with a watched level: the record node is a different
	; node that the ZKILL does not touch, and a sibling within the depth keeps the record.
	;
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"t4zk"
	new subs,recsub,aimgbl
	;
	;---------------------------------------------------------------------------
	; A. Value node IS the record node, indexed as "^" pieces.
	;    Record 1 has a value and nothing else; record 2 has a value and a child, which lies
	;    below the depth and so does not keep the record.
	;---------------------------------------------------------------------------
	kill @g
	set @g@(1)="v1^x"
	set @g@(2)="v2^x",@g@(2,"NOTE")="keep"
	kill subs,recsub set subs(1)=":",recsub(1)=":"
	set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,0,4,,,g,.recsub)
	do assert($data(@aimgbl@(1,"v1",1)),"A: record 1 indexed under its value")
	do assert($data(@aimgbl@(1,"v2",2)),"A: record 2 indexed under its value")
	do assert('$data(@aimgbl@(1,"",1))&'$data(@aimgbl@(1,"",2)),"A: neither record is in the empty-string entry")
	;
	zkill @g@(1)
	do assert('$data(@aimgbl@(1,"v1",1)),"A: zkilled record 1 loses its value entry")
	do assert('$data(@aimgbl@(1,"",1)),"A: ...and gains NO empty-string entry - the record went with it")
	;
	zkill @g@(2)
	do assert('$data(@aimgbl@(1,"v2",2)),"A: zkilled record 2 loses its value entry")
	do assert('$data(@aimgbl@(1,"",2)),"A: ...and gains NO empty-string entry either - its child lies below the depth")
	do t4rebld(.subs,.recsub,g,g,aimgbl,"A, after both records were zkilled")
	do UNXREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,0,4,,g,.recsub)
	;
	;---------------------------------------------------------------------------
	; B. The same structure indexed as a whole node, which uses the other template.
	;---------------------------------------------------------------------------
	kill @g
	set @g@(1)="solo"
	set @g@(2)="pair",@g@(2,"NOTE")="keep"
	kill subs,recsub set subs(1)=":",recsub(1)=":"
	set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"",,0,0,1,0,4,,,g,.recsub)
	do assert($data(@aimgbl@(0,"solo",1)),"B: record 1 indexed under its value")
	do assert($data(@aimgbl@(0,"pair",2)),"B: record 2 indexed under its value")
	do assert('$data(@aimgbl@(0,"",1))&'$data(@aimgbl@(0,"",2)),"B: neither record is in the empty-string entry")
	;
	zkill @g@(1)
	do assert('$data(@aimgbl@(0,"solo",1)),"B: zkilled record 1 loses its value entry")
	do assert('$data(@aimgbl@(0,"",1)),"B: ...and gains NO empty-string entry - the record went with it")
	;
	zkill @g@(2)
	do assert('$data(@aimgbl@(0,"pair",2)),"B: zkilled record 2 loses its value entry")
	do assert('$data(@aimgbl@(0,"",2)),"B: ...and gains NO empty-string entry either - its child lies below the depth")
	do UNXREFDATA^%YDBAIM(g,.subs,"",,0,0,1,0,4,,g,.recsub)
	;
	;---------------------------------------------------------------------------
	; C. The contrasting structure: the value hangs BELOW the record, specification ^t4zk(:,*).
	;    Here the ZKILL removes a node that is not the record node, and level 2 is
	;    watched, so the sibling that keeps record 2 in existence is visible to a trigger.
	;---------------------------------------------------------------------------
	kill @g
	set @g@(1,0)="v1^x"
	set @g@(2,0)="v2^x",@g@(2,"NOTE")="keep"
	kill subs,recsub set subs(1)=":",subs(2)=0,recsub(1)=":",recsub=2
	set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,0,4,,,g,.recsub)
	do assert($data(@aimgbl@(1,"v1",1)),"C: record 1 indexed under its value")
	do assert($data(@aimgbl@(1,"v2",2)),"C: record 2 indexed under its value")
	do assert('$data(@aimgbl@(1,"",1))&'$data(@aimgbl@(1,"",2)),"C: neither record is in the empty-string entry")
	;
	zkill @g@(1,0)
	do assert('$data(@aimgbl@(1,"v1",1)),"C: zkilled record 1 loses its value entry")
	do assert('$data(@aimgbl@(1,"",1)),"C: ...and gains no empty-string entry - that was its only node")
	;
	zkill @g@(2,0)
	do assert('$data(@aimgbl@(1,"v2",2)),"C: zkilled record 2 loses its value entry")
	do assert($data(@aimgbl@(1,"",2)),"C: record 2 keeps an empty-string entry - its sibling holds the record open")
	do t4rebld(.subs,.recsub,g,g,aimgbl,"C, after both value nodes were zkilled")
	;
	; And when that sibling goes, so does the record. In cases A and B the node that went
	; was the record node itself; here the record node is untouched, and it is the
	; watched level 2 that lets a trigger see the sibling go.
	zkill @g@(2,"NOTE")
	do assert('$data(@aimgbl@(1,"",2)),"C: last node gone -> the empty-string entry is retired")
	do t4rebld(.subs,.recsub,g,g,aimgbl,"C, at the end")
	do UNXREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,0,4,,g,.recsub)
	;
	kill @g
	quit
	;
t4setnull	; @TEST type=4 SET of the value node: a record created with an EMPTY piece gets its empty-string entry; two pieces share one cross reference
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	; The value-side SET trigger loops over the pieces cross referenced (for i=<pieces>) and
	; compares each piece's old and new value, p and q. A changed piece moves the key's entry
	; from p to q. An unchanged piece needs nothing - except when it is empty: that is a node
	; being CREATED with that piece empty, which makes the record an empty-string entry for the piece. No record
	; side trigger fires for the value node (it is the value side's own), so the trigger's
	; else branch is the only place that empty-string entry can come from.
	;
	; ^t4sn(id,"V")="lo^hi": two pieces of one node in one cross reference, so the loop is
	; for i=1,2 (pieces "1;2"), and the record is ^t4sn(id) with depth 2.
	;
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"t4sn"
	new subs,recsub,aimgbl
	kill @g
	set @g@(1,"V")="apple^red"
	set @g@(2,"V")="^green"						; piece 1 empty from the start: the bulk build's empty-string entry
	kill subs,recsub set subs(1)=":",subs(2)="""V""",recsub(1)=":",recsub=2
	set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^","1;2",0,0,1,0,4,,,g,.recsub)
	do assert($data(@aimgbl@(1,"apple",1))&$data(@aimgbl@(2,"red",1)),"build: record 1 indexed under both pieces")
	do assert($data(@aimgbl@(1,"",2))&$data(@aimgbl@(2,"green",2)),"build: record 2 is "" for piece 1 and green for piece 2")
	;
	; A record created by a SET of the value node with piece 1 empty: p=q="" for piece 1, so only
	; the else branch can cross reference it under the empty string; piece 2 takes the changed-piece path.
	set @g@(3,"V")="^blue"
	do assert($data(@aimgbl@(1,"",3)),"born with piece 1 empty: record 3 is in piece 1's empty-string entry")
	do assert($data(@aimgbl@(2,"blue",3)),"born with piece 1 empty: record 3 indexed under blue for piece 2")
	;
	; A record created with BOTH pieces empty: both go through the else branch.
	set @g@(4,"V")=""
	do assert($data(@aimgbl@(1,"",4))&$data(@aimgbl@(2,"",4)),"born empty: record 4 is in both empty-string entries")
	;
	; An unchanged non-empty piece is left alone while the other piece moves.
	set @g@(1,"V")="apple^ripe"
	do assert($data(@aimgbl@(1,"apple",1)),"piece 1 unchanged: record 1 keeps its apple entry")
	do assert('$data(@aimgbl@(2,"red",1))&$data(@aimgbl@(2,"ripe",1)),"piece 2 changed: red replaced by ripe")
	;
	; A empty piece given a value leaves the empty-string entry.
	set @g@(3,"V")="pear^blue"
	do assert('$data(@aimgbl@(1,"",3))&$data(@aimgbl@(1,"pear",3)),"piece 1 valued: record 3 leaves the empty-string entry for pear")
	do t4rebld(.subs,.recsub,g,g,aimgbl,"after the SETs")
	;
	; ZKILL of a record's only node, through the two-piece ZKILL loop: every entry of the record
	; goes, and since the record goes with its node no empty-string entry is left behind.
	zkill @g@(4,"V")
	do assert('$data(@aimgbl@(1,"",4))&'$data(@aimgbl@(2,"",4)),"only node zkilled: record 4 has no entry under either piece")
	do t4rebld(.subs,.recsub,g,g,aimgbl,"at the end")
	do UNXREFDATA^%YDBAIM(g,.subs,"^","1;2",0,0,1,0,4,,g,.recsub)
	kill @g
	quit
	;
t4rkill	; @TEST type=4 recursive KILL: a KILL above either node decomposes into ZKILLs
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	; A KILL of a subtree does not fire the SET/ZKILL triggers that maintain the
	; index, so AIM places a recursive-KILL trigger (rk0/rk1) at every level above
	; the nodes it watches. That trigger walks the subtree depth first and ZKILLs
	; each node, which is what the index maintenance actually sees. type=4 has two
	; trees to protect: the value node's (on gbl) and the record node's (on recgbl).
	; Here both are ^t4rkill, which also exercises the "r" trigger-name prefix that keeps
	; the record side's KILL triggers from colliding with the value side's.
	;
	;   value node  ^t4rkill(order_id)             <- the value node
	;   record node ^t4rkill(order_id,line_id,0)   <- a record
	;
	new base set base="^"_$select(xdref:xdrefloc,1:"")_"t4rkill"
	;
	kill @base
	set @base@(1)="Alice",@base@(1,1,0)="Widget",@base@(1,2,0)="Gadget"
	set @base@(2)="Bob",@base@(2,1,0)="Gizmo"
	set @base@(3,1,0)="Sprocket"
	;
	new subs   set subs(1)=":"
	new recsub set recsub(1)=":",recsub(2)=":",recsub(3)=0
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(base,.subs,"^",1,0,0,1,0,4,,,base,.recsub)
	;
	; The build indexed all three orders: 1 and 2 by value, 3 (no value node) under the empty string.
	do assert($data(@aimgbl@(1,"Alice",1)),"built: order 1 = Alice")
	do assert($data(@aimgbl@(1,"Bob",2)),"built: order 2 = Bob")
	do assert('$data(@aimgbl@(1,"",1))&'$data(@aimgbl@(1,"",2)),"built: orders 1 and 2 are not in the empty-string entry")
	do assert($data(@aimgbl@(1,"",3)),"built: order 3 = "" (record without a value node)")
	;
	;---------------------------------------------------------------------------
	; KILL one level ABOVE the record node (^t4rkill(2,1), not ^t4rkill(2,1,0)). This is
	; order 2's only record, so its entry goes away.
	;---------------------------------------------------------------------------
	kill @base@(2,1)
	do assert('$data(@aimgbl@(1,"Bob",2)),"KILL of the record above the record node -> Bob entry gone")
	do assert('$data(@aimgbl@(1,"",2)),"...and no empty-string entry: order 2 has no record left")
	;
	;---------------------------------------------------------------------------
	; KILL the record's subtree subtree -> its empty-string entry goes away too.
	;---------------------------------------------------------------------------
	kill @base@(3,1)
	do assert('$data(@aimgbl@(1,"",3)),"KILL of the record's subtree -> empty-string entry gone")
	;
	;---------------------------------------------------------------------------
	; KILL the value node's whole subtree: ^t4rkill(1) is both the value AND the
	; ancestor of order 1's two records, so this removes the value and both records.
	;---------------------------------------------------------------------------
	kill @base@(1)
	do assert('$data(@aimgbl@(1,"Alice",1)),"KILL of value node + its records -> Alice entry gone")
	do assert('$data(@aimgbl@(1,"",1)),"...and order 1 is not left behind as an empty-string entry")
	;
	;---------------------------------------------------------------------------
	; Nothing is left, so piece 1 of the index has no entries under it. Note that
	; the test is for descendants ($data\10) rather than for the node: @aimgbl@(1)
	; also carries metadata (it holds nsubs), so it always exists.
	;---------------------------------------------------------------------------
	do assert('($data(@aimgbl@(1))\10),"all records killed -> index has no entries")
	;
	;---------------------------------------------------------------------------
	; Fresh data, then a KILL of the entire global, which fires the triggers placed
	; on the bare global name.
	;---------------------------------------------------------------------------
	set @base@(7)="Zed",@base@(7,1,0)="Widget7"
	do assert($data(@aimgbl@(1,"Zed",7)),"new order 7 indexed = Zed")
	do assert('$data(@aimgbl@(1,"",7)),"...and not under the empty string")
	kill @base
	do assert('($data(@aimgbl@(1))\10),"KILL of the whole global -> index has no entries")
	;
	do UNXREFDATA^%YDBAIM(base,.subs,"^",1,0,0,1,0,4,,base,.recsub)
	kill @base
	quit
	;
t4stat	; @TEST type=4 statistics: stat=0/1/2 counts survive every kind of transition
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	; stat=1 keeps @aimgbl@(-1,value) = how many value subscripts carry that value; stat=2
	; adds @aimgbl@(-1) = how many distinct values there are. The bulk build has always
	; written those nodes, so the thing under test is that all four maintenance
	; triggers keep them right afterwards. The same five transitions run at each stat
	; level, and at stat=0 no statistics nodes may appear at all.
	;
	;   value node  ^t4stat(order_id)             <- the value node
	;   record node ^t4stat(order_id,line_id,0)   <- a record
	;
	new base set base="^"_$select(xdref:xdrefloc,1:"")_"t4stat"
	;
	;
	new stat for stat=0:1:2 do
	. kill @base
	. set @base@(1)="Alice",@base@(1,1,0)="Widget",@base@(1,2,0)="Gadget"
	. set @base@(2)="Bob",@base@(2,1,0)="Gizmo"
	. set @base@(3,1,0)="Sprocket"
	. ;
	. new subs   set subs(1)=":"
	. new recsub set recsub(1)=":",recsub(2)=":",recsub(3)=0
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(base,.subs,"^",1,0,0,1,stat,4,,,base,.recsub)
	. new p set p="stat="_stat_": "		; prefix so a failure says which level
	. ;
	. ;-------------------------------------------------------------------------
	. ; After the build: Alice, Bob, and one "" (order 3 has a record but no value).
	. ;-------------------------------------------------------------------------
	. do t4chk(aimgbl,stat,p_"built","Alice",1,"Bob",1,"",1,3)
	. ;
	. ;-------------------------------------------------------------------------
	. ; Value SET, changing a value: Bob gives way to Carol, still 3 distinct.
	. ;-------------------------------------------------------------------------
	. set @base@(2)="Carol"
	. do t4chk(aimgbl,stat,p_"value changed","Alice",1,"Carol",1,"",1,3)
	. ;
	. ;-------------------------------------------------------------------------
	. ; Value ZKILL where records survive: order 1 becomes an empty-string entry rather than vanishing,
	. ; so the empty-string count rises to 2 and Alice is gone -- 2 distinct values left.
	. ;-------------------------------------------------------------------------
	. zkill @base@(1)
	. do t4chk(aimgbl,stat,p_"value killed, records remain","Alice",0,"Carol",1,"",2,2)
	. ;
	. ;-------------------------------------------------------------------------
	. ; Record ZKILL of the last record for a value subscripts: order 2 leaves the index
	. ; altogether, taking Carol's count with it.
	. ;-------------------------------------------------------------------------
	. zkill @base@(2,1,0)
	. do t4chk(aimgbl,stat,p_"last record removed","Alice",0,"Carol",0,"",2,1)
	. ;
	. ;-------------------------------------------------------------------------
	. ; Record SET creating an record without a value node: order 5 joins the empty-string entry, and the
	. ; distinct count does not move because the empty string was already represented.
	. ;-------------------------------------------------------------------------
	. set @base@(5,1,0)="Widget5"
	. do t4chk(aimgbl,stat,p_"record without a value node born","Alice",0,"Carol",0,"",3,1)
	. ;
	. do UNXREFDATA^%YDBAIM(base,.subs,"^",1,0,0,1,stat,4,,base,.recsub)
	;
	;---------------------------------------------------------------------------
	; The same exercise against a record specification one level deeper than its named subscripts,
	; where a record is the record node itself and any child of it keeps the record in existence. That structure has TWO record
	; trigger pairs -- one watching the children, one watching the record node --
	; and both can reach the same count node, so the statistics have to be checked
	; here in their own right rather than inherited from the ":" case.
	;
	;   value node  ^t4stat(key,"col")   <- the value node
	;   record node ^t4stat(key)         <- a record: its own value, or any child
	;
	; Records 1 and 2 have a value, record 3 exists only through another field, and
	; record 4 exists only by carrying a value itself -- the structure that needs the second
	; trigger pair to be seen at all.
	;---------------------------------------------------------------------------
	new wstat for wstat=0:1:2 do
	. kill @base
	. set @base@(1,"col")="Alice^x",@base@(1,"other")="keep"
	. set @base@(2,"col")="Bob^y"
	. set @base@(3,"other")="junk"
	. set @base@(4)="a record that is only a value"
	. ;
	. new subs   set subs(1)=":",subs(2)="""col"""
	. new recsub set recsub(1)=":",recsub=2
	. new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(base,.subs,"^",1,0,0,1,wstat,4,,,base,.recsub)
	. new p set p="watched level, stat="_wstat_": "
	. ;
	. ; The build sees all four records: two with a value, two as NULLs.
	. do assert($data(@aimgbl@(1,"Alice",1)),p_"record 1 = Alice")
	. do assert($data(@aimgbl@(1,"Bob",2)),p_"record 2 = Bob")
	. do assert($data(@aimgbl@(1,"",3)),p_"record 3 (other field only) = """)
	. do assert($data(@aimgbl@(1,"",4)),p_"record 4 (record node value only) = """)
	. do t4chk(aimgbl,wstat,p_"built","Alice",1,"Bob",1,"",2,3)
	. ;
	. ; A value change, through the value node's own trigger.
	. set @base@(2,"col")="Carol^z"
	. do t4chk(aimgbl,wstat,p_"value changed","Bob",0,"Carol",1,"",2,3)
	. ;
	. ; The value node goes but the record lives on through its other field, so record 1 moves
	. ; into the empty-string entry instead of leaving: Alice's count goes, the empty string's grows.
	. zkill @base@(1,"col")
	. do assert($data(@aimgbl@(1,"",1)),p_"value node killed, other field remains -> """)
	. do t4chk(aimgbl,wstat,p_"value node killed, record lives","Alice",0,"Carol",1,"",3,2)
	. ;
	. ; Removing record 3's only field ends the record -- the child trigger's work.
	. zkill @base@(3,"other")
	. do assert('$data(@aimgbl@(1,"",3)),p_"last field killed -> record 3 gone")
	. do t4chk(aimgbl,wstat,p_"last field killed","Alice",0,"Carol",1,"",2,2)
	. ;
	. ; A record created by giving a record node a value -- the record node trigger's work.
	. set @base@(5)="a new record"
	. do assert($data(@aimgbl@(1,"",5)),p_"record node given a value -> record 5 = """)
	. do t4chk(aimgbl,wstat,p_"record created as a valued record node","Alice",0,"Carol",1,"",3,2)
	. ;
	. ; ...and taking that value away ends it again, since it has no children.
	. zkill @base@(4)
	. do assert('$data(@aimgbl@(1,"",4)),p_"record node value removed -> record 4 gone")
	. do t4chk(aimgbl,wstat,p_"valued record node emptied","Alice",0,"Carol",1,"",2,2)
	. ;
	. do UNXREFDATA^%YDBAIM(base,.subs,"^",1,0,0,1,wstat,4,,base,.recsub)
	;
	kill @base
	quit
	;
t4chk(aimgbl,stat,msg,v1,c1,v2,c2,v3,c3,distinct)	; [t4stat] Check the statistics nodes
	; Asserts the count for each of three values (a count of 0 meaning the node must
	; not exist) and, for stat=2, the number of distinct values. At stat=0 it instead
	; asserts that no statistics node was created at all.
	if 'stat  do assert('$data(@aimgbl@(-1)),msg_" - stat=0 writes no statistics nodes")  quit
	do assert($get(@aimgbl@(-1,v1),0)=c1,msg_" - count of """_v1_""" is "_c1)
	do assert($get(@aimgbl@(-1,v2),0)=c2,msg_" - count of """_v2_""" is "_c2)
	do assert($get(@aimgbl@(-1,v3),0)=c3,msg_" - count of """_v3_""" is "_c3)
	do:2=stat assert($get(@aimgbl@(-1),0)=distinct,msg_" - "_distinct_" distinct values")
	quit
	;
t4cnsub	; @TEST type=4: a numeric constant in the value path does not corrupt the record triggers
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	; The record-side triggers are the only ones that have to name the value node in full.
	; They fire on the record node and read the value through it, so their trigger code carry the
	; whole path, constants and all. Where a numeric constant sits immediately before the
	; key subscript the finished trigger code contains the text
	;
	;   ^t4oe(100,sub2,3)
	;
	; and "(100,sub2," is character for character the structure of
	;
	;   @name(@locsnum,sub@locsnum,@sub)
	;
	; that exptempl: strips a repeated subscript out of - for XREFSUB() only. That cleanup
	; was guarded by nothing but the pattern, and it assumed a "$data(" stood in front of
	; the match. With no "$data(" to split on it appended "$data(,sub" to the trigger code and
	; threw away the rest, so $ZTRIGGER() rejected the trigger (%YDB-E-SPOREOL) and
	; XREFDATA() came back with SETZTRIGGERFAIL. No other type puts a path into the source
	; global in a trigger code, which is why nothing before type=4 ever reached it.
	;
	; The layout is a Fileman one: records are ^t4oe(100,ien) and the value nodes are "^" pieces of
	; the node one level below them, ^t4oe(100,ien,3).
	;
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"t4oe"
	kill @g
	set @g@(100,1,3)="5^Alpha"
	set @g@(100,2,3)="6^Beta"
	set @g@(100,3,3)="^Gamma"		; piece 1 is empty
	;
	new subs   set subs(1)=100,subs(2)=":",subs(3)=3
	new recsub set recsub(1)=100,recsub(2)=":",recsub=3	; depth 3: level 3 varies and is watched, not a record subscript
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,0,4,,,g,.recsub)
	;
	;---------------------------------------------------------------------------
	; Getting this far at all is the regression - the build used to fail outright.
	; Slots 100+lvl hold the record-side SET triggers, one per watched level, and
	; recplast..recnsubs is 2..3 here: the record node and the level below it.
	;---------------------------------------------------------------------------
	do assert('$find(@aimgbl@(102),"$data(,sub"),"record SET trigger on the record node is intact")
	do assert('$find(@aimgbl@(103),"$data(,sub"),"record SET trigger one level below is intact")
	;
	do assert($data(@aimgbl@(1,"5",1)),"order 1 -> 5")
	do assert($data(@aimgbl@(1,"6",2)),"order 2 -> 6")
	do assert('$data(@aimgbl@(1,"",1))&'$data(@aimgbl@(1,"",2)),"orders 1 and 2 are not in the empty-string entry")
	do assert($data(@aimgbl@(1,"",3)),"order 3 has an empty piece 1 -> """)
	;
	;---------------------------------------------------------------------------
	; The record-side SET trigger - the corrupt one - is what indexes a record that has
	; no value node at all. Neither write below touches ^t4oe(100,ien,3), so nothing
	; but the record side can produce these two entries.
	;---------------------------------------------------------------------------
	set @g@(100,4,1)="a node that is not the value node"
	do assert($data(@aimgbl@(1,"",4)),"record created one level below the record node -> """)
	set @g@(100,5)="a valued, childless record node"
	do assert($data(@aimgbl@(1,"",5)),"record created on the record node itself -> """)
	do t4rebld(.subs,.recsub,g,g,aimgbl,"after records were born with no value node")
	;
	;---------------------------------------------------------------------------
	; And the record-side ZKILL retires them again.
	;---------------------------------------------------------------------------
	zkill @g@(100,4,1)
	do assert('$data(@aimgbl@(1,"",4)),"that record taken away again -> entry gone")
	zkill @g@(100,5)
	do assert('$data(@aimgbl@(1,"",5)),"record node emptied, nothing below it -> entry gone")
	do t4rebld(.subs,.recsub,g,g,aimgbl,"after both records were removed")
	;
	;---------------------------------------------------------------------------
	; The value side reaches the same entries through the same path: a value node stored
	; where a record already exists moves it out of the empty-string entry.
	;---------------------------------------------------------------------------
	set @g@(100,6)="a record with no value node yet"
	do assert($data(@aimgbl@(1,"",6)),"a further record -> """)
	set @g@(100,6,3)="9^Zeta"
	do assert($data(@aimgbl@(1,"9",6)),"...given a value node -> 9")
	do assert('$data(@aimgbl@(1,"",6)),"...and its empty-string entry is gone")
	do t4rebld(.subs,.recsub,g,g,aimgbl,"at the end")
	;
	do UNXREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,0,4,,g,.recsub)
	kill @g
	quit
	;
t4omitfix	; @TEST type=4 with omitfix=0: the record side's entries carry the fixed subscripts too
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	; With omitfix=0 an index entry keeps the value node's constant subscripts. Every
	; writer of an entry has to agree on its structure: the build, the value node's own
	; triggers, and the record node's triggers, which bind the RECORD node's subscripts
	; and so have to spell the constants out themselves.
	;
	;   value node  ^t4omv(100,id,7)   <- the value node, constants 100 and 7
	;   record node ^t4omr(id)         <- a record
	;   entry       (1,value,100,id,7)
	;
	new vgbl set vgbl="^"_$select(xdref:xdrefloc,1:"")_"t4omv"
	new rgbl set rgbl="^"_$select(xdref:xdrefloc,1:"")_"t4omr"
	kill @vgbl,@rgbl
	set @rgbl@(1)="",@rgbl@(2)="",@vgbl@(100,1,7)="Alice^x"
	;
	new subs   set subs(1)=100,subs(2)=":",subs(3)=7
	new recsub set recsub(1)=":",recsub=1
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(vgbl,.subs,"^",1,0,0,0,0,4,,,rgbl,.recsub)
	do assert($data(@aimgbl@(1,"Alice",100,1,7)),"build: record 1 under its value, constants kept")
	do assert($data(@aimgbl@(1,"",100,2,7)),"build: record 2 is "", constants kept")
	do assert('$data(@aimgbl@(1,"Alice",1)),"build: no entry without the constants")
	;
	set @vgbl@(100,2,7)="Carol^y"
	do assert($data(@aimgbl@(1,"Carol",100,2,7)),"value SET: record 2 under its value")
	do assert('$data(@aimgbl@(1,"",100,2,7)),"value SET: ...and out of the empty-string entry")
	zkill @vgbl@(100,2,7)
	do assert($data(@aimgbl@(1,"",100,2,7)),"value ZKILL: record 2 back in the empty-string entry")
	do assert('$data(@aimgbl@(1,"Carol",100,2,7)),"value ZKILL: ...and its value entry gone")
	;
	set @vgbl@(100,3,7)="Bob^z",@rgbl@(3)=""
	do assert($data(@aimgbl@(1,"Bob",100,3,7)),"record SET: record 3 under its value, constants kept")
	do assert('$data(@aimgbl@(1,"Bob",3)),"record SET: no entry without the constants")
	zkill @rgbl@(1)
	do assert('$data(@aimgbl@(1,"Alice",100,1,7)),"record ZKILL: record 1's entry gone")
	do assert('$data(@aimgbl@(1,"",100,1,7)),"record ZKILL: ...and not "" either")
	;
	; The triggers and a fresh build agree.
	new before merge before=@aimgbl
	do UNXREFDATA^%YDBAIM(vgbl,.subs,"^",1,0,0,0,0,4,,rgbl,.recsub)
	set aimgbl=$$XREFDATA^%YDBAIM(vgbl,.subs,"^",1,0,0,0,0,4,,,rgbl,.recsub)
	do assert($$t4same(.before,aimgbl),"triggered index matches a fresh rebuild")
	;
	do UNXREFDATA^%YDBAIM(vgbl,.subs,"^",1,0,0,0,0,4,,rgbl,.recsub)
	kill @vgbl,@rgbl
	quit
	;
t4rngstart	; @TEST type=4 with a RANGE record subscript: the START key is scanned without an $ORDER, so it is the one key that may not exist
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	; A bounded range is scanned from its START key, which the scan visits directly rather
	; than reaching by $ORDER, so it is the one key that need not exist. The scan used to
	; ask $QUERY from it regardless: with records beyond the START, $QUERY answered with the
	; successor, which is no descendant, and the START was indexed as a record without a value node; with no
	; record beyond it, $QUERY answered "" and $QLENGTH of that killed the scan (NOCANONICNAME).
	; The triggers test for a record's existence with $ORDER and never made such a record, so the index and a
	; rebuild of it disagreed - which is how Octo's TX27 found this.
	;
	;   value node  ^t4rng(id,0)   <- the value node
	;   record node ^t4rng(id)     <- a record; ids from 0 up, so recsub(1)="0:"
	;
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"t4rng"
	kill @g
	set @g@(1,0)="mango^mango",@g@(2,0)="apple^apple",@g@(3,0)="banana^banana"
	new subs   set subs(1)="0:",subs(2)=0
	new recsub set recsub(1)="0:",recsub=2
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,0,4,,,g,.recsub)
	do assert('$data(@aimgbl@(1,"",0)),"build: the absent START key 0 is not a record without a value node")
	do assert($data(@aimgbl@(1,"mango",1))&$data(@aimgbl@(1,"apple",2))&$data(@aimgbl@(1,"banana",3)),"build: the three records are indexed")
	do assert(3=$$t4nent(aimgbl),"build: and nothing else is")
	;
	; Key 0 is a record only while something is there, and the triggers must agree with that.
	set @g@(0,0)="aaaa^zzzz"
	do assert($data(@aimgbl@(1,"aaaa",0)),"value SET at the START key: record 0 under its value")
	zkill @g@(0,0)
	do assert('$data(@aimgbl@(1,"aaaa",0))&'$data(@aimgbl@(1,"",0)),"value ZKILL: record 0 gone, not "": nothing is left at key 0")
	set @g@(0)="header"
	do assert($data(@aimgbl@(1,"",0)),"record SET: a value on the record node alone makes record 0, an empty-string entry")
	kill @g@(0)
	do assert('$data(@aimgbl@(1,"",0)),"record KILL: record 0 gone again")
	do assert(3=$$t4nent(aimgbl),"after the round trip, the three records and nothing else")
	do t4rebld(.subs,.recsub,g,g,aimgbl,"A, START key absent, records beyond it")
	;
	; A START key beyond every record: nothing to scan, and no error.
	kill subs,recsub set subs(1)="5:",subs(2)=0,recsub(1)="5:",recsub=2
	new aimgbl2 set aimgbl2=$$XREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,0,4,,,g,.recsub)
	do assert(0=$$t4nent(aimgbl2),"build from a START key beyond every record: an empty index")
	set @g@(7,0)="kiwi^kiwi"
	do assert($data(@aimgbl2@(1,"kiwi",7)),"...and a record beyond the START is indexed by the trigger")
	do assert($data(@aimgbl@(1,"kiwi",7)),"...and by the first index's trigger, whose range 0: holds it too")
	do t4rebld(.subs,.recsub,g,g,aimgbl2,"B, START key beyond every record")
	;
	do UNXREFDATA^%YDBAIM(aimgbl2)
	do UNXREFDATA^%YDBAIM(aimgbl)
	kill @g
	quit
	;
t4wdeep	; @TEST type=4: every unnamed level down to the record specification's depth is watched, and a child keeps its parent in existence
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	; Records are ^t4emp(id), and a record's value nodes need not all sit at one depth below the
	; record:
	;
	;   ^t4emp(id,"EMAIL")         one level down - the indexed value node
	;   ^t4emp(id,0,"DEPT")        two levels down - another value node of the same record
	;   ^t4emp(id,0,"DEPT","NOTE") three levels down - and another
	;
	; A record comes into existence as soon as ANY of them is stored, so every level down to
	; the record specification's depth has to be watched. recsub=4 with only level 1 named
	; means levels 2, 3 AND 4 are watched, not level 4 alone.
	;
	; The same reasoning applies to a node losing its own value: a ZKILL takes the value
	; and leaves the descendants, so a watched node that still has a child has not taken
	; the record with it.
	;
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"t4emp"
	kill @g
	set @g@(1,"EMAIL")="alice@hosp.org",@g@(1,0)="Alice^Cardiology"
	set @g@(2,0)="Bob^Cardiology",@g@(2,0,"X")="an extra node"
	;
	new subs   set subs(1)=":",subs(2)="""EMAIL"""
	new recsub set recsub(1)=":",recsub=4
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,0,4,,,g,.recsub)
	do assert($data(@aimgbl@(1,"alice@hosp.org",1)),"employee 1 has an email")
	do assert('$data(@aimgbl@(1,"",1)),"...and is not in the empty-string entry")
	do assert($data(@aimgbl@(1,"",2)),"employee 2 has none -> """)
	;
	;---------------------------------------------------------------------------
	; Records born at each of the three watched levels.
	;---------------------------------------------------------------------------
	set @g@(4,0,"DEPT")="Neurology"
	do assert($data(@aimgbl@(1,"",4)),"record created at level 3 -> """)
	set @g@(5,0)="Eve^Oncology"
	do assert($data(@aimgbl@(1,"",5)),"record created at level 2 -> """)
	set @g@(6,0,"DEPT","NOTE")="on call"
	do assert($data(@aimgbl@(1,"",6)),"record created at level 4 -> """)
	do t4rebld(.subs,.recsub,g,g,aimgbl,"after records were born at all three levels")
	;
	;---------------------------------------------------------------------------
	; A watched node loses its own value but keeps a child. A ZKILL removes only
	; the value, so ^t4emp(2,0,"X") survives and employee 2 is still a record.
	;---------------------------------------------------------------------------
	zkill @g@(2,0)
	do assert($data(@aimgbl@(1,"",2)),"watched node zkilled, keeps a child -> record lives")
	do t4rebld(.subs,.recsub,g,g,aimgbl,"after a zkill left a child behind")
	;
	;---------------------------------------------------------------------------
	; And when the last of it goes, so does the record.
	;---------------------------------------------------------------------------
	kill @g@(2)
	do assert('$data(@aimgbl@(1,"",2)),"whole record killed -> entry gone")
	do assert($data(@aimgbl@(1,"",4))&$data(@aimgbl@(1,"",6)),"...and the other records are untouched")
	do t4rebld(.subs,.recsub,g,g,aimgbl,"at the end")
	;
	;---------------------------------------------------------------------------
	; Below the depth nothing counts, for the scan as for the triggers. A node at
	; level 5 is created and killed without any trigger firing, since the nodes at
	; levels 1 to 4 come and go implicitly and only level 5 is written; and the
	; scan, which walks ^t4emp(8) with $DATA 10, looks for a node with a value down
	; to the depth and finds none. Neither side makes it a record.
	;---------------------------------------------------------------------------
	set @g@(8,0,"DEPT","NOTE","X")="deep"
	do assert('$data(@aimgbl@(1,"",8)),"record created below the depth: no trigger fires, no entry")
	do t4rebld(.subs,.recsub,g,g,aimgbl,"after a node was born below the depth")
	do assert('$data(@aimgbl@(1,"",8)),"...and a rebuild does not make it a record either")
	kill @g@(8,0,"DEPT","NOTE","X")
	do assert('$data(@g@(8)),"the subtree is gone")
	do assert('$data(@aimgbl@(1,"",8)),"node killed below the depth: still no entry")
	do t4rebld(.subs,.recsub,g,g,aimgbl,"after that node was killed")
	;
	; A node with a value AT the depth, under valueless parents, is a record for both.
	set @g@(9,0,"DEPT","NOTE")="in reach"
	do assert($data(@aimgbl@(1,"",9)),"record created at the depth through valueless parents -> """)
	do t4rebld(.subs,.recsub,g,g,aimgbl,"after a record was born at the depth")
	;
	; A record whose FIRST node in $QUERY order lies below the depth, while a later
	; one lies within it, is a record: the scan has to look past the deep node.
	set @g@(10,0,"DEPT","NOTE","X")="deep first",@g@(10,1)="shallow later"
	do assert($data(@aimgbl@(1,"",10)),"record with a deep node first and a shallow one later -> """)
	do t4rebld(.subs,.recsub,g,g,aimgbl,"after a record with a deep node before a shallow one")
	;
	do UNXREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,0,4,,g,.recsub)
	kill @g
	quit
	;
t4vdeep	; @TEST type=4: ZKILL of a value node MORE than one level below the record keeps the empty-string entry
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	; t4wdeep indexes ^t4emp(id,"EMAIL"), one level below the record. This indexes a value node two
	; levels below it, ^t4emp(id,1,"DEPT"), which is where "does a record survive this ZKILL?"
	; gets hard: the record is ^t4emp(id) and the levels in between can hold nodes of their own.
	;
	; Three things keeps the record in existence when the indexed node is ZKILLed, and each has to be
	; noticed or the entry is dropped instead of turning into an empty-string entry:
	;
	;   - a sibling of the value node, ^t4emp(id,1,<other>)  - the deepest level
	;   - a sibling one level UP, ^t4emp(id,0) or ^t4emp(id,"EMAIL")
	;   - a value on the node in between, ^t4emp(id,1) itself
	;
	new g set g="^"_$select(xdref:xdrefloc,1:"")_"t4emp"
	kill @g
	set @g@(1,0)="Alice^Cardiology",@g@(1,"EMAIL")="alice@hosp.org"
	set @g@(1,1,"DEPT")="Cardiology"	; record 1: indexed node, plus other nodes one level up
	set @g@(2,1,"DEPT")="Oncology"		; record 2: the indexed node and nothing else
	set @g@(3,0)="Carol^"			; record 3: no indexed node at all -> ""
	;
	new subs   set subs(1)=":",subs(2)=1,subs(3)="""DEPT"""	; value ^t4emp(id,1,"DEPT")
	new recsub set recsub(1)=":",recsub=3	; record ^t4emp(id), two levels watched
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,0,4,,,g,.recsub)
	do assert($data(@aimgbl@(1,"Cardiology",1)),"baseline: record 1 = Cardiology")
	do assert($data(@aimgbl@(1,"Oncology",2)),"baseline: record 2 = Oncology")
	do assert('$data(@aimgbl@(1,"",1))&'$data(@aimgbl@(1,"",2)),"baseline: records 1 and 2 are not in the empty-string entry")
	do assert($data(@aimgbl@(1,"",3)),"baseline: record 3 has no such node -> """)
	;
	;---------------------------------------------------------------------------
	; A sibling ONE LEVEL UP keeps the record in existence. ^t4emp(1,1) empties out, but
	; ^t4emp(1,0) and ^t4emp(1,"EMAIL") remain, so record 1 is still a record and its
	; value node is now "". Checking only for a sibling of "DEPT" under ^t4emp(1,1)
	; finds nothing and drops the record from the index entirely.
	;---------------------------------------------------------------------------
	zkill @g@(1,1,"DEPT")
	do assert('$data(@aimgbl@(1,"Cardiology",1)),"zkill: old value entry removed")
	do assert($data(@aimgbl@(1,"",1)),"zkill: record survives one level up -> """)
	do t4rebld(.subs,.recsub,g,g,aimgbl,"after a zkill the record survived")
	;
	;---------------------------------------------------------------------------
	; A value on the node BETWEEN the record and the value node keeps it in existence too.
	;---------------------------------------------------------------------------
	set @g@(4,1)="a value of its own",@g@(4,1,"DEPT")="Radiology"
	do assert($data(@aimgbl@(1,"Radiology",4)),"record 4 indexed")
	do assert('$data(@aimgbl@(1,"",4)),"...and not under the empty string")
	zkill @g@(4,1,"DEPT")
	do assert($data(@aimgbl@(1,"",4)),"zkill: intervening node holds a value -> """)
	do assert('$data(@aimgbl@(1,"Radiology",4)),"...and its value entry is gone")
	do t4rebld(.subs,.recsub,g,g,aimgbl,"after a zkill left a value in between")
	;
	;---------------------------------------------------------------------------
	; And when nothing is left, the record really does go.
	;---------------------------------------------------------------------------
	zkill @g@(2,1,"DEPT")
	do assert('$data(@aimgbl@(1,"Oncology",2)),"zkill: last node of record 2 -> value gone")
	do assert('$data(@aimgbl@(1,"",2)),"zkill: last node of record 2 -> no "" either")
	do t4rebld(.subs,.recsub,g,g,aimgbl,"at the end")
	;
	do UNXREFDATA^%YDBAIM(g,.subs,"^",1,0,0,1,0,4,,g,.recsub)
	kill @g
	quit
	;
t4wildk	; @TEST type=4 watched level below the record where the record has MORE subscripts than the value
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	; Several records then share one cross reference entry, and the entry has to outlive any
	; one of them. Two arrangements, differing in where the value node sits:
	;
	;   A  ^cust(order)              the value node, in its own global
	;      ^ord(order,line)+below    the records
	;   B  ^ord(order)               the value node, on the parent of the records, same global
	;      ^ord(order,line)+below    the records
	;
	; The value node's trigger can only name its own value subscripts, so it cannot
	; name a record here -- it can ask about ^ord(order), the prefix ABOVE the records, and only
	; whether anything hangs below it. The record node's trigger can name a record, and has
	; to look for a surviving sibling record before retiring the entry.
	;
	new vgbl set vgbl="^"_$select(xdref:xdrefloc,1:"")_"cust"
	new rgbl set rgbl="^"_$select(xdref:xdrefloc,1:"")_"ord"
	new subs   set subs(1)=":"
	new recsub set recsub(1)=":",recsub(2)=":",recsub=3
	new aimgbl,i
	;
	for i="A","B" do
	. kill @vgbl,@rgbl
	. set:"B"=i vgbl=rgbl			; B puts the value node on the records' parent
	. set @vgbl@(1)="Alice"			; the value node for order 1
	. set @rgbl@(1,1)="Widget"		; record (1,1): a value, no children
	. set @rgbl@(1,2,0)="Gadget"		; record (1,2): no value, one child
	. set @rgbl@(2,1)="Sprocket"		; record (2,1): order 2 has no value
	. set aimgbl=$$XREFDATA^%YDBAIM(vgbl,.subs,"^",1,0,0,1,0,4,,,rgbl,.recsub)
	. do assert($data(@aimgbl@(1,"Alice",1)),i_": order 1 has records and a value -> Alice")
	. do assert('$data(@aimgbl@(1,"",1)),i_": ...and order 1 is not in the empty-string entry")
	. do assert($data(@aimgbl@(1,"",2)),i_": order 2 has records and no value -> """)
	. ;
	. ; The value node goes; the records do not, so the entry moves to the empty-string entry.
	. zkill @vgbl@(1)
	. do assert($data(@aimgbl@(1,"",1)),i_": value node killed, records remain -> """)
	. do assert('$data(@aimgbl@(1,"Alice",1)),i_": ...and Alice is gone")
	. ;
	. ; One of order 1's two records goes. The other still holds the entry open -- this is
	. ; what a test of the killed record alone (its own $ZTDATA) cannot see.
	. zkill @rgbl@(1,1)
	. do assert($data(@aimgbl@(1,"",1)),i_": one record of two killed -> entry survives")
	. do t4rebld(.subs,.recsub,vgbl,rgbl,aimgbl,i_", one record left for order 1")
	. ;
	. ; The last one goes and the entry goes with it.
	. kill @rgbl@(1,2)
	. do assert('$data(@aimgbl@(1,"",1)),i_": last record killed -> entry gone")
	. do assert($data(@aimgbl@(1,"",2)),i_": ...and order 2 is untouched")
	. ;
	. ; A record reappears for order 1, and a value arrives for order 2.
	. set @rgbl@(1,5)="New"
	. do assert($data(@aimgbl@(1,"",1)),i_": a record reappears -> "" again")
	. set @vgbl@(2)="Bob"
	. do assert($data(@aimgbl@(1,"Bob",2)),i_": order 2 given a value -> Bob")
	. do assert('$data(@aimgbl@(1,"",2)),i_": ...and its empty-string entry is gone")
	. do t4rebld(.subs,.recsub,vgbl,rgbl,aimgbl,i_", at the end")
	. ;
	. do UNXREFDATA^%YDBAIM(vgbl,.subs,"^",1,0,0,1,0,4,,rgbl,.recsub)
	kill @vgbl,@rgbl
	quit
	;
t4xgblw	; @TEST type=4 cross-global specification deeper than its named levels: a valued, childless record node is a record
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	;   value node  ^cust(order_id,line_id)   <- the value node, one per record
	;   record node ^ord(order_id,line_id)    <- a record, with a watched level below it
	;
	; recsub(1)=":",recsub(2)=":" with recsub=3 says the record is ^ord(order_id,line_id)
	; and that anything hanging under it keeps it in existence too. A record may therefore take either structure:
	;
	;   ^ord(1,1)="Widget"    a value of its own, no children
	;   ^ord(1,2,0)="Gadget"  no value, kept in existence by a child
	;
	; and both must count as records. The bulk build tests the record node with $data
	; (xr4walk:), which is true for either, so the value-side ZKILL trigger has to agree:
	; when the value is killed the record must move to the empty-string entry, not drop out
	; of the cross reference. The trigger's "does a record still exist" test may only
	; demand a child (\10) when the value node being killed is itself one of the record
	; node's children, which cannot happen once the two globals differ.
	;
	new vgbl set vgbl="^"_$select(xdref:xdrefloc,1:"")_"cust"
	new rgbl set rgbl="^"_$select(xdref:xdrefloc,1:"")_"ord"
	kill @vgbl,@rgbl
	;
	set @vgbl@(1,1)="Alice",@vgbl@(1,2)="Bob"
	set @rgbl@(1,1)="Widget"		; record (1,1): value, no children
	set @rgbl@(1,2,0)="Gadget"		; record (1,2): no value, one child
	set @rgbl@(1,3)="Sprocket"		; record (1,3): value, no children, no value
	;
	new subs   set subs(1)=":",subs(2)=":"                    ; value node  ^cust(order,line)
	new recsub set recsub(1)=":",recsub(2)=":",recsub=3  ; record node ^ord(order,line)
	new aimgbl set aimgbl=$$XREFDATA^%YDBAIM(vgbl,.subs,"^",1,0,0,1,0,4,,,rgbl,.recsub)
	;
	do assert($data(@aimgbl@(1,"Alice",1,1)),"record (1,1), valued record node -> Alice")
	do assert($data(@aimgbl@(1,"Bob",1,2)),"record (1,2), record node with a child -> Bob")
	do assert('$data(@aimgbl@(1,"",1,1))&'$data(@aimgbl@(1,"",1,2)),"records (1,1) and (1,2) are not in the empty-string entry")
	do assert($data(@aimgbl@(1,"",1,3)),"record (1,3), no value -> """)
	;
	;---------------------------------------------------------------------------
	; Killing the value leaves the record: it belongs in the empty-string entry. The
	; first of these is the structure where demanding a child of the record node would be
	; wrong: ^ord(1,1) has a value but no child, and the record exists.
	;---------------------------------------------------------------------------
	zkill @vgbl@(1,1)
	do assert($data(@aimgbl@(1,"",1,1)),"value killed, valued record node survives -> """)
	do assert('$data(@aimgbl@(1,"Alice",1,1)),"...and Alice is gone")
	zkill @vgbl@(1,2)
	do assert($data(@aimgbl@(1,"",1,2)),"value killed, record node has a child -> """)
	do assert('$data(@aimgbl@(1,"Bob",1,2)),"...and Bob is gone")
	;
	; Both records are now in the empty-string entry by two different routes -- one record node
	; in existence by its own value, the other by a child. A rebuild reads them with $data and
	; sees both; the state the triggers left has to say the same thing.
	do t4rebld(.subs,.recsub,vgbl,rgbl,aimgbl,"after the value were killed")
	;
	;---------------------------------------------------------------------------
	; A value arriving for a record without a value node replaces its empty-string entry.
	;---------------------------------------------------------------------------
	set @vgbl@(1,3)="Dave"
	do assert($data(@aimgbl@(1,"Dave",1,3)),"value set on a record without a value node -> Dave")
	do assert('$data(@aimgbl@(1,"",1,3)),"...and its empty-string entry is gone")
	;
	;---------------------------------------------------------------------------
	; Killing the record node itself ends the record, empty-string entry and all; a new one
	; with no value is born into the empty-string entry.
	;---------------------------------------------------------------------------
	zkill @rgbl@(1,1)
	do assert('$data(@aimgbl@(1,"",1,1)),"record node killed -> record leaves the xref")
	set @rgbl@(1,4)="Thing"
	do assert($data(@aimgbl@(1,"",1,4)),"new valued record node -> """)
	;
	;---------------------------------------------------------------------------
	; The triggers and the bulk build must have reached the same place: rebuild
	; from scratch over the mutated data and compare it against what is there.
	;---------------------------------------------------------------------------
	do t4rebld(.subs,.recsub,vgbl,rgbl,aimgbl,"after the record nodes were changed")
	do assert($data(@aimgbl@(1,"",1,2)),"rebuild: record (1,2) -> """)
	do assert($data(@aimgbl@(1,"Dave",1,3)),"rebuild: record (1,3) -> Dave")
	do assert($data(@aimgbl@(1,"",1,4)),"rebuild: record (1,4) -> """)
	do assert('$data(@aimgbl@(1,"",1,1)),"rebuild: record (1,1) is not there")
	;
	do UNXREFDATA^%YDBAIM(vgbl,.subs,"^",1,0,0,1,0,4,,rgbl,.recsub)
	kill @vgbl,@rgbl
	quit
	;
t4rebld(subs,recsub,vgbl,rgbl,aimgbl,msg)	; [t4zknull, t4setnull, t4cnsub, t4wdeep, t4vdeep, t4wildk, t4xgblw, t4xdref, t4xdrefr] Assert the triggers and the bulk build agree
	; Save what the triggers have produced, rebuild the cross reference from scratch
	; over the same data, and compare. The two are meant to be two implementations of
	; one definition of a record, so any divergence is a defect in one of them.
	new before merge before=@aimgbl
	do UNXREFDATA^%YDBAIM(vgbl,.subs,"^",1,0,0,1,0,4,,rgbl,.recsub)
	do assert('($data(@aimgbl)\10),"the cross reference was removed: "_msg)
	set aimgbl=$$XREFDATA^%YDBAIM(vgbl,.subs,"^",1,0,0,1,0,4,,,rgbl,.recsub)
	do assert($$t4same(.before,aimgbl),"triggered index matches a fresh rebuild "_msg)
	quit
	;
t4same(before,aimgbl)	; [t4rebld, t4omitfix] Do a saved copy of an index and the live one hold the same entries?
	; Compares subscripts and values both ways. Only the index proper is compared:
	; entries live at (pnum,value,key...), so a node is one when it has more than two
	; subscripts and the first is the piece number. Everything else is metadata, which
	; carries a build timestamp and a trigger definition and so differs by construction.
	new d,now,q,r
	merge now=@aimgbl
	set d=0
	set q="before" for  set q=$query(@q) quit:'$zlength(q)  do:(2<$qlength(q))&(1=$qsubscript(q,1))
	. set r="now"_$zextract(q,7,$zlength(q))
	. ; M has no short circuit evaluation, so @r cannot be named in the same
	. ; expression that tests whether it exists
	. if '($data(@r)#10) set d=d+1
	. else  set:@r'=@q d=d+1
	set q="now" for  set q=$query(@q) quit:'$zlength(q)  do:(2<$qlength(q))&(1=$qsubscript(q,1))
	. set r="before"_$zextract(q,4,$zlength(q))
	. set:'($data(@r)#10) d=d+1
	quit 'd
	;
t4nent(aimgbl)	; [t4rngstart] How many index entries does piece 1 hold?
	; An entry is (pnum,value,key...): more than two subscripts, the first of them 1.
	new n,q
	set n=0,q=aimgbl_"(1)"
	for  set q=$query(@q) quit:'$zlength(q)!(1'=$qsubscript(q,1))  set:2<$qlength(q) n=n+1
	quit n
	;
t4xdref	; @TEST type=4 across DATABASES: record ^G(A,B) here, value node ^|"other.gld"|H(A) there
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	; Every other record-specification test puts both specifications in the same database (either both
	; local, or both moved together by xdref). This one splits them:
	;
	;   value node  ^|"testgld"|H(A)   <- the value node, in a database of its own
	;   record node ^G(A,B)            <- a record, in the caller's database
	;
	; The two globals are now not merely different names, they are in different files
	; reached through different global directories. That matters because AIM switches
	; $zgbldir to the VALUE global's database for the duration of the call (setgbldir^),
	; so a record global named without an extended reference is at risk of being
	; resolved -- for the build walk, and for the database a trigger is defined in --
	; against the wrong global directory. A plain "^G" has to keep meaning the caller's
	; ^G, not whatever ^G the value database's directory maps (and the temp directory
	; is -autodb, so it would happily invent an empty one).
	;
	; Don't run this test if we are in extended reference mode: xdref moves every
	; global to db2 together, which would put both specifications back in one database and
	; test nothing new.
	if xdref quit
	;
	new aimgbl,currgld,envgld,io,rgbl,testdat,testdir,testgld,vgbl,xrefgbl
	do aim93init			; builds a temp database and leaves $zgbldir=currgld
	;
	set rgbl="^G"			; the caller's database
	set vgbl="^|"""_testgld_"""|H"	; the temp database
	kill @rgbl,@vgbl
	;
	; Orders 1 and 2 have a customer; order 3 has records but NO customer; customer 9
	; has no records at all and so must not appear in the index.
	set @vgbl@(1)="Alice",@vgbl@(2)="Bob",@vgbl@(9)="Nobody"
	set @rgbl@(1,1)="Widget",@rgbl@(1,2)="Gadget"
	set @rgbl@(2,1)="Gizmo"
	set @rgbl@(3,1)="Sprocket"
	;
	new subs   set subs(1)=":"			; value node  ^|"testgld"|H(A)
	new recsub set recsub(1)=":",recsub(2)=":"	; record node ^G(A,B)
	set aimgbl=$$XREFDATA^%YDBAIM(vgbl,.subs,"^",1,0,0,1,0,4,,,rgbl,.recsub)
	;
	; The cross reference follows the value global into the other database, exactly
	; as it does for a plain type=0/1 extended reference (see aim93).
	set xrefgbl="^"_$zpiece(aimgbl,"|",3)	; aimgbl without the extended reference
	do assert($data(@aimgbl),"Cross reference created in the extended reference database")
	do assert('$data(@xrefgbl),"Cross reference not created in this database")
	;
	;---------------------------------------------------------------------------
	; The build is driven by the RECORD global, which lives in the caller's
	; database. It must index exactly the orders that have records there -- including
	; order 3 with no customer, and excluding customer 9 which has none.
	;---------------------------------------------------------------------------
	do assert($data(@aimgbl@(1,"Alice",1)),"order 1 = Alice")
	do assert($data(@aimgbl@(1,"Bob",2)),"order 2 = Bob")
	do assert('$data(@aimgbl@(1,"",1))&'$data(@aimgbl@(1,"",2)),"orders 1 and 2 are not in the empty-string entry")
	do assert($data(@aimgbl@(1,"",3)),"order 3 has records but no customer -> """)
	do assert('$data(@aimgbl@(1,"Nobody",9)),"customer 9 has no records -> not indexed")
	do assert('$data(@aimgbl@(1,"",9)),"...nor under the empty string")
	;
	;---------------------------------------------------------------------------
	; Value-side triggers, in the OTHER database.
	;---------------------------------------------------------------------------
	set @vgbl@(3)="Eve"
	do assert($data(@aimgbl@(1,"Eve",3)),"customer set -> order 3 = Eve")
	do assert('$data(@aimgbl@(1,"",3)),"...and order 3 left the empty-string entry")
	;
	zkill @vgbl@(1)
	do assert($data(@aimgbl@(1,"",1)),"customer killed but records remain -> order 1 = """)
	do assert('$data(@aimgbl@(1,"Alice",1)),"...and Alice is gone")
	;
	;---------------------------------------------------------------------------
	; Record-side triggers, in THIS database. These are the ones a mis-resolved
	; global directory would silently drop: the trigger would be defined on the
	; temp database's ^G, which no one ever writes to.
	;---------------------------------------------------------------------------
	zkill @rgbl@(2,1)
	do assert('$data(@aimgbl@(1,"Bob",2)),"last record gone -> order 2 not indexed")
	do assert('$data(@aimgbl@(1,"",2)),"...and no empty-string entry: order 2 has no record left")
	do assert($data(@vgbl@(2)),"...even though the customer node still exists")
	;
	set @rgbl@(9,1)="Widget9"
	do assert($data(@aimgbl@(1,"Nobody",9)),"record created -> order 9 = Nobody")
	do assert('$data(@aimgbl@(1,"",9)),"...and not under the empty string")
	;
	kill @rgbl@(1)
	do assert('$data(@aimgbl@(1,"",1)),"KILL up the record tree -> order 1 gone")
	;
	;---------------------------------------------------------------------------
	; A rebuild has to agree with what the triggers have been maintaining.
	;---------------------------------------------------------------------------
	do t4rebld(.subs,.recsub,vgbl,rgbl,aimgbl,"cross-database")
	;
	do UNXREFDATA^%YDBAIM(vgbl,.subs,"^",1,0,0,1,0,4,,rgbl,.recsub)
	kill @rgbl,@vgbl
	do aim93exit
	quit
	;
t4xdrefr	; @TEST type=4 across DATABASES, the other way: record ^|"other.gld"|G(A,B), value node ^H(A)
	quit:aimtype=4  ; this test passes its own type, so the type=4 pass would only run it again
	;
	;   value node  ^H(A)                <- the value node, in the caller's database
	;   record node ^|"testgld"|G(A,B)   <- a record, in a database of its own
	;
	; The mirror of t4xdref, and NOT the same code path. There, gbl carried the extended
	; reference, so initxref moved $zgbldir to the value global's database for the whole
	; call. Here gbl is bare, setgbldir never runs, $zgbldir never moves, and the cross
	; reference is built in the caller's database - it is the RECORD side that lives
	; elsewhere. So the directory a record-side trigger must be defined in, and the
	; database its trigger code has to name to reach the cross reference, are arrived at from
	; the opposite side. Getting one direction right does not settle the other.
	;
	; Don't run this test if we are in extended reference mode: xdref moves every
	; global to db2 together, which would put both specifications back in one database and
	; test nothing new.
	if xdref quit
	;
	new aimgbl,currgld,envgld,io,rgbl,testdat,testdir,testgld,vgbl
	do aim93init			; builds a temp database and leaves $zgbldir=currgld
	;
	set vgbl="^H"			; the caller's database
	set rgbl="^|"""_testgld_"""|G"	; the temp database
	kill @vgbl,@rgbl
	;
	set @vgbl@(1)="Alice",@vgbl@(2)="Bob",@vgbl@(9)="Nobody"
	set @rgbl@(1,1)="Widget",@rgbl@(1,2)="Gadget"
	set @rgbl@(2,1)="Gizmo"
	set @rgbl@(3,1)="Sprocket"
	;
	new subs   set subs(1)=":"			; value node  ^H(A)
	new recsub set recsub(1)=":",recsub(2)=":"	; record node ^|"testgld"|G(A,B)
	set aimgbl=$$XREFDATA^%YDBAIM(vgbl,.subs,"^",1,0,0,1,0,4,,,rgbl,.recsub)
	;
	; Nothing was extended on the value side, so the cross reference stays here.
	do assert(aimgbl'["|","cross reference built in this database, not the record's")
	do assert($data(@aimgbl),"...and it exists")
	;
	; Driven by the record global, which is the one in the other database.
	do assert($data(@aimgbl@(1,"Alice",1)),"order 1 = Alice")
	do assert($data(@aimgbl@(1,"Bob",2)),"order 2 = Bob")
	do assert('$data(@aimgbl@(1,"",1))&'$data(@aimgbl@(1,"",2)),"orders 1 and 2 are not in the empty-string entry")
	do assert($data(@aimgbl@(1,"",3)),"order 3 has records but no customer -> """)
	do assert('$data(@aimgbl@(1,"Nobody",9)),"customer 9 has no records -> not indexed")
	do assert('$data(@aimgbl@(1,"",9)),"...nor under the empty string")
	;
	; Value-side triggers, in THIS database, reaching across to check for records.
	set @vgbl@(3)="Eve"
	do assert($data(@aimgbl@(1,"Eve",3)),"customer set -> order 3 = Eve")
	do assert('$data(@aimgbl@(1,"",3)),"...and order 3 left the empty-string entry")
	zkill @vgbl@(1)
	do assert($data(@aimgbl@(1,"",1)),"customer killed but records remain -> order 1 = """)
	do assert('$data(@aimgbl@(1,"Alice",1)),"...and Alice is gone")
	;
	; Record-side triggers, defined in the OTHER database and writing back to this one.
	zkill @rgbl@(2,1)
	do assert('$data(@aimgbl@(1,"Bob",2)),"last record gone -> order 2 not indexed")
	do assert('$data(@aimgbl@(1,"",2)),"...and no empty-string entry: order 2 has no record left")
	set @rgbl@(9,1)="Widget9"
	do assert($data(@aimgbl@(1,"Nobody",9)),"record created -> order 9 = Nobody")
	do assert('$data(@aimgbl@(1,"",9)),"...and not under the empty string")
	kill @rgbl@(1)
	do assert('$data(@aimgbl@(1,"",1)),"KILL up the record tree -> order 1 gone")
	;
	do t4rebld(.subs,.recsub,vgbl,rgbl,aimgbl,"cross-database, record side remote")
	;
	do UNXREFDATA^%YDBAIM(vgbl,.subs,"^",1,0,0,1,0,4,,rgbl,.recsub)
	kill @vgbl,@rgbl
	do aim93exit
	quit
	;
