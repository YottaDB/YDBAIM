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
%YDBAIMSAN	; Sanity Tester for %YDBAIM
	do en^%ut($t(+0),3)
	quit
	;
assert:(boolexpr,msg) ; [private] shortcut to tf^%ut
	do tf^%ut(boolexpr,$get(msg))
	quit
	;
tmain	; @TEST Main Sanity YDBAIM tester
	write !
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
	write "Starting ",nproc," simulated application processes",!
	lock +^%ydb674test("simapp")					; get lock that tells simapp processes to run
	for i=1:1:nproc job simapp set ^%ydb674test("simapp","jobs",$zjob)=""
	for  quit:'^%ydb674test("simapp","start")  hang .001		; zero ^%ydb674test("simapp","start") means all simapp processes are up
	quit

	; Stop simulated application
simappstop
	lock -^%ydb674test("simapp")					; releasing lock instructs simapp processes to stop
	write "Stopping simulated application",!
	; Wait for all children to die
	new zjob set zjob=""
	for  set zjob=$order(^%ydb674test("simapp","jobs",zjob)) quit:'zjob  do
	. for  quit:'$zgetjpi(zjob,"isprocalive")  hang .001
	quit

	; Check complete nodes and cross references for Consistency - a cross reference for each node, and a node for each cross reference
xrefgblchk
	new flag,sub1,sub2,sub3,val
	write "Checking that a node exists for each cross reference ",!
	; Should check whether a $query() loop is faster than the following nested $order() loop
	set flag=0,val="" for  set val=$order(@xrefgbl@(0,val)) quit:""=val  do
	. set sub1="" for  set sub1=$order(@xrefgbl@(0,val,sub1)) quit:""=sub1  do
	. . set sub2="" for  set sub2=$order(@xrefgbl@(0,val,sub1,sub2)) quit:""=sub2  do
	. . . set sub3="" for  set sub3=$order(@xrefgbl@(0,val,sub1,sub2,sub3)) quit:""=sub3  do
	. . . . if '$data(^ABC(sub1,sub2,sub3))#10 write !,xrefgbl,"(",val,",",sub1,",",sub2,",",sub3,") exists but ^ABC node does not" set flag=1
	. . . . else  if val'=^ABC(sub1,sub2,sub3) write !,"^ABC(",sub1,",",sub2,",",sub3,")=",^ABC(sub1,sub2,sub3)," but xref is ",val set flag=1
	do assert('flag)
	write "Checking that an xref exists for each global node ",!
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
	write "Starting ",nproc," concurrent xrefs",!
	for i=1:1:nproc job xrefproc set ^%ydb674test("xref","jobs",$zjob)=""
	for  quit:'^%ydb674test("xref","start")  hang .001		; zero ^%ydb674test("xref","start") means all xref processes are up
	kill ^%ydb674test("xref","start")
	lock -^%ydb674test("xref","start")
	lock +^%ydb674test("xref","stop")
	; Make sure all xref jobs died off
	new zjob set zjob=""
	for  set zjob=$order(^%ydb674test("xref","jobs",zjob)) quit:'zjob  do
	. for  quit:'$zgetjpi(zjob,"isprocalive")  hang .001
	write "Cross reference processes complete",!
	quit

xrefproc
	lock +^%ydb674test("xref","stop",$job)
	if $increment(^%ydb674test("xref","start"),-1)
	lock +^%ydb674test("xref","start",$job)				; Gate to ensure all xref processes are up and running
	lock -^%ydb674test("xref","start",$job)
	set ^%ydb674test("xref","aimxref",$job)=$$XREFDATA^%YDBAIM("^ABC",3)
	lock -^%ydb674test("xref","stop",$job)
	quit

