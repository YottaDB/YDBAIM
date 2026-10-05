%YDBAIMCOV	; Line coverage of a routine from YottaDB M-profiling dumps, as Cobertura XML
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;								;
	; Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	;
	; All rights reserved.						;
	;								;
	;	This source code contains the intellectual property	;
	;	of its copyright holder(s), and is made available	;
	;	under a license.  If you do not know the terms of	;
	;	the license, please stop and do not read further.	;
	;								;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; The suite is run with ydb_trace_gbl_name set to ^%ydbcov($J), so every process - the
	; test runner, the scan workers it JOBs, the bash tests' processes - dumps its
	; M-profiling data at exit under its own pid:
	;
	;   ^%ydbcov(pid,routine,label,offset)="count:usertime:systime:totaltime"
	;
	; where offset counts lines from the nearest preceding PUBLIC label, comment lines
	; included. A source line is covered if any process executed it.
	quit
	;
COBERTURA(src,out,rtn,file)	; Write the Cobertura XML for one routine
	; src:  path of the routine's source file
	; out:  path of the XML to write
	; rtn:  the routine name as it appears in the profile, default %YDBAIM
	; file: the file name to report, relative to the repository root, default _YDBAIM.m
	new code,covered,exe,h,hits,io,lab,line,n,name,nexec,off,pid,rate,rest
	set rtn=$get(rtn,"%YDBAIM"),file=$get(file,"_YDBAIM.m"),io=$io
	; 1. Hits per (label,offset), summed over every process that dumped a profile
	set pid="" for  set pid=$order(^%ydbcov(pid)) quit:""=pid  do
	. set lab="" for  set lab=$order(^%ydbcov(pid,rtn,lab)) quit:""=lab  do
	. . set off="" for  set off=$order(^%ydbcov(pid,rtn,lab,off)) quit:""=off  if $increment(hits(lab,off),+$get(^(off)))
	; 2. Read the source and give each executable line its (label,offset)
	set lab="",off=0,n=0,nexec=0,covered=0
	open src:(readonly) use src
	for  read line quit:$zeof  do
	. set n=n+1
	. if line?1(1"%",1AN).E do		; a label starts in column 1, with % or an alphanumeric; a comment there does not
	. . ; a label: its name runs to the space, tab, "(", ":" or ";" that ends it (%YDBAIM;3, XREFDATA(gbl,...);5)
	. . set name=$zextract(line,1,$$namelen(line)),rest=$zextract(line,$zlength(name)+1,$zlength(line))
	. . if ":"'=$zextract(rest) set lab=name,off=0	; a PUBLIC label restarts the offsets
	. . else  set off=off+1,rest=$zextract(rest,2,$zlength(rest))
	. . ; a formal list executes as a line of its own; otherwise the code after the label
	. . set code=$select("("=$zextract(rest):"(",1:$$trim(rest))
	. else  set off=off+1,code=$$trim(line)
	. quit:'$zlength(lab)  quit:'$zlength(code)  quit:";"=$zextract(code)
	. set exe(n)=$get(hits(lab,off),0),nexec=nexec+1
	. set:exe(n) covered=covered+1
	close src
	set rate=$select(nexec:$justify(covered/nexec,0,4),1:0)
	; 3. The XML
	open out:(newversion) use out
	write "<?xml version=""1.0"" ?>",!
	write "<coverage line-rate=""",rate,""" branch-rate=""0"" lines-covered=""",covered,""" lines-valid=""",nexec,""" branches-covered=""0"" branches-valid=""0"" complexity=""0"" version=""0.1"" timestamp=""",$zut\1E6,""">",!
	write "<sources><source>.</source></sources>",!
	write "<packages><package name=""",rtn,""" line-rate=""",rate,""" branch-rate=""0"" complexity=""0""><classes>",!
	write "<class name=""",file,""" filename=""",file,""" line-rate=""",rate,""" branch-rate=""0"" complexity=""0""><methods/><lines>",!
	set n="" for  set n=$order(exe(n)) quit:""=n  write "<line number=""",n,""" hits=""",exe(n),"""/>",!
	write "</lines></class></classes></package></packages></coverage>",!
	close out
	use io
	write "Coverage: ",$justify(100*rate,0,1),"% (",covered," of ",nexec," executable lines of ",rtn,")",!
	quit
	;
namelen(line)	; length of the label name at the start of line: up to the first space, tab, "(", ":" or ";"
	new d,i,n
	set n=$zlength(line)
	for d=" ",$char(9),"(",":",";" set i=$zfind(line,d)-2 set:(i'<0)&(i<n) n=i
	quit n
	;
trim(s)	; s without leading spaces, tabs and the dots of a DO block
	new c,i
	for i=1:1:$zlength(s) set c=$zextract(s,i) quit:(" "'=c)&($char(9)'=c)&("."'=c)
	quit $zextract(s,i,$zlength(s))
