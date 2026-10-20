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
; Application Independent Metadata (AIM)
;
; Compute and maintain application independent metadata:
; - Cross References
; - Optionally, statistics on the count of each value and the number of
;   distinct values
;
; The number in the comment is a minor version number for the software.
; Refer to comments before the VERSION() label.
%YDBAIM;3
	; Top level entry not supported
	new $etrap,io do etrap
	set $ecode=",U255,"	; top level entry not supported
	quit			; should get here only in direct mode

; Set error trap. The first action the error trap does is set a failsafe error
; trap (e.g., if $zroutines is not correct). Thereafter, it jumps to the actual
; error trap to print an error message and terminate, with a return code.
;
; Note that all external entry points:
; - NEW $ETRAP and DO etrap if $ETRAP is the default error handler. This allows
;   them to report the error messages for %YDBAIM errors, instead of just
;   reporting that $ECODE was assigned a non-empty value, which the default
;   error trap will report. If an application sets an error trap ($ETRAP or
;   $ZTRAP) that will be used.
; - Capture existing locks in currlck so that the error trap can release
;   locks acquired by %YDBAIM. In non-error code paths, acquired locks are
;   released without the need to refer to captured locks.
etrap
	set $etrap="set io=$io,$etrap=""open """"/proc/self/fd/2"""" use """"/proc/self/fd/2"""" write $zstatus,! zshow """"*"""" zhalt $piece($zstatus,"""","""",1)""  goto err^"_$text(+0)
	quit

err	; Primary Error Handler
	; -----------------------------------------------------------------
	; This is where control reaches when any error is encountered inside AIM.
	; We do AIM-specific cleanup here and then switch $etrap to a non-AIM default handler that rethrows
	; the error one caller frame at a time until we unwind to a non-AIM caller frame that has $etrap set
	; at which point it can handle the error accordingly.
	; -----------------------------------------------------------------
	new errcode,errtxt
	set errcode=$zpiece($ecode,",",2),errtxt=$text(@errcode)
	; Check for AIM-specific errors (in that case "errtxt" will be non-empty).
	if $zlength(errtxt) do
	. new xstr
	. ; This is an AIM specific error. Extract error text with potential unfilled parameter values.
	. ; Run "xecute" on that string to fill it with actual values.
	. set xstr="set errtxt="_$zpiece(errtxt,";",2,$zlength(errtxt,";")) xecute xstr
	. set $zstatus=$zpiece($zstatus,",",1,2)_","_$text(+0)_errtxt
	; Rollback the transaction to $TLEVEL at entry into the first caller AIM frame.
	trollback:$tlevel&$data(tlevel)&(tlevel<$tlevel) tlevel
	; Undo ztrigger_output changes if any done
	view:$data(ztout) "ztrigger_output":ztout
	; Release locks obtained inside AIM
	do unsnaplck(.currlck)
	; Terminate any JOB'd xrefdata()/xrefsub() worker processes
	do:$data(xrefproc) xrefjobskill
	; Now that primary error handling is done, switch to different handler to rethrow error in caller AIM frames.
	; The rethrow will cause a different $etrap to be invoked in the first non-AIM caller frame (because AIM
	; did a "new $etrap" at entry).
	do:$data(currgld) resetgbldir
	set $etrap="quit:$quit """" quit"
	use io
	quit:$quit "" quit

; List metadata for a data cross reference, all data cross references for a
; global variable, or all data cross references
LSXREFDATA(lvn,gbl)
	new $etrap,io do etrap
	new currgld,currlck,envgld,extgld,name,tlevel,xrefvar
	set tlevel=$tlevel
	set gbl=$get(gbl)
	do:gbl?1"^|".E1"|"1(1"%",1AN).AN setgbldir
	do snaplck(.currlck)
	if '$zlength(gbl) do
	. set gbl="" for  set gbl=$order(^%ydbAIMDxref(gbl)) quit:'$zlength(gbl)  do
	. . set xrefvar="" for  set xrefvar=$order(^%ydbAIMDxref(gbl,xrefvar)) quit:'$zlength(xrefvar)  do lsxref(.lvn,xrefvar)
	else  if gbl'?1"^%ydbAIMD".E do
	. set xrefvar="" for  set xrefvar=$order(^%ydbAIMDxref(gbl,xrefvar)) quit:'$zlength(xrefvar)  do lsxref(.lvn,xrefvar)
	else  do lsxref(.lvn,gbl)
	do:$data(currgld) resetgbldir
	quit

; List metadata for a subscript cross reference , all subscript cross references for
; a global variable, or all subscript cross references
LSXREFSUB(lvn,gbl)
	new $etrap do etrap
	new currgld,currlck,envgld,extgld,name,tlevel,xrefvar
	set tlevel=$tlevel
	set gbl=$get(gbl)
	do:gbl?1"^|".E1"|"1(1"%",1AN).AN setgbldir
	do snaplck(.currlck)
	if '$zlength(gbl) do
	. set gbl="" for  set gbl=$order(^%ydbAIMSxref(gbl)) quit:'$zlength(gbl)  do
	. . set xrefvar="" for  set xrefvar=$order(^%ydbAIMSxref(gbl,xrefvar)) quit:'$zlength(xrefvar)  do lsxref(.lvn,xrefvar)
	else  if gbl'?1"^%ydbAIMS".E do
	. set xrefvar="" for  set xrefvar=$order(^%ydbAIMSxref(gbl,xrefvar)) quit:'$zlength(xrefvar)  do lsxref(.lvn,xrefvar)
	else  do lsxref(.lvn,gbl)
	do:$data(currgld) resetgbldir
	quit

; Remove triggers and cross references for a specified global, or all globals.
; The parameters for UNXREFDATA() mirror those of XREFDATA() to simplify calling
; it to remove triggers and cross references, even though some parameters are
; not required and are therefore ignored.
UNXREFDATA(gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type,force,recgbl,recsub)
	new $etrap,io do etrap
	new currgld,currlck,envgld,extgld,i,name,nsubs,tlevel,xrefvar
	set tlevel=$tlevel
	; Ensure type & force have values and convert 0 to "" for backward compatibility
	if '$data(type)!(0=type) set type=""
	if '$data(force)!(0=force) set force=""
	set gbl=$get(gbl)
	do:gbl?1"^|".E1"|"1(1"%",1AN).AN setgbldir
	do snaplck(.currlck)
	if gbl?1"^%ydbAIMD".22AN do
	. tstart (gbl,xrefvar):transactionid="batch"
	. if $data(@gbl) set xrefvar=gbl,gbl=@xrefvar do unxref(xrefvar)
	. tcommit
	else  if '$zlength(gbl) do			; remove all xrefs
	. lock +^%ydbAIMD
	. set gbl="" for  set gbl=$order(^%ydbAIMDxref(gbl)) quit:'$zlength(gbl)  do
	. . set xrefvar="" for  set xrefvar=$order(^%ydbAIMDxref(gbl,xrefvar)) quit:'$zlength(xrefvar)  do unxref(xrefvar)
	. lock -^%ydbAIMD
	else  do
	. ; Xrefs are only supported for global variables
	. set:(gbl'?1"^"1(1"%",1AN).AN)!(32<$zlength(gbl)) $ecode=",U252,"
	. set nsubs=$get(xsub,0)
	. ; If constraints specified for subscripts, ensure all refer to
	. ; subscripts that are integers in the range 1 through 29
	. do:$data(xsub)\10
	. . set i=$order(xsub(""))
	. . set:1>i!(i\1'=i)!(29<i) $ecode=",U247,"
	. . for  set i=$order(xsub(i)) quit:'$zlength(i)  set:i\1'=i!(29<i) $ecode=",U247,"
	. . set i=$order(xsub(""),-1)
	. . set:i>nsubs nsubs=i
	. set:29<nsubs $ecode=",U247,"
	. if 'nsubs do		; remove all xrefs for gbl
	. . lock +^%ydbAIMD(gbl)
	. . set xrefvar="" for  set xrefvar=$order(^%ydbAIMDxref(gbl,xrefvar)) quit:'$zlength(xrefvar)  do unxref(xrefvar)
	. . lock -^%ydbAIMD(gbl)
	. else  do unxref($$XREFDATA(gbl,.xsub,$get(sep),,1,$get(zpiece),$get(omitfix,1),$get(stat,0),type,force,,$get(recgbl,gbl),.recsub))
	do:$data(currgld) resetgbldir
	quit:$quit "" quit

; Remove triggers and cross references for a specified global, or all globals.
; The parameters for UNXREFSUB() mirror those of XREFSUB() to simplify calling
; it to remove triggers and cross references, even though some parameters are
; not required and are therefore ignored.
UNXREFSUB(gbl,xsub,snum,nmonly,omitfix,stat,type,force)
	new etrap do etrap
	new currgld,currlck,envgld,extgld,i,name,nsubs,tlevel,xrefvar
	set tlevel=$tlevel
	; Ensure force has a value and convert 0 to "" for backward compatibility
	if '$data(force)!(0=force) set force=""
	set gbl=$get(gbl)
	do:gbl?1"^|".E1"|"1(1"%",1AN).AN setgbldir
	do snaplck(.currlck)
	if gbl?1"^%ydbAIMS".22AN do
	. tstart (gbl,xrefvar):transactionid="batch"
	. if $data(@gbl) set xrefvar=gbl,gbl=@xrefvar do unxref(xrefvar)
	. tcommit
	else  if '$zlength(gbl) do			; remove all xrefs
	. lock +^%ydbAIMS
	. set gbl="" for  set gbl=$order(^%ydbAIMSxref(gbl)) quit:'$zlength(gbl)  do
	. . set xrefvar="" for  set xrefvar=$order(^%ydbAIMSxref(gbl,xrefvar)) quit:'$zlength(xrefvar)  do unxref(xrefvar)
	. lock -^%ydbAIMS
	else  do
	. ; Xrefs are only supported for global variables
	. set:(gbl'?1"^"1(1"%",1AN).AN)!(32<$zlength(gbl)) $ecode=",U252,"
	. set nsubs=$get(xsub,0)
	. ; If constraints specified for subscripts, ensure all refer to
	. ; subscripts that are integers in the range 1 through 29
	. do:$data(xsub)\10
	. . set i=$order(xsub(""))
	. . set:1>i!(i\1'=i)!(29<i) $ecode=",U247,"
	. . for  set i=$order(xsub(i)) quit:'$zlength(i)  set:i\1'=i!(29<i) $ecode=",U247,"
	. . set i=$order(xsub(""),-1)
	. . set:i>nsubs nsubs=i
	. set:29<nsubs $ecode=",U247,"
	. if 'nsubs do		; remove all xrefs for gbl
	. . lock +^%ydbAIMS(gbl)
	. . set xrefvar="" for  set xrefvar=$order(^%ydbAIMSxref(gbl,xrefvar)) quit:'$zlength(xrefvar)  do unxref(xrefvar)
	. . lock -^%ydbAIMS(gbl)
	. else  do unxref($$XREFSUB(gbl,.xsub,snum,1,$get(omitfix,1),$get(stat),$get(type),$get(force)))
	do:$data(currgld) resetgbldir
	quit:$quit "" quit

; Per the guidelines at https://semver.org/, a version number has three period seoarated parts:
; - major version, a change in which indicates a breaking change,
; - minor version, a change in which indidates an upward-compatible (non-breaking) change, and
; - patch level.
; VERSION() interprets these as follows:
; - The major version corresponds to the cross reference schema. Since there are separate schemas
;   for data and for subscripts, there are separate major versions for the two. These major
;   numbers are in the comment following the XREFDATA() and XREFSUB() labels. Note that
;   functional changes to trigger code potentially change the metadata schem. When VERSION()
;   is called without an argument, or an argument other than case-insensitive "DATA" or "SUB",
;   it reports the sum of the two major version numbers.
; - The minor version is in the comment following the %YDBAIM label, and should be updated for
;   every upward compatible software change (like a bug fix, or new feature).
; - No patch level is reported as it cannot be reliably detetmined owing to the variety of ways
;   in which %YDBAIM can be installed and executed.
VERSION(type)
	new dver,loctype,majver,minver,sver
	set loctype=$zconvert($get(type),"u")
	set dver=$zpiece($text(XREFDATA^%YDBAIM),";",2)
	set sver=$zpiece($text(XREFSUB^%YDBAIM),";",2)
	set minver=$zpiece($text(%YDBAIM^%YDBAIM),";",2)
	set majver=$select("DATA"=loctype:dver,"SUB"=loctype:sver,1:dver+sver)
	quit majver_"."_minver

; XREFDATA() and XREFSUB() create triggers that maintain cross references and then
; compute cross references for existing global nodes. This allows them to run concurrently
; with application code that is changing the data cross referenced. Concurrent execution OK.

; XREFDATA() cross references metadata of nodes, or pieces of nodes.
;
; The number in the comment is the major version for data metadata. Refer to
; comments before the VERSION() label.
;
; The initial scan of existing nodes divides them among several processes. How
; many is the ydb_aim_nproc environment variable if defined, and otherwise one
; less than the number of online CPUs (one on a single CPU machine), the parent
; needing a CPU for the walk that writes the ranges those processes scan. It
; does not affect the cross reference produced, only how long producing it
; takes, and is not part of the cross reference name. Types 4 and 6 divide
; their scan the same way, over the record specification.
;
; Types 4 and 6 (6 = 4 with a transformation function) take a record specification,
; recgbl and recsub, which says which global variable nodes define that a record
; exists, separately from gbl and xsub, which say where the value to cross reference
; lives. A record whose value node is absent is then cross referenced under the empty
; string. The record specification is described where it is parsed, below. Types 1 and
; 3 are treated as types 4 and 6, with a record specification whose record node is the
; parent of the value node.
; E.g. with xsub(1)=":", recsub(1)=":" and recsub=1,
;   $$XREFDATA^%YDBAIM("^H",.xsub,,,,,,,4,,,"^G",.recsub)
; cross references ^H(id) for every record ^G(id), and a record 2 with no ^H(2) under "".
XREFDATA(gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type,force,comment,recgbl,recsub);5
	new $etrap,io do etrap
	new asciisep,constlist,currgld,currlck,envgld,extgld
	new fullsub,fulltrigsub,gblind,gblx,i,j,killtrg
	new lastfullsub,lastsub,lastsubind,lastvarsub,locxsub,modflag,name
	new nameind,newpnum,newpstr,pieces,nsubs,nullsub,omitflag,oldpstr
	new recalive,recaliver,recconstlist,recdup,recgblb,recgblind,recgblx,recgld
	new reck,reckey,recklast,reclocxsub,reclvlspec,recn,recname,recnsubs,recnullsub
	new recpfx,recpfxr,recpl,recplast,recsamedb,recspec,recsynth,recvlast,recvskip
	new recwild,rpos,rtsub,rvalnode,stacklvl1,sub,subary,suffix,swver,tlevel,tmp
	new trigdel,trigdelx,trigprefix,trigset,trigsub,ttprfx,valcntind,valind,veck
	new vpos,vrecexq,xfnp1,xfnp2,xfntest,xnl,xrefind,xv1,xv2,z,zintrptsav,zlsep
	new ztout,zyintrsig
	;
	set stacklvl1=$stack	; required by premature termination
	set tlevel=$tlevel	; required by error trap to rollback/unwind
	do initxref		; initialization shared with XREFSUB()
	;
	; A Fileman (type 1 or 3) specification ends in a constant, the record node being
	; its parent, and does not support omitfix=0.
	; E.g. ^ORD(100.01,ien,.1): .1 is the constant, and the record node is ^ORD(100.01,ien).
	set:(type#2)&'constlist(nsubs) $ecode=",U236,"
	set:(type#2)&'omitfix $ecode=",U254,"
	;
	; --- Record specification, types 4 and 6 ---------------------------------------------
	; The structures, in the terms the rest of this routine uses:
	;   the VALUE global, gbl, and the value specification, xsub, say which nodes hold
	;       the values to cross reference, as for every type: e.g. ^H(:) or ^ORD(100.01,:,.1).
	;       The levels of xsub that vary are the VALUE SUBSCRIPTS, the ones a cross
	;       reference entry carries.
	;   the RECORD global, recgbl, and the record specification, recsub, say which nodes
	;       define that a record exists: a RECORD NODE is a node at recsub's deepest named
	;       level, e.g. ^G(:) or ^ORD(100.01,:), and a record exists when the record node,
	;       or a node below it within the depth, holds a value. A record whose value node
	;       is absent is cross referenced under the empty string.
	; recgbl defaults to gbl. recsub uses the grammar of xsub:
	;   ":" or "*" (optionally a range) is a RECORD SUBSCRIPT, corresponding positionally
	;       to a value subscript: the i-th ":" here is the i-th ":" in xsub;
	;   a constant names a fixed subscript, e.g. the 0 of ^ORD(100.01,ien,0);
	;   the ROOT of recsub is the record specification's total DEPTH. A level between the deepest
	;       named one and that depth varies, names nothing, and is WATCHED: a node there keeps
	;       the record in existence. No level below the depth is watched.
	; So: recsub(1)=":", recsub=1                          a record is ^g(A), nothing below it
	;     recsub(1)=":", recsub=2                          a record is ^g(A), kept in existence by any ^g(A,x)
	;     recsub(1)=":",recsub(2)=":",recsub(3)=0, recsub=3   a record is ^g(A,B,0)
	; The depth has to cover every level at which a record can come into or go out of
	; existence, because a trigger fires only on the node actually written.
	; Given no record specification at all, the value specification serves as it: the record
	; node is the value node itself. Only a record specification in gbl can be derived (U219 otherwise).
	; E.g. type=4, gbl="^names", xsub(1)=":" and no recsub: recsub(1)=":", recsub=1 is derived.
	;
	; Types 1 and 3 are treated as types 4 and 6: the record node is the parent of the value
	; node, so the record specification names every level but the last, and the last is
	; watched - ^ORD(100.01,ien,.1) with type=1 is the record ^ORD(100.01,ien), in existence
	; while any of its children exists. The checks a Fileman specification must pass
	; (U236, U254) have been made above.
	; E.g. type=1, xsub(1)=100.01, xsub(2)=":", xsub(3)=.1: recsub(1)=100.01, recsub(2)=":",
	; recsub=3 is derived, and type becomes 4.
	set recgbl=$select($zlength($get(recgbl)):recgbl,1:gbl),recname=""			; omitted, or "" from a positional caller reaching recsub: gbl
	merge:$data(recsub) recspec=recsub							; a private copy: the derivation must not reach the caller's recsub
	set recsynth=(type#2)!((type\4#2)&'$data(recspec)&(recgbl=gbl))				; derive a record specification? always for types 1/3; for types 4/6 only when none is given and the records live in gbl
	do:recsynth
	. set:(type#2)&($data(recspec)!(recgbl'=gbl)) $ecode=",U219,"				; types 1/3 take no record specification of their own
	. for i=1:1:nsubs-(type#2) set recspec(i)=$select("*"=locxsub(i):":",1:locxsub(i))	; copy xsub, less its last level for types 1/3; "*" is spelled ":" in recsub
	. set recspec=nsubs									; the depth: the value node's level
	. set:type#2 type=type-1+4								; 1 -> 4, 3 -> 6
	set recnsubs=$get(recspec,0)								; the depth as stated at the root; 0 for none, raised below to the deepest named level
	;
	; Three spellings of the record global, because its uses answer to different
	; global directories:
	;   recgbl  - as the caller wrote it; only the cross reference NAME derives from it
	;   recgblb - with any extended reference dropped: a trigger SPECIFICATION may not
	;             carry one, and this is what compares against gbl, which initxref
	;             has already stripped
	;   recgblx - with the database always spelled out, for the scan and the trigger
	;             code, which runs under a global directory that is not necessarily the
	;             record global's
	; E.g. called with $zgbldir=/a/c.gld:
	;   recgbl                      recgblb   recgblx
	;   ^G                          ^G        ^G
	;   ^|"x.gld"|G                 ^G        ^|"x.gld"|G
	;   ^G, with gbl=^|"y.gld"|H    ^G        ^|"/a/c.gld"|G
	set recgblb=recgbl
	set:recgblb?1"^|".E1"|"1(1"%",1AN).AN recgblb="^"_$zpiece(recgblb,"|",3)	; ^|"x.gld"|G -> ^G
	;
	; The record global's database. initxref has switched $zgbldir to the value
	; global's database if gbl was an extended reference, saving the caller's in
	; currgld, so a bare recgbl means the ^G the CALLER's directory maps - except one
	; AIM derived from gbl, which lives where gbl does.
	; E.g. called with $zgbldir=/a/c.gld:
	;   gbl=^H            recgbl=^G             recgld=/a/c.gld
	;   gbl=^|"y.gld"|H   recgbl=^G             recgld=/a/c.gld, though $zgbldir is now y.gld
	;   gbl=^H            recgbl=^|"x.gld"|G    recgld=the file $zsearch finds for x.gld (U227 if none)
	;   gbl=^|"y.gld"|H   no recgbl             recgld=$zgbldir, which is y.gld
	do
	. new tmp
	. if recsynth set recgld=$zgbldir quit							; derived from gbl: where gbl lives
	. if recgbl?1"^|".E1"|"1(1"%",1AN).AN do  quit						; an extended reference names its own database
	. . set tmp=$zpiece(recgbl,"|",2)
	. . set recgld=$zsearch($select(""""=$zextract(recgbl,3):$zwrite(tmp,1),1:tmp),-1)	; the directory as written, unquoted if quoted, resolved to a file as $zgbldir would be
	. . set:'$zlength(recgld) $ecode=",U227,"
	. set recgld=$select($data(currgld):currgld,1:$zgbldir)					; a bare recgbl: the caller's directory, which initxref may have switched away from
	;
	; recsamedb: one database holds the value global, the record global and the cross
	; reference, and every global is named bare. Otherwise the trigger code says
	; which database it means, because a trigger fires in whichever process wrote
	; the node, and that process's $zgbldir is not ours.
	; E.g. gbl=^H, recgbl=^|"x.gld"|G, $zgbldir=/a/c.gld: recsamedb=0, recgblx=^|"x.gld"|G and
	; gblx=^|"/a/c.gld"|H, so a trigger on ^G, firing in a process that uses x.gld, still reads
	; the value node in /a/c.gld.
	set recsamedb=(recgld=$zgbldir)															; records and values in one database?
	set recgblx=$select(recgbl?1"^|".E1"|"1(1"%",1AN).AN:recgbl,recsamedb:recgblb,1:"^|"""_recgld_"""|"_$zextract(recgblb,2,$zlength(recgblb)))	; an extended reference as written; bare in one database; else qualified with recgld
	set gblx=$select(recsamedb:gbl,1:"^|"""_$zgbldir_"""|"_$zextract(gbl,2,$zlength(gbl)))								; gbl as the record side's trigger code sees it: qualified when the databases differ
	;
	; Parse recsub into reclocxsub/recconstlist/reckey, and build recname, which feeds the
	; cross reference name.
	; E.g. recsub(1)=100.01, recsub(2)=":", recsub=3:
	;   i   reclocxsub(i)   recconstlist(i)   reckey(i)
	;   1   100.01          1                 0
	;   2   "*"             0                 1
	;   3   "*"             0                 0           (unspecified: varies, names nothing)
	;   recnsubs=3, recname="100.01*:*"
	do:$data(recspec)\10
	. set i="" for  set i=$order(recspec(i)) quit:'$zlength(i)  do:$data(recspec(i))#10		; each named level
	. . set:1>i!(i\1'=i)!(29<i) $ecode=",U247,"							; a level is an integer 1 through 29
	. . set tmp=recspec(i)
	. . set reclocxsub(i)=$select(":"=tmp:"*",1:tmp)						; ":" (any value) is spelled "*" internally, as xsub's is; a stated "*" is the same thing
	. . set recconstlist(i)=$$chktrgspec(.reclocxsub,i)						; 1 for a constant, 0 for a range; validates the spelling
	. . set reckey(i)=(0=recconstlist(i))								; a named varying level is a record subscript
	. set i=$order(reclocxsub(""),-1)								; the deepest named level
	. set:($data(recspec)#10)&(i>recnsubs) $ecode=",U223,"						; a stated depth covers every named subscript
	. set:i>recnsubs recnsubs=i									; no stated depth: the deepest named level is the depth
	set:($data(recspec)#10)&((recnsubs\1'=recnsubs)!(1>recnsubs)!(29<recnsubs)) $ecode=",U223,"	; a stated depth is an integer 1 through 29
	for i=1:1:recnsubs set:'$data(reclocxsub(i)) reclocxsub(i)="*",recconstlist(i)=0		; a level within the depth the caller did not name: it varies and is not a record subscript
	for i=1:1:recnsubs set:'$data(reckey(i)) reckey(i)=0						; unnamed: not a record subscript
	for i=1:1:recnsubs set recname=recname_reclocxsub(i)_$select(reckey(i):":",1:"")		; e.g. "100.01*:*": the record specification's part of the cross reference name
	;
	; A record specification is permitted only with types 4 and 6, and one in a global
	; other than gbl has to be stated.
	; E.g. type=0 with a recsub is U219, and so is type=4 with recgbl="^G" and no recsub.
	set:(type\4#2)&'$data(recspec) $ecode=",U219,"
	set:'(type\4#2)&($data(recspec)!(recgbl'=gbl)) $ecode=",U219,"
	;
	; The value subscripts must be a left-prefix of the record subscripts, and no record subscript may
	; sit below a "*". Positions, all subscript numbers:
	;   recvlast - the deepest varying subscript
	;   recklast - the deepest record subscript
	;   recwild  - the record specification ends in "*": recvlast is below recklast
	;   recplast - THE RECORD NODE: the deepest subscript the value subscripts plus the
	;              record specification's constants fix; with a trailing "*", the node the "*"
	;              hangs off. A record exists when that node exists, carrying a value or
	;              a child.
	; E.g. xsub(1)=100.01, xsub(2)=":", xsub(3)=.1 with recsub(1)=100.01, recsub(2)=":", recsub=3:
	;   recklast=2, recvlast=3, recwild=1, recplast=2. The record node is ^ORD(100.01,ien), and a
	;   child at level 3 keeps it in existence.
	; E.g. xsub(1)=":" with recsub(1)=":", recsub(2)=":", recsub=2:
	;   recklast=2, recvlast=2, recwild=0, recplast=2. The record node is ^G(A,B), and the value
	;   key is A alone.
	; E.g. xsub(1)=":", xsub(2)=":" with recsub(1)=":", recsub=1 is U220: two value subscripts
	;   against one record subscript.
	; E.g. xsub(1)=":" with recsub(2)=":", recsub=2 is U221: level 1 varies and names nothing,
	;   above the record subscript at level 2.
	do:type\4#2
	. set reck=0 for i=1:1:nsubs set:'constlist(i) reck=reck+1	; value subscripts: the varying levels of xsub
	. set recn=0,recvlast=0,recklast=0
	. for i=1:1:recnsubs set:reckey(i) recn=recn+1,recklast=i	; record subscripts, and the deepest
	. for i=1:1:recnsubs set:'recconstlist(i) recvlast=i		; the deepest varying level, record subscript or "*"
	. set:reck>recn $ecode=",U220,"					; the value subscripts cannot outnumber the record subscripts
	. set recwild=recvlast>recklast,recplast=recnsubs		; a "*" below the key? the record node is the depth until refined below
	. do:recwild
	. . new j set j=recklast
	. . for  set j=j+1 quit:j>recnsubs  quit:'recconstlist(j)	; the constants below the last record subscript
	. . set recplast=j-1						; the record node: the last of those constants, or the last record subscript
	. new seenwild set seenwild=0
	. for i=1:1:recnsubs do
	. . set:'recconstlist(i)&'reckey(i) seenwild=1			; an unnamed level
	. . set:reckey(i)&seenwild $ecode=",U221,"			; no record subscript below one
	;
	; Derive subscript specification for trigger definitions from parameters
	; and build string from which to derive xref variable name
	for i=1:1:nsubs do
	. set name=name_locxsub(i)
	. set lastfullsub="sub"_i,trigsub=trigsub_lastfullsub_"=",fulltrigsub=fulltrigsub_lastfullsub_","
	. set fullsub=fullsub_lastfullsub_","
	. set trigsub=trigsub_locxsub(i)_","
	. if omitfix&constlist(i) set omitflag=1
	. else  set lastsub=lastfullsub,sub=sub_lastfullsub_","
	; Ensure at least one subscript selected for cross reference global
	; Remove trailing commas from building subscript lists
	set $zextract(sub,$zlength(sub))=""
	set $zextract(fullsub,$zlength(fullsub))=""
	set $zextract(fulltrigsub,$zlength(fulltrigsub))=""
	set $zextract(trigsub,$zlength(trigsub))=""
	set sep=$get(sep),zlsep=$zlength(sep),zpiece=+$get(zpiece),z=$select(zpiece:"z",1:"")
	set asciisep=1
	for i=1:1:zlsep set:$zascii($zextract(sep,i))>127 asciisep=0 quit:'asciisep
	set suffix=$zysuffix(gbl_name_$select(zlsep:sep_$select(asciisep:0,zpiece:1,1:$zchset),1:"")_$select(omitflag:1,1:"")_type_force_$select(type\4#2:recgbl_recname,1:""))	; everything that makes a different cross reference, the record specification included
	set name="^%ydbAIMD"_suffix
	; Flagging this error needs to be deferred as the error handler may
	; need name to be set.
	set:'$data(lastsub) $ecode=",U244,"
	; Quit if caller only wants the variable name. Note that asking for a
	; name requires that XREFDATA() be called as a function that returns
	; a value, as calling it as a routine asking for a name is a
	; meaningless operation that is likely an application program bug.
	if +$get(nmonly) do:$data(currgld) resetgbldir quit name
	do mkindxrefdata		; Create indirection strings to be used
	set ztout=$view("ztrigger_output")
	; Common prefix for all triggers
	set trigprefix="+"_gbl_"("_trigsub_") -command="
	; As cross referencing does not require exclusivity, multiple processes
	; can invoke XREFDATA() with the same parameters and all will run
	; correctly to completion. The process acquires locks to ensure that
	; XREFDATA() and UNXREFDATA() do not run concurrently. So XREFDATA()
	; uses $JOB as a subscript to the lock resource, whereas UNXREFDATA()
	; does not.
	lock +(^%ydbAIMD($job),^%ydbAIMD(gbl,$job),@name@($job))
	set stat=+$get(stat)
	; Determine whether application global permits null subscripts. For a
	; global variable that spans multiple regions, all regions must be
	; consistent in allowing or not allowing null subscripts. Two cascading
	; unary operators ('') are used to force non-zero values to 1.
	set tmp=$view("region",gbl),nullsub=''$$^%PEEKBYNAME("sgmnt_data.null_subs",$zpiece(tmp,",",1))
	for i=2:1:$zlength(tmp,",") set:nullsub'=''$$^%PEEKBYNAME("sgmnt_data.null_subs",$zpiece(tmp,",",i)) $ecode=",U251,"
	;
	; The record global's database may differ from the value global's on whether null
	; subscripts are allowed, and it is the record global the type=4/6 scan walks.
	; E.g. ^H in a region that allows null subscripts and ^|"x.gld"|G in one that does not:
	; nullsub=1, recnullsub=0.
	if recsamedb set recnullsub=nullsub
	else  do
	. new rgn,sav
	. set sav=$zgbldir,$zgbldir=recgld
	. set rgn=$view("region",recgblb)						; under the record database's directory
	. set recnullsub=''$$^%PEEKBYNAME("sgmnt_data.null_subs",$zpiece(rgn,",",1))	; the record database's null-subscript setting, as nullsub is gbl's
	. for i=2:1:$zlength(rgn,",") set:recnullsub'=''$$^%PEEKBYNAME("sgmnt_data.null_subs",$zpiece(rgn,",",i)) $ecode=",U251,"
	. set $zgbldir=sav
	;
	set ttprfx="tt"_type				; prefix for trigger template
	set:type\4#2 ttprfx="tt4"			; types 4 and 6 share one template family; see @xv1/@xv2 in xr4subst
	set killtrg="rk"_nullsub			; template for KILL trigger
	set recvskip=""					; the KILL template's guard: the record side sets it per level (xr4trigs)
	do:type\4#2 xr4subst				; substitution strings for the tt4* templates
	; Determine whether to xref pieces or entire node, and act accordingly
	if $zlength(sep) do				; xref pieces
	. set:'$zlength($get(pnum)) $ecode=",U250,"
	. ; Make any trigger updates needed
	. view "ztrigger_output":0
	. tstart ():transactionid="batch"
	. do:$data(@name@(10))#10
	. . set tmp=^(10)
	. . set:stat>tmp $ecode=",U240,"
	. . set:stat<tmp stat=tmp
	. set oldpstr=$get(@name@(4),"#")
	. set newpstr=$$unravel(pnum)
	. do:oldpstr'=newpstr
	. . for i=2:1:$zlength(newpstr) set:+$zextract(oldpstr,i)'=+$zextract(newpstr,i) $zextract(newpstr,i)=1
	. . set:$zlength(oldpstr)>$zlength(newpstr) $zextract(newpstr,i+1,$zlength(oldpstr))=$zextract(oldpstr,i+1,$zlength(oldpstr))
	. . set pieces=$$ravel(newpstr)
	. . ; remove existing triggers
	. . if $ztrigger("item","-%ydb"_$zextract(name,10,$zlength(name))_"*")
	. . if 'stat do
	. . . set trigset=trigprefix_"set -name=%ydb"_suffix_"S -xecute="_$$exptempl(ttprfx_"Sp0")
	. . . set trigdel=trigprefix_"kill -name=%ydb"_suffix_"K -xecute="_$$exptempl(killtrg)
	. . . set trigdelx=trigprefix_"zkill -name=%ydb"_suffix_"Z -xecute="_$$exptempl(ttprfx_"ZKp0")
	. . else  if 1=stat do
	. . . set trigset=trigprefix_"set -name=%ydb"_suffix_"S -xecute="_$$exptempl(ttprfx_"Sp1")
	. . . set trigdel=trigprefix_"kill -name=%ydb"_suffix_"K -xecute="_$$exptempl(killtrg)
	. . . set trigdelx=trigprefix_"zkill -name=%ydb"_suffix_"Z -xecute="_$$exptempl(ttprfx_"ZKp1")
	. . else  if 2=stat do
	. . . set trigset=trigprefix_"set -name=%ydb"_suffix_"S -xecute="_$$exptempl(ttprfx_"Sp2")
	. . . set trigdel=trigprefix_"kill -name=%ydb"_suffix_"K -xecute="_$$exptempl(killtrg)
	. . . set trigdelx=trigprefix_"zkill -name=%ydb"_suffix_"Z -xecute="_$$exptempl(ttprfx_"ZKp2")
	. . else  set $ecode=",U241,"
	. . set @name=gbl,@name@(4)=newpstr,^(5)=z,^(6)=trigset,^(7)=trigdel,^(8)=trigdelx,^(9)=omitfix,^(10)=stat
	. . do:$zlength($get(comment)) setcmt($$unravel(pnum))	; per piece: a comment belongs to the piece the caller named
	. . set:'($ztrigger("item",trigset)&$ztrigger("item",trigdel)&$ztrigger("item",trigdelx)) $ecode=",U239,"
	. . do xtratrig		; set additional triggers for higher levels in the tree
	. . do:type\4#2 xr4trigs	; and the record side's triggers
	. . set ^%ydbAIMDxref(gbl,name)=""
	. tcommit
	. view "ztrigger_output":ztout
	. ; Cross reference existing nodes, if needed. Note that even if this
	. ; process set triggers, another concurrent process might have
	. ; cross referenced the pieces this process wants xref'd.
	. set oldpstr=$get(@name@(3),"#"),modflag=0
	. for i=2:1:$zlength(newpstr) if +$zextract(newpstr,i)&'+$zextract(oldpstr,i) set modflag=1 quit
	. do:modflag
	. . do xrefdatajobs(nsubs)
	. . ; Update metadata to indicate completion
	. . tstart ():transactionid="batch"
	. . set @name=gbl,@name@(0)=$zut_" "_$job_" "_$zyrelease_" "_swver,^(1)=nsubs,^(2)=sep
	. . set oldpstr=$get(^(3),"#")
	. . for i=2:1:$zlength(newpstr) set $zextract(oldpstr,i)=$select(+$zextract(newpstr,i):1,1:+$zextract(oldpstr,i))
	. . set ^(3)=oldpstr
	. . tcommit
	else  do			; No piece sep; xref entire node
	. set:$zlength($get(pnum)) $ecode=",U238,"
	. view "ztrigger_output":0
	. tstart ():transactionid="batch"
	. do:$data(@name@(10))#10
	. . set tmp=^(10)
	. . set:stat>tmp $ecode=",U240,"
	. . set:stat<tmp stat=tmp
	. do:'($data(@name@(0))#10)
	. . if $ztrigger("item","-%ydb"_$zextract(name,10,$zlength(name))_"*")
	. . if 'stat do
	. . . set trigset=trigprefix_"set -name=%ydb"_suffix_"S -xecute="_$$exptempl(ttprfx_"Se0")
	. . . set trigdel=trigprefix_"kill -name=%ydb"_suffix_"K -xecute="_$$exptempl(killtrg)
	. . . set trigdelx=trigprefix_"zkill -name=%ydb"_suffix_"Z -xecute="_$$exptempl(ttprfx_"ZKe0")
	. . else  if 1=stat do
	. . . set trigset=trigprefix_"set -name=%ydb"_suffix_"S -xecute="_$$exptempl(ttprfx_"Se1")
	. . . set trigdel=trigprefix_"kill -name=%ydb"_suffix_"K -xecute="_$$exptempl(killtrg)
	. . . set trigdelx=trigprefix_"zkill -name=%ydb"_suffix_"Z -xecute="_$$exptempl(ttprfx_"ZKe1")
	. . else  if 2=stat do
	. . . set trigset=trigprefix_"set -name=%ydb"_suffix_"S -xecute="_$$exptempl(ttprfx_"Se2")
	. . . set trigdel=trigprefix_"kill -name=%ydb"_suffix_"K -xecute="_$$exptempl(killtrg)
	. . . set trigdelx=trigprefix_"zkill -name=%ydb"_suffix_"Z -xecute="_$$exptempl(ttprfx_"ZKe2")
	. . else  set $ecode=",U241,"
	. . set @name=gbl,@name@(6)=trigset,^(7)=trigdel,^(8)=trigdelx,^(9)=omitfix,^(10)=stat
	. . set:$zlength($get(comment)) ^(11)=comment
	. . set:'($ztrigger("item",trigset)&$ztrigger("item",trigdel)&$ztrigger("item",trigdelx)) $ecode=",U239,"
	. . do xtratrig		; set additional triggers for higher level nodes
	. . do:type\4#2 xr4trigs	; and the record side's triggers
	. . set ^%ydbAIMDxref(gbl,name)=""
	. tcommit
	. view "ztrigger_output":ztout
	. set newpstr=""
	. do xrefdatajobs(nsubs)
	. ; Add metadata to indicate completion
	. tstart ():transactionid="batch"
	. set @name@(0)=$zut_" "_$job_" "_$zyrelease_" "_swver,^(1)=nsubs,(^(2),^(3),^(4),^(5))=""
	. tcommit
	; label to which premature termination of xrefdatajobs() does ZGOTO
XREFDATAQUIT
	; Release locks that block UNXREFDATA()
	lock:$data(name) -@name@($job)
	lock:$data(gbl) -^%ydbAIMD(gbl,$job)
	lock -^%ydbAIMD($job)
	do:$data(currgld) resetgbldir
	set $zinterrupt=zintrptsav
	quit:$quit name quit

; XREFSUB() cross references metadata of subscripts.  The number in the comment is
; the major version for subscript metadata. Refer to comments before the VERSION()
; label. The initial scan uses several processes, as described for XREFDATA().
XREFSUB(gbl,xsub,snum,nmonly,omitfix,stat,type,force,comment);2
	new $etrap,io do etrap
	new constlist,currgld,currlck,envgld,extgld,fullsub,fulltrigsub
	new gblind,i,j,killtrg,lastfullsub,lastsub,lastsubind,lastvarsub,locxsub,recvskip
	new locsnum,modflag,name,nameind,newsbits,nsnum,nsubs,nullsub,oldsbits
	new omitflag,stacklvl1,sub,subary,suffix,swver,tlevel,tmp,trigdel,trigdelx
	new trigprefix,trigset,trigsub,ttprfx,valcntind,xrefind,zintrptsav,ztout
	new zyintrsig
	set stacklvl1=$stack	; required by premature termination
	set tlevel=$tlevel	; required by error trap to rollback/unwind
	do initxref		; initialization code shared with XREFDATA()
	set:$zlength(type)&((2'=type)) $ecode=",U237,"
	set tmp=0 for i=1:1:nsubs if '$get(constlist(i),0) set tmp=1 quit
	set:'tmp $ecode=",U230,"
	; Derive subscript specification for trigger definitions from parameters
	; and build string from which to derive xref variable name
	for i=1:1:nsubs do
	. set name=name_locxsub(i)
	. set lastfullsub="sub"_i,trigsub=trigsub_lastfullsub_"=",fulltrigsub=fulltrigsub_lastfullsub_","
	. set fullsub=fullsub_lastfullsub_","
	. set trigsub=trigsub_locxsub(i)_","
	. if omitfix&constlist(i) set omitflag=1
	. else  set lastsub=lastfullsub,sub=sub_lastfullsub_","
	; Remove trailing commas from building subscript lists
	set $zextract(sub,$zlength(sub))=""
	set $zextract(fullsub,$zlength(fullsub))=""
	set $zextract(fulltrigsub,$zlength(fulltrigsub))=""
	set $zextract(trigsub,$zlength(trigsub))=""
	set suffix=$zysuffix("sub"_gbl_name_$select(omitflag:1,1:"")_type_force)	; "sub" is included to avoid hash collision with XREFDATA()
	set name="^%ydbAIMS"_suffix
	; Flagging this error needs to be deferred as the error handler may
	; need name to be set.
	set:'$data(lastsub) $ecode=",U244,"
	; Quit if caller only wants the variable name. Note that asking for a
	; name requires that XREFSUB() be called as a function that returns
	; a value, as calling it as a routine asking for a name is a
	; meaningless operation that is likely an application program bug.
	if +$get(nmonly) do:$data(currgld) resetgbldir quit name
	; Generate enumerated list of subscripts to be cross referenced
	set:'$zlength($get(snum)) $ecode=",U228,"
	set locsnum=""
	for i=1:1:$zlength(snum,";") do
	. set tmp=$zpiece(snum,";",i) do:$zlength(tmp)
	. . for j=+$zpiece(tmp,":",1):1:+$zpiece(tmp,":",$zlength(tmp,":")) set locsnum=locsnum_","_j
	set $zextract(locsnum,1)="",nsnum=$zlength(locsnum,",")
	set:1>+$zpiece(locsnum,",",1)!(nsubs<$zpiece(locsnum,",",nsnum)) $ecode=",U228,"
	set newsbits=$get(@name@(3),$zbitstr(nsubs))
	set tmp=nsnum for i=1:1:tmp set newsbits=$zbitset(newsbits,$zpiece(locsnum,",",i),1)
	do mkindxrefsub
	set ztout=$view("ztrigger_output")
	; Common prefix for all triggers
	set trigprefix="+"_gbl_"("_trigsub_") -command="
	lock +(^%ydbAIMS($job),^%ydbAIMS(gbl,$job),@name@($job))
	set stat=+$get(stat)
	set ttprfx="tts"_type				; prefix for trigger template
	set killtrg="rk"_nullsub			; template for KILL trigger
	set recvskip=""					; the KILL template's guard: the record side sets it per level (xr4trigs)
	; Set triggers
	view "ztrigger_output":0
	tstart ():transactionid="batch"
	set oldsbits=$get(@name@(4),$zbitstr(nsubs))
	do:$data(@name@(10))#10
	. set tmp=^(10)
	. set:stat>tmp $ecode=",U240,"
	. set:stat<tmp stat=tmp
	do:oldsbits'=newsbits
	. set newsbits=$zbitor(newsbits,oldsbits)
	. if $ztrigger("item","-%ydb"_$zextract(name,10,$zlength(name))_"*")
	. if 'stat do
	. . set trigset=trigprefix_"set -name=%ydb"_suffix_"S -xecute="_$$exptempl(ttprfx_"S0")
	. . set trigdel=trigprefix_"kill -name=%ydb"_suffix_"K -xecute="_$$exptempl(killtrg)
	. . set trigdelx=trigprefix_"zkill -name=%ydb"_suffix_"Z -xecute="_$$exptempl(ttprfx_"ZK0")
	. else  if 1=stat do
	. . set trigset=trigprefix_"set -name=%ydb"_suffix_"S -xecute="_$$exptempl(ttprfx_"S1")
	. . set trigdel=trigprefix_"kill -name=%ydb"_suffix_"K -xecute="_$$exptempl(killtrg)
	. . set trigdelx=trigprefix_"zkill -name=%ydb"_suffix_"Z -xecute="_$$exptempl(ttprfx_"ZK1")
	. else  if 2=stat do
	. . set trigset=trigprefix_"set -name=%ydb"_suffix_"S -xecute="_$$exptempl(ttprfx_"S2")
	. . set trigdel=trigprefix_"kill -name=%ydb"_suffix_"K -xecute="_$$exptempl(killtrg)
	. . set trigdelx=trigprefix_"zkill -name=%ydb"_suffix_"Z -xecute="_$$exptempl(ttprfx_"ZK2")
	. else  set $ecode=",U241,"
	. set @name=gbl,@name@(4)=newsbits,@name@(6)=trigset,^(7)=trigdel,^(8)=trigdelx,^(9)=omitfix,^(10)=stat
	. set:$zlength($get(comment)) ^(11)=comment
	. set:'($ztrigger("item",trigset)&$ztrigger("item",trigdel)&$ztrigger("item",trigdelx)) $ecode=",U239,"
	. do xtratrig		; set additional triggers for higher level nodes
	. set ^%ydbAIMSxref(gbl,name)=""
	tcommit
	view "ztrigger_output":ztout
	; Cross reference existing nodes, if needed. Note that even if this
	; process set triggers, another concurrent process might have
	; cross referenced the pieces this process wants xref'd.
	do:$get(@name@(3),$zbitstr(nsubs))'=newsbits
	. do xrefsubjobs(nsubs)
	. ; Update metadata to indicate completion
	. tstart ():transactionid="batch"
	. set @name@(0)=$zut_" "_$job_" "_$zyrelease_" "_swver,^(1)=nsubs,^(3)=$zbitor($get(^(3),$zbitstr(nsubs)),newsbits)
	. tcommit
	; label to which premature termination of xrefsubjobs() does ZGOTO
XREFSUBQUIT
	; Release locks that block UNXREFSUB()
	lock:$data(name) -@name@($job)
	lock:$data(gbl) -^%ydbAIMS(gbl,$job)
	lock -^%ydbAIMS($job)
	do:$data(currgld) resetgbldir
	set $zinterrupt=zintrptsav
	quit:$quit name quit

; The functions below are intended only to be called internally. Therefore,
; they assume that parameters have been validated by the caller. Also, as
; some of them are helper functions, analogous to macros in some other
; languages, they use variables from caller code, as documented.

; Checks for whether the single parameter meets one of the following trigger
; specifications supported by Application Independent Metadata:
; - A specific value, e.g., 100.1 or "PATIENT" with * (the default if not
;   specified) to indicate that all subscripts at that level should be matched.
; - An inclusive range of specific values separated by a colon, e.g.,
;   "1700:1799" or "A":"Z" where an omitted value is either the first possible
;   subscript or the last possible subscript in the collation sequence for that
;   global variable. If both subscripts are omitted, all subscripts are matched
;   (i.e., ":" is equivalent to "*").
; - A semicolon (;) separated list of values or inclusive values of either of
;   the two forms above.
; Returns 1 if the trigger specification is a constant, 0 otherwise. Invalid
; specifications set $ecode and raise an error.
chktrgspec:(locxsub,n)
	new i,lower,okflag,piecelen,subpiece,upper
	quit:"*"=locxsub(n) 0
	set okflag=1
	for i=1:1:$length(locxsub(n),";") do  quit:'okflag
	. set subpiece=$piece(locxsub(n),";",i),piecelen=$length(subpiece,":")
	. ; Nothing to check if single value; only ranges need to be checked
	. if 1=piecelen
	. else  if 2=piecelen do
	. . if (":"=$zextract(subpiece,1))!(":"=$zextract(subpiece,$zlength(subpiece)))
	. . else  do
	. . . set lower=$piece(subpiece,":",1),upper=$piece(subpiece,":",2)
	. . . ; if upper=lower, this piece is really a constant value
	. . . if upper=lower set piecelen=1,$piece(locxsub(n),";",i)=lower
	. . . else  set:lower]]upper $ecode=",U246,"
	. else  set okflag=0,$ecode=",U245,"
	quit (1=i)&(1=piecelen)

; Expand multiline trigger templates for XREFSUB() and pass through trigger template
; lines for XREFDATA()
; Uses local variables from XREFSUB(): locsnum
; Modifies local variables from caller: line
expline:(line)
	set line=$zpiece(line,";",2,$zlength(line,";"))
	quit:'$data(locsnum)!'$find(line,"@locsnum") line
	new i,j,k,newline
	set newline=" "
	for i=1:1:nsnum do
	. set j=$zpiece(locsnum,",",i),newline=newline_$zpiece(line,"@locsnum",1)
	. for k=2:1:$zlength(line,"@locsnum") set newline=newline_j_$zpiece(line,"@locsnum",k)
	. set newline=newline_$char(10)_" "
	quit $zextract(newline,1,$zlength(newline)-2)

; Expand trigger template into -xecute string for trigger Note that this relies
; on the (per standard) behavior of $text() to report empty source code lines
; as a single space.
; Notes:
; - exptempl() uses the line just before the label to get list of variables
;   to replace.  A variable name ending in $ means $ZWRITE() its value when
;   expanding.
; - code between slashes (///) is replaced, with the first part used if force
;   is non-zero, and the second if it is zero
; Uses local variables from XREF*() but does not substitute them: force
; Uses local variables from XREF*(): fullsub,fulltrigsub,gbl,lastfullsub,lastsubind,lastsub,name,nsubs,pieces,recaliver,recalive,recvskip,rtsub,rvalnode,sep$,sub,vrecexq,xfnp1,xfnp2,xv1,xv2,xnl,z
exptempl:(lab)
	new i,j,len,line,multiline,num,outstr,npieces,prefix,rep,str,suffix,tmp,var,vars,zflag
	set line=$$expline($text(@lab))
	set str=line
	for i=1:1 set tmp=$text(@lab+i) quit:" "=tmp  set str=str_$char(10)_$$expline(tmp)
	; Set template options to force string collation if specified
	set npieces=$zlength(str,"/")-1
	set:npieces#3 $ecode=",U231,"
	do:npieces
	. for i=npieces:-3:2 set $zpiece(str,"/",$select(force:i,1:i-1))=""
	. set str=$ztranslate(str,"/")	; Remove / option separators
	set vars=$zpiece($text(exptempl+-1),": ",2)
	set outstr=str	; handle case where there are no substitutions
	for i=1:1:$zlength(vars,",") set tmp=$zpiece(vars,",",i),var=$zpiece(tmp,"$",1),zflag=$zlength(tmp,"$")-1 do:$data(@var)
	. set rep="@"_var,len=$zlength(str,rep),outstr=$zpiece(str,rep,1)
	. for j=2:1:len do
	. . set outstr=outstr_$select(zflag:$zwrite(@var),1:@var)_$zpiece(str,rep,j)
	. set str=outstr
	; Remove repeated subscripts for XREFSUB() triggers that don't force string collation or use transformation function
	set prefix=""
	;
	; XREFSUB() trigger code only ($data(locsnum)): type=4/6 record-side trigger code embeds a path
	; into the source global, whose constant subscript matches the pattern by accident
	; E.g. XREFDATA() trigger code reading $get(^t4oe(100,sub2,"S")) contains "(100,sub2,", and is
	; left as it is.
	;
	for  quit:'$data(locsnum)!(outstr'?.E1"(".N1",sub".N1",".E)  do
	. set prefix=prefix_$zpiece(outstr,"$data(",1)_"$data(",outstr=$zpiece(outstr,"$data(",2,$zlength(outstr,"$data("))
	. set prefix=prefix_$zpiece(outstr,",sub",1)_",sub",outstr=$zpiece(outstr,",sub",2,$zlength(outstr,",sub"))
	. set num=$zpiece(outstr,",",1)
	. set prefix=prefix_num,outstr=$zpiece(outstr,num,2,$zlength(outstr,num))
	. set tmp=",sub"_num
	. set prefix=prefix_$zpiece(outstr,tmp,1),outstr=$zpiece(outstr,tmp,2,$zlength(outstr,tmp))
	set:$zlength(prefix) outstr=prefix_outstr
	do:$data(extgld)	; Triggers should use extended references to match global variable
	. set tmp=$zextract(name,2,$zlength(name)),str=$zpiece(outstr,name,1)
	. for i=2:1:$zlength(outstr,name) set str=str_"^|"""_extgld_"""|"_tmp_$zpiece(outstr,name,i)
	. set outstr=str
	. ; Also prefix source global (gbl) references in trigger body so they hit extended db (the tt4 templates name it)
	. set tmp=$zextract(gbl,2,$zlength(gbl)),str=$zpiece(outstr,gbl,1)
	. for i=2:1:$zlength(outstr,gbl) set str=str_"^|"""_extgld_"""|"_tmp_$zpiece(outstr,gbl,i)
	. set outstr=str
	; a substitution may bring a line of its own (xr4alv), so count the lines last
	set multiline=$zlength(outstr,$char(10))-1
	quit $select(multiline:"<<"_$char(10)_outstr_$c(10),1:$zwrite(outstr))

; Output metadata for a specific xref variable.
lsxref:(lvn,xref)
	tstart ():transactionid="batch"
	do:$data(@xref)
	. new s
	. set lvn(xref)=@xref
	. set s="" for  set:$data(@xref@(s))#10 lvn(xref,s)=^(s) set s=$order(^(s)) quit:'$zlength(s)
	tcommit
	quit

; Initialization common to XREFDATA() and XREFSUB()
; References or modifies caller's local variables:
;  constlist,currgld,currlck,extgld,force,fullsub,fulltrigsub,gbl,lastfullsub,
;  locxsub,name,nsubs,nullsub,omitfix,omitflag,sub,swver,trigsub,type,xsub,
;  zintrptsav
initxref:
	new i,lab,rtn,tmp
	set zintrptsav=$zinterrupt
	set tmp=$stack($stack-1,"place")
	set lab=$zpiece($zpiece(tmp,"^",1),"+",1),rtn=$zpiece(tmp,"^",2)
	set swver=$zpiece($text(@(lab_"^"_rtn)),";",2)_"."_$zpiece($text(@(rtn_"^"_rtn)),";",2)
	set:'$data(gbl) $ecode=",U252,"
	do:gbl?1"^|".E1"|"1(1"%",1AN).AN setgbldir
	do snaplck(.currlck)
	; Xrefs are only supported for valid global variables other than AIM global variables
	set:gbl'?1"^"1(1"%",1AN).AN!(32<$zlength(gbl)) $ecode=",U252,"
	set:gbl?1"^%ydbAIM".AN $ecode=",U243,"
	; Ensure force has a value and convert 0 to "" for backward compatibility
	if '$data(force)!(0=force) set force=""
	; Ensure type has a value and convert 0 to "" for backward compatibility
	if '$data(type)!(0=type) set type=""
	; While the caller is expected to pass both type and force if specified as 1
	; this raises the possibility of ambiguity in the AIM variable name if one of
	; them defaults to and the other does not (both add 1 to the suffix). Therefore
	; for calculating the suffix, force is negated, so that type_force has the
	; following values:
	; - "" both default
	; - "1" type specified, force defaults
	; - "-1" type defaults; force specified
	; - "1-1" both specified
	; If type>1, there is no ambiguity
	if type\2#2 do	; Superficial syntax check of transformation function
	. set xfnp1=$zpiece(force,"(",1)_"(",xfnp2=$zpiece(force,"(",2,$zlength(force,"("))
	. set:(xfnp1'?1(1"$$"0.1"%".an1"^"0.1"%",1"$").an1"(")!(xfnp2'?1(1")",1","1.e1")")) $ecode=",U229,"
	. set xfntest=$$^%ZMVALID("set zyx="_xfnp1_"zyx"_xfnp2),$ecode=""	; compile & discard errors
	. set:$zlength(xfntest) $ecode=",U229,"
	else  do
	. set:($zlength(type)&(1'=type)&(4'=type))!($zlength(force)&(1'=force)) $ecode=",U237,"
	. set:$zlength(force) force=-force
	;
	; A record specification combines with a transformation function as type=6, but not with
	; the Fileman bit: 5 and 7 mean nothing (5 is rejected just above).
	; E.g. type=6 with force="$$f^rtn()" is accepted; type=7 with the same force is U237.
	set:7=type $ecode=",U237,"
	;
	; If constraints specified for subscripts, ensure all refer to
	; subscripts that are integers in the range 1 through 29
	set nsubs=$get(xsub,0)	; Ensure Number of subcripts has a value
	do:$data(xsub)\10
	. set i="" for  set i=$order(xsub(i)) quit:'$zlength(i)  do:$data(xsub(i))#10
	. . set:1>i!(i\1'=i)!(29<i) $ecode=",U247,"
	. . set tmp=xsub(i),locxsub(i)=$select(":"=tmp:"*",1:tmp)
	. . set constlist(i)=$$chktrgspec(.locxsub,i)
	. set i=$order(locxsub(""),-1)
	. set:i>nsubs nsubs=i
	set:nsubs\1'=nsubs!(29<nsubs) $ecode=",U247,"
	; Reject an invalid number of processes here, before anything is created.
	; The scan runs after the triggers are set and the cross reference is
	; registered, so an error raised there would leave a cross reference that
	; is registered but was never scanned. xrefnproc() resolves this value.
	set tmp=$ztrnlnm("ydb_aim_nproc")
	set:$zlength(tmp)&((tmp'?1.N)!('tmp)) $ecode=",U225,"
	set:$select(1=type:2,1:1)>nsubs $ecode=",U253,"
	for i=1:1:nsubs set:'$data(locxsub(i)) locxsub(i)="*",constlist(i)=0
	; Determine whether application global permits null subscripts. For a
	; global variable that spans multiple regions, all regions must be
	; consistent in allowing or not allowing null subscripts. Two cascading
	; unary operators ('') are used to force non-zero values to 1.
	set tmp=$view("region",gbl),nullsub=''$$^%PEEKBYNAME("sgmnt_data.null_subs",$zpiece(tmp,",",1))
	for i=2:1:$zlength(tmp,",") set:nullsub'=''$$^%PEEKBYNAME("sgmnt_data.null_subs",$zpiece(tmp,",",i)) $ecode=",U251,"
	set omitfix=+$get(omitfix,1),omitflag=0
	set (fullsub,fulltrigsub,name,sub,trigsub)=""
	quit

; Create indirection strings to be used by xrefdata()
; Uses or references local variables passed to or defined in XREFDATA():
;   constlist, gbl, gblind, lastlit, lastvarsub, name, nameind, nsubs,
;   omitfix, recdup, sep, subary, type, valcntind, valind, xrefind
mkindxrefdata:
	new i,tmp,valtype
	set gblind(1)=gbl_"("_$select(constlist(1):locxsub(1),1:"subary(1)")
	for i=2:1:nsubs set gblind(i)=gblind(i-1)_","_$select(constlist(i):locxsub(i),1:"subary("_i_")")
	for i=1:1:nsubs set gblind(i)=gblind(i)_")"
	set xrefind=name_"("_$select($zlength(sep):"k,"_$select(force:"""#""_",1:"")_"pieceval",1:"0,"_$select(force:"""#""_",1:"")_"nodeval")
	for i=1:1:nsubs do
	. if constlist(i) do
	. . if 'omitfix set lastvarsub=i,lastsubind=locxsub(i),xrefind=xrefind_","_lastsubind
	. else  set lastvarsub=i,lastsubind="subary("_i_")",xrefind=xrefind_","_lastsubind
	set xrefind=xrefind_")"
	; The code that writes cross reference entries at the bottom of xrefdata() reads the value node as
	; $get(@valind) - a reference, not
	; an expression: atom indirection reaches a variable only - and writes a cross reference entry's
	; last subscript as lastsubind itself when that is a literal (a constant last subscript
	; with omitfix=0), through indirection otherwise; both are per scan.
	set valind=gblind(nsubs),lastlit=constlist(nsubs)&(lastvarsub=nsubs)
	if $zlength(sep) set nameind=name_"(-k,"_$select(force:"""#""_",1:"")_"pieceval)",valcntind=name_"(-k)"
	else  set nameind=name_"("""","_$select(force:"""#""_",1:"")_"nodeval)",valcntind=name_"("""")"
	do:type\2#2	; Transformation function used for metadata
	. set valtype=$select($zfind(xrefind,"nodeval"):"nodeval",1:"pieceval")
	. set xrefind=$zpiece(xrefind,valtype,1)_xfnp1_valtype_xfnp2_$zpiece(xrefind,valtype,2)
	. set nameind=$zpiece(nameind,valtype,1)_xfnp1_valtype_xfnp2_$zpiece(nameind,valtype,2)
	;
	; Types 4 and 6: the scan walks the RECORD global, so subary() holds record subscripts,
	; while the strings built above name the VALUE global by the value specification's own
	; levels. This block maps between the two.
	; - vpos(k) is the level of xsub holding the k-th value subscript; rpos(k) is the level
	;   of the record specification holding the same subscript; veck is how many there are.
	; - recgblind(i) is the record global down to level i, for the walk.
	; - recdup says whether two records can share the same value subscripts, which is so only
	;   when the record has more subscripts than the value, e.g. records ^G(A,B) under the value
	;   key A. Then the scan must cross reference each key once, which xr4rec does by comparing a
	;   record's key with the previous record's; otherwise it skips that comparison.
	; - valind is the value node, named by the record subscripts its key maps to. The code
	;   that writes cross reference entries at the bottom of xrefdata() reads $get of it, so a value
	;   node that is absent or valueless cross references its record under the empty string.
	; - xrefind and lastsubind, built above as subary(vpos(k)), have to say subary(rpos(k)).
	;   The renaming goes through a placeholder because a value position can equal another
	;   subscript's record position: renaming subary(1) to subary(2) and then subary(2) to
	;   subary(3) in one pass would carry the first subscript on to subary(3) as well.
	; E.g. gbl=^H, xsub(1)=":", xsub(2)=":" with recgbl=^G, recsub(1)=100, recsub(2)=":",
	; recsub(3)=":", recsub=3:
	;   veck=2, vpos(1)=1, vpos(2)=2, rpos(1)=2, rpos(2)=3, recdup=0
	;   recgblind(3)=^G(100,subary(2),subary(3)), valind=^H(subary(2),subary(3))
	;   xrefind names subary(2) and subary(3) where it named subary(1) and subary(2).
	do:type\4#2
	. set veck=0 for i=1:1:nsubs set:'constlist(i) veck=veck+1,vpos(veck)=i												; vpos(k): the k-th value subscript as a level of xsub
	. set tmp=0 for i=1:1:recnsubs set:reckey(i) tmp=tmp+1,rpos(tmp)=i												; rpos(k): the same subscript as a level of the record specification
	. set recdup=recklast>rpos(veck)																; can two records share their value subscripts? only when the record has more subscripts
	. set recgblind(1)=recgblx_"("_$select(recconstlist(1):reclocxsub(1),1:"subary(1)")										; recgblind(i): the record global down to level i, constants spelled out, varying levels as subary(i)
	. for i=2:1:recnsubs set recgblind(i)=recgblind(i-1)_","_$select(recconstlist(i):reclocxsub(i),1:"subary("_i_")")
	. for i=1:1:recnsubs set recgblind(i)=recgblind(i)_")"
	. set tmp=0,valind=gbl_"("
	. for i=1:1:nsubs set valind=valind_$select(constlist(i):locxsub(i),1:"subary("_rpos($increment(tmp))_")")_$select(i<nsubs:",",1:")")				; e.g. ^H(subary(2)): the value node, named by the record subscripts its key maps to
	. for i=1:1:veck set xrefind=$$subst(xrefind,"subary("_vpos(i)_")","subary(#"_i_"#)"),lastsubind=$$subst(lastsubind,"subary("_vpos(i)_")","subary(#"_i_"#)")	; subary(vpos(k)) -> placeholder
	. for i=1:1:veck set xrefind=$$subst(xrefind,"subary(#"_i_"#)","subary("_rpos(i)_")"),lastsubind=$$subst(lastsubind,"subary(#"_i_"#)","subary("_rpos(i)_")")	; placeholder -> subary(rpos(k))
	quit

; Replace every occurrence of from in str with to
; E.g. $$subst("a(1),a(2)","a(1)","a(#1#)") returns "a(#1#),a(2)"
subst:(str,from,to)
	new i,out
	set out=$zpiece(str,from,1)
	for i=2:1:$zlength(str,from) set out=out_to_$zpiece(str,from,i)
	quit out

; Create indirection strings to be used by xrefsub()
; References or updates variables passed in to, or defined in XREFSUB():
;   constlist,gblind,lastsubind,locsnum,locxsub,nsubs,xrefind
mkindxrefsub:
	new i,snum,tmp
	; Indirection variables for application globals
	set gblind(1)=gbl_"("_$select(constlist(1):locxsub(1),1:"subary(1)")
	for i=2:1:nsubs set gblind(i)=gblind(i-1)_","_$select(constlist(i):locxsub(i),1:"subary("_i_")")
	for i=1:1:nsubs set gblind(i)=gblind(i)_")"
	; Indirection variables for cross reference globals
	for i=1:1:nsnum do
	. set snum=$zpiece(locsnum,",",i)
	. set valcntind(i)=name_"("_-snum_")"
	. if 1<type do
	. . set nameind(i)=name_"("_-snum_","_xfnp1_"subary("_snum_")"_xfnp2_")"
	. . set xrefind(i)=name_"("_snum_","_xfnp1_"subary("_snum_")"_xfnp2
	. else  do
	. . set nameind(i)=name_"("_-snum_","_$select(force:"""#""_",1:"")_"subary("_snum_"))"
	. . set xrefind(i)=name_"("_snum_","_$select(force:"""#""_",1:"")_"subary("_snum_")"
	. set lastsubind="sub1"
	. for j=1:1:nsubs do:'(constlist(j)&omitfix)&(j'=snum!$zlength(force)!(1<type))
	. . set lastvarsub=j,lastsubind="subary("_j_")",xrefind(i)=xrefind(i)_","_lastsubind,lastsubind="sub"_j
	. set xrefind(i)=xrefind(i)_")"
	set:'$data(xrefind) $ecode=",U244,"
	quit

; ravel() takes a bit-map like piece number string, e.g., "#0010100111", and
; composes from it the form suitable for the index values of a for loop,
; e.g., 3,5,8,9,10
ravel:(pstr)
	new i,newpspec
	set newpspec=""
	for i=2:1:$zlength(pstr) if $zextract(pstr,i) set newpspec=newpspec_(i-1)_","
	quit $zextract(newpspec,1,$zlength(newpspec)-1)

; If original global reference was an extended reference, restore the
; global directory and environment variable (if set). It is assumed
; that the global variable (gbl) is passed by value, and therefore
; does not need to be restored. The returned name needs to have the
; directory of the extended reference.
; References or modifies the caller's local variables:
;  envgld,name
resetgbldir:
	new tmp
	; Add the extended reference to the cross reference name if it exists
	; (LSXREF* and UNXREF* labels NEW name, leaving it undefined)
	set:$data(name) name="^|"""_$zgbldir_"""|"_$zpiece(name,"^",2)
	set $zgbldir=currgld
	if $data(envgld) set tmp=$order(envgld("")) view "setenv":tmp:envgld(tmp)
	else  view "unsetenv":"ydb_gbldir"
	quit

; Record the caller's comment in metadata slot 11, subscripted by piece number. One
; cross reference serves every piece of a node, and callers describing different
; pieces share it, so a comment belongs to the piece the caller named: pstr is the
; piece bit-map of this call alone. Uses XREFDATA() locals: comment,name
; E.g. a call with pnum=3 and comment="orders.status" sets @name@(11,3)="orders.status".
setcmt:(pstr)
	new i
	for i=2:1:$zlength(pstr) set:+$zextract(pstr,i) @name@(11,i-1)=comment
	quit

; If gbl is an extended reference, save the current global directory,
; the ydb_gbldir/gtmgbldir environment variables, if set (they might
; not be set), switch global directories, and make gbl a normal
; global variable, i.e., not an extended reference.
; References or modifies the caller's local variables:
;  currgld,envgld,extgld,gbl
setgbldir:
	new tmp
	set tmp=$select($zlength($ztrnlnm("ydb_gbldir")):"ydb_gbldir",$zlength($ztrnlnm("gtmgbldir")):"gtmgbldir",1:"")
	set:$zlength(tmp) envgld(tmp)=$ztrnlnm(tmp)
	set extgld=$zsearch($select(""""=$zextract(gbl,3):$zwrite($zpiece(gbl,"|",2),1),1:$zpiece(gbl,"|",2)),-1)
	set:'$zlength(extgld) $ecode=",U227,"
	set currgld=$zgbldir,$zgbldir=extgld	; global directory for this process
	view "setenv":"ydb_gbldir":$zgbldir	; global directory for JOB'd processes
	set gbl="^"_$zpiece(gbl,"|",3)	; with $zgbldir set, no need for extended reference
	quit

; Snapshots state of locks. Parameter must be passed by reference.
snaplck:(snap)
	new i,lcks,tmp
	zshow "l":lcks
	for i=1:1 quit:'$data(lcks("L",i))  do
	. set tmp=lcks("L",i)
	. set snap($piece($piece(tmp,"LOCK ",2)," LEVEL=",1))=$piece(tmp," LEVEL=",2)
	quit

; This label takes a piece number specification, e.g., "3;5;8:10" and returns
; the bit-map like piece specification, preceded by "#", e.g., "#0010100111" for
; the example above.  and determines if there are new pieces to xref. Pieces are
; separated by semi-colons. Each piece can be a number, or two colon separated
; numbers, with the first number smaller than the second.
unravel:(pnum)
	new k,newpstr,nextp,nextp1,nextpl,nextplen,nextpu
	set newpstr="#"
	for k=1:1:$zlength(pnum,";") do
	. set nextp=$zpiece(pnum,";",k),nextplen=$zlength(nextp,":")
	. if 1=nextplen do
	. . set:nextp\1'=nextp!(nextp<=0) $ecode=",U248,"
	. . set nextp1=nextp+1
	. . set:nextp1>$zlength(newpstr) newpstr=newpstr_$ztranslate($justify("",nextp1-$zlength(newpstr))," ",0)
	. . set $zextract(newpstr,nextp1)=1
	. else  if 2=nextplen do
	. . set nextpl=$zpiece(nextp,":",1),nextpu=$zpiece(nextp,":",2)
	. . set:(nextpl\1'=nextpl)!(0>=nextpl)!(nextpu\1'=nextpu)!(0>=nextpu)!(nextpu<nextpl) $ecode=",U248,"
	. . set nextp1=nextpu+1
	. . set:nextp1>$zlength(newpstr) newpstr=newpstr_$ztranslate($justify("",nextp1-$zlength(newpstr))," ",0)
	. . for j=nextpl+1:1:nextp1 set $zextract(newpstr,j)=1
	. else  set $ecode=",U249,"
	quit newpstr

; Releases locks so that locks owned by the process match those in the
; parameter, which should be created by snaplck(). Parameter should be
; passed by reference.
unsnaplck:(oldlck)
	new currlck,i,lck,relstr
	do snaplck(.currlck)
	set lck=""
	for  set lck=$order(currlck(lck)) quit:'$zlength(lck)  do
	. if $increment(currlck(lck),-$get(oldlck(lck),0))
	. for i=1:1:currlck(lck) lock -@lck
	quit

; Given the name of a cross reference global, this function removes the triggers
; used to maintain the xref, and then deletes the cross reference global
; variable. Variable(s) used from caller: gbl
unxref:(xrefgbl)
	do:$data(@xrefgbl)
	. new i,tmp,trig,xrefvar,ztout
	. ; Caller determines whether data or subscript xref to be deleted
	. set xrefvar="^%ydbAIM"_$select("UNXREFDATA"=$zpiece($zpiece($stack($stack-2,"place"),"^",1),"+",1):"D",1:"S")_"xref"
	. ; The M lock protects against concurrent XREFDATA() and UNXREFDATA()
	. ; and the transaction ensures Consistency of the metadata about the
	. ; metadata.
	. lock +@xrefgbl
	. set ztout=$view("ztrigger_output")
	. view "ztrigger_output":0
	. tstart ():transactionid="batch"
	. ; YDBAIM metadata variables start with ^%ydbAIMD/^%ydbAIMS, but the trigger
	. ; names start with %ydb. Both have the same $ZYHASH(), but trigger
	. ; names additionally have a suffix.
	. if $ztrigger("item","-%ydb"_$zextract(xrefgbl,10,$zlength(xrefgbl))_"*")
	. ;
	. ; Types 4 and 6 with the record global in another database (slot 99): its
	. ; record-side triggers are defined there, where the wildcard above cannot see them
	. ; E.g. gbl=^H, recgbl=^|"x.gld"|G: the triggers on ^G are in the database x.gld maps, and
	. ; slot 99 holds the path of x.gld.
	. do:$data(@xrefgbl@(99))#10
	. . new sav
	. . set sav=$zgbldir,$zgbldir=@xrefgbl@(99)	; the record database, recorded by xr4trigs
	. . if $ztrigger("item","-%ydb"_$zextract(xrefgbl,10,$zlength(xrefgbl))_"*")
	. . set $zgbldir=sav
	. ;
	. kill @xrefgbl
	. zkill @xrefvar@(gbl,xrefgbl)
	. tcommit
	. view "ztrigger_output":ztout
	. lock -@xrefgbl
	quit

; Generate cross references for nodes at the specified subscript level. It goes
; through the global at each level with fewer subscripts than the cross
; reference calls for, and where a subtree is found, recursively calls itself on
; that subtree till it reaches the specified level, whereon it generates the
; cross references. Since triggers are set before calling this routine, they
; will take care of cross references for any node deletions. This label only
; needs to be concerned with setting cross references for nodes that don't
; already have cross references, which is done in a transaction to ensure
; Consistency.
; The following code has some built-in assumptions about the frequency of
; occurrence of different options, and actual application data might imply
; a different ordering of tests for the various code paths. Fortunately, this
; code is executed only for the initial creation of a cross reference, and not
; in the triggers that maintain cross references as globals are updated.
; References variables defined in parent or passed through from XREFDATA():
;   constlist, force, gbl, gblind, lastlit, lastsubind, locxsub, name,
;   nameind, newpstr, nullsub, omitfix, recdup, sep, stacklvl1, stacklvl2, stat,
;   subary, type, valcntind, valind, xfnp1, xfnp2, xrefind, zpiece
; A worker receives only the variables xrefjobmore() keeps visible, so a
; variable added here must be added to its exclusive NEW as well.
; xrefwork() calls this once for each range of subscripts it claims, with
; locxsub(chunklvl) set to that range.
xrefdata(nsubsxref,ppid)
	new flag,i,j,k,nodelen1,nodeval,nranges,piece1,piece2,pieceval,quitflag
	new rangebegin,rangeend,sublvl,thisrange,tmp2
	; If nsubsxref>1 it means call the function recursively for the next
	; subscript level.
	set flag=nullsub
	set sublvl=$order(locxsub(""),-1)-nsubsxref+1
	set subary(sublvl)=""
	if nsubsxref>1 do
	. if "*"=locxsub(sublvl) for  do:flag  set flag=1,subary(sublvl)=$order(@gblind(sublvl)) quit:'$zlength(subary(sublvl))
	. . do:$data(@gblind(sublvl))\10 xrefdata(nsubsxref-1,ppid)
	. ; The IF 1 leaves $TEST true: a DO with an argument leaves $TEST as the callee's
	. ; last IF set it, and the ELSE below would then walk this level a second time.
	. ; E.g. ^g("fix",:): the call for the bottom level returns with $TEST=0, and the ELSE
	. ; would walk the "fix" level again.
	. else  if $get(constlist(sublvl),0) do:$data(@gblind(sublvl))\10 xrefdata(nsubsxref-1,ppid) if 1
	. else  do
	. . set nranges=$select(zpiece:$zlength(locxsub(sublvl),";"),1:$length(locxsub(sublvl),";"))
	. . for i=1:1:nranges do
	. . . if zpiece do
	. . . . set thisrange=$zpiece(locxsub(sublvl),";",i)
	. . . . set piece1=$zpiece(thisrange,":",1),piece2=$zpiece(thisrange,":",$zlength(thisrange,":"))
	. . . else  do
	. . . . set thisrange=$piece(locxsub(sublvl),";",i)
	. . . . set piece1=$piece(thisrange,":",1),piece2=$piece(thisrange,":",$length(thisrange,":"))
	. . . if $zlength(piece1) set rangebegin=$zwrite(piece1,1)
	. . . else  set rangebegin=""
	. . . if $zlength(piece2) set rangeend=$zwrite(piece2,1)
	. . . else  set rangeend=""
	. . . set flag=$select($zlength(piece1):1,1:nullsub)
	. . . set subary(sublvl)=rangebegin
	. . . for  do:flag  set flag=1,(subary(sublvl),tmp2)=$order(@gblind(sublvl)) quit:'$zlength(tmp2)!($zlength(piece2)&(tmp2]]rangeend))
	. . . . do:$data(@gblind(sublvl))\10 xrefdata(nsubsxref-1,ppid)
	; if nsubsxref=1 (the rest of the code below) the traversal has reached the
	; subscript level at which globals are to have metadata
	; computed. Compute metadata per subscript and metadata type
	; specification.
	else  if constlist(sublvl) do
	. if type\4#2 set xr4ok=$data(@gblind(sublvl)) do xr4rec quit:'xr4ok	; types 4 and 6: xr4rec checks whether a record exists here and is not yet cross referenced; if not, skip it
	. tstart ():transactionid="batch"
	. if '(type\4#2),'($data(@gblind(sublvl))#10) tcommit  quit	; a node without a value is not cross referenced - except for types 4 and 6, where xr4rec has admitted the record and $get below makes an absent value node an empty-string entry
	. set nodeval=$get(@valind)
	. if $zlength(sep) do
	. . set nodelen1=$zlength(newpstr)
	. . for i=2:1:nodelen1 do:+$zextract(newpstr,i)
	. . . set k=i-1,pieceval=$select(zpiece:$zpiece(nodeval,sep,k),1:$piece(nodeval,sep,k))
	. . . do:'($data(@xrefind)#10)
	. . . . set ^($select(lastlit:lastsubind,1:@lastsubind))=""
	. . . . if stat,$increment(@nameind),(2=stat),1=@nameind,$increment(@valcntind)
	. else  do:'$data(@xrefind)
	. . set ^($select(lastlit:lastsubind,1:@lastsubind))=""
	. . if stat,$increment(@nameind),(2=stat),1=@nameind,$increment(@valcntind)
	. tcommit
	else  do								; not Fileman or a constant subscript specification
	. set nranges=$select(zpiece:$zlength(locxsub(sublvl),";"),1:$length(locxsub(sublvl),";"))
	. for i=1:1:nranges do
	. . if "*"=locxsub(sublvl) do
	. . . set (piece2,rangebegin,rangeend,subary(sublvl))="",flag=nullsub
	. . else  do
	. . . if zpiece do
	. . . . set thisrange=$zpiece(locxsub(sublvl),";",i)
	. . . . set piece1=$zpiece(thisrange,":",1),piece2=$zpiece(thisrange,":",$zlength(thisrange,":"))
	. . . else  do
	. . . . set thisrange=$piece(locxsub(sublvl),";",i)
	. . . . set piece1=$piece(thisrange,":",1),piece2=$piece(thisrange,":",$length(thisrange,":"))
	. . . if $zlength(piece1) set rangebegin=$zwrite(piece1,1)
	. . . else  set rangebegin=""
	. . . if $zlength(piece2) set rangeend=$zwrite(piece2,1)
	. . . else  set rangeend=""
	. . . ; flag=1 means the for-loop below processes subary's starting value before $order'ing onward.
	. . . ; Set it only when subary starts on a real bounded subscript, i.e. when the range has a
	. . . ; lower bound; otherwise subary="" is just a walk sentinel, and only a database that
	. . . ; permits null subscripts has a subscript there to process.
	. . . set flag=$select($zlength(piece1):1,1:nullsub)
	. . . set subary(sublvl)=rangebegin
	. . for  do:flag  set flag=1,(subary(sublvl),tmp2)=$order(@gblind(sublvl)) quit:'$zlength(tmp2)!($zlength(piece2)&(tmp2]]rangeend))
	. . . if type\4#2 set xr4ok=$data(@gblind(sublvl)) do xr4rec quit:'xr4ok	; types 4 and 6: xr4rec checks whether a record exists here and is not yet cross referenced; if not, skip it
	. . . tstart (j,k):transactionid="batch"
	. . . if '(type\4#2),'($data(@gblind(sublvl))#10) tcommit  quit	; a node without a value is not cross referenced - except for types 4 and 6, where xr4rec has admitted the record and $get below makes an absent value node an empty-string entry
	. . . set nodeval=$get(@valind)
	. . . if $zlength(sep) do
	. . . . set nodelen1=$zlength(newpstr)
	. . . . for j=2:1:nodelen1 do:+$zextract(newpstr,j)
	. . . . . set k=j-1,pieceval=$select(zpiece:$zpiece(nodeval,sep,k),1:$piece(nodeval,sep,k))
	. . . . . if '($data(@xrefind)#10) set ^($select(lastlit:lastsubind,1:@lastsubind))="" if stat,$increment(@nameind),(2=stat),1=@nameind,$increment(@valcntind)
	. . . else  if '($data(@xrefind)#10) set ^($select(lastlit:lastsubind,1:@lastsubind))="" if stat,$increment(@nameind),(2=stat),1=@nameind,$increment(@valcntind)
	. . . tcommit
	quit

; Types 4 and 6: xrefdata() has reached a record node, subary(1..nsubs) - nsubs being the
; record node's depth in a type=4/6 scan (xrefdatajobs) - with xr4ok its $DATA. Decides
; whether xrefdata() cross references it: xr4ok=1 for a record node at which a record exists and the first of its
; value subscripts, 0 otherwise. Two records share their value subscripts only when the record has more
; subscripts than the value (recdup); then record nodes are visited in subscript order, so equal value subscripts
; are adjacent, and lastvk holds the previous one. The indexing is xrefdata()'s own code:
; it reads $get of the value node, valind (mkindxrefdata), "" when that node is absent
; or valueless, and names the record subscripts the value subscripts map to.
; E.g. the value node ^H(A) with records ^G(A,B), the records being ^G(1,1), ^G(1,2), ^G(2,1):
; at ^G(1,1) the key 1 is new, so ^H(1) is cross referenced; ^G(1,2) has the same key, so nothing is
; done; at ^G(2,1) the key 2 is new, so ^H(2) is cross referenced - as the empty-string entry for key 2 when
; ^H(2) does not exist.
xr4rec:
	; Nothing is NEWed here: this runs once per record node over million-node globals.
	; xr4i, xr4ok, xr4pfx, xr4q, xr4vk and lastvk are NEWed once per worker by xrefwork().
	;
	; A record node without a value of its own is a record only if a node with a value
	; exists within the depth, the levels the triggers watch. xr4ok arrives as the
	; $DATA of the record node, which the caller evaluated to get here. Only 10 is
	; examined: no value but descendants, so the first $QUERY from the node is a
	; descendant, and its $QLENGTH against the depth settles it. 0 is possible, and
	; is no record: the first key of a bounded range is visited without an $ORDER, so
	; it need not exist, and a $QUERY from an absent node returns its successor,
	; which is no descendant, or "", which $QLENGTH rejects. Only when that first descendant lies
	; below the depth does the walk go on, through the subtree while a node starts
	; with the record node's name, until a node within the depth is found or the
	; subtree ends; a subtree with nodes below the depth alone makes no record. With no
	; level below the record node to watch there is nothing to look for.
	; The indirection is through gblind(sublvl), one string for the whole scan, which
	; the indirection cache serves; the continuation, with @xr4q a different string
	; per record, is the rare path.
	; E.g. recsub(1)=":", recsub=2, at the record node ^G(8), which has no value of its own:
	;   ^G(8,1)="" alone       the first $QUERY from ^G(8) is ^G(8,1): $QLENGTH 2, within the depth,
	;                          so ^G(8) is a record and the walk is never entered
	;   ^G(8,0,"X")="" alone   the first $QUERY is ^G(8,0,"X"): $QLENGTH 3, below the depth, so the
	;                          walk goes on; the next $QUERY is ^G(9) or beyond, outside "^G(8,",
	;                          and ^G(8) is not a record
	;   both                   the first $QUERY is ^G(8,0,"X"), below; the next is ^G(8,1), within,
	;                          so ^G(8) is a record
	if 10=xr4ok,recnsubs>sublvl set xr4q=$query(@gblind(sublvl)),xr4i=$qlength(xr4q),xr4ok=(xr4i>sublvl)&(xr4i'>recnsubs) do:'xr4ok&(xr4i>sublvl)
	. set xr4pfx=$name(@gblind(sublvl)),xr4pfx=$zextract(xr4pfx,1,$zlength(xr4pfx)-1)_","
	. for  set xr4q=$query(@xr4q) quit:$zextract(xr4q,1,$zlength(xr4pfx))'=xr4pfx  set:$qlength(xr4q)'>recnsubs xr4ok=1 quit:xr4ok
	set xr4ok=xr4ok#10	; 1 or 11: a value of its own, a record; 0: absent, no record; 10 the IF left alone: nothing below to watch, no record
	quit:'xr4ok
	quit:'recdup	; every record has value subscripts of its own: nothing to collapse
	set xr4vk="" for xr4i=1:1:veck set xr4vk=xr4vk_$char(1)_subary(rpos(xr4i))	; this record's value subscripts, joined
	if xr4vk=$get(lastvk) set xr4ok=0 quit						; the previous record had the same key: that key is cross referenced already
	set lastvk=xr4vk								; remember it for the next record
	quit

; The JOB'd process that scans. It repeatedly takes the next unclaimed range of
; subscripts at level chunklvl, which the parent writes to
; ^%ydbAIMtmp($text(+0),ppid,"q",n), assigns that range to locxsub(chunklvl),
; and calls xrefdata() (issub=0, for XREFDATA()) or xrefsub() (issub=1, for
; XREFSUB()) to scan it. Ranges are claimed with $INCREMENT() on "take", so each
; range is scanned by exactly one process.
;
; "n" is how many ranges the parent has written so far and "end" means it has
; written the last one. A process whose claim is beyond "n" waits, since the
; parent may not have written that range yet, and stops when "end" shows none
; are coming or when the parent is no longer alive. The parent sets "end" after
; the last "n", so re-reading "n" after seeing "end" reads its final value.
;
; Check whether the parent is still alive before claiming each range, and if not
; (e.g., it might have been stopped), then exit. The granularity is one range, so
; a range being scanned when the parent goes away is scanned to its end.
;
; The pid is recorded first and the completion flag last. The first narrows the
; window described in xrefdatajobs(); the second is how the parent knows this
; process scanned every range it claimed, which no other process covers.
; Local variables come from the parent through passcurlvn, limited by
; xrefjobmore() to those listed there: xrefwork() itself uses chunklvl, locxsub
; and nsubs.
xrefwork(ppid,wnum,issub)
	new lastvk,more,seq,xr4i,xr4ok,xr4pfx,xr4q,xr4vk
	set ^%ydbAIMtmp($text(+0),ppid,0,$job)=""
	for  do  quit:'more
	. set more=1
	. ; Terminate with an incomplete scan if the parent process no longer exists.
	. if '$zgetjpi(ppid,"isprocalive") set more=0 quit
	. set seq=$increment(^%ydbAIMtmp($text(+0),ppid,"take"))
	. for  quit:seq'>+$get(^%ydbAIMtmp($text(+0),ppid,"n"))  do  quit:'more
	. . if $data(^%ydbAIMtmp($text(+0),ppid,"end")),seq>+$get(^%ydbAIMtmp($text(+0),ppid,"n")) set more=0 quit
	. . ; The parent produces the queue, so if it is gone there will never be
	. . ; a chunk with this ticket, and nothing is left to report to.
	. . if '$zgetjpi(ppid,"isprocalive") set more=0 quit
	. . hang .001
	. do:more
	. . set:chunklvl locxsub(chunklvl)=^%ydbAIMtmp($text(+0),ppid,"q",seq)
	. . ; Postconditionals rather than IF/ELSE: the scan does not restore $TEST
	. . do:issub xrefsub(nsubs,ppid)
	. . do:'issub xrefdata(nsubs,ppid)
	set ^%ydbAIMtmp($text(+0),ppid,"done",wnum)=""
	quit

; JOB nworkers processes to scan the global variable, and write the ranges of
; subscripts they scan. Each process is numbered 1 through nworkers, and scans
; only the ranges it claims, so the scan is complete only when all of them have
; finished; U226 if any did not.
;
; The parent writes the ranges itself rather than JOBing a process to do it. The
; walk is $ORDER() over one subscript level with none of the $DATA(), SET and TP
; the scan does, so it costs far less, and it overlaps with the scanning because
; the JOB'd processes start on the first ranges while the parent is still
; walking. The parent therefore does not notice a process dying until the walk
; is done. That delays the error but cannot produce an incomplete cross
; reference, because the completion flags are checked before XREFDATA() records
; the cross reference as complete.
; xrefdatajobs() cleans up stdout and stderr files of the JOB'd processes.
; Comments preceding xrefdata() list local variables passed through
xrefdatajobs:(nsubs)
	new chunklvl,cmd,err,i,io,issub,j,jobby,k,line,msgprefix,njobbed,nworkers,out,prefix,stacklvl2,tmp,totcnt,val,xrefproc
	;
	; Types 4 and 6 scan the RECORD specification. Present it under the names the
	; chunker and xrefdata() read, down to the record node; the workers inherit it.
	; E.g. gbl=^H, xsub(1)=":" with recgbl=^G, recsub(1)=100, recsub(2)=":", recsub=2: from here
	; on nsubs=2, constlist(1)=1, gblind(2)=^G(100,subary(2)), and the walk ends on the record
	; nodes ^G(100,id).
	new:type\4#2 constlist,gblind,locxsub,nullsub,subary
	if type\4#2 for i=1:1:recplast set locxsub(i)=reclocxsub(i),gblind(i)=recgblind(i),constlist(i)=recconstlist(i)	; the walk's arrays, from the record specification down to the record node
	set:type\4#2 nsubs=recplast,nullsub=recnullsub
	;
	set io=$io
	set msgprefix=$ztrnlnm("ydb_msgprefix")
	set:'$zlength(msgprefix) msgprefix="YDB"
	set prefix="/tmp/xrefdata^"_$text(+0)_"_"_$job_"_"_($zut\1E6)_"_"
	set stacklvl2=$stack	; required by premature termination
	set chunklvl=$$xrefchunklvl,nworkers=$select(chunklvl:$$xrefnproc,1:1)
	;
	; Types 4 and 6: xr4rec collapses records sharing their value subscripts by comparing each
	; with the previous one, which holds only while one process visits them in key
	; order. A level below the deepest record subscript the value subscripts map to, rpos(veck), is
	; reached only when every level above it holds one subscript, so one chunk is
	; the same division.
	; E.g. records ^G(A,B) with the value subscript A, so rpos(veck)=1: dividing at level 2 would hand
	; the B's of one A to several workers, and each of them would cross reference A.
	set:(type\4#2)&(chunklvl>rpos(veck)) chunklvl=0,nworkers=1
	;
	set:1<nworkers nworkers=$$xrefcapworkers(nworkers)
	; Job the worker processes and record their pids so that they can be
	; subsequently identified, e.g., to terminate them. Since JOB'd
	; processes sever all ties with the parent process, if a terminating
	; interrupt is received after a JOB command executes, but before the
	; pid is captured from $ZJOB, there is potentially a JOB'd process
	; that is not reflected in ^%ydbAIMtmp($text(+0),$job,0). To avoid
	; a scenario where the parent is terminated, but a JOB'd process
	; continues scanning and creating cross references after the parent
	; has been terminated, the parent temporarily ignores interrupts
	; until it records the JOB'd pids. Once it records those pids
	; it sets the interrupt handler to terminate them, and itself in
	; response to an interrupt. The pids are recorded as subscripts of
	; ^%ydbAIMtmp($text(+0),$job,0) rather than as a list, so that a
	; worker recording its own pid (see xrefwork()) and the parent
	; recording it write the same node.
	kill ^%ydbAIMtmp($text(+0),$job)	; Clear any prior subprocess metadata
	set issub=0,njobbed=0,jobby=$zut+50000	; 50ms before recruiting the rest
	do xrefjobmore(1)	; one to begin with; the rest only if the scan is long
	do xrefqueue		; produce the chunks the workers scan
	; Wait for every worker to finish, recruiting the rest along the way
	for  do  quit:'$data(xrefproc)  hang .001
	. do xrefrecruit
	. set i="" for  set i=$order(xrefproc(i)) quit:'$zlength(i)  kill:'$zgetjpi(xrefproc(i),"isprocalive") xrefproc(i)
	; quit		; uncomment for debugging
	; Raise an error for any message that is neither informational (-I-) nor success
	; (-S-), other than FORCEDHALT. YottaDB writes a message such as GVIS on the same
	; line as the message it describes, so it is the severity of the message the line
	; begins with, not any severity on the line, that says whether it reports an error.
	for i=1:1:njobbed do
	. set out(i)=$zsearch(out(i)_"*") open out(i)
	. for j=1:1 use out(i) read line quit:$zeof  if $zlength(line) use io write line,! use out(i)
	. use io close out(i):delete
	. set err(i)=$zsearch(err(i)_"*") open err(i)
	. ; Check for error messages generated by YottaDB
	. set tmp="%"_msgprefix_"-"
	. for j=1:1 use err(i) read line quit:$zeof  do:$zlength(line)&'$zlength($zpiece(line,tmp,1))
	. . use io
	. . set:'($zfind(line,"FORCEDHALT")!($zextract(line,$zlength(tmp)+1,$zlength(tmp)+2)?1(1"I",1"S")1"-")) $ecode=",U233,"
	. . use err(i)
	. use io close err(i):delete
	; Every worker must have recorded completion. A worker that ended without
	; reporting an error, e.g. one that was killed, has left part of the
	; global variable unscanned, and no other worker covers it.
	for i=1:1:njobbed set:'$data(^%ydbAIMtmp($text(+0),$job,"done",i)) $ecode=",U226,"
	kill ^%ydbAIMtmp($text(+0),$job)	; Clear subprocess metadata on clean exit
	quit
	;
ZINTERRUPT ;
	set zyintrsig=$zyintrsig
	zgoto:"SIGUSR2"=$zyintrsig stacklvl2:xrefjobsterm
	; switch global directories back to original one while doing interrupt
	set:$data(currgld) $zgbldir=currgld
	xecute zintrptsav
	set:$data(extgld) $zgbldir=extgld
	quit

; Kill child processes and handle an interrrupt
; Uses local variables from caller: stacklvl, stacklvl2, xrefproc, zintrptsav
; It tests for the existence of zpiece.
xrefjobsterm
	new i,proc,waiting
	set proc="" for  set proc=$order(^%ydbAIMtmp($text(+0),$job,0,proc)) quit:'$zlength(proc)  if $zsigproc(proc,"TERM")
	; As noted in xrefdatajobs, there is the potential for a child process
	; to exist whose pid is not captured in ^%ydbAIMtmp($text(+0),$job,0).
	; As this pid will be captured in $zjob, terminate that process if it
	; exists. This has the infinitesimally small probability that if the
	; pid is reused by a different process after the $zsigproc() above,
	; and that different process has the same uid, then that different
	; process will be inadvertently terminated.
	if $zjob,$zsigproc($zjob,"TERM")
	; Wait for the processes signaled above to exit before releasing the locks
	; that block UNXREFDATA(). One still running when this process returns can
	; write to a cross reference the caller has since removed, leaving an AIM
	; global variable with no ^%ydbAIMDxref entry for UNXREFDATA() to find.
	; Bounded, since this is the interrupt path; U224 if the bound expires.
	for i=1:1:10000 do  quit:'waiting
	. set waiting=0,proc=""
	. for  set proc=$order(^%ydbAIMtmp($text(+0),$job,0,proc)) quit:'$zlength(proc)  set:$zgetjpi(proc,"isprocalive") waiting=1
	. set:$zjob&$zgetjpi($zjob,"isprocalive") waiting=1
	. hang:waiting .001
	set:waiting $ecode=",U224,"
	; Release M locks
	if $data(zpiece) do XREFDATAQUIT
	else  do XREFSUBQUIT
	; Restore the original interrupt handler, and re-throw the interrupt,
	; an asynchronous operation.
	set $zinterrupt=zintrptsav
	if $zsigproc($job,$select("SIGUSR2"=zyintrsig:"USR2",1:"USR1"))
	hang 1E46		; wait for interrupt
	set $ecode=",U232,"	; error - caller's $ZINTERRUPT returned control
	zhalt 1			; should never get here

; JOB processes numbered njobbed+1 through "upto" and record their pids, so that
; they can be identified later, e.g. to terminate them.
;
; Since JOB'd processes sever all ties with the parent process, if a terminating
; interrupt is received after a JOB command executes, but before the pid is
; captured from $ZJOB, there is potentially a JOB'd process that is not
; reflected in ^%ydbAIMtmp($text(+0),$job,0). To avoid a scenario where the
; parent is terminated, but a JOB'd process continues scanning and creating
; cross references after the parent has been terminated, the parent temporarily
; ignores interrupts until it records the JOB'd pids. Once it records those pids
; it sets the interrupt handler to terminate them, and itself in response to an
; interrupt. The pids are recorded as subscripts of
; ^%ydbAIMtmp($text(+0),$job,0) rather than as a list, so that a worker
; recording its own pid (see xrefwork()) and the parent recording it write the
; same node.
;
; The exclusive NEW below ensures that only variables used by the JOB'd
; processes, and by AIM while it JOBs them, are passed to them. Specifically:
; - those the workers use: see the lists preceding xrefwork(), xrefdata() and
;   xrefsub();
; - those this label uses and updates: upto, err, issub, njobbed, out, prefix,
;   xrefproc;
; - those the error handler needs: io, tlevel, ztout, currlck, currgld, envgld,
;   name, xrefproc, nworkers;
; - those ZINTERRUPT needs: zyintrsig, stacklvl2, currgld, zintrptsav, extgld.
; A variable that any of those starts to use must be added to this list.
; Uses and updates local variables from caller: err, issub, njobbed, out,
; prefix, xrefproc
xrefjobmore:(upto)
	new (chunklvl,constlist,currgld,currlck,envgld,err,extgld,force,gbl,gblind,io,issub,lastlit,lastsubind,locxsub,name,nameind,newpstr,njobbed,nsnum,nsubs,nullsub,nworkers,omitfix,out,prefix,recdup,recnsubs,rpos,sep,stacklvl1,stacklvl2,stat,subary,tlevel,type,upto,valcntind,valind,veck,xfnp1,xfnp2,xrefind,xrefproc,zintrptsav,zpiece,ztout,zyintrsig)
	new cmd,i,intdefer
	quit:upto'>njobbed
	set intdefer="",$zinterrupt="set intdefer=$zyintrsig"	; defer interrupts temporarily
	for i=njobbed+1:1:upto do
	. set err(i)=prefix_i_".err"
	. set out(i)=prefix_i_".out"
	. set cmd="xrefwork($job,i,"_issub_"):(passcurlvn:error="""_err(i)_""":output="""_out(i)_""")"
	. job @cmd
	. if $zjob set xrefproc(i)=$zjob,^%ydbAIMtmp($text(+0),$job,0,$zjob)=""
	. else  set $ecode=",U234,"
	set njobbed=upto
	set $zinterrupt="do ZINTERRUPT^%YDBAIM"
	if $zlength(intdefer),$zsigproc($job,intdefer)	; throw any deferred interrupts
	quit

; JOB the rest of the processes, but only once the scan has been running long
; enough to be worth them and only while there are ranges nobody has claimed.
;
; A process costs a few milliseconds to JOB, which is more than the entire scan
; of a small global variable takes, so a cross reference that is over quickly
; must not pay for a process per CPU. How much data there is cannot be known
; before scanning it: a level of a thousand subscripts may hold a thousand nodes
; or a billion. Elapsed time is therefore the signal, and the one process JOB'd
; at the outset is what produces it. A scan still going when jobby is reached,
; 50ms after it started, has enough work left to be worth dividing further; one
; that is over by then never JOBs anything else.
;
; There are two callers. One is the walk that writes the ranges, which catches a
; scan made slow by a level with many subscripts. The other is the loop in which
; the parent waits for the processes to finish, which catches a scan made slow by
; the amount of data below those subscripts.
; Uses local variables from caller: jobby, njobbed, nworkers
xrefrecruit:
	quit:njobbed'<nworkers
	quit:$zut<jobby
	quit:+$get(^%ydbAIMtmp($text(+0),$job,"take"))'<+$get(^%ydbAIMtmp($text(+0),$job,"n"))
	do xrefjobmore(nworkers)
	quit

; The subscript level whose subscripts are divided among the JOB'd processes, or
; 0 if there is none. It is the outermost level whose specification is not a
; constant, except that a level holding exactly one subscript is pinned to that
; subscript and the next level that is not a constant is taken instead. Levels
; above the one returned are therefore either constants which
; mkindxrefdata()/mkindxrefsub() have already built into gblind(), or levels
; pinned here, so a range of this level identifies part of the global variable by
; itself; levels below it are scanned in full for each subscript in the range.
;
; Descending past a level with one subscript is what lets a specification like
; (*,*,*) over ^g(1,2,:) be divided at all: dividing the outermost level would
; produce a single range covering everything, leaving one process to scan the
; whole global variable, which is slower than the two process scan this
; replaces. Pinning it costs two $ORDER()s and hands the division to the level
; that has the subscripts. There is no descent when every level below is a
; constant, since a constant level has no subscripts of its own to divide.
;
; It follows that no more processes can be kept busy than there are subscripts
; at the level returned, whatever ydb_aim_nproc says. A specification of (*,*)
; over a global variable with three first subscripts divides three ways.
; Uses local variables from caller: constlist, gblind, locxsub, nsubs, nullsub,
; subary. Pins a level by setting subary() and locxsub() for it.
xrefchunklvl:()
	new i,lvl,nxt
	set lvl=0
	for i=1:1:nsubs if '$get(constlist(i),0) set lvl=i quit
	quit:'lvl 0
	; Descend while this level holds one subscript and there is another level
	; below it that is not a constant to divide instead. A constant level is
	; skipped over rather than divided: mkindxrefdata()/mkindxrefsub() have
	; already built it into gblind(), and it has no subscripts of its own to cut.
	for  do  quit:'nxt
	. set nxt=0
	. for i=lvl+1:1:nsubs if '$get(constlist(i),0) set nxt=i quit
	. quit:'nxt				; nothing below to divide instead
	. if '$$xrefonesub(lvl) set nxt=0 quit	; more than one subscript; divide here
	. set lvl=nxt
	quit lvl

; Pin level lvl if it holds exactly one subscript: set subary(lvl) to that
; subscript, for the walk that writes ranges, and locxsub(lvl) to it in the
; ZWRITE format a caller writes a specification in, for the scan the JOB'd
; processes run, which parses it as a range whose ends are that one subscript.
; Returns whether it did.
;
; Only a level specified as "*" is pinned. A caller's own range is left as
; written, so its endpoints, and with them whether a null subscript at an open
; start is scanned, are unchanged. A level that has a null subscript is left
; alone as well: $ORDER() does not return one, so pinning would drop it. So is a
; subscript whose ZWRITE representation holds ":" or ";", which cannot be
; written into a specification, the restriction xrefqueue() observes in choosing
; where to cut.
;
; A subscript that appears at a pinned level after this runs is not scanned, and
; does not need to be: the triggers are in place before the scan starts, so
; every node written from then on is cross referenced as it is written. This is
; the same reason the ranges may be cut from the subscripts that exist at the
; time of the walk.
; Uses local variables from caller: gblind, locxsub, nullsub, subary
xrefonesub:(lvl)
	new sub,zw
	quit:"*"'=locxsub(lvl) 0
	set subary(lvl)=""
	; Separate IF arguments, so that $DATA() is not evaluated, and NULSUBSC not
	; raised, in a region that does not permit null subscripts
	if nullsub,$data(@gblind(lvl)) quit 0
	set sub=$order(@gblind(lvl))
	quit:'$zlength(sub) 0		; no subscripts at all; nothing to divide either
	set subary(lvl)=sub,zw=$zwrite(sub)
	quit:$zfind(zw,":")!$zfind(zw,";") 0
	quit:$zlength($order(@gblind(lvl))) 0	; more than one; this is the level to divide
	set locxsub(lvl)=zw
	quit 1

;
; How many processes to JOB: the ydb_aim_nproc environment variable if defined,
; else one less than the number of CPUs, because the parent walks the level to
; write the ranges while the JOB'd processes scan, and so needs a CPU of its
; own. On a single CPU machine that leaves one process. initxref() has already
; rejected a value that is not a positive integer.
; Uses local variable io from caller, to restore $IO
xrefnproc:()
	new n
	set n=$ztrnlnm("ydb_aim_nproc")
	quit:$zlength(n) +n
	set n=$$xrefcpus
	quit:'n 2			; CPU count unavailable; the number AIM used before
	quit $select(1<n:n-1,1:1)

; No more processes than there are subscripts for them to divide. See
; xrefrecruit() above for why a process with nothing to do is worth avoiding.
;
; At most nworkers subscripts are counted, so this walk costs no more than the
; process count, whatever the size of the level. Only the first range of the
; specification is counted: a specification with further ranges has at least as
; many subscripts as its first, so counting that one is enough to decide whether
; there are fewer subscripts than processes.
;
; A level whose subscripts cannot be counted this way, e.g. one holding only a
; null subscript, which $ORDER() does not return, gives one process. That is a
; performance choice and not a correctness one: the ranges cover the whole
; specification whatever the process count, so fewer processes scan the same
; nodes, more slowly.
; Uses local variables from caller: chunklvl, gblind, locxsub, subary
xrefcapworkers:(nworkers)
	new cnt,piece1,piece2,rangeend,sub,tmp
	if "*"=locxsub(chunklvl) set (piece1,piece2)=""
	else  do
	. set tmp=$zpiece(locxsub(chunklvl),";",1)
	. set piece1=$zpiece(tmp,":",1),piece2=$zpiece(tmp,":",$zlength(tmp,":"))
	set rangeend=$select($zlength(piece2):$zwrite(piece2,1),1:"")
	set subary(chunklvl)=$select($zlength(piece1):$zwrite(piece1,1),1:"")
	set cnt=0
	for  set sub=$order(@gblind(chunklvl)) quit:'$zlength(sub)!($zlength(piece2)&(sub]]rangeend))  do  quit:cnt'<nworkers
	. set subary(chunklvl)=sub,cnt=cnt+1
	quit $select(cnt<nworkers:$select(cnt:cnt,1:1),1:nworkers)

; Number of online CPUs, read from the file in which Linux reports them as a
; comma separated list of CPU numbers and inclusive ranges, e.g. "0-3", "0,2-5"
; or, on a single CPU machine, "0". Returns 0 if the file cannot be read, for
; the caller to replace with a default; how many processes to use is a
; performance choice, not a reason to fail a cross reference, so the error trap
; returns 0 for anything the file access raises. Note that these are the CPUs
; that are online, not necessarily those this process may run on.
; Uses local variable io from caller, to restore $IO
xrefcpus:()
	new $etrap,i,line,n,tmp
	set $etrap="use io close tmp set $ecode="""" quit 0"
	set n=0,line="",tmp="/sys/devices/system/cpu/online"
	open tmp:readonly use tmp read line use io close tmp
	quit:'$zlength(line) 0
	for i=1:1:$zlength(line,",") do
	. set tmp=$zpiece(line,",",i)
	. set n=n+$zpiece(tmp,"-",$zlength(tmp,"-"))-$zpiece(tmp,"-",1)+1
	quit n

; Write the ranges of subscripts that the JOB'd processes scan, by walking
; level chunklvl and cutting its subscripts into groups. Each range is a
; subscript specification for that level, in the ZWRITE format a caller uses,
; e.g. 113:120, so that a JOB'd process scans one with no more than an
; assignment to locxsub(chunklvl).
;
; The ranges abut: one ends at a subscript and the next starts at the subscript
; after it. Together they therefore cover exactly the subscripts the caller's
; specification selects, with none scanned twice and none missed. The first
; range written for one of the caller's ranges starts where the caller's range
; starts, and the last ends where it ends, so the caller's own endpoints, and
; with them whether a null subscript at an open start is scanned, are unchanged.
;
; A subscript can end a range only if its ZWRITE representation contains no ":"
; and no ";". Those are the characters that separate a subscript specification
; into ranges, as in "a:b;c:d", so a subscript containing either cannot be
; written into one. This is the same restriction a caller has in writing a
; specification, YottaDB/DB/YDB#691. Cutting waits for the next subscript that
; can be used, which only makes a range larger.
;
; A range holds one subscript to start with, and the number doubles every
; nworkers*4 ranges to a maximum of 8192. How many subscripts the level has, and
; how much of the global variable is below each of them, are not known until the
; walk finishes. Small ranges at the start therefore get every process working
; immediately, and divide a level that has hardly more subscripts than there are
; processes; larger ones later keep down the cost of writing and claiming them.
; Uses local variables from caller: chunklvl, gblind, locxsub, nworkers, subary
xrefqueue:
	new cnt,csize,i,nemit,nranges,ok,piece1,piece2,prevok,prevzw,rangeend,seq,start,sub,tmp,zw
	set (cnt,nemit,seq)=0,csize=1
	if 'chunklvl do  quit		; nothing to divide; one chunk scans it all
	. set ^%ydbAIMtmp($text(+0),$job,"q",1)=""
	. set ^%ydbAIMtmp($text(+0),$job,"n")=1
	. set ^%ydbAIMtmp($text(+0),$job,"end")=1
	set nranges=$select("*"=locxsub(chunklvl):1,1:$zlength(locxsub(chunklvl),";"))
	for i=1:1:nranges do
	. if "*"=locxsub(chunklvl) set (piece1,piece2)=""
	. else  do
	. . set tmp=$zpiece(locxsub(chunklvl),";",i)
	. . set piece1=$zpiece(tmp,":",1),piece2=$zpiece(tmp,":",$zlength(tmp,":"))
	. set rangeend=$select($zlength(piece2):$zwrite(piece2,1),1:"")
	. set (prevok,prevzw)="",cnt=0,start=piece1
	. set subary(chunklvl)=$select($zlength(piece1):$zwrite(piece1,1),1:"")
	. for  set sub=$order(@gblind(chunklvl)) quit:'$zlength(sub)!($zlength(piece2)&(sub]]rangeend))  do
	. . set subary(chunklvl)=sub,zw=$zwrite(sub)
	. . set ok='($zfind(zw,":")!$zfind(zw,";"))
	. . if cnt'<csize,ok,prevok do
	. . . do xrefqemit(start_":"_prevzw)
	. . . set start=zw,cnt=0
	. . set cnt=cnt+1,prevzw=zw,prevok=ok
	. do xrefqemit(start_":"_piece2)
	set ^%ydbAIMtmp($text(+0),$job,"end")=1
	quit

; Write one range. "n" is set after the range itself, so a JOB'd process never
; sees a count that includes a range that has not been written yet.
; Uses and updates local variables from caller: csize, nemit, nworkers, seq
xrefqemit:(chunk)
	set ^%ydbAIMtmp($text(+0),$job,"q",$increment(seq))=chunk
	set ^%ydbAIMtmp($text(+0),$job,"n")=seq
	set:'($increment(nemit)#(nworkers*4))&(8192>csize) csize=csize+csize
	do xrefrecruit
	quit

; Terminate every JOB'd process still running, and forget those that have
; exited or have been sent the signal. One that can neither be signaled nor
; shown to have exited stays in xrefproc for the caller's wait loop to retry.
;
; Testing whether the process is alive before signaling it narrows, but does not
; close, the window in which its pid is reused between the test and the signal.
; This is an acceptable risk, as Linux generates pids in order, rather than
; randomly. So for this to terminate an unrelated process, the pids must cycle
; between the two operations, and the target pid must have the same uid.
; Uses and updates xrefproc from the caller.
xrefjobskill:
	new i
	set i="" for  set i=$order(xrefproc(i)) quit:'$zlength(i)  do
	. if '$zgetjpi(xrefproc(i),"isprocalive") kill xrefproc(i) quit
	. kill:'$zsigproc(xrefproc(i),"term") xrefproc(i)
	quit

; Child process to generate cross references for nodes starting with statistics
; for the first subscript if requested, and cross references and statistics (if
; requested) for nodes through the level nsubs. As this is modeled on xrefdata(),
; comments here are specific to xrefsub().
; References variables defined in parent or passed through from XREFSUB().
;   constlist,gblind,locxsub,nameind,nsnum,nsubs,nullsub,stat,subary,valcntind,
;   xrefind
; A worker receives only the variables xrefjobmore() keeps visible, so a
; variable added here must be added to its exclusive NEW as well.
; xrefwork() calls this once for each range of subscripts it claims, with
; locxsub(chunklvl) set to that range.
xrefsub(nsubsxref,ppid)
	new i,j,nranges,flag,piece1,piece2,rangebegin,rangeend,snum,sublvl,subval,thisrange
	set flag=nullsub
	set sublvl=$order(locxsub(""),-1)-nsubsxref+1
	set subary(sublvl)=""
	if nsubsxref>1 do
	. if "*"=locxsub(sublvl) for  do:flag  set flag=1,subary(sublvl)=$order(@gblind(sublvl)) quit:'$zlength(subary(sublvl))
	. . do:$data(@gblind(sublvl))\10 xrefsub(nsubsxref-1,ppid)
	. else  if $get(constlist(sublvl),0),($data(@gblind(sublvl))\10) set subary(sublvl)=$zwrite(locxsub(sublvl),1) do xrefsub(nsubsxref-1,ppid)
	. else  do
	. . set nranges=$zlength(locxsub(sublvl),";")
	. . for i=1:1:nranges do
	. . . set thisrange=$zpiece(locxsub(sublvl),";",i)
	. . . set piece1=$zpiece(thisrange,":",1),piece2=$zpiece(thisrange,":",$zlength(thisrange,":"))
	. . . if $zlength(piece1) set rangebegin=$zwrite(piece1,1)
	. . . else  set rangebegin=""
	. . . if $zlength(piece2) set rangeend=$zwrite(piece2,1)
	. . . else  set rangeend=""
	. . . set flag=$select($zlength(piece1):1,1:nullsub)
	. . . set subary(sublvl)=rangebegin
	. . . for  do:flag  set flag=1,(subary(sublvl),subval)=$order(@gblind(sublvl)) quit:'$zlength(subval)!($zlength(piece2)&(subval]]rangeend))
	. . . . do:$data(@gblind(sublvl))\10 xrefsub(nsubsxref-1,ppid)
	else  do	; lowest level subscript; cross referencing happens here
	. set (subary(sublvl),subval)=""
	. if "*"=locxsub(sublvl) for  do:flag  set flag=1,(subary(sublvl),subval)=$order(@gblind(sublvl)) quit:'$zlength(subval)
	. . tstart ():transactionid="batch"
	. . do:$data(@gblind(sublvl))#10
	. . .  for snum=1:1:nsnum do:'($data(@xrefind(snum))#10)
	. . . . set @xrefind(snum)=""
	. . . . if stat,$increment(@nameind(snum)),(2=stat),1=@nameind(snum),$increment(@valcntind(snum))
	. . tcommit
	. else  if constlist(sublvl) do
	. . set subary(sublvl)=$zwrite(locxsub(sublvl),1)
	. . tstart ():transactionid="batch"
	. . do:$data(@gblind(sublvl))#10
	. . . for snum=1:1:nsnum do:'($data(@xrefind(snum))#10)
	. . . . set @xrefind(snum)=""
	. . . . if stat,$increment(@nameind(snum)),(2=stat),1=@nameind(snum),$increment(@valcntind(snum))
	. . tcommit
	. else  do
	. . set nranges=$zlength(locxsub(sublvl),";")
	. . for i=1:1:nranges do
	. . . set thisrange=$zpiece(locxsub(sublvl),";",i)
	. . . set piece1=$zpiece(thisrange,":",1),piece2=$zpiece(thisrange,":",$zlength(thisrange,":"))
	. . . if $zlength(piece1) set rangebegin=$zwrite(piece1,1),flag=1
	. . . else  set rangebegin=""
	. . . if $zlength(piece2) set rangeend=$zwrite(piece2,1)
	. . . else  set rangeend="",flag=1
	. . . set flag=$select($zlength(piece1):1,1:nullsub)
	. . . set (subary(sublvl),subval)=rangebegin
	. . . for  do:flag  set flag=1,(subary(sublvl),subval)=$order(@gblind(sublvl)) quit:'$zlength(subval)!($zlength(piece2)&(subval]]rangeend))
	. . . . tstart ():transactionid="batch"
	. . . . do:$data(@gblind(sublvl))#10
	. . . . . for snum=1:1:nsnum do:'($data(@xrefind(snum))#10)
	. . . . . . set @xrefind(snum)=""
	. . . . . . if stat,$increment(@nameind(snum)),(2=stat),1=@nameind(snum),$increment(@valcntind(snum))
	. . . . tcommit
	quit

; JOB nworkers processes to scan for XREFSUB(), and write the ranges of
; subscripts they scan. As xrefsubjobs() is modeled on xrefdatajobs(),
; additional comments here are specific to xrefsub().
; Comments preceding xrefsub() list local variables passsed through.
xrefsubjobs:(nsubs)
	new chunklvl,cmd,err,i,io,issub,j,jobby,line,msgprefix,njobbed,nworkers,out,prefix,stacklvl2,tmp,totcnt,val,xrefproc
	set io=$io
	set msgprefix=$ztrnlnm("ydb_msgprefix")
	set:'$zlength(msgprefix) msgprefix="YDB"
	set prefix="/tmp/xrefsub^"_$text(+0)_"_"_$job_"_"_($zut/1E6)_"_"
	set stacklvl2=$stack
	set chunklvl=$$xrefchunklvl,nworkers=$select(chunklvl:$$xrefnproc,1:1)
	set:1<nworkers nworkers=$$xrefcapworkers(nworkers)
	kill ^%ydbAIMtmp($text(+0),$job)
	set issub=1,njobbed=0,jobby=$zut+50000	; 50ms before recruiting the rest
	do xrefjobmore(1)	; one to begin with; the rest only if the scan is long
	do xrefqueue		; produce the chunks the workers scan
	for  do  quit:'$data(xrefproc)  hang .001
	. do xrefrecruit
	. set i="" for  set i=$order(xrefproc(i)) quit:'$zlength(i)  kill:'$zgetjpi(xrefproc(i),"isprocalive") xrefproc(i)
	; quit		; uncomment for debugging
	; Raise an error for any message that is neither informational (-I-) nor success
	; (-S-), other than FORCEDHALT. YottaDB writes a message such as GVIS on the same
	; line as the message it describes, so it is the severity of the message the line
	; begins with, not any severity on the line, that says whether it reports an error.
	for i=1:1:njobbed do
	. set out(i)=$zsearch(out(i)_"*") open out(i) use out(i)
	. for j=1:1 read line quit:$zeof  if $zlength(line) use io write line,! use out(i)
	. use io close out(i):delete
	. set err(i)=$zsearch(err(i)_"*") open err(i) use err(i)
	. ; Check for error messages generated by YottaDB
	. set tmp="%"_msgprefix_"-"
	. for j=1:1 read line quit:$zeof  do:$zlength(line)&'$zlength($zpiece(line,tmp,1))
	. . use io
	. . set:'($zfind(line,"FORCEDHALT")!($zextract(line,$zlength(tmp)+1,$zlength(tmp)+2)?1(1"I",1"S")1"-")) $ecode=",U233,"
	. . use err(i)
	. close err(i):delete
	for i=1:1:njobbed set:'$data(^%ydbAIMtmp($text(+0),$job,"done",i)) $ecode=",U226,"
	kill ^%ydbAIMtmp($text(+0),$job)	; Clear subprocess metadata on clean exit
	quit

; Set additional triggers as needed. Triggers are set for nodes that are above
; the nodes for which XREFDATA() / XREFSUB() are being called, since XREFDATA() / XREFSUB()
; will set triggers for the nodes for which they should compute and maintain
; metadata. Use the following variables from caller:
; killtrg,locxsub,name,nullsub,sep,stat,suffix,type
xtratrig:
	new endsub,i,tmp,trig,trig1,trig2,trig3,trigsub,trigsuffix
	set trigsuffix=$zpiece($text(xtratrigsufx),";",2)
	set trig1="+"_gbl,trig2=" -command=kill -name=%ydb"_suffix,trig3=" -xecute="_$$exptempl(killtrg)
	set trig=trig1_trig2_$zextract(trigsuffix,1)_trig3
	set:'$ztrigger("item",trig) $ecode=",U239,"
	set @name@(12)=trig
	set trigsub="("
	set endsub=$order(locxsub(""),-1)
	set i="" for  set i=$order(locxsub(i)) quit:i=endsub  do  set $zextract(trigsub,$zlength(trigsub))=","
	. set trigsub=trigsub_"sub"_i_"="_locxsub(i)_")"
	. set trig=trig1_trigsub_trig2_$zextract(trigsuffix,i+1)_trig3
	. set:'$ztrigger("item",trig) $ecode=",U239,"
	. set ^(12+i)=trig
	quit

; Types 4 and 6: the record side of the cross reference. Called from XREFDATA() inside
; its trigger transaction, after xtratrig has defined the value tree's triggers.
; - Removes this cross reference's triggers from the record global's database, which
;   the wildcard in XREFDATA() cannot reach when that is another database, and records
;   that database in slot 99 for unxref().
; - Defines one SET/ZKILL pair per WATCHED level of the record node, slots 100+lvl and
;   140+lvl: a record can be brought into or taken out of existence by a write at any of
;   them, and a trigger fires only for the node actually written.
; - Defines the recursive-KILL triggers on the record tree, slots 51 and 53+i, laid out as
;   the value tree's are: the deepest watched level in 51, as the value node's own KILL is in
;   7, and level i above it in 53+i, the record global itself being level 0, as xtratrig's
;   are in 12+i; 52 is unused. Any specification xtratrig has already covered is skipped: the
;   two trees overlap when recgbl is gbl, and YottaDB folds a second definition of an
;   identical trigger into the first, after which deleting the family by name wildcard fails
;   with TRIGDEFBAD.
; - A trigger belongs to the database holding the global it fires on, and $ztrigger() picks
;   that database from $zgbldir as it runs - the specification itself cannot say which, since
;   a trigger specification may not carry an extended reference (that is why recgblb is the
;   stripped name). initxref left $zgbldir on the value global's database, so a record global
;   living anywhere else needs the switch while its triggers are defined, or they land on a
;   same-named global in the wrong database and never fire. $zgbldir is restored before any
;   error is raised, so the caller unwinds cleanly.
; E.g. gbl=^H, xsub(1)=":" with recgbl=^G, recsub(1)=":", recsub=2:
;   SET/ZKILL pairs on ^G(sub1=*), slots 101 and 141, and on ^G(sub1=*,sub2=*), slots 102 and 142
;   KILL triggers on ^G, slot 53, on ^G(sub1=*), slot 54, and on ^G(sub1=*,sub2=*), slot 51
; Uses XREFDATA() locals: gbl,name,nsubs,recgblb,recgld,reclocxsub,reclvlspec,recnsubs,
;   recnullsub,recplast,recsamedb,sep,stat,suffix
xr4trigs:
	new done,i,lvl,nch,ok,pe,rectrigdelx,rectrigprefix,rectrigset,recvskip,sav,self,spec,trig,trigsuffix,vguard
	do:'recsamedb
	. new sav
	. set sav=$zgbldir,$zgbldir=recgld
	. if $ztrigger("item","-%ydb"_$zextract(name,10,$zlength(name))_"*")	; remove any triggers of this cross reference left in the record database
	. set $zgbldir=sav
	. set @name@(99)=recgld							; where unxref() finds the record database
	;
	; The record node's trigger specification at each level.
	; E.g. recsub(1)=":", recsub=2: reclvlspec(1)="^G(sub1=*)", reclvlspec(2)="^G(sub1=*,sub2=*)"
	set spec=recgblb_"("
	for i=1:1:recnsubs set spec=spec_$select(1<i:",",1:"")_"sub"_i_"="_reclocxsub(i),reclvlspec(i)=spec_")"	; reclvlspec(i): e.g. ^G(sub1=100.01,sub2=*)
	set trigsuffix=$zpiece($text(xtratrigsufx),";",2)							; one letter per level for trigger names
	set pe=$select($zlength(sep):"p",1:"e")									; the piece or the entire-node templates
	;
	; The value node in the record tree: same global and database, at or below the record node.
	; Its own triggers do its work, so the record side defines no second pair on the record
	; node when that IS the value node (the KILL trigger is already skipped, by done() below),
	; and at the watched level that holds the value node its triggers quit for that one node,
	; through @recvskip, which is empty for every other level. The value specification's
	; levels below the record node are constants, since the value subscripts are a prefix of the
	; record subscripts, so the node is named by them.
	; E.g. ^ORD(100.01,ien,.1) with the record ^ORD(100.01,ien), depth 3: at level 3 the
	;      record side's trigger code starts quit:(sub3=.1)
	set vguard=""
	if (recgblb=gbl)&recsamedb&(nsubs>recplast)&(nsubs'>recnsubs) do
	. for i=recplast+1:1:nsubs set vguard=vguard_"&(sub"_i_"="_locxsub(i)_")"
	. set vguard=" ; the value node itself is skipped: its own trigger does its work"_$char(10)_" quit:"_$zextract(vguard,2,$zlength(vguard))_$char(10)_" "	; a line of its own, so that what follows (a comment or a command) starts a line; the leading spaces: body lines that start with a command, not a label
	for lvl=recplast:1:recnsubs do							; the record node and every watched level below it
	. quit:recsamedb&(("+"_reclvlspec(lvl))=$zpiece(@name@(6)," -command=",1))	; the record node is the value node: its own pair serves
	. set recvskip=$select(lvl=nsubs:vguard,1:"")
	. set rectrigprefix="+"_reclvlspec(lvl)_" -command="
	. set self=recgblx_"(sub1" for i=2:1:lvl set self=self_",sub"_i			; this level's node, as the trigger's sub1..subN name it
	. set recaliver=$$xr4alvc(recpfxr,recpl,"$name("_self_"))","alive: a record survives this ZKILL - a node with a value at the record node or below it within the depth, not counting the node being removed, which is still there while the trigger runs")	; the one substitution that varies by level
	. set nch=$zextract(trigsuffix,lvl)						; this level's letter
	. set rectrigset=rectrigprefix_"set -name=%ydb"_suffix_"S"_nch_" -xecute="_$$xr4body($$exptempl("tt4pS"_pe_stat))
	. set rectrigdelx=rectrigprefix_"zkill -name=%ydb"_suffix_"Z"_nch_" -xecute="_$$xr4body($$exptempl("tt4pZK"_pe_stat))
	. set @name@(100+lvl)=rectrigset,^(140+lvl)=rectrigdelx				; while $zgbldir still maps the cross reference
	. set sav=$zgbldir set:'recsamedb $zgbldir=recgld				; define both in the record global's database
	. set ok=$ztrigger("item",rectrigset)&$ztrigger("item",rectrigdelx)
	. set $zgbldir=sav set:'ok $ecode=",U239,"
	;
	; The value tree's recursive-KILL specifications: slot 7 is the value node, 12 the
	; global, 12+i the levels above the value node.
	; E.g. recgbl=gbl=^ORD: xtratrig has a KILL trigger on ^ORD(sub1=100.01), so
	; done("+^ORD(sub1=100.01)") is set and xr4ktrig defines no second one there.
	set done($zpiece(@name@(7)," -command=",1))=""				; the value node's own KILL trigger
	for i=12:1:11+nsubs set done($zpiece(@name@(i)," -command=",1))=""	; the KILL triggers xtratrig defined above it
	;
	; One recursive-KILL trigger per level of the record tree, level 0 being the record global
	; itself: the deepest watched level, recnsubs, in slot 51 with a name ending in rK, as the
	; value node's own is in slot 7 ending in K; every level above it in slot 53+i. Its code is
	; the KILL template the RECORD database's null-subscript setting selects, with the value
	; node's guard at the value node's level.
	; E.g. level 1, spec ^G(sub1=*): +^G(sub1=*) -command=kill -name=%ydb<suffix>r1 -xecute=<the rk0 or rk1 code>
	for i=0:1:recnsubs do
	. set spec=$select(i:reclvlspec(i),1:recgblb)
	. quit:recsamedb&$data(done("+"_spec))			; one database: the value tree already has a KILL trigger on this specification
	. set recvskip=$select(i=nsubs:vguard,1:"")
	. set nch=$select(i=recnsubs:"rK",1:"r"_$zextract(trigsuffix,i+1))
	. set trig="+"_spec_" -command=kill -name=%ydb"_suffix_nch_" -xecute="_$$xr4body($$exptempl("rk"_recnullsub))
	. set @name@($select(i=recnsubs:51,1:53+i))=trig
	. set sav=$zgbldir set:'recsamedb $zgbldir=recgld	; define it in the record global's database
	. set ok=$ztrigger("item",trig)
	. set $zgbldir=sav set:'ok $ecode=",U239,"
	quit

; Record-side trigger code maintains the cross reference, which lives in the VALUE
; global's database, while firing in a process that wrote the record global and is
; therefore looking at another database. Name the cross reference with its database so
; the code means the same thing wherever it runs.
;
; This is done on the finished code rather than by handing exptempl a qualified name,
; because substituting a value containing an extended reference for @name yields it
; twice over. The cross reference name is "^%ydbAIM" plus a hash, so a plain textual
; replacement cannot collide with anything else in the code.
;
; The quoting depends on how exptempl delimited the code: a single line comes back in
; quotes with every quote inside it doubled, a multi-line one in <> with no doubling.
; Returns body unchanged when one database holds everything - every case but a split
; specification. Uses XREFDATA locals: name,recsamedb
; E.g. $zgbldir=/a/c.gld and name=^%ydbAIMDabc: in <> code, ^%ydbAIMDabc(0,p,sub1)
; becomes ^|"/a/c.gld"|%ydbAIMDabc(0,p,sub1); in quoted code, ^|""/a/c.gld""|%ydbAIMDabc(0,p,sub1).
xr4body:(body)
	quit:recsamedb body							; one database: the cross reference's name is good as it is
	new i,out,q,qn
	set q=$select(""""=$zextract(body,1,1):$char(34)_$char(34),1:$char(34))	; a body that is itself a quoted string doubles the quotes inside it
	set qn="^|"_q_$zgbldir_q_"|"_$zextract(name,2,$zlength(name))		; the cross reference as seen from the record database: ^|"<directory>"|%ydbAIMD...
	set out=$zpiece(body,name,1)
	for i=2:1:$zlength(body,name) set out=out_qn_$zpiece(body,name,i)	; every occurrence of the name
	quit out

; Build the type=4/6 trigger-code substitution strings, from the value and record specs:
;   @recalive - value-side ZKILL: a statement line that sets alive to whether a record still
;               exists for the value subscripts once the node being killed has lost its value
;   @vrecexq  - value-side SET: the same line, without an exclusion, asking whether a
;               record exists for the value subscripts at all, followed by the quit that guards
;               the SET; or nothing, when the write being cross referenced is itself what
;               brings the record into existence
;   @rtsub    - the cross reference entry's subscripts, under the record node's trigger names
;   @rvalnode - the value node in gbl, read through the record node's subscripts
; @recaliver, the record-side ZKILL's line, is per level and is built in xr4trigs by the
; same generator, xr4alv.
;
; One definition of a record serves the scan (xr4rec) and every trigger: a record exists
; at a record node when it, or a node below it within the depth, has a value. The node being
; ZKILLed does not count, since its value is going. The generated line walks the
; subtree of the value subscripts' record prefix with $QUERY, bounded by the prefix and the
; depth, and stops at the first node that qualifies.
;
; E.g. gbl=^H, xsub(1)=":" with recgbl=^G, recsub(1)=100, recsub(2)=":", recsub=2:
;   @rtsub=sub2, @rvalnode=^H(sub2), and @vrecexq is
;    set alive=($data(^G(100,sub1))#10),rq=$name(^G(100,sub1)),rq=$zextract(rq,1,$zlength(rq)-1)_",",rp=$query(^G(100,sub1)) for  quit:alive!($zextract(rp,1,$zlength(rq))'=rq)  set:($qlength(rp)'<2)&($qlength(rp)'>2) alive=1 set rp=$query(@rp)
xr4subst:
	new i,jj,onerec,self
	;
	; @xv1/@xv2 bracket a value with however this type represents it in the cross reference, and
	; @xnl is that representation applied to the empty value - the subscript of the empty-string entry:
	;   type=4, force=0 : nothing at all;              the empty value is ""
	;   type=4, force=1 : the "#" marker, "#"_value;   the empty value is "#"
	;   type=6          : the transformation function; the empty value is the function applied to ""
	; Parameterising it this way is what lets type=6 share every template with type=4,
	; rather than needing a transformed copy of each one, and it is why the templates
	; say @xv1...@xv2 instead of the /"#"_//  force conditional the other types use.
	; E.g. type=6, force="$$f^rtn()": @xv1 is $$f^rtn( and @xv2 is ), so @xv1$ztvalue@xv2 is
	; $$f^rtn($ztvalue), and @xnl is $$f^rtn("").
	if type\2#2 set xv1=xfnp1,xv2=xfnp2,xnl=xfnp1_""""""_xfnp2				; type 6: the transformation function around the value; the empty value is it applied to ""
	else  set xv1=$select(force:"""#""_",1:""),xv2="",xnl=$select(force:"""#""",1:"""""")	; type 4: the "#" marker when force; the empty value is "#" or ""
	;
	; The record prefix of the value subscripts: the record specification from the top down to
	; the last value subscript and the constants that follow it, the deepest node the
	; value subscripts fix. Every record for them lies under it. Where no subscript varies
	; below the deepest value subscript it is the record node itself. Spelled twice:
	; recpfx under the value node's trigger names, recpfxr under the record node's.
	; E.g. ^H(id) with records ^G(100,id): recpfx=^G(100,sub1), recpfxr=^G(100,sub2), recpl=2
	;      ^H(id) with records ^G(id,line): recpfx=recpfxr=^G(sub1), recpl=1
	set recpl=rpos(veck),onerec=recvlast'>recpl						; recpl: the level of the last value subscript; onerec: nothing varies below it
	if onerec set recpl=recnsubs								; one record per set of value subscripts: the prefix is the whole specification
	else  for i=recpl+1:1:recvlast quit:'recconstlist(i)  set recpl=i			; else extend it over the constants that follow
	set (recpfx,recpfxr)=recgblx_"(",jj=0
	for i=1:1:recpl do
	. if recconstlist(i) set recpfx=recpfx_reclocxsub(i),recpfxr=recpfxr_reclocxsub(i)	; a constant: spelled out in both
	. else  set jj=jj+1,recpfx=recpfx_"sub"_vpos(jj),recpfxr=recpfxr_"sub"_i		; a record subscript: sub<its level in xsub> under the value node's triggers, sub<i> under the record node's
	. set:i<recpl recpfx=recpfx_",",recpfxr=recpfxr_","
	set recpfx=recpfx_")",recpfxr=recpfxr_")"
	;
	; The value node, as its own triggers name it, so that its going value is not counted.
	; It can only be in the record tree when the two globals are one.
	set self=""
	if (recgblb=gbl)&recsamedb set self=gbl_"(sub1" for i=2:1:nsubs set self=self_",sub"_i	; the value node is in the record tree only when the globals are one
	set:$zlength(self) self="$name("_self_"))"
	set recalive=$$xr4alvc(recpfx,recpl,self,"alive: a record survives this ZKILL - a node with a value at the record node or below it within the depth, not counting the node being removed, which is still there while the trigger runs")
	;
	; The SET guard is not applied when writing the value node is itself what brings the
	; record into existence: same global, same database, and the value node at or below the
	; record node. The node is not in the database while its SET trigger runs, so the
	; walk would not see the record the write creates.
	; E.g. ^H(id) with records ^G(id): the walk; ^ORD(100.01,ien,.1) with the record
	;      ^ORD(100.01,ien): nothing.
	set vrecexq=$select((recgblb=gbl)&recsamedb&(nsubs'<recplast):"",1:$$xr4alvc(recpfx,recpl,"","this SET cannot create a record, the value node being off the record tree: quit unless a record exists - a node with a value at the record node or below it within the depth")_"quit:'alive  ")	; no guard when the SET itself creates the record; else the test, and quit on no record
	;
	; @rtsub: the cross reference entry's subscripts, as @sub lists them for the value node's own
	; triggers, under the names the record node's triggers bind: a value subscript is
	; the record subscript it maps to, and a constant is kept, as a literal, when omitfix=0.
	; E.g. ^H(id) with records ^G(100,id): sub2
	;      ^H(100,id,"X") with records ^G(id), omitfix=0: 100,sub1,"X"
	set rtsub="",jj=0
	for i=1:1:nsubs do
	. if constlist(i) set:'omitfix rtsub=rtsub_locxsub(i)_","	; a constant: kept as a literal when omitfix=0
	. else  set rtsub=rtsub_"sub"_rpos($increment(jj))_","		; a value subscript: the record subscript it maps to
	set $zextract(rtsub,$zlength(rtsub))=""
	;
	; @rvalnode is read by the RECORD-side triggers, which fire in a process that wrote
	; the record global and is looking at that database, not the value global's.
	; E.g. ^H(id) with records ^G(100,id): ^H(sub2), or ^|"/a/c.gld"|H(sub2) when ^G is in
	; another database.
	set rvalnode=gblx_"(",jj=0
	for i=1:1:nsubs set rvalnode=rvalnode_$select(constlist(i):locxsub(i),1:"sub"_rpos($increment(jj)))_$select(i<nsubs:",",1:")")
	;
	quit

; The statement line that sets alive, for trigger code: does a record exist for the value
; key under the record prefix pfx, at level plvl, counting the prefix itself when it is a
; record node, and never the node self, whose value the ZKILL is taking? A record is a node
; with a value at a level from the record node down to the depth, whose constant
; subscripts match the record specification. The result is put in front of the
; template's own line: a prefix test alone shares that line, and a walk, whose FOR
; takes the rest of a line, is a line of its own, ending in the line break and the
; space the next line must start with.
; Three forms, by what lies below the prefix within the depth:
; - nothing: the prefix test alone;
; - one level, the usual Fileman structure: that level's nodes are read directly, a single
;   $DATA when the level is a constant, else $ORDER through them to the first with a value
;   that is not self. No $QUERY, no name strings, and no indirection, which would miss the
;   indirection cache for every record, the name being different each time;
; - more, or a database allowing null subscripts, which $ORDER cannot step through: a
;   $QUERY walk of the subtree, bounded by the prefix's name and the depth, that stops at
;   the first node that is a record and only then takes no further step.
; E.g. ^H(id) with records ^G(id), recsub=1, the value side's ZKILL: " set alive=0 "
; E.g. ^ORD(100.01,ien,.1) with the record ^ORD(100.01,ien), depth 3, the value side's ZKILL:
;    set alive=($data(^ORD(100.01,sub2))#10)&($name(^ORD(100.01,sub2))'=$name(^ORD(sub1,sub2,sub3))) if 'alive set c="" for  set c=$order(^ORD(100.01,sub2,c)) quit:c=""  if c'=sub3,$data(^ORD(100.01,sub2,c))#10 set alive=1 quit
; E.g. pfx="^G(sub1)", plvl=1, records ^G(id,line,0) (recplast=3, depth 3), self="":
;    set alive=0,rq=$name(^G(sub1)),rq=$zextract(rq,1,$zlength(rq)-1)_",",rp=$query(^G(sub1)) for  quit:alive!($zextract(rp,1,$zlength(rq))'=rq)  set:($qlength(rp)'<3)&($qlength(rp)'>3)&($qsubscript(rp,3)=0) alive=1 set:'alive rp=$query(@rp)
; Uses the XREFDATA() locals recconstlist, reclocxsub, recnsubs, recnullsub, recplast.
; xr4alv with a comment line of its own ahead of the walk, so that the generated code says what alive is.
xr4alvc:(pfx,plvl,self,why)
	quit " ; "_why_$char(10)_$$xr4alv(pfx,plvl,self)

xr4alv:(pfx,plvl,self)
	new c,i,n,q,t
	; the prefix itself: a record when it is a record node with a value, unless it is self
	set t=" set alive="_$select(plvl<recplast:0,self=("$name("_pfx_")"):0,'$zlength(self):"($data("_pfx_")#10)",1:"($data("_pfx_")#10)&($name("_pfx_")'="_self_")")								; above the record node, or self: never a record; else a value, and not self
	quit:plvl'<recnsubs t_" "																								; nothing below the prefix is within the depth
	set n=$select($zlength(self):$zlength(self,"(sub")+$zlength(self,",sub")-2,1:0)																		; self's level: its subscripts are sub1..subN
	set q=$zextract(pfx,1,$zlength(pfx)-1)_","																						; the prefix less ")" plus ",": a child is q_<subscript>_")"
	if recnsubs=(plvl+1),'recnullsub do  quit t																						; one watched level below the prefix: look at its nodes directly, no $QUERY and no indirection
	. if recconstlist(plvl+1) set t=t_"!(($data("_q_reclocxsub(plvl+1)_"))#10)"_$select(n=(plvl+1):"&(sub"_n_"'="_reclocxsub(plvl+1)_")",1:"")_") "										; the level is a constant: one node, unless it is self
	. else  set t=t_" if 'alive set c="""" for  set c=$order("_q_"c)) quit:c=""""  if "_$select(n=(plvl+1):"c'=sub"_n_",",1:"")_"$data("_q_"c))#10 set alive=1 quit"_$char(10)_" "						; a varying level: its first node with a value, other than self, makes a record
	set c=""
	for i=plvl+1:1:recplast set:recconstlist(i) c=c_"&($qsubscript(rp,"_i_")="_reclocxsub(i)_")"																; the constants between the prefix and the record node must match
	set t=t_",rq=$name("_pfx_"),rq=$zextract(rq,1,$zlength(rq)-1)_"","",rp=$query("_pfx_")"																	; rq: what every descendant's name starts with; rp: the first descendant
	set t=t_" for  quit:alive!($zextract(rp,1,$zlength(rq))'=rq)  set:($qlength(rp)'<"_recplast_")&($qlength(rp)'>"_recnsubs_")"_c_$select($zlength(self):"&(rp'="_self_")",1:"")_" alive=1 set:'alive rp=$query(@rp)"	; walk the subtree: a node from the record node to the depth, constants matching, not self, is a record; the next node only while none is found (an ELSE on the line would be skipped with the IF's rest)
	quit t_$char(10)_" "

; The following is a list of trigger suffixes for trigger names for the
; potential additional triggers of higher level nodes, with 0 being a trigger
; for the top level global variable. Note that K, S, and Z are missing, as
; those suffixes are used for KILL, SET and ZKILL triggers at the level that
; XREFDATA() has been asked to track.
xtratrigsufx:	;0123456789ABCDEFGHIJLMNOPQRTUVWX

; Templates for triggers
; The labeling convention for XREFDATA() trigger templates is as follows:
; - tt[s][t] for trigger template where the optional [s] parameter is for subscript
;   triggers and [t] is the type parameter. Not all combinations of s and t
;   are valid.
;   - t=2 for type 2 global nodes
;   - t=4 for types 4 and 6, and 4p for their record-side nodes
; - trigger command: S, ZK, K
; - for data triggers, e for entire node, p for pieces of node for data;
;   empty string for subscript triggers.
; - statistics level: 0, 1 or 2
; Note:
; - Each template must end in a blank line so that exptempl() knows the end of
;   a template.
; - Since / is used as a separator for template options when forcing string
;   collation, if a template needs a slash, a different separator is needed
;   for exptempl() to work.
; - The special templates rk0 and rk1 (depending on whether the region permits)
;   empty string subscripts (1=permitted) does a depth first traversal of the
;   global variable tree, deleting each node with a ZKILL in order to invoke
;   triggers for each node, including, the root node of the subtree at the end.
; - Multiline trigger templates must have a space or tab after the semicolon
;   whereas single line trigger templates do not.
; - rk0 and rk1 have unreachable code. This is a workaround for the YDB#799 bug.
;   After that is fixed, the workaround can be removed.
; - Since $ZTOLDVAL is an empty string for both a previous empty string value
;   as well as for a non-existent previous node, and since the triggers may run
;   concurrently with xrefdata(), i.e., they cannot assume the existence of
;   metadata, there are $DATA() tests for nodes, some of which may prove to be
;   redundant on detailed analysis.
; - The current triggers are written for correctness, and can likely be
;   optimized once a complete test suite exists and code paths are analyzed,
;   especially by replacing global accesses with local accesses. The code is
;   written so as to minimize run-time operations.
; Discussion: a depth first traversal is only needed if the subtree being
; traversed has global nodes with triggers. If there are none, it would be
; faster just to KILL the root node. However, this technique incurs the runtime
; cost of examining the triggers. For large trees, the cost of examining
; triggers will be amortized, but for small trees, it is faster to perform the
; depth first traversal. Since sub-trees KILLed while maintaining metadata are
; expected to be smaller trees, the choice was made to perform the depth first
; traversal. To handle KILLs of large trees while maintaining metadata, the
; recommended approach is to create a new metadata type which is identical to
; an existing type except for the way that KILLs are handled by triggers. Once
; the functionality in https://gitlab.com/YottaDB/DB/YDB/-/issues/517 exists
; the recursive triggers here can be replaced with simpler triggers.
; A leaf, $ZTDATA under 10, is ZKILLed directly, without the traversal's call: the
; ZKILL is what fires the triggers that maintain the metadata, and $ZTDATA costs no
; database access. @recvskip is the record side's guard (xr4trigs), empty elsewhere.
; The traversal ZKILLs a leaf for the same reason: for a node without children the two
; are one operation, and a KILL would only fire KILL triggers whose job is to ZKILL it.
rk0	; ; KILL: turn the KILL into ZKILLs, so that the ZKILL triggers retire the entries of every node that goes
	; ; a node with no children ($ZTDATA under 10) is ZKILLed outright; a subtree is walked depth first by dft
	; @recvskip if '($ztdata\10) set var=$reference zkill:$ztdata @var quit
	; do dft($reference) quit
	; ; never reached: keeps the label below inside the trigger code (YDB#799)
	; if @name
	;dft(var)
	; ; depth first: recurse into each child that has children, ZKILL each that has none, then ZKILL the node itself
	; new sub,vard,vars,varsd,vsroot
	; set vard=$data(@var)
	; do:vard\10
	; . if $qlength(var) set vsroot=$zextract(var,1,$zlength(var)-1)_","
	; . else  set vsroot=var_"("
	; . set vars=vsroot_"sub)"
	; . set sub="" for  set sub=$order(@vars) quit:'$zlength(sub)  do
	; . . set varsd=$data(@vars)
	; . . if varsd\10 do dft(vsroot_$zwrite(sub)_")")
	; . . else  zkill:varsd#10 @(vsroot_$zwrite(sub)_")")
	; zkill:vard#10 @var
	; quit

rk1	; ; KILL: turn the KILL into ZKILLs, so that the ZKILL triggers retire the entries of every node that goes
	; ; a node with no children ($ZTDATA under 10) is ZKILLed outright; a subtree is walked depth first by dft
	; @recvskip if '($ztdata\10) set var=$reference zkill:$ztdata @var quit
	; do dft($reference) quit
	; ; never reached: keeps the label below inside the trigger code (YDB#799)
	; if @name
	;dft(var)
	; ; depth first: recurse into each child that has children, ZKILL each that has none, then ZKILL the node itself
	; new sub,vard,vars,varsd,vsroot
	; set vard=$data(@var)
	; do:vard\10
	; . if $qlength(var) set vsroot=$zextract(var,1,$zlength(var)-1)_","
	; . else  set vsroot=var_"("
	; . set vars=vsroot_"sub)"
	; . set sub="" for  do  set sub=$order(@vars) quit:'$zlength(sub)
	; . . set varsd=$data(@vars)
	; . . if varsd\10 do dft(vsroot_$zwrite(sub)_")")
	; . . else  zkill:varsd#10 @(vsroot_$zwrite(sub)_")")
	; zkill:vard#10 @var
	; quit

ttSe0	; ; SET: move the key's entry from the old value of the node to the new
	; zkill @name(0,/"#"_//$ztoldval,@sub) set @name(0,/"#"_//$ztvalue,@sub)=""

ttZKe0	; ; ZKILL: remove the key's entry for the value that is going
	; zkill @name(0,/"#"_//$ztoldval,@sub)

ttSe1	; ; SET: move the key's entry from the old value of the node to the new; stat=1: the per-value entry counts, name("",value), follow
	; /set tmp="#"_$ztoldval //if $data(@name(0,/tmp/$ztoldval/,@sub))#10 zkill ^(@lastsub) zkill:1>$increment(@name("",/tmp/$ztoldval/),-1) ^($ztoldval)
	; /set tmp="#"_$ztvalue //if '$data(@name(0,/tmp/$ztvalue/,@sub)) set ^(@lastsub)="" if $increment(@name("",/tmp/$ztvalue/))

ttZKe1	; ; ZKILL: remove the key's entry for the value that is going; stat=1: the per-value entry counts follow
	; /set tmp="#"_$ztoldval //if $data(@name(0,/tmp/$ztoldval/,@sub))#10 zkill ^(@lastsub) zkill:1>$increment(@name("",/tmp/$ztoldval/),-1) ^(/tmp/$ztoldval/)

ttSe2	; ; SET: move the key's entry from the old value of the node to the new; stat=2: the per-value entry counts name("",value) and the distinct-value count name("") follow
	; /set tmp="#"_$ztoldval //if $data(@name(0,/tmp/$ztoldval/,@sub))#10 zkill ^(@lastsub) if 1>$increment(@name("",/tmp/$ztoldval/),-1) zkill ^(/tmp/$ztoldval/) zkill:1>$increment(@name(""),-1) ^("")
	; /set tmp="#"_$ztvalue //if '$data(@name(0,/tmp/$ztvalue/,@sub)) set ^(@lastsub)="" if (1=$increment(@name("",/tmp/$ztvalue/))),$increment(@name(""))

ttZKe2	; ; ZKILL: remove the key's entry for the value that is going; stat=2: the per-value and distinct-value counts follow
	; /set tmp="#"_$ztoldval //if $data(@name(0,/tmp/$ztoldval/,@sub))#10 zkill ^(@lastsub) if 1>$increment(@name("",/tmp/$ztoldval/),-1) zkill ^(/tmp/$ztoldval/) if 1>$increment(@name(""),-1) zkill ^("")

ttSp0	; ; SET: for each cross referenced piece i, p and q are the old and the new value of that piece
	; ; a changed piece moves the key's entry from p to q; an unchanged EMPTY piece is a node created with that piece empty, so the key's empty-string entry for it is made here
	; for i=@pieces set p=/"#"_//$@zpiece($ztoldval,@sep,i),q=/"#"_//$@zpiece($ztvalue,@sep,i) do
	; . if p'=q zkill @name(i,p,@sub) set @name(i,q,@sub)=""
	; . else  set:'($zlength(q)/-1//) @name(i,"/#//",@sub)=""

ttZKp0	; ; ZKILL: for each cross referenced piece i, remove the key's entry for the value that is going
	; for i=@pieces zkill @name(i,/"#"_//$@zpiece($ztoldval,@sep,i),@sub)

ttSp1	; ; SET: for each cross referenced piece i, p and q are the old and the new value of that piece
	; ; a changed piece moves the key's entry from p to q; an unchanged EMPTY piece is a node created with that piece empty, so the key's empty-string entry for it is made here
	; ; stat=1: the per-value entry counts, name(-i,value), follow
	; for i=@pieces set p=/"#"_//$@zpiece($ztoldval,@sep,i),q=/"#"_//$@zpiece($ztvalue,@sep,i),j=-i do
	; . if p'=q do
	; . . if $data(@name(i,p,@sub))#10 zkill ^(@lastsub) zkill:1>$increment(@name(j,p),-1) ^(p)
	; . . if '($data(@name(i,q,@sub))#10) set ^(@lastsub)="" if $increment(@name(j,q))
	; . else  if '($zlength(q)/-1//),'($data(@name(i,"/#//",@sub))#10) set ^(@lastsub)="" if $increment(@name(j,"/#//"))

ttZKp1	; ; ZKILL: for each cross referenced piece i, remove the key's entry for the value that is going; stat=1: the per-value entry counts follow
	; for i=@pieces set j=-i,p=/"#"_//$@zpiece($ztoldval,@sep,i) if $data(@name(i,p,@sub)) zkill ^(@lastsub) zkill:1>$increment(@name(j,p),-1) ^(p)

ttSp2	; ; SET: for each cross referenced piece i, p and q are the old and the new value of that piece
	; ; a changed piece moves the key's entry from p to q; an unchanged EMPTY piece is a node created with that piece empty, so the key's empty-string entry for it is made here
	; ; stat=2: the per-value entry counts name(-i,value) and the distinct-value counts name(-i) follow
	; for i=@pieces set p=/"#"_//$@zpiece($ztoldval,@sep,i),q=/"#"_//$@zpiece($ztvalue,@sep,i),j=-i do
	; . if p'=q do
	; . . if $data(@name(i,p,@sub))#10 zkill ^(@lastsub) if 1>$increment(@name(j,p),-1) zkill ^(p) if 1>$increment(@name(j),-1) zkill ^(j)
	; . . if '($data(@name(i,q,@sub))#10) set ^(@lastsub)="" if (1=$increment(@name(j,q))),$increment(@name(j))
	; . else  if '($zlength(q)/-1//),'($data(@name(i,"/#//",@sub))#10) set ^(@lastsub)="" if (1=$increment(@name(j,"/#//"))),$increment(@name(j))

ttZKp2	; ; ZKILL: for each cross referenced piece i, remove the key's entry for the value that is going; stat=2: the per-value and distinct-value counts follow
	; for i=@pieces set j=-i,p=/"#"_//$@zpiece($ztoldval,@sep,i) if $data(@name(i,p,@sub)) zkill ^(@lastsub) if 1>$increment(@name(j,p),-1) zkill ^(p) zkill:1>$increment(@name(j),-1) ^(j)

; Types 4 and 6. @xv1/@xv2 bracket a value as this type represents it in the cross reference, and
; @xnl is that representation of the empty value, the empty-string entry (xr4subst); that is what
; lets type 6 share these templates. Each trigger's code opens with a line that sets alive, whether
; a record exists for the value subscripts (xr4alv), on a line of its own when it walks. The value
; side (tt4S*, tt4ZK*) is the value node's own trigger: a SET quits when no record exists
; for the key, and a ZKILL re-adds an empty-string entry when a record survives. The record side (tt4pS*, tt4pZK*) fires on the record
; node and the watched levels below it, reads the value node through @rvalnode, and on a
; ZKILL removes the entry unless a record survives.

tt4Se0	; ; type 4 value node SET: move the key's entry from the old value of the node to the new
	; @vrecexq zkill @name(0,@xv1$ztoldval@xv2,@sub) set @name(0,@xv1$ztvalue@xv2,@sub)=""

tt4Se1	; ; type 4 value node SET: move the key's entry from the old value of the node to the new; stat=1: the per-value entry counts, name("",value), follow
	; @vrecexq set p=@xv1$ztoldval@xv2,q=@xv1$ztvalue@xv2
	; if $data(@name(0,p,@sub))#10 zkill @name(0,p,@sub) zkill:1>$increment(@name("",p),-1) @name("",p)
	; if '($data(@name(0,q,@sub))#10) set @name(0,q,@sub)="" if $increment(@name("",q))

tt4Se2	; ; type 4 value node SET: move the key's entry from the old value of the node to the new; stat=2: the per-value entry counts name("",value) and the distinct-value count name("") follow
	; @vrecexq set p=@xv1$ztoldval@xv2,q=@xv1$ztvalue@xv2
	; if $data(@name(0,p,@sub))#10 zkill @name(0,p,@sub) if 1>$increment(@name("",p),-1) zkill @name("",p) zkill:1>$increment(@name(""),-1) @name("")
	; if '($data(@name(0,q,@sub))#10) set @name(0,q,@sub)="" if (1=$increment(@name("",q))),$increment(@name(""))

tt4Sp0	; ; type 4 value node SET: for each cross referenced piece i, p and q are the old and the new value of that piece
	; ; a changed piece moves the key's entry from p to q; an unchanged EMPTY piece is a node created with that piece empty, which makes the record's entry for it the empty string: no record side trigger fires for the value node, so that entry is made here
	; @vrecexq for i=@pieces set p=@xv1$@zpiece($ztoldval,@sep,i)@xv2,q=@xv1$@zpiece($ztvalue,@sep,i)@xv2 do
	; . if p'=q zkill @name(i,p,@sub) set @name(i,q,@sub)=""
	; . ; p=q and empty: a node created with this piece empty; cross reference the key under the empty string
	; . else  set:q=@xnl @name(i,@xnl,@sub)=""

tt4Sp1	; ; type 4 value node SET: for each cross referenced piece i, p and q are the old and the new value of that piece
	; ; as for stat=0, and the per-value entry counts, name(-i,value), follow
	; @vrecexq for i=@pieces set p=@xv1$@zpiece($ztoldval,@sep,i)@xv2,q=@xv1$@zpiece($ztvalue,@sep,i)@xv2,j=-i do
	; . if p'=q do
	; . . if $data(@name(i,p,@sub))#10 zkill @name(i,p,@sub) zkill:1>$increment(@name(j,p),-1) @name(j,p)
	; . . if '($data(@name(i,q,@sub))#10) set @name(i,q,@sub)="" if $increment(@name(j,q))
	; . else  if q=@xnl,'($data(@name(i,@xnl,@sub))#10) set @name(i,@xnl,@sub)="" if $increment(@name(j,@xnl))

tt4Sp2	; ; type 4 value node SET: for each cross referenced piece i, p and q are the old and the new value of that piece
	; ; as for stat=0, and the per-value entry counts name(-i,value) and the distinct-value counts name(-i) follow
	; @vrecexq for i=@pieces set p=@xv1$@zpiece($ztoldval,@sep,i)@xv2,q=@xv1$@zpiece($ztvalue,@sep,i)@xv2,j=-i do
	; . if p'=q do
	; . . if $data(@name(i,p,@sub))#10 zkill @name(i,p,@sub) if 1>$increment(@name(j,p),-1) zkill @name(j,p) if 1>$increment(@name(j),-1) zkill @name(j)
	; . . if '($data(@name(i,q,@sub))#10) set @name(i,q,@sub)="" if (1=$increment(@name(j,q))),$increment(@name(j))
	; . else  if q=@xnl,'($data(@name(i,@xnl,@sub))#10) set @name(i,@xnl,@sub)="" if (1=$increment(@name(j,@xnl))),$increment(@name(j))

tt4ZKe0	; ; type 4 value node ZKILL: remove the key's entry for the value that is going; when a record survives (alive) the key is cross referenced under the empty string instead
	; @recalive zkill @name(0,@xv1$ztoldval@xv2,@sub) set:alive @name(0,@xnl,@sub)=""

tt4ZKp0	; ; type 4 value node ZKILL: for each cross referenced piece i, remove the key's entry for the value that is going; when a record survives (alive) the key is cross referenced under the empty string instead
	; @recalive for i=@pieces zkill @name(i,@xv1$@zpiece($ztoldval,@sep,i)@xv2,@sub) set:alive @name(i,@xnl,@sub)=""

tt4pSe0	; ; type 4 record side SET: a record exists for this key now; cross reference the value node as it is, "" when it is absent
	; @recvskip set @name(0,@xv1$get(@rvalnode)@xv2,@rtsub)=""

tt4pSp0	; ; type 4 record side SET: a record exists for this key now; cross reference each piece of the value node as it is, "" when the node is absent
	; @recvskip for i=@pieces set @name(i,@xv1$@zpiece($get(@rvalnode),@sep,i)@xv2,@rtsub)=""

tt4pZKe0	; ; type 4 record side ZKILL: when no record survives this node's removal (alive, below), retire the key's entry
	; @recvskip @recaliver zkill:'alive @name(0,@xv1$get(@rvalnode)@xv2,@rtsub)

tt4pZKp0	; ; type 4 record side ZKILL: when no record survives this node's removal (alive, below), retire the key's entries for every cross referenced piece
	; @recvskip @recaliver if 'alive for i=@pieces zkill @name(i,@xv1$@zpiece($get(@rvalnode),@sep,i)@xv2,@rtsub)

; stat=1 and stat=2 variants: the counts are the same nodes the other types maintain,
; @name(-i,value)/@name(-i) for pieces and @name("",value)/@name("") for the whole node.
; E.g. @name(-2,"Alice") is the number of nodes whose piece 2 is "Alice" (stat=1 and 2), and
; @name(-2) the number of distinct values of piece 2 (stat=2).

tt4ZKe1	; ; type 4 value node ZKILL: as for stat=0, and the per-value entry counts follow
	; @recalive set p=@xv1$ztoldval@xv2
	; if $data(@name(0,p,@sub))#10 zkill @name(0,p,@sub) zkill:1>$increment(@name("",p),-1) @name("",p)
	; if alive,'($data(@name(0,@xnl,@sub))#10) set @name(0,@xnl,@sub)="" if $increment(@name("",@xnl))

tt4ZKe2	; ; type 4 value node ZKILL: as for stat=0, and the per-value and distinct-value counts follow
	; @recalive set p=@xv1$ztoldval@xv2
	; if $data(@name(0,p,@sub))#10 zkill @name(0,p,@sub) if 1>$increment(@name("",p),-1) zkill @name("",p) zkill:1>$increment(@name(""),-1) @name("")
	; if alive,'($data(@name(0,@xnl,@sub))#10) set @name(0,@xnl,@sub)="" if (1=$increment(@name("",@xnl))),$increment(@name(""))

tt4ZKp1	; ; type 4 value node ZKILL: as for stat=0, and the per-value entry counts follow
	; @recalive for i=@pieces set j=-i,p=@xv1$@zpiece($ztoldval,@sep,i)@xv2 do
	; . if $data(@name(i,p,@sub))#10 zkill @name(i,p,@sub) zkill:1>$increment(@name(j,p),-1) @name(j,p)
	; . if alive,'($data(@name(i,@xnl,@sub))#10) set @name(i,@xnl,@sub)="" if $increment(@name(j,@xnl))

tt4ZKp2	; ; type 4 value node ZKILL: as for stat=0, and the per-value and distinct-value counts follow
	; @recalive for i=@pieces set j=-i,p=@xv1$@zpiece($ztoldval,@sep,i)@xv2 do
	; . if $data(@name(i,p,@sub))#10 zkill @name(i,p,@sub) if 1>$increment(@name(j,p),-1) zkill @name(j,p) zkill:1>$increment(@name(j),-1) @name(j)
	; . if alive,'($data(@name(i,@xnl,@sub))#10) set @name(i,@xnl,@sub)="" if (1=$increment(@name(j,@xnl))),$increment(@name(j))

tt4pSe1	; ; type 4 record side SET: as for stat=0, and the per-value entry counts follow
	; @recvskip set q=@xv1$get(@rvalnode)@xv2 if '($data(@name(0,q,@rtsub))#10) set @name(0,q,@rtsub)="" if $increment(@name("",q))

tt4pSe2	; ; type 4 record side SET: as for stat=0, and the per-value and distinct-value counts follow
	; @recvskip set q=@xv1$get(@rvalnode)@xv2 if '($data(@name(0,q,@rtsub))#10) set @name(0,q,@rtsub)="" if (1=$increment(@name("",q))),$increment(@name(""))

tt4pSp1	; ; type 4 record side SET: as for stat=0, and the per-value entry counts follow
	; @recvskip for i=@pieces set j=-i,q=@xv1$@zpiece($get(@rvalnode),@sep,i)@xv2 if '($data(@name(i,q,@rtsub))#10) set @name(i,q,@rtsub)="" if $increment(@name(j,q))

tt4pSp2	; ; type 4 record side SET: as for stat=0, and the per-value and distinct-value counts follow
	; @recvskip for i=@pieces set j=-i,q=@xv1$@zpiece($get(@rvalnode),@sep,i)@xv2 if '($data(@name(i,q,@rtsub))#10) set @name(i,q,@rtsub)="" if (1=$increment(@name(j,q))),$increment(@name(j))

tt4pZKe1	; ; type 4 record side ZKILL: as for stat=0, and the per-value entry counts follow
	; @recvskip @recaliver if 'alive set q=@xv1$get(@rvalnode)@xv2 if $data(@name(0,q,@rtsub))#10 zkill @name(0,q,@rtsub) zkill:1>$increment(@name("",q),-1) @name("",q)

tt4pZKe2	; ; type 4 record side ZKILL: as for stat=0, and the per-value and distinct-value counts follow
	; @recvskip @recaliver if 'alive set q=@xv1$get(@rvalnode)@xv2 if $data(@name(0,q,@rtsub))#10 zkill @name(0,q,@rtsub) if 1>$increment(@name("",q),-1) zkill @name("",q) zkill:1>$increment(@name(""),-1) @name("")

tt4pZKp1	; ; type 4 record side ZKILL: as for stat=0, and the per-value entry counts follow
	; @recvskip @recaliver if 'alive for i=@pieces set j=-i,q=@xv1$@zpiece($get(@rvalnode),@sep,i)@xv2 if $data(@name(i,q,@rtsub))#10 zkill @name(i,q,@rtsub) zkill:1>$increment(@name(j,q),-1) @name(j,q)

tt4pZKp2	; ; type 4 record side ZKILL: as for stat=0, and the per-value and distinct-value counts follow
	; @recvskip @recaliver if 'alive for i=@pieces set j=-i,q=@xv1$@zpiece($get(@rvalnode),@sep,i)@xv2 do
	; . if $data(@name(i,q,@rtsub))#10 zkill @name(i,q,@rtsub) if 1>$increment(@name(j,q),-1) zkill @name(j,q) zkill:1>$increment(@name(j),-1) @name(j)

tt2Se0	; ; value through the transformation function: SET: move the key's entry from the old value of the node to the new
	; zkill @name(0,@xfnp1$ztoldval@xfnp2,@sub) set @name(0,@xfnp1$ztvalue@xfnp2,@sub)=""

tt2ZKe0	; ; value through the transformation function: ZKILL: remove the key's entry for the value that is going
	; zkill @name(0,@xfnp1$ztoldval@xfnp2,@sub)

tt2Se1	; ; value through the transformation function: SET: move the key's entry from the old value of the node to the new; stat=1: the per-value entry counts, name("",value), follow
	; if $data(@name(0,@xfnp1$ztoldval@xfnp2,@sub))#10 zkill ^(@lastsub) zkill:1>$increment(@name("",@xfnp1$ztoldval@xfnp2),-1) ^(@xfnp1$ztoldval@xfnp2)
	; if '$data(@name(0,@xfnp1$ztvalue@xfnp2,@sub)) set ^(@lastsub)="" if $increment(@name("",@xfnp1$ztvalue@xfnp2))

tt2ZKe1	; ; value through the transformation function: ZKILL: remove the key's entry for the value that is going; stat=1: the per-value entry counts follow
	; if $data(@name(0,@xfnp1$ztoldval@xfnp2,@sub))#10 zkill ^(@lastsub) zkill:1>$increment(@name("",@xfnp1$ztoldval@xfnp2),-1) ^(@xfnp1$ztoldval@xfnp2)

tt2Se2	; ; value through the transformation function: SET: move the key's entry from the old value of the node to the new; stat=2: the per-value entry counts name("",value) and the distinct-value count name("") follow
	; set tmp=@xfnp1$ztoldval@xfnp2 if $data(@name(0,tmp,@sub))#10 zkill ^(@lastsub) if 1>$increment(@name("",tmp),-1) zkill ^(tmp) zkill:1>$increment(@name(""),-1) ^("")
	; set tmp=@xfnp1$ztvalue@xfnp2 if '$data(@name(0,tmp,@sub)) set ^(@lastsub)="" if (1=$increment(@name("",tmp))),$increment(@name(""))

tt2ZKe2	; ; value through the transformation function: ZKILL: remove the key's entry for the value that is going; stat=2: the per-value and distinct-value counts follow
	; set tmp=@xfnp1$ztoldva@xfnp2 if $data(@name(0,tmp,@sub))#10 zkill ^(@lastsub) if 1>$increment(@name("",tmp),-1) zkill ^(tmp) if 1>$increment(@name(""),-1) zkill ^("")

tt2Sp0	; ; value through the transformation function: SET: for each cross referenced piece i, p and q are the old and the new value of that piece
	; ; a changed piece moves the key's entry from p to q; an unchanged EMPTY piece is a node created with that piece empty, so the key's empty-string entry for it is made here
	; for i=@pieces set p=@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2,q=@xfnp1$@zpiece($ztvalue,@sep,i)@xfnp2 do
	; . if p'=q zkill @name(i,p,@sub) set @name(i,q,@sub)=""
	; . else  set:'($zlength(q)) @name(i,"",@sub)=""

tt2ZKp0	; ; value through the transformation function: ZKILL: for each cross referenced piece i, remove the key's entry for the value that is going
	; for i=@pieces zkill @name(i,@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2,@sub)

tt2Sp1	; ; value through the transformation function: SET: for each cross referenced piece i, p and q are the old and the new value of that piece
	; ; a changed piece moves the key's entry from p to q; an unchanged EMPTY piece is a node created with that piece empty, so the key's empty-string entry for it is made here
	; ; stat=1: the per-value entry counts, name(-i,value), follow
	; for i=@pieces set p=@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2,q=@xfnp1$@zpiece($ztvalue,@sep,i)@xfnp2,j=-i do
	; . if p'=q do
	; . . if $data(@name(i,p,@sub))#10 zkill ^(@lastsub) zkill:1>$increment(@name(j,p),-1) ^(p)
	; . . if '($data(@name(i,q,@sub))#10) set ^(@lastsub)="" if $increment(@name(j,q))
	; . else  if '($zlength(q)),'($data(@name(i,"",@sub))#10) set ^(@lastsub)="" if $increment(@name(j,q))

tt2ZKp1	; ; value through the transformation function: ZKILL: for each cross referenced piece i, remove the key's entry for the value that is going; stat=1: the per-value entry counts follow
	; for i=@pieces set j=-i,p=@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2 if $data(@name(i,p,@sub)) zkill ^(@lastsub) zkill:1>$increment(@name(j,p),-1) ^(p)

tt2Sp2	; ; value through the transformation function: SET: for each cross referenced piece i, p and q are the old and the new value of that piece
	; ; a changed piece moves the key's entry from p to q; an unchanged EMPTY piece is a node created with that piece empty, so the key's empty-string entry for it is made here
	; ; stat=2: the per-value entry counts name(-i,value) and the distinct-value counts name(-i) follow
	; for i=@pieces set p=@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2,q=@xfnp1$@zpiece($ztvalue,@sep,i)@xfnp2,j=-i do
	; . if p'=q do
	; . . if $data(@name(i,p,@sub))#10 zkill ^(@lastsub) if 1>$increment(@name(j,p),-1) zkill ^(p) if 1>$increment(@name(j),-1) zkill ^(j)
	; . . if '($data(@name(i,q,@sub))#10) set ^(@lastsub)="" if (1=$increment(@name(j,q))),$increment(@name(j))
	; . else  if '($zlength(q)),'($data(@name(i,"",@sub))#10) set ^(@lastsub)="" if (1=$increment(@name(j,""))),$increment(@name(j))

tt2ZKp2	; ; value through the transformation function: ZKILL: for each cross referenced piece i, remove the key's entry for the value that is going; stat=2: the per-value and distinct-value counts follow
	; for i=@pieces set j=-i,p=@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2 if $data(@name(i,p,@sub)) zkill ^(@lastsub) if 1>$increment(@name(j,p),-1) zkill ^(p) zkill:1>$increment(@name(j),-1) ^(j)

ttsS0	; ; subscript SET: enter the subscript's value under the key, if it is not there
	; set:'$data(@name(@locsnum,/"#"_//sub@locsnum,@sub)) ^(@lastsubind)=""

ttsZK0	; ; subscript ZKILL: remove the subscript's value from under the key, if it is there
	; zkill:$data(@name(@locsnum,/"#"_//sub@locsnum,@sub)) ^(@lastsubind)

ttsS1	; ; subscript SET: as for stat=0, and the per-value entry counts follow
	; /set tmp="#"_sub@locsnum //if '$data(@name(@locsnum,/tmp/sub@locsnum/,@sub)) set ^(@lastsubind)="" if $increment(@name(-@locsnum,/tmp/sub@locsnum/))

ttsZK1	; ; subscript ZKILL: as for stat=0, and the per-value entry counts follow
	; /set tmp="#"_sub@locsnum //if $data(@name(@locsnum,/tmp/sub@locsnum/,@sub)) zkill ^(@lastsubind) zkill:1>$increment(@name(-@locsnum,/tmp/sub@locsnum/),-1) ^(/tmp/sub@locsnum/)

ttsS2	; ; subscript SET: as for stat=0, and the per-value and distinct-value counts follow
	; /set tmp="#"_sub@locsnum //if '$data(@name(@locsnum,/tmp/sub@locsnum/,@sub)) set ^(@lastsubind)="" if 1=$increment(@name(-@locsnum,/tmp/sub@locsnum/)),$increment(@name(-@locsnum))

ttsZK2	; ; subscript ZKILL: as for stat=0, and the per-value and distinct-value counts follow
	; /set tmp="#"_sub@locsnum //if $data(@name(@locsnum,/tmp/sub@locsnum/,@sub)) zkill ^(@lastsubind) if 1>$increment(@name(-@locsnum,/tmp/sub@locsnum/),-1) zkill ^(/tmp/sub@locsnum/) zkill:1>$increment(@name(-@locsnum),-1) ^(-@locsnum)

tts2S0	; ; subscript SET, value through the transformation function: enter it under the key, if it is not there
	; set:'$data(@name(@locsnum,@xfnp1sub@locsnum@xfnp2,@sub)) ^(@lastsubind)=""

tts2ZK0	; ; subscript ZKILL, value through the transformation function: remove it from under the key, if it is there
	; zkill:$data(@name(@locsnum,@xfnp1sub@locsnum@xfnp2,@sub)) ^(@lastsubind)

tts2S1	; ; subscript SET: as for stat=0, and the per-value entry counts follow
	; set tmp=@xfnp1sub@locsnum@xfnp2 if '$data(@name(@locsnum,tmp,@sub)) set ^(@lastsubind)="" if $increment(@name(-@locsnum,tmp))

tts2ZK1	; ; subscript ZKILL: as for stat=0, and the per-value entry counts follow
	; set tmp=@xfnp1sub@locsnum@xfnp2 if $data(@name(@locsnum,tmp,@sub)) zkill ^(@lastsubind) zkill:1>$increment(@name(-@locsnum,tmp),-1) ^(tmp)

tts2S2	; ; subscript SET: as for stat=0, and the per-value and distinct-value counts follow
	; set tmp=@xfnp1sub@locsnum@xfnp2 if '$data(@name(@locsnum,tmp,@sub)) set ^(@lastsubind)="" if 1=$increment(@name(-@locsnum,tmp)),$increment(@name(-@locsnum))

tts2ZK2	; ; subscript ZKILL: as for stat=0, and the per-value and distinct-value counts follow
	; set tmp=@xfnp1sub@locsnum@xfnp2 if $data(@name(@locsnum,tmp,@sub)) zkill ^(@lastsubind) if 1>$increment(@name(-@locsnum,tmp),-1) zkill ^(tmp) zkill:1>$increment(@name(-@locsnum),-1) ^(-@locsnum)

;	Error message texts
;	A process returns $ECODE to the shell as an exit status, which is modulo
;	256, so these must stay at or below 255. New codes are therefore assigned
;	downward from the lowest one in use.
U219	;"-F-RECSPEC type="_$get(type)_" inconsistent with record specification - recgbl/recsub are permitted only with the record specification types (4 and 6), which need recsub whenever recgbl is not gbl"
U220	;"-F-RECPREFIX the value subscripts (count "_$get(reck)_") must be a left-prefix of the record subscripts (count "_$get(recn)_")"
U221	;"-F-RECWILD record specification subscript "_$get(i)_" is a record subscript ("":"") below a level that varies and names nothing; name every level down to the deepest record subscript"
U223	;"-F-RECDEPTH record specification depth recsub="_$get(recsub)_" must be an integer 1 through 29 and at least the deepest subscript named in recsub"
U224	;"-F-JOBNOTERM JOB'd process(es) scanning "_$get(gbl)_" did not exit after being signaled; the cross reference may still be written to"
U225	;"-F-INVNPROC Number of processes for the initial scan """_$get(tmp)_""" is not a positive integer"
U226	;"-F-SCANINCOMPLETE Process "_$get(i)_" of "_$get(nworkers)_" did not complete its scan of "_$get(gbl)_"; cross reference is incomplete"
U227	;"-F-BADEXTREF Extended reference "_$zpiece(gbl,"""|""",2)_" is not a valid filename"
U228	;"-F-INVSNUM snum="_$get(snum)_" includes subscript numbers that cannot be cross referenced"
U229	;"-F-BADTRANSFORM  with type>1, force="""_$get(force)_""" is not a valid function entryref"
U230	;"-F-ALLCONST Cross referencing a subscript requires at least one non-constant subscript; No of subscripts="_nsubs
U231	;"-F-BADTEMPLATE Trigger "_$get(outstr)_" has incorrect number of / delimiters for text substitution"
U232	;"-F-BADZINTERRUPT Caller's $ZINTERRUPT handler returned control to %YDBAIM"
U233	;"-F-JOBERR Error(s) reported by JOB'd process in "_err(i)
U234	;"-F-JOBFAIL Failed to JOB worker "_$get(i)_" of "_$get(nworkers)_" to scan "_$get(gbl)
U236	;"-F-SUBERR1 Subscript specification does not match type=1 requirements"
U237	;"-F-BADTYPEFORCE Schema type="_$get(type)_" and/or force="_$get(force)_" not recognized"
U238	;"-F-NOPSEP Piece numbers "_$get(pnum)_" specified, but piece separator not specifed"
U239	;"-F-SETZTRIGGERFAIL Out of design condition - setting $ZTRIGGER() failed"
U240	;"-F-CANTADDSTAT stat="_$get(stat)_" and "_$get(name)_"(10)="_+$get(@name@(10))_" - adding statistics to existing metadata not supported"
U241	;"-F-INVSTAT """_$get(stat)_""" is invalid stat; must be 0, 1, or 2"
U242	;"-F-OUTOFDESIGN """_$get(name)_""" already used to xref """_@name_""" cannot reuse for """_$get(gbl)_""""
U243	;"-F-ALREADYXREF """_$get(gbl)_""" is already a cross reference global variable"
U244	;"-F-NEEDSUB Need at least one subscript for cross reference"
U245	;"-F-INVPIECE Range """_$get(subpiece)_""" has invalid number of pieces: "_piecelen
U246	;"-F-INVRANGE upper """_$get(upper)_""" is less than lower """_$get(lower)_""" in range specification"
U247	;"-F-INVSUB Subscript """_$get(i,nsubs)_""" is not an integer 1 through 29"
U248	;"-F-INVPIECE Piece """_$get(nextp)_""" is invalid piece specification"
U249	;"-F-INVPNUMSEP Range specification "_$get(k)_" ("_$zwrite(nextp)_") has "_$get(nextplen)_" "":"" separated pieces, invalid"
U250	;"-F-NOPIECE Piece separator """_$get(sep)_""" specified, but no piece numbers"
U251	;"-F-INCONSISTENTNULL Regions "_$get(tmp)_" for global variable "_$get(gbl)_" are inconsistent with regard to null subscripts"
U252	;"-F-NOTAGBL Variable """_$get(gbl)_""" is not a valid global variable name"
U253	;"-F-NOSUBS Need at least 1 subscript to cross reference default type, 2 for type=1; nsubs="_nsubs
U254	;"-F-NOOMITFIX omitfix=0 is not supported with type="_$get(type)_"; type 1 and 3 require omitfix=1"
U255	;"-F-BADINVOCATION Top level invocation of "_$text(+0)_" not supported; must invoke a label"
