;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2021-2025 YottaDB LLC and/or its subsidiaries.	;
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
%YDBAIM;1
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
	; Terminate any JOB'd xrefdata() processes
	if $data(xrefproc(1)),'$zsigproc(xrefproc(1),"term") zkill xrefproc(1)
	if $data(xrefproc(-1)),'$zsigproc(xrefproc(-1),"term") zkill xrefproc(-1)
	; Now that primary error handling is done, switch to different handler to rethrow error in caller AIM frames.
	; The rethrow will cause a different $etrap to be invoked in the first non-AIM caller frame (because AIM
	; did a "new $etrap" at entry).
	set $etrap="quit:$quit """" quit"
	use io
	quit:$quit "" quit

; List metadata for a data cross reference, all data cross references for a
; global variable, or all data cross references
LSXREFDATA(lvn,gbl)
	new $etrap,io do etrap
	new currlck,tlevel,xrefvar
	set tlevel=$tlevel
	do snaplck(.currlck)
	if '$zlength($get(gbl)) do
	. set gbl="" for  set gbl=$order(^%ydbAIMDxref(gbl)) quit:'$zlength(gbl)  do
	. . set xrefvar="" for  set xrefvar=$order(^%ydbAIMDxref(gbl,xrefvar)) quit:'$zlength(xrefvar)  do lsxref(.lvn,xrefvar)
	else  if gbl'?1"^%ydbAIMD".E do
	. set xrefvar="" for  set xrefvar=$order(^%ydbAIMDxref(gbl,xrefvar)) quit:'$zlength(xrefvar)  do lsxref(.lvn,xrefvar)
	else  do lsxref(.lvn,gbl)
	quit

; List metadata for a subscript cross reference , all subscript cross references for
; a global variable, or all subscript cross references
LSXREFSUB(lvn,gbl)
	new $etrap do etrap
	new currlck,tlevel,xrefvar
	set tlevel=$tlevel
	do snaplck(.currlck)
	if '$zlength($get(gbl)) do
	. set gbl="" for  set gbl=$order(^%ydbAIMSxref(gbl)) quit:'$zlength(gbl)  do
	. . set xrefvar="" for  set xrefvar=$order(^%ydbAIMSxref(gbl,xrefvar)) quit:'$zlength(xrefvar)  do lsxref(.lvn,xrefvar)
	else  if gbl'?1"^%ydbAIMS".E do
	. set xrefvar="" for  set xrefvar=$order(^%ydbAIMSxref(gbl,xrefvar)) quit:'$zlength(xrefvar)  do lsxref(.lvn,xrefvar)
	else  do lsxref(.lvn,gbl)
	quit

; Remove triggers and cross references for a specified global, or all globals.
; The parameters for UNXREFDATA() mirror those of XREFDATA() to simplify calling
; it to remove triggers and cross references, even though some parameters are
; not required and are therefore ignored.
UNXREFDATA(gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type,force)
	new $etrap,io do etrap
	new currlck,i,nsubs,tlevel,xrefvar
	set tlevel=$tlevel
	; Ensure type & force have values and convert 0 to "" for backward compatibility
	if '$data(type)!(0=type) set type=""
	if '$data(force)!(0=force) set force=""
	do snaplck(.currlck)
	set gbl=$get(gbl)
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
	. else  do unxref($$XREFDATA(gbl,.xsub,$get(sep),,1,$get(zpiece),$get(omitfix,1),$get(stat,0),type,force))
	quit:$quit "" quit

; Remove triggers and cross references for a specified global, or all globals.
; The parameters for UNXREFSUB() mirror those of XREFSUB() to simplify calling
; it to remove triggers and cross references, even though some parameters are
; not required and are therefore ignored.
UNXREFSUB(gbl,xsub,snum,nmonly,omitfix,stat,type,force)
	new etrap do etrap
	new currlck,i,nsubs,tlevel,xrefvar
	set tlevel=$tlevel
	; Ensure force has a value and convert 0 to "" for backward compatibility
	if '$data(force)!(0=force) set force=""
	do snaplck(.currlck)
	set gbl=$get(gbl)
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
; Note that for historical reasons, variables whose names suggest they are
; relevant just to type=1, e.g., type1last, are also relevant to type=3,
; i.e., to Fileman globals. The number in the comment is the major version
; for data metadata. Refer to comments before the VERSION() label.
XREFDATA(gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat,type,force);2
	new $etrap,io do etrap
	new altlastsub,altsub,asciisep,constlist,currlck,fullsub,fullsubprnt
	new fulltrigsub,gblind,gblindtype1,i,j,killtrg,lastfullsub
	new lastsub,lastsubind,lastvarsub,locxsub,modflag,name,nameind,newpnum
	new newpstr,pieces,nsubs,nullsub,omitflag,oldpstr,stacklvl1,sub,subary
	new suffix,swver,tlevel,tmp,trigdel,trigdelx,type1last,trigprefix
	new trigset,trigsub,ttprfx,valcntind,xfnp1,xfnp2,xfntest,xrefind
	new xrefindtype1,z,zintrptsav,zlsep,ztout,zyintrsig
	set stacklvl1=$stack	; required by premature termination
	set tlevel=$tlevel	; required by error trap to rollback/unwind
	do initxref		; initialization shared with XREFSUB()
	; Ensure subscript specifications for Fileman global variables end in
	; a constant. The actual trigger subscript specification for the last
	; subscript matches all subscripts, as metadata varies depending on
	; whether or not there are other nodes at that last level, if no node
	; exists with the constant subscript.
	set:(type#2)&'constlist(nsubs) $ecode=",U236,"
	; Derive subscript specification for trigger definitions from parameters
	; and build string from which to derive xref variable name
	for i=1:1:nsubs do
	. set name=name_locxsub(i)
	. set lastfullsub="sub"_i,trigsub=trigsub_lastfullsub_"=",fulltrigsub=fulltrigsub_lastfullsub_","
	. if constlist(i)&(type#2) do
	. . set fullsub=fullsub_locxsub(i)_","
	. . set trigsub=trigsub_$select(i=nsubs:"*",1:locxsub(i))_","
	. else  do
	. . set fullsub=fullsub_lastfullsub_","
	. . set trigsub=trigsub_locxsub(i)_","
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
	set suffix=$zysuffix(gbl_name_$select(zlsep:sep_$select(asciisep:0,zpiece:1,1:$zchset),1:"")_$select(omitflag:1,1:"")_type_force)
	set name="^%ydbAIMD"_suffix
	; Flagging this error needs to be deferred as the error handler may
	; need name to be set.
	set:'$data(lastsub) $ecode=",U244,"
	; Quit if caller only wants the variable name. Note that asking for a
	; name requires that XREFDATA() be called as a function that returns
	; a value, as calling it as a routine asking for a name is a
	; meaningless operation that is likely an application program bug.
	quit:+$get(nmonly) name
	; For Fileman application globals need to search all subtrees at bottom
	; level when other subscripts match. Note that these application globals
	; have at least two subscripts.
	do:type#2
	. if omitfix set altsub=sub,altlastsub=$zpiece(sub,",",$zlength(sub,","))
	. else  set tmp=$zlength(sub,",")-1,altsub=$zpiece(sub,",",1,tmp)_$select(tmp:",""""",1:""""""),altlastsub=""
	. set type1last=locxsub(nsubs),locxsub(nsubs)="*",constlist(nsubs)=0
	. set fullsubprnt=$zpiece(fullsub,",",1,$zlength(fullsub,",")-1)
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
	set:nullsub&(type#2) $ecode=",U235,"		; null subscripts not permitted for Fileman globals
	set ttprfx="tt"_type				; prefix for trigger template
	set killtrg="rk"_nullsub			; template for KILL trigger
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
	. . set:'($ztrigger("item",trigset)&$ztrigger("item",trigdel)&$ztrigger("item",trigdelx)) $ecode=",U239,"
	. . do xtratrig		; set additional triggers for higher levels in the tree
	. . set ^%ydbAIMDxref(gbl,name)=""
	. tcommit
	. view "ztrigger_output":ztout
	. ; Cross reference existing nodes, if needed. Note that even if this
	. ; process set triggers, another concurrent process might have
	. ; cross referenced the pieces this process wants xref'd.
	. set oldpstr=$get(@name@(3),"#"),modflag=0
	. for i=2:1:$zlength(newpstr) if +$zextract(newpstr,i)&'+$zextract(oldpstr,i) set modflag=1 quit
	. do:modflag
	. . set:type#2 tmp=$order(constlist(""),-1),constlist(tmp)=1,locxsub(tmp)=$zwrite(type1last,1)
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
	. . set:'($ztrigger("item",trigset)&$ztrigger("item",trigdel)&$ztrigger("item",trigdelx)) $ecode=",U239,"
	. . do xtratrig		; set additional triggers for higher level nodes
	. . set ^%ydbAIMDxref(gbl,name)=""
	. tcommit
	. view "ztrigger_output":ztout
	. set newpstr=""
	. set:type#2 tmp=$order(constlist(""),-1),constlist(tmp)=1,locxsub(tmp)=$zwrite(type1last,1)
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
	set $zinterrupt=zintrptsav
	quit:$quit name quit

; XREFSUB() cross references metadata of subscripts.  The number in the comment is
; the major version for subscript metadata. Refer to comments before the VERSION()
; label.
XREFSUB(gbl,xsub,snum,nmonly,omitfix,stat,type,force);1
	new $etrap,io do etrap
	new constlist,currlck,fullsub,fullsubprnt,fulltrigsub,gblind,i,j,killtrg
	new lastfullsub,lastsub,lastsubind,lastvarsub,locxsub,locsnum,modflag,name
	new nameind,newsbits,nsnum,nsubs,nullsub,oldsbits,omitflag,stacklvl1,sub,subary
	new suffix,swver,tlevel,tmp,trigdel,trigdelx,trigprefix,trigset,trigsub,ttprfx
	new valcntind,xrefind,zintrptsav,ztout
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
	quit:+$get(nmonly) name
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
	. else  set $ecode=",U241"
	. set @name=gbl,@name@(4)=newsbits,@name@(6)=trigset,^(7)=trigdel,^(8)=trigdelx,^(9)=omitfix,^(10)=stat
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
; - fullsubprnt needs to precede fullsub in the list of substitutions otherwise
;   the @fullsub part of @fullsubprnt will be replaced leaving "prnt" in place,
;   which creates incorrect triggers.
; - code between slashes (///) is replaced, with the first part used if force
;   is non-zero, and the second if it is zero
; Uses local variables from XREF*() but does not substitute them: force
; Uses local variables from XREF*(): altlastsub,altsub,fullsubprnt,fullsub,fulltrigsub,gbl,lastfullsub,lastsubind,lastsub,name,nsubs,pieces,sep$,sub,type1last,xfnp1,xfnp2,z
exptempl:(lab)
	new i,j,len,line,multiline,num,outstr,npieces,prefix,rep,str,suffix,tmp,var,vars,zflag
	set line=$$expline($text(@lab))
	set str=line
	for i=1:1 set tmp=$text(@lab+i) quit:" "=tmp  set str=str_$char(10)_$$expline(tmp)
	set multiline=$zlength(str,$char(10))-1
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
	for  quit:outstr'?.E1"(".N1",sub".N1",".E  do
	. set prefix=prefix_$zpiece(outstr,"$data(",1)_"$data(",outstr=$zpiece(outstr,"$data(",2,$zlength(outstr,"$data("))
	. set prefix=prefix_$zpiece(outstr,",sub",1)_",sub",outstr=$zpiece(outstr,",sub",2,$zlength(outstr,",sub"))
	. set num=$zpiece(outstr,",",1)
	. set prefix=prefix_num,outstr=$zpiece(outstr,num,2,$zlength(outstr,num))
	. set tmp=",sub"_num
	. set prefix=prefix_$zpiece(outstr,tmp,1),outstr=$zpiece(outstr,tmp,2,$zlength(outstr,tmp))
	set:$zlength(prefix) outstr=prefix_outstr
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
;  constlist,currlck,force,fullsub,fulltrigsub,gbl,lastfullsub,locxsub,
;  name,nsubs,nullsub,omitfix,omitflag,sub,swver,trigsub,type,xsub,zintrptsav
initxref:
	new i,lab,rtn,tmp
	set zintrptsav=$zinterrupt
	set tmp=$stack($stack-1,"place")
	set lab=$zpiece(tmp,"+",1),rtn=$zpiece(tmp,"^",2)
	set swver=$zpiece($text(@(lab_"^"_rtn)),";",2)_"."_$zpiece($text(@(rtn_"^"_rtn)),";",2)
	do snaplck(.currlck)
	set:'$data(gbl) $ecode=",U252,"
	; Extended references are not supported
	set:""'=$qsubscript(gbl,-1) $ecode=",U254,"
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
	if 1<type do	; Superficial syntax check of transformation function
	. set xfnp1=$zpiece(force,"(",1)_"(",xfnp2=$zpiece(force,"(",2,$zlength(force,"("))
	. set:(xfnp1'?1(1"$$"0.1"%".an1"^"0.1"%",1"$").an1"(")!(xfnp2'?1(1")",1","1.e1")")) $ecode=",U229,"
	. set xfntest=$$^%ZMVALID("set zyx="_xfnp1_"zyx"_xfnp2),$ecode=""	; compile & discard errors
	. set:$zlength(xfntest) $ecode=",U229,"
	else  do
	. set:($zlength(type)&(1'=type))!($zlength(force)&(1'=force)) $ecode=",U237,"
	. set:$zlength(force) force=-force
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
;   constlist, gbl, gblind, gblindtype1, lastvarsub, name, nameind, nsubs,
;   omitfix, sep, subary, type, valcntind, xrefind, xrefindtype1
mkindxrefdata:
	new i,tmp,valtype
	set gblind(1)=gbl_"("_$select(constlist(1):locxsub(1),1:"subary(1)")
	for i=2:1:nsubs set gblind(i)=gblind(i-1)_","_$select(constlist(i):locxsub(i),1:"subary("_i_")")
	for i=1:1:nsubs set gblind(i)=gblind(i)_")"
	set xrefind=name_"("_$select($zlength(sep):"k,"_$select(force:"""#""_",1:"")_"pieceval",1:"0,"_$select(force:"""#""_",1:"")_"nodeval")
	for i=1:1:nsubs do
	. if constlist(i)!((type#2)&(i=nsubs)) do
	. . if 'omitfix set lastvarsub=i,lastsubind=locxsub(i),xrefind=xrefind_","_lastsubind
	. else  set lastvarsub=i,lastsubind="subary("_i_")",xrefind=xrefind_","_lastsubind
	set xrefind=xrefind_")"
	if $zlength(sep) set nameind=name_"(-k,"_$select(force:"""#""_",1:"")_"pieceval)",valcntind=name_"(-k)"
	else  set nameind=name_"("""","_$select(force:"""#""_",1:"")_"nodeval)",valcntind=name_"("""")"
	do:type#2	; Fileman global schema
	. set xrefindtype1=$select(omitfix:xrefind,1:$zpiece(xrefind,",",1,$zlength(xrefind,",")-1)_","""")")
	. set gblindtype1=gblind(nsubs)
	. set $zpiece(gblindtype1,",",$zlength(gblindtype1,","))=type1last_")"
	do:1<type	; Transformation function used for metadata
	. set valtype=$select($zfind(xrefind,"nodeval"):"nodeval",1:"pieceval")
	. set xrefind=$zpiece(xrefind,valtype,1)_xfnp1_valtype_xfnp2_$zpiece(xrefind,valtype,2)
	. set nameind=$zpiece(nameind,valtype,1)_xfnp1_valtype_xfnp2_$zpiece(nameind,valtype,2)
	quit

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
;   constlist, force, gbl, gblind, gblindtype1, lastsubind, lastvarsub, locxsub,
;   name, nameind, newpstr, nullsub, omitfix, stacklvl1, stacklvl2, stat, subary,
;   tick, type, type1last, valcntind, xfnp1, xfnp2, xrefind, xrefindtype1, zpiece
xrefdata(nsubsxref,dir,ppid)
	new flag,i,j,k,nodelen1,nodeval,nranges,piece2,piece2,pieceval,quitflag
	new rangebegin,rangeend,rangefirst,rangeflag,rangelast,sublvl,thisrange,tmp1,tmp2
	; As noted in xrefdatajobs() there is a small window during which if an
	; interrupt is received before the pid of the child process is captured
	; the child process pid may not be captured, resulting in an interrupt
	; not terminating a child process. By having the child process record
	; its pid, the window is made even smaller, although it cannot be
	; eliminated entirely.
	set:nsubsxref=nsubs $zpiece(^%ydbAIMtmp($text(+0),ppid,0),",",$select(1=dir:1,1:2))=$job
	; If nsubsxref>1 it means call the function recursively for the next
	; subscript level.
	set flag=nullsub
	set sublvl=$order(locxsub(""),-1)-nsubsxref+1
	set subary(sublvl)=""
	if nsubsxref>1 do
	. if "*"=locxsub(sublvl) for  do:flag  set flag=1,subary(sublvl)=$order(@gblind(sublvl),dir) quit:'$zlength(subary(sublvl))
	. . do:$data(@gblind(sublvl))\10 xrefdata(nsubsxref-1,dir,ppid)
	. else  if $get(constlist(sublvl),0) do:$data(@gblind(sublvl))\10 xrefdata(nsubsxref-1,dir,ppid)
	. else  do
	. . set nranges=$select(zpiece:$zlength(locxsub(sublvl),";"),1:$length(locxsub(sublvl),";"))
	. . if 1=dir set rangefirst=1,rangelast=nranges
	. . else  set rangefirst=nranges,rangelast=1
	. . for i=rangefirst:dir:rangelast do
	. . . if zpiece do
	. . . . set thisrange=$zpiece(locxsub(sublvl),";",i)
	. . . . set piece1=$zpiece(thisrange,":",1),piece2=$zpiece(thisrange,":",$zlength(thisrange,":"))
	. . . else  do
	. . . . set thisrange=$piece(locxsub(sublvl),";",i)
	. . . . set piece1=$piece(thisrange,":",1),piece2=$piece(thisrange,":",$length(thisrange,":"))
	. . . set rangeflag=0
	. . . if $zlength(piece1) set rangebegin=$zwrite(piece1,1),rangeflag=1
	. . . else  set rangebegin=""
	. . . if $zlength(piece2) set rangeend=$zwrite(piece2,1),rangeflag=1
	. . . else  set rangeend=""
	. . . set flag=$select(1=dir:$select($zlength(piece1):1,1:nullsub),1:$select($zlength(piece2):1,1:0))
	. . . set subary(sublvl)=$select(1=dir:rangebegin,1:rangeend)
	. . . for  do:flag  set flag=1,(subary(sublvl),tmp2)=$order(@gblind(sublvl),dir) quit:'$zlength(tmp2)!(rangeflag&$select(1=dir:$select($zlength(piece2):tmp2]]rangeend,1:0),1:rangebegin]]tmp2))
	. . . . if (type#2)&(2=nsubsxref) do
	. . . . . tstart ():transactionid="batch"
	. . . . . set tmp1=$data(@gblind(sublvl))
	. . . . . if tmp1\10 do xrefdata(nsubsxref-1,dir,ppid)
	. . . . . else  do:tmp1#10
	. . . . . . set nodeval=""
	. . . . . . if $zlength(sep) do
	. . . . . . . set nodelen1=$zlength(newpstr),pieceval=""
	. . . . . . . for j=2:1:nodelen1 do:+$zextract(newpstr,j)
	. . . . . . . . set k=j-1
	. . . . . . . . do:'($data(@xrefindtype1)#10)
	. . . . . . . . . set ^(@lastsubind)=""
	. . . . . . . . . if stat,$increment(@nameind),(2=stat),1=@nameind,$increment(@valcntind)
	. . . . . . else  do:'($data(@xrefindtype1)#10)
	. . . . . . . set ^(@lastsubind)=""
	. . . . . . . if stat,$increment(@nameind),(2=stat),1=@nameind,$increment(@valcntind)
	. . . . . tcommit
	. . . . . if $zut-tick\1E4 do xrefjobsrec(nsubsxref) set tick=$zut
	. . . . else  do:$data(@gblind(sublvl))\10 xrefdata(nsubsxref-1,dir,ppid)
	; if nsubsxref=1 (the rest of the code below) the traversal has reached the
	; subscript level at which globals are to have metadata
	; computed. Compute metadata per subscript and metadata type
	; specification.
	; Note that the code for Fileman type comes before the code for
	; constlist(sublvl) because constlist(sublvl)=1 for Fileman data but
	; Fileman schema has precedence over constlist(sublvl)=1
	else  if type#2 do
	. tstart ():transactionid="batch"
	. if $data(@gblindtype1)#10 do
	. . set nodeval=@gblindtype1
	. . if $zlength(sep) do
	. . . set nodelen1=$zlength(newpstr)
	. . . for i=2:1:nodelen1 do:+$zextract(newpstr,i)
	. . . . set k=i-1,pieceval=$select(zpiece:$zpiece(nodeval,sep,k),1:$piece(nodeval,sep,k))
	. . . . do:'($data(@xrefind)#10)
	. . . . . set ^($select(lastvarsub=sublvl:lastsubind,1:@lastsubind))=""
	. . . . . if stat,$increment(@nameind),(2=stat),1=@nameind,$increment(@valcntind)
	. . else  do:'$data(@xrefind)
	. . . set ^($select(lastvarsub=sublvl:lastsubind,1:@lastsubind))=""
	. . . if stat,$increment(@nameind),(2=stat),1=@nameind,$increment(@valcntind)
	. else  do	; node doesn't exist but peers or peer subtrees do
	. . set nodeval=""
	. . if $zlength(sep) do
	. . . set nodelen1=$zlength(newpstr),pieceval=""
	. . . for i=2:1:nodelen1 do:+$zextract(newpstr,i)
	. . . . set k=i-1
	. . . . do:'($data(@xrefindtype1)#10)
	. . . . . set ^($select(lastvarsub=sublvl:lastsubind,1:@lastsubind))=""
	. . . . . if stat,$increment(@nameind),(2=stat),1=@nameind,$increment(@valcntind)
	. . else  do:'$data(@xrefindtype1)
	. . . set ^($select(lastvarsub=sublvl:lastsubind,1:@lastsubind))=""
	. . . if stat,$increment(@nameind),(2=stat),1=@nameind,$increment(@valcntind)
	. tcommit
	. if $zut-tick\1E4 do xrefjobsrec(nsubsxref) set tick=$zut
	else  if constlist(sublvl) do
	. tstart ():transactionid="batch"
	. if $data(@gblind(sublvl))#10 do
	. . set nodeval=@gblind(sublvl)
	. . if $zlength(sep) do
	. . . set nodelen1=$zlength(newpstr)
	. . . for i=2:1:nodelen1 do:+$zextract(newpstr,i)
	. . . . set k=i-1,pieceval=$select(zpiece:$zpiece(nodeval,sep,k),1:$piece(nodeval,sep,k))
	. . . . do:'($data(@xrefind)#10)
	. . . . . set ^($select(lastvarsub=sublvl:lastsubind,1:@lastsubind))=""
	. . . . . if stat,$increment(@nameind),(2=stat),1=@nameind,$increment(@valcntind)
	. . else  do:'$data(@xrefind)
	. . . set ^($select(lastvarsub=sublvl:lastsubind,1:@lastsubind))=""
	. . . if stat,$increment(@nameind),(2=stat),1=@nameind,$increment(@valcntind)
	. tcommit
	. if $zut-tick\1E4 do xrefjobsrec(nsubsxref) set tick=$zut
	else  do	; not Fileman or a constant subscript specification
	. set nranges=$select(zpiece:$zlength(locxsub(sublvl),";"),1:$length(locxsub(sublvl),";"))
	. if 1=dir set rangefirst=1,rangelast=nranges
	. else  set rangefirst=nranges,rangelast=1
	. for i=rangefirst:dir:rangelast do
	. . if "*"=locxsub(sublvl) do
	. . . set (piece2,rangebegin,rangeend,subary(sublvl))="",flag=nullsub,rangeflag=0
	. . else  do
	. . . if zpiece do
	. . . . set thisrange=$zpiece(locxsub(sublvl),";",i)
	. . . . set piece1=$zpiece(thisrange,":",1),piece2=$zpiece(thisrange,":",$zlength(thisrange,":"))
	. . . else  do
	. . . . set thisrange=$piece(locxsub(sublvl),";",i)
	. . . . set piece1=$piece(thisrange,":",1),piece2=$piece(thisrange,":",$length(thisrange,":"))
	. . . set rangeflag=0
	. . . if $zlength(piece1) set rangebegin=$zwrite(piece1,1),(flag,rangeflag)=1
	. . . else  set rangebegin="",flag=nullsub
	. . . if $zlength(piece2) set rangeend=$zwrite(piece2,1),rangeflag=1
	. . . else  set rangeend="",flag=1
	. . . set subary(sublvl)=$select(1=dir:rangebegin,1:rangeend)
	. . for  do:flag  set flag=1,(subary(sublvl),tmp2)=$order(@gblind(sublvl),dir) quit:'$zlength(tmp2)!(rangeflag&$select(1=dir:$select($zlength(piece2):tmp2]]rangeend,1:0),1:rangebegin]]tmp2))
	. . . tstart (j,k):transactionid="batch"
	. . . do:$data(@gblind(sublvl))#10
	. . . . set nodeval=@gblind(sublvl)
	. . . . if $zlength(sep) do
	. . . . . set nodelen1=$zlength(newpstr)
	. . . . . for j=2:1:nodelen1 do:+$zextract(newpstr,j)
	. . . . . . set k=j-1,pieceval=$select(zpiece:$zpiece(nodeval,sep,k),1:$piece(nodeval,sep,k))
	. . . . . . if '($data(@xrefind)#10) set ^(@lastsubind)="" if stat,$increment(@nameind),(2=stat),1=@nameind,$increment(@valcntind)
	. . . . else  if '($data(@xrefind)#10) set ^(@lastsubind)="" if stat,$increment(@nameind),(2=stat),1=@nameind,$increment(@valcntind)
	. . . tcommit
	. if $zut-tick\1E4 do xrefjobsrec(nsubsxref) set tick=$zut
	quit

; JOB two processes for xrefdata(); one to scan the global subscripts in the
; forward direction, and one to scan in the reverse direction. The scan can be
; terminated when either of the followiing conditions are met:
; - Either process terminates: Since either process completing means that the
;   the metadata computation is complete, the other process can be terminated.
; - For all non-constant subscript specifications in nsubs-1 subscripts, the
;   process scanning in the forward direction is scanning subscripts that
;   collate after subscripts the other process is scanning. In other words,
;   since they are both scanning data that has already been scanned, they can
;   both be terminated.
; xrefdatajobs() cleans up stdout and stderr files of the JOB'd processes.
; Comments preceding xrefdata() list local variables passed through
xrefdatajobs:(nsubs)
	new cmd,err,i,intdefer,io,j,k,line,msgprefix,out,prefix,stacklvl2,tick,totcnt,val,xrefproc
	set io=$io
	set msgprefix=$ztrnlnm("ydb_msgprefix")
	set:'$zlength(msgprefix) msgprefix="YDB"
	set prefix="/tmp/xrefdata^"_$text(+0)_"_"_$job_"_"_($zut\1E6)_"_"
	set stacklvl2=$stack	; required by premature termination
	set tick=$zut
	; Job the two processes and record their pids so that they can be
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
	; response to an interrupt.
	kill ^%ydbAIMtmp($text(+0),$job,0) for i=1:1:nsubs kill ^(i),^(-i)	; Clear any prior subprocess metadata
	set intdefer="",$zinterrupt="set intdefer=$zyintrsig"	; defer interrupts temporarily
	for i=1,-1 do
	. set err(i)=prefix_i_".err"
	. set out(i)=prefix_i_".out"
	. set cmd="xrefdata(nsubs,i,$job):(passcurlvn:error="""_err(i)_""":output="""_out(i)_""")"
	. job @cmd
	. if $zjob set xrefproc(i)=$zjob set $zpiece(^%ydbAIMtmp($text(+0),$job,0),",",$select(1=i:1,1:2))=$zjob
	. else  set $ecode=",U234,"
	set $zinterrupt="set zyintrsig=$zyintrsig zgoto:""SIGUSR2""=$zyintrsig stacklvl2:xrefjobsterm xecute zintrptsav"
	if $zlength(intdefer),$zsigproc($job,intdefer)	; throw any deferred interrupts
	; - check whether processes have crossed, if processes have crossed, terminate one process
	; - if one process has terminated, because it was terminated or completed the scan, terminate the other process
	for  do  quit:'$data(xrefproc)  hang .01
	. if $$xrefjobsckdone set i=$order(xrefproc("")) kill:$zlength(i)&('$zsigproc(xrefproc(i),"term")) xrefproc(i)
	. set i="" for  set i=$order(xrefproc(i)) quit:'$zlength(i)  do:'$zgetjpi(xrefproc(i),"isprocalive")  quit:'$data(xrefproc)
	. . if $data(xrefproc(-i)),$zsigproc(xrefproc(-i),"term")
	. . kill xrefproc
	; quit		; uncomment for debugging
	; Raise error if there is any non-information (-I-), non-success (S) message other than FORCEDHALT.
	for i=1,-1 do
	. set out(i)=$zsearch(out(i)_"*") open out(i)
	. for j=1:1 use out(i) read line quit:$zeof  if $zlength(line) use io write line,! use out(i)
	. use io close out(i):delete
	. set err(i)=$zsearch(err(i)_"*") open err(i)
	. for j=1:1 use err(i) read line quit:$zeof  do:$zlength(line)
	. . use io
	. . set:'($zfind(line,"FORCEDHALT")!(line?@((".E1"""_msgprefix_"""1""-""1(1""I"",1""S"")1""-"".E")))) $ecode=",U233,"
	. . use err(i)
	. use io close err(i):delete
	kill ^%ydbAIMtmp($text(+0),$job,0) for i=1:1:nsubs kill ^(i),^(-i)	; Clear subprocess metadata on clean exit
	quit

; Kill child processes and handle an interrrupt
; Uses local variables from caller: stacklvl, stacklvl2, xrefproc, zintrptsav
; It tests for the existence of zpiece.
xrefjobsterm
	new i,proc
	for i=1:1:2 set proc=+$zpiece(^%ydbAIMtmp($text(+0),$job,0),",",i) if proc,$zsigproc(proc,"TERM")
	; As noted in xrefdatajobs, there is the potential for a child process
	; to exist whose pid is not captured in ^%ydbAIMtmp($text(+0),$job,0).
	; As this pid will be captured in $zjob, terminate that process if it
	; exists. This has the infinitesimally small probability that if the
	; pid is reused by a different process after the $zsigproc() above,
	; and that different process has the same uid, then that different
	; process will be inadvertently terminated.
	if $zjob,$zsigproc($zjob,"TERM")
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

; Check whether concurrent JOBs indexing existing data can be terminated because
; the process scanning in the reverse direction is at subscripts in the global
; variable tree that precede subscripts being scanned by the process scanning
; in the forward direction.
; Uses local variables from caller: constlist, nsubs, stacklvl1, stacklvl2
xrefjobsckdone:()
	new chk,flag,i
	set (chk,flag)=0
	for i=1:1:nsubs do:'constlist(i)  quit:flag
	. tstart (chk,flag,i):transactionid="batch"
	. if $data(^%ydbAIMtmp($text(+0),$job,i)),$data(^(-i)),$increment(chk) set:^(i)']]^(-i) flag=1
	. tcommit
	quit $select(flag!'chk:0,1:1)

; In JOB'd process, record the state of the current scanning process
; Uses local variables from caller: constlist, dir, ppid, subary
xrefjobsrec:(depth)
	new i
	tstart ():transactionid="batch"
	for i=1:1:depth set:'constlist(i) ^%ydbAIMtmp("%YDBAIM",ppid,dir*i)=subary(i)
	tcommit
	quit

; Child process to generate cross references for nodes starting with statistics
; for the first subscript if requested, and cross references and statistics (if
; requested) for nodes through the level nsubs. As this is modeled on xrefdata(),
; comments here are specific to xrefsub().
; References variables defined in parent or passed through from XREFSUB().
;   locxsub,nameind,nsnum,nsubs,nullsub,subary,tick,xrefind
xrefsub(nsubsxref,dir,ppid)
	new i,j,nranges,flag,piece1,piece2,rangebegin,rangeend,rangefirst,rangeflag,rangelast,snum,sublvl,subval,thisrange
	; Set pid of child process to allow parent process to terminate the
	; child process.
	set:nsubsxref=nsubs $zpiece(^%ydbAIMtmp($text(+0),ppid,0),",",$select(1=dir:1,1:2))=$job
	set flag=nullsub
	set sublvl=$order(locxsub(""),-1)-nsubsxref+1
	set subary(sublvl)=""
	if nsubsxref>1 do
	. if "*"=locxsub(sublvl) for  do:flag  set flag=1,subary(sublvl)=$order(@gblind(sublvl),dir) quit:'$zlength(subary(sublvl))
	. . do:$data(@gblind(sublvl))\10 xrefsub(nsubsxref-1,dir,ppid)
	. else  if $get(constlist(sublvl),0),($data(@gblind(sublvl))\10) set subary(sublvl)=$zwrite(locxsub(sublvl),1) do xrefsub(nsubsxref-1,dir,ppid)
	. else  do
	. . set nranges=$zlength(locxsub(sublvl),";")
	. . if 1=dir set rangefirst=1,rangelast=nranges
	. . else  set rangefirst=nranges,rangelast=1
	. . for i=rangefirst:dir:rangelast do
	. . . set thisrange=$zpiece(locxsub(sublvl),";",i)
	. . . set piece1=$zpiece(thisrange,":",1),piece2=$zpiece(thisrange,":",$zlength(thisrange,":"))
	. . . set rangeflag=0
	. . . if $zlength(piece1) set rangebegin=$zwrite(piece1,1),rangeflag=1
	. . . else  set rangebegin=""
	. . . if $zlength(piece2) set rangeend=$zwrite(piece2,1),rangeflag=1
	. . . else  set rangeend=""
	. . . set flag=$select(1=dir:$select($zlength(piece1):1,1:nullsub),1:$select($zlength(piece2):1,1:0))
	. . . set subary(sublvl)=$select(1=dir:rangebegin,1:rangeend)
	. . . for  do:flag  set flag=1,(subary(sublvl),subval)=$order(@gblind(sublvl),dir) quit:'$zlength(subval)!(rangeflag&$select(1=dir:$select($zlength(piece2):subval]]rangeend,1:0),1:rangebegin]]subval))
	. . . . do:$data(@gblind(sublvl))\10 xrefsub(nsubsxref-1,dir,ppid)
	else  do	; lowest level subscript; cross referencing happens here
	. set (subary(sublvl),subval)=""
	. if "*"=locxsub(sublvl) for  do:flag  set flag=1,(subary(sublvl),subval)=$order(@gblind(sublvl),dir) quit:'$zlength(subval)
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
	. . if 1=dir set rangefirst=1,rangelast=nranges
	. . else  set rangefirst=nranges,rangelast=1
	. . for i=rangefirst:dir:rangelast do
	. . . set thisrange=$zpiece(locxsub(sublvl),";",i)
	. . . set piece1=$zpiece(thisrange,":",1),piece2=$zpiece(thisrange,":",$zlength(thisrange,":"))
	. . . set rangeflag=0
	. . . if $zlength(piece1) set rangebegin=$zwrite(piece1,1),(flag,rangeflag)=1
	. . . else  set rangebegin=""
	. . . if $zlength(piece2) set rangeend=$zwrite(piece2,1),rangeflag=1
	. . . else  set rangeend="",flag=1
	. . . set flag=$select(1=dir:$select($zlength(piece1):1,1:nullsub),1:$select($zlength(piece2):1,1:0))
	. . . set (subary(sublvl),subval)=$select(1=dir:rangebegin,1:rangeend)
	. . . for  do:flag  set flag=1,(subary(sublvl),subval)=$order(@gblind(sublvl),dir) quit:'$zlength(subval)!(rangeflag&$select(1=dir:$select($zlength(piece2):subval]]rangeend,1:0),1:rangebegin]]subval))
	. . . . tstart ():transactionid="batch"
	. . . . do:$data(@gblind(sublvl))#10
	. . . . . for snum=1:1:nsnum do:'($data(@xrefind(snum))#10)
	. . . . . . set @xrefind(snum)=""
	. . . . . . if stat,$increment(@nameind(snum)),(2=stat),1=@nameind(snum),$increment(@valcntind(snum))
	. . . . tcommit
	. if $zut-tick>1E4 do xrefjobsrec(sublvl) set tick=$zut
	quit

; JOB two xrefsub() processes, scanning global subscripts in the forward and
; reverse directions. As xrefsubjobs() is modeled on xrefdatajobs(),
; additional comments here are specific to xrefsub().
; Comments preceding xrefsub() list local variables passsed through.
xrefsubjobs:(nsubs)
	new cmd,err,i,intdefer,io,j,line,msgprefix,out,prefix,stacklvl2,tick,totcnt,val,xrefproc
	set io=$io
	set msgprefix=$ztrnlnm("ydb_msgprefix")
	set:'$zlength(msgprefix) msgprefix="YDB"
	set prefix="/tmp/xrefsub^"_$text(+0)_"_"_$job_"_"_($zut/1E6)_"_"
	set stacklvl2=$stack
	set tick=$zut
	kill ^%ydbAIMtmp($text(+0),$job,0) for i=1:1:nsubs kill ^(i),^(-i)
	set intdefer="",$zinterrupt="set intdefer=$zyintrsig"	; defer interrupts temporarily
	for i=1,-1 do
	. set err(i)=prefix_i_".err"
	. set out(i)=prefix_i_".out"
	. set cmd="xrefsub(nsubs,i,$job):(passcurlvn:error="""_err(i)_""":output="""_out(i)_""")"
	. job @cmd
	. if $zjob set xrefproc(i)=$zjob set $zpiece(^%ydbAIMtmp($text(+0),$job,0),",",$select(1=i:1,1:2))=$zjob
	. else  set $ecode=",U234,"
	set $zinterrupt="set zyintrsig=$zyintrsig zgoto:""SIGUSR2""=$zyintrsig stacklvl2:xrefjobsterm xecute zintrptsav"
	if $zlength(intdefer),$zsigproc($job,intdefer)	; throw any deferred interrupts
	for  do  quit:'$data(xrefproc)  hang .01
	. if $$xrefjobsckdone set i=$order(xrefproc("")) kill:$zlength(i)&('$zsigproc(xrefproc(i),"term")) xrefproc(i)
	. set i="" for  set i=$order(xrefproc(i)) quit:'$zlength(i)  do:'$zgetjpi(xrefproc(i),"isprocalive")  quit:'$data(xrefproc)
	. . if $data(xrefproc(-i)),$zsigproc(xrefproc(-i),"term")
	. . kill xrefproc
	; quit		; uncomment for debugging
	; Raise error if there is any non-information (-I-), non-success (S) message other than FORCEDHALT.
	for i=1,-1 do
	. set out(i)=$zsearch(out(i)_"*") open out(i) use out(i)
	. for j=1:1 read line quit:$zeof  if $zlength(line) use io write line,! use out(i)
	. use io close out(i):delete
	. set err(i)=$zsearch(err(i)_"*") open err(i) use err(i)
	. for j=1:1 read line quit:$zeof  do:$zlength(line)
	. . use io
	. . set:'($zfind(line,"FORCEDHALT")!(line?@((".E1"""_msgprefix_"""1""-""1(1""I"",1""S"")1""-"".E")))) $ecode=",U233,"
	. . use err(i)
	. close err(i):delete
	kill ^%ydbAIMtmp($text(+0),$job,0) for i=1:1:nsubs kill ^(i),^(-i)	; Clear subprocess metadata on clean exit
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
	; type specific additional triggers, caution: code uses & extends i from above
	do:type#2
	. set tmp=$zlength(trigsub)
	. set:","=$zextract(trigsub,tmp) $zextract(trigsub,tmp)=""
	. set trigsub=trigsub_")"
	. set trig2=trig1_trigsub
	. set trigsuffix=$select($zlength(sep):"p",1:"e")
	. set trig3=" -command=set -name=%ydb"_suffix_"s -xecute="_$$exptempl("tt1pS"_trigsuffix_stat)
	. set trig=trig2_trig3
	. set:'$ztrigger("item",trig) $ecode=",U239,"
	. set ^(12+$increment(i))=trig
	. set trig3=" -command=zkill -name=%ydb"_suffix_"z -xecute="_$$exptempl("tt1pZK"_trigsuffix_stat)
	. set trig=trig2_trig3
	. set:'$ztrigger("item",trig) $ecode=",U239,"
	. set ^(12+$increment(i))=trig
	quit

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
;   - t=1 for type 1 global nodes, and 1p for Fileman parent global nodes
;   - t=2 or t=3 for type 2 or 3 global nodes respectively
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
rk0	; do dft($reference) quit
	; if @name
	;dft(var)
	; new sub,vard,vars,varsd,vsroot
	; set vard=$data(@var)
	; do:vard\10
	; . if $qlength(var) set vsroot=$zextract(var,1,$zlength(var)-1)_","
	; . else  set vsroot=var_"("
	; . set vars=vsroot_"sub)"
	; . set sub="" for  set sub=$order(@vars) quit:'$zlength(sub)  do
	; . . set varsd=$data(@vars)
	; . . if varsd\10 do dft(vsroot_$zwrite(sub)_")")
	; . . else  kill:varsd#10 @(vsroot_$zwrite(sub)_")")
	; zkill:vard#10 @var
	; quit

rk1	; do dft($reference) quit
	; if @name
	;dft(var)
	; new sub,vard,vars,varsd,vsroot
	; set vard=$data(@var)
	; do:vard\10
	; . if $qlength(var) set vsroot=$zextract(var,1,$zlength(var)-1)_","
	; . else  set vsroot=var_"("
	; . set vars=vsroot_"sub)"
	; . set sub="" for  do  set sub=$order(@vars) quit:'$zlength(sub)
	; . . set varsd=$data(@vars)
	; . . if varsd\10 do dft(vsroot_$zwrite(sub)_")")
	; . . else  kill:varsd#10 @(vsroot_$zwrite(sub)_")")
	; zkill:vard#10 @var
	; quit

ttSe0	;zkill @name(0,/"#"_//$ztoldval,@sub) set @name(0,/"#"_//$ztvalue,@sub)=""

ttZKe0	;zkill @name(0,/"#"_//$ztoldval,@sub)

ttSe1	; /set tmp="#"_$ztoldval //if $data(@name(0,/tmp/$ztoldval/,@sub))#10 zkill ^(@lastsub) zkill:1>$increment(@name("",/tmp/$ztoldval/),-1) ^($ztoldval)
	; /set tmp="#"_$ztvalue //if '$data(@name(0,/tmp/$ztvalue/,@sub)) set ^(@lastsub)="" if $increment(@name("",/tmp/$ztvalue/))

ttZKe1	;/set tmp="#"_$ztoldval //if $data(@name(0,/tmp/$ztoldval/,@sub))#10 zkill ^(@lastsub) zkill:1>$increment(@name("",/tmp/$ztoldval/),-1) ^(/tmp/$ztoldval/)

ttSe2	; /set tmp="#"_$ztoldval //if $data(@name(0,/tmp/$ztoldval/,@sub))#10 zkill ^(@lastsub) if 1>$increment(@name("",/tmp/$ztoldval/),-1) zkill ^(/tmp/$ztoldval/) zkill:1>$increment(@name(""),-1) ^("")
	; /set tmp="#"_$ztvalue //if '$data(@name(0,/tmp/$ztvalue/,@sub)) set ^(@lastsub)="" if (1=$increment(@name("",/tmp/$ztvalue/))),$increment(@name(""))

ttZKe2	;/set tmp="#"_$ztoldval //if $data(@name(0,/tmp/$ztoldval/,@sub))#10 zkill ^(@lastsub) if 1>$increment(@name("",/tmp/$ztoldval/),-1) zkill ^(/tmp/$ztoldval/) if 1>$increment(@name(""),-1) zkill ^("")

ttSp0	; for i=@pieces set p=/"#"_//$@zpiece($ztoldval,@sep,i),q=/"#"_//$@zpiece($ztvalue,@sep,i) do
	; . if p'=q zkill @name(i,p,@sub) set @name(i,q,@sub)=""
	; . else  set:'($zlength(q)/-1//) @name(i,"/#//",@sub)=""

ttZKp0	;for i=@pieces zkill @name(i,/"#"_//$@zpiece($ztoldval,@sep,i),@sub)

ttSp1	; for i=@pieces set p=/"#"_//$@zpiece($ztoldval,@sep,i),q=/"#"_//$@zpiece($ztvalue,@sep,i),j=-i do
	; . if p'=q do
	; . . if $data(@name(i,p,@sub))#10 zkill ^(@lastsub) zkill:1>$increment(@name(j,p),-1) ^(p)
	; . . if '($data(@name(i,q,@sub))#10) set ^(@lastsub)="" if $increment(@name(j,q))
	; . else  if '($zlength(q)/-1//),'($data(@name(i,"/#//",@sub))#10) set ^(@lastsub)="" if $increment(@name(j,"/#//"))

ttZKp1	;for i=@pieces set j=-i,p=/"#"_//$@zpiece($ztoldval,@sep,i) if $data(@name(i,p,@sub)) zkill ^(@lastsub) zkill:1>$increment(@name(j,p),-1) ^(p)

ttSp2	; for i=@pieces set p=/"#"_//$@zpiece($ztoldval,@sep,i),q=/"#"_//$@zpiece($ztvalue,@sep,i),j=-i do
	; . if p'=q do
	; . . if $data(@name(i,p,@sub))#10 zkill ^(@lastsub) if 1>$increment(@name(j,p),-1) zkill ^(p) if 1>$increment(@name(j),-1) zkill ^(j)
	; . . if '($data(@name(i,q,@sub))#10) set ^(@lastsub)="" if (1=$increment(@name(j,q))),$increment(@name(j))
	; . else  if '($zlength(q)/-1//),'($data(@name(i,"/#//",@sub))#10) set ^(@lastsub)="" if (1=$increment(@name(j,"/#//"))),$increment(@name(j))

ttZKp2	;for i=@pieces set j=-i,p=/"#"_//$@zpiece($ztoldval,@sep,i) if $data(@name(i,p,@sub)) zkill ^(@lastsub) if 1>$increment(@name(j,p),-1) zkill ^(p) zkill:1>$increment(@name(j),-1) ^(j)

tt1Se0	; if @type1last=@lastfullsub zkill @name(0,"/#//",@altsub),@name(0,/"#"_//$ztoldval,@sub) set @name(0,/"#"_//$ztvalue,@sub)=""
	; else  set:'($data(@gbl(@fullsub))#10) @name(0,"/#//",@altsub)=""

tt1ZKe0	; if @type1last=@lastfullsub zkill @name(0,/"#"_//$ztoldval,@sub) set:($data(@gbl(@fullsubprnt))#10)!$zlength($order(@gbl(@fullsub)))!$zlength($order(@gbl(@fullsub),-1)) @name(0,"/#//",@altsub)=""
	; else  zkill:'($data(@gbl(@fullsubprnt))#10)&('$data(@gbl(@fullsub))#10)&('($zlength($order(@gbl(@fulltrigsub)))!($zlength($order(@gbl(@fulltrigsub),-1))))) @name(0,"/#//",@altsub)

tt1Se1	; if @type1last=@lastfullsub do
	; . if $data(@name(0,"/#//",@altsub)) zkill ^(@altlastsub) zkill:1>$increment(@name("","/#//"),-1) ^("/#//")
	; . /set tmp="#"_$ztoldval //if $data(@name(0,/tmp/$ztoldval/,@sub)) zkill ^(@lastsub) zkill:1>$increment(@name("",/tmp/$ztoldval/),-1) ^(/tmp/$ztoldval/)
	; . /set tmp="#"_$ztvalue //if '$data(@name(0,/tmp/$ztvalue/,@sub)) set ^(@lastsub)="" if $increment(@name("",/tmp/$ztvalue/))
	; else  if '($data(@gbl(@fullsub))#10),'$data(@name(0,"/#//",@altsub)) set ^(@altlastsub)="" if $increment(@name("","/#//"))

tt1ZKe1	; if @type1last=@lastfullsub do
	; . /set tmp="#"_$ztoldval //if $data(@name(0,/tmp/$ztoldval/,@sub)) zkill ^(@lastsub) zkill:1>$increment(@name("",/tmp/$ztoldval/),-1) ^(/tmp/$ztoldval/)
	; . if ($data(@gbl(@fullsubprnt))#10)!$zlength($order(@gbl(@fullsub)))!$zlength($order(@gbl(@fullsub),-1)),$increment(@name("","/#//")) set @name(0,"/#//",@altsub)=""
	; else  if '($data(@gbl(@fullsubprnt))#10),'$data(@gbl(@fullsub))#10,('($zlength($order(@gbl(@fulltrigsub)))!($zlength($order(@gbl(@fulltrigsub),-1))))),$data(@name(0,"/#//",@altsub)) zkill ^(@altlastsub) zkill:1>$increment(@name("","/#//"),-1) ^("")

tt1Se2	; if @type1last=@lastfullsub do
	; . if $data(@name(0,"/#//",@altsub)) zkill ^(@altlastsub) if 1>$increment(@name("","/#//"),-1) zkill ^("/#//") if 1>$increment(@name(""),-1) zkill ^("")
	; . /set tmp="#"_$ztoldval //if $data(@name(0,/tmp/$ztoldval/,@sub)) zkill ^(@lastsub) if 1>$increment(@name("",/tmp/$ztoldval/),-1) zkill ^(/tmp/$ztoldval/) if 1>$increment(@name(""),-1) zkill ^("")
	; . /set tmp="#"_$ztvalue //if '$data(@name(0,/tmp/$ztvalue/,@sub)) set ^(@lastsub)="" if 1=$increment(@name("",/tmp/$ztvalue/)),$increment(@name(""))
	; else  if '($data(@gbl(@fullsubprnt))#10),'($data(@gbl(@fullsub))#10),'$data(@name(0,"/#//",@altsub)) set ^(@altlastsub)="" if 1=$increment(@name("","/#//")),$increment(@name(""))

tt1ZKe2	; if @type1last=@lastfullsub do
	; . /set tmp="#"_$ztoldval //if $data(@name(0,/tmp/$ztoldval/,@sub)) zkill ^(@lastsub) if 1>$increment(@name("",/tmp/$ztoldval/),-1) zkill ^(/tmp/$ztoldval/) if 1>$increment(@name(""),-1) zkill ^("")
	; . if ($data(@gbl(@fullsubprnt))#10)!$zlength($order(@gbl(@fullsub)))!$zlength($order(@gbl(@fullsub),-1)) set @name(0,"/#//",@altsub)="" if 1=$increment(@name("","/#//")),$increment(@name(""))
	; else  if '($data(@gbl(@fullsubprnt))#10),'$data(@gbl(@fullsub)),('($zlength($order(@gbl(@fulltrigsub)))!($zlength($order(@gbl(@fulltrigsub),-1))))),$data(@name(0,"/#//",@altsub)) zkill ^(@altlastsub) if 1>$increment(@name("","/#//"),-1) zkill ^("/#//") if 1>$increment(@name("/#//"),-1) zkill ^("/#//")

tt1Sp0	; if @type1last=@lastfullsub do
	; . for i=@pieces set p=/"#"_//$@zpiece($ztoldval,@sep,i),q=/"#"_//$@zpiece($ztvalue,@sep,i) zkill @name(i,"/#//",@altsub),@name(i,p,@sub) set @name(i,q,@sub)=""
	; else  if '($data(@gbl(@fullsub))#10) for i=@pieces set @name(i,"/#//",@altsub)=""

tt1ZKp0	; if @type1last=@lastfullsub do
	; . if $data(@gbl(@fullsubprnt))#10!$zlength($order(@gbl(@fullsub)))!$zlength($order(@gbl(@fullsub),-1)) for i=@pieces zkill @name(i,/"#"_//$@zpiece($ztoldval,@sep,i),@sub) set @name(i,"/#//",@altsub)=""
	; . else  for i=@pieces zkill @name(i,/"#"_//$@zpiece($ztoldval,@sep,i),@sub)
	; else  if '($data(@gbl(@fullsubprnt))#10),'($data(@gbl(@fullsub))#10),'($zlength($order(@gbl(@fulltrigsub)))!($zlength($order(@gbl(@fulltrigsub),-1)))) for i=@pieces zkill @name(i,"/#//",@altsub)

tt1Sp1	; if @type1last=@lastfullsub do
	; . for i=@pieces set p=/"#"_//$@zpiece($ztoldval,@sep,i),q=/"#"_//$@zpiece($ztvalue,@sep,i) do
	; . . if $data(@name(i,"/#//",@altsub)) zkill ^(@altlastsub) zkill:1>$increment(@name(-i,"/#//"),-1) ^("/#//")
	; . . else  if $data(@name(i,p,@sub)) zkill ^(@lastsub) zkill:1>$increment(@name(-i,p),-1) ^(p)
	; . . if '$data(@name(i,q,@sub)) set ^(@lastsub)="" if $increment(@name(-i,q))
	; else  if '($data(@gbl(@fullsub))#10) for i=@pieces if '$data(@name(i,"/#//",@altsub)) set ^(@altlastsub)="" if $increment(@name(-i,"/#//"))

tt1ZKp1	; if @type1last=@lastfullsub do
	; . if $data(@gbl(@fullsubprnt))#10!$zlength($order(@gbl(@fullsub)))!$zlength($order(@gbl(@fullsub),-1)) for i=@pieces set p=/"#"_//$@zpiece($ztoldval,@sep,i) do
	; . . if $data(@name(i,p,@sub)) zkill ^(@lastsub) zkill:1>$increment(@name(-i,p),-1) ^(p)
	; . . if '$data(@name(i,"/#//",@altsub)) set ^(@altlastsub)="" if $increment(@name(-i,"/#//"))
	; . else  for i=@pieces set p=/"#"_//$@zpiece($ztoldval,@sep,i) if $data(@name(i,p,@sub)) zkill ^(@lastsub) zkill:1>$increment(@name(-i,p),-1) ^(p)
	; else  if '($data(@gbl(@fullsubprnt))#10),'($data(@gbl(@fullsub))#10),'($zlength($order(@gbl(@fulltrigsub)))!($zlength($order(@gbl(@fulltrigsub),-1)))) for i=@pieces if $data(@name(i,"/#//",@altsub)) zkill ^(@altlastsub) zkill:1>$increment(@name(-i,"/#//"),-1) ^("/#//")

tt1Sp2	; if @type1last=@lastfullsub do
	; . for i=@pieces set j=-i,p=/"#"_//$@zpiece($ztoldval,@sep,i),q=/"#"_//$@zpiece($ztvalue,@sep,i) do
	; . . if $data(@name(i,"/#//",@altsub)) zkill ^(@altlastsub) if 1>$increment(@name(j,"/#//"),-1) zkill ^("/#//") zkill:1>$increment(@name(j),-1) ^(j)
	; . . else  if $data(@name(i,p,@sub)) zkill ^(@lastsub) if 1>$increment(@name(j,p),-1) zkill ^(p) zkill:1>$increment(@name(j),-1) ^(j)
	; . . if '$data(@name(i,q,@sub)) set ^(@lastsub)="" if 1=$increment(@name(j,q)),$increment(@name(j))
	; else  if '($data(@gbl(@fullsub))#10) for i=@pieces set j=-i if '$data(@name(i,"/#//",@altsub)) set ^(@altlastsub)="" if 1=$increment(@name(j,"/#//")),$increment(@name(j))

tt1ZKp2	; if @type1last=@lastfullsub do
	; . if $data(@gbl(@fullsubprnt))#10!$zlength($order(@gbl(@fullsub)))!$zlength($order(@gbl(@fullsub),-1)) for i=@pieces set j=-i,p=/"#"_//$@zpiece($ztoldval,@sep,i) do
	; . . if $data(@name(i,p,@sub)) zkill ^(@lastsub) if 1>$increment(@name(j,p),-1) zkill ^(p) zkill:1>$increment(@name(j),-1) ^(j)
	; . . if '$data(@name(i,"/#//",@altsub)) set ^(@altlastsub)="" if 1=$increment(@name(j,"/#//")),$increment(@name(j))
	; . else  for i=@pieces set j=-i,p=/"#"_//$@zpiece($ztoldval,@sep,i) if $data(@name(i,p,@sub)) zkill ^(@lastsub) if 1>$increment(@name(j,p),-1) zkill ^(p) zkill:1>$increment(@name(j),-1) ^(j)
	; else  if '($data(@gbl(@fullsubprnt))#10),'($data(@gbl(@fullsub))#10),'($zlength($order(@gbl(@fulltrigsub)))!($zlength($order(@gbl(@fulltrigsub),-1)))) for i=@pieces if $data(@name(i,"/#//",@altsub)) set j=-i zkill ^(@altlastsub) if 1>$increment(@name(j,"/#//"),-1) zkill ^("/#//") zkill:1>$increment(@name(j),-1) ^(j)

tt1pSe0	;set:'($data(@gbl(@fullsub))#10) @name(0,"/#//",@altsub)=""

tt1pZKe0;zkill:'($ZTDATA\10) @name(0,"/#//",@altsub)

tt1pSe1	;if '($data(@gbl(@fullsub))#10),'$data(@name(0,"/#//",@altsub)) set ^(@altlastsub)="" if $increment(@name("","/#//"))

tt1pZKe1;if '($ZTDATA\10),$data(@name(0,"/#//",@altsub)) zkill ^(@altlastsub) zkill:1>$increment(@name("","/#//"),-1) ^("/#//")

tt1pSe2	;if '($data(@gbl(@fullsub))#10),'$data(@name(0,"/#//",@altsub)) set ^(@altlastsub)="" if 1=$increment(@name("","/#//")),$increment(@name("/#//"))

tt1pZKe2 ;if '($ZTDATA\10),$data(@name(0,"/#//",@altsub)) zkill ^(@altlastsub) if 1>$increment(@name("","/#//"),-1) zkill ^("/#//") if 1>$increment(@name("/#//"),-1) zkill ^("/#//")

tt1pSp0	;if '($data(@gbl(@fullsub))#10) for i=@pieces set @name(i,"/#//",@altsub)=""

tt1pZKp0;if '($ZTDATA\10) for i=@pieces zkill @name(i,"/#//",@altsub)

tt1pSp1	;if '($data(@gbl(@fullsub))#10) for i=@pieces if '$data(@name(i,"/#//",@altsub)) set ^(@altlastsub)="" if $increment(@name(-i,"/#//"))

tt1pZKp1;if '($ZTDATA\10) for i=@pieces if $data(@name(i,"/#//",@altsub)) zkill ^(@altlastsub) zkill:1>$increment(@name(-i,"/#//"),-1) ^("/#//")

tt1pSp2	;if '($data(@gbl(@fullsub))#10) for i=@pieces set j=-i if '$data(@name(i,"/#//",@altsub)) set ^(@altlastsub)="" if 1=$increment(@name(j,"/#//")),$increment(@name(j))

tt1pZKp2 ;if '($ZTDATA\10) for i=@pieces if $data(@name(i,"/#//",@altsub)) set j=-i zkill ^(@altlastsub) if 1>$increment(@name(j,"/#//"),-1) zkill ^("/#//") zkill:1>$increment(@name(j),-1) ^(j)

tt2Se0	;zkill @name(0,@xfnp1$ztoldval@xfnp2,@sub) set @name(0,@xfnp1$ztvalue@xfnp2,@sub)=""

tt2ZKe0	;zkill @name(0,@xfnp1$ztoldval@xfnp2,@sub)

tt2Se1	; if $data(@name(0,@xfnp1$ztoldval@xfnp2,@sub))#10 zkill ^(@lastsub) zkill:1>$increment(@name("",@xfnp1$ztoldval@xfnp2),-1) ^(@xfnp1$ztoldval@xfnp2)
	; if '$data(@name(0,@xfnp1$ztvalue@xfnp2,@sub)) set ^(@lastsub)="" if $increment(@name("",@xfnp1$ztvalue@xfnp2))

tt2ZKe1	;if $data(@name(0,@xfnp1$ztoldval@xfnp2,@sub))#10 zkill ^(@lastsub) zkill:1>$increment(@name("",@xfnp1$ztoldval@xfnp2),-1) ^(@xfnp1$ztoldval@xfnp2)

tt2Se2	; set tmp=@xfnp1$ztoldval@xfnp2 if $data(@name(0,tmp,@sub))#10 zkill ^(@lastsub) if 1>$increment(@name("",tmp),-1) zkill ^(tmp) zkill:1>$increment(@name(""),-1) ^("")
	; set tmp=@xfnp1$ztvalue@xfnp2 if '$data(@name(0,tmp,@sub)) set ^(@lastsub)="" if (1=$increment(@name("",tmp))),$increment(@name(""))

tt2ZKe2	;set tmp=@xfnp1$ztoldva@xfnp2 if $data(@name(0,tmp,@sub))#10 zkill ^(@lastsub) if 1>$increment(@name("",tmp),-1) zkill ^(tmp) if 1>$increment(@name(""),-1) zkill ^("")

tt2Sp0	; for i=@pieces set p=@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2,q=@xfnp1$@zpiece($ztvalue,@sep,i)@xfnp2 do
	; . if p'=q zkill @name(i,p,@sub) set @name(i,q,@sub)=""
	; . else  set:'($zlength(q)) @name(i,"",@sub)=""

tt2ZKp0	;for i=@pieces zkill @name(i,@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2,@sub)

tt2Sp1	; for i=@pieces set p=@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2,q=@xfnp1$@zpiece($ztvalue,@sep,i)@xfnp2,j=-i do
	; . if p'=q do
	; . . if $data(@name(i,p,@sub))#10 zkill ^(@lastsub) zkill:1>$increment(@name(j,p),-1) ^(p)
	; . . if '($data(@name(i,q,@sub))#10) set ^(@lastsub)="" if $increment(@name(j,q))
	; . else  if '($zlength(q)),'($data(@name(i,"",@sub))#10) set ^(@lastsub)="" if $increment(@name(j,q))

tt2ZKp1	;for i=@pieces set j=-i,p=@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2 if $data(@name(i,p,@sub)) zkill ^(@lastsub) zkill:1>$increment(@name(j,p),-1) ^(p)

tt2Sp2	; for i=@pieces set p=@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2,q=@xfnp1$@zpiece($ztvalue,@sep,i)@xfnp2,j=-i do
	; . if p'=q do
	; . . if $data(@name(i,p,@sub))#10 zkill ^(@lastsub) if 1>$increment(@name(j,p),-1) zkill ^(p) if 1>$increment(@name(j),-1) zkill ^(j)
	; . . if '($data(@name(i,q,@sub))#10) set ^(@lastsub)="" if (1=$increment(@name(j,q))),$increment(@name(j))
	; . else  if '($zlength(q)),'($data(@name(i,"",@sub))#10) set ^(@lastsub)="" if (1=$increment(@name(j,""))),$increment(@name(j))

tt2ZKp2	;for i=@pieces set j=-i,p=@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2 if $data(@name(i,p,@sub)) zkill ^(@lastsub) if 1>$increment(@name(j,p),-1) zkill ^(p) zkill:1>$increment(@name(j),-1) ^(j)

tt3Se0	; if @type1last=@lastfullsub zkill @name(0,"",@altsub),@name(0,@xfnp1$ztoldval@xfnp2,@sub) set @name(0,@xfnp1$ztvalue@xfnp2,@sub)=""
	; else  set:'($data(@gbl(@fullsub))#10) @name(0,"",@altsub)=""

tt3ZKe0	; if @type1last=@lastfullsub zkill @name(0,@xfnp1$ztoldval@xfnp2,@sub) set:($data(@gbl(@fullsubprnt))#10)!$zlength($order(@gbl(@fullsub)))!$zlength($order(@gbl(@fullsub),-1)) @name(0,"",@altsub)=""
	; else  zkill:'($data(@gbl(@fullsubprnt))#10)&('$data(@gbl(@fullsub))#10)&('($zlength($order(@gbl(@fulltrigsub)))!($zlength($order(@gbl(@fulltrigsub),-1))))) @name(0,"",@altsub)

tt3Se1	; if @type1last=@lastfullsub do
	; . if $data(@name(0,"",@altsub)) zkill ^(@altlastsub) zkill:1>$increment(@name("",""),-1) ^("")
	; . set tmp=@xfnp1$ztoldval@xfnp2 if $data(@name(0,tmp,@sub)) zkill ^(@lastsub) zkill:1>$increment(@name("",tmp),-1) ^(tmp)
	; . set tmp=@xfnp1$ztvalue@xfnp2 if '$data(@name(0,tmp,@sub)) set ^(@lastsub)="" if $increment(@name("",tmp))
	; else  if '($data(@gbl(@fullsub))#10),'$data(@name(0,"",@altsub)) set ^(@altlastsub)="" if $increment(@name("",""))

tt3ZKe1	; if @type1last=@lastfullsub do
	; . set tmp=@xfnp1$ztoldval@xfnp2 if $data(@name(0,tmp,@sub)) zkill ^(@lastsub) zkill:1>$increment(@name("",tmp),-1) ^(tmp)
	; . if ($data(@gbl(@fullsubprnt))#10)!$zlength($order(@gbl(@fullsub)))!$zlength($order(@gbl(@fullsub),-1)),$increment(@name("","")) set @name(0,"",@altsub)=""
	; else  if '($data(@gbl(@fullsubprnt))#10),'$data(@gbl(@fullsub))#10,('($zlength($order(@gbl(@fulltrigsub)))!($zlength($order(@gbl(@fulltrigsub),-1))))),$data(@name(0,"",@altsub)) zkill ^(@altlastsub) zkill:1>$increment(@name("",""),-1) ^("")

tt3Se2	; if @type1last=@lastfullsub do
	; . if $data(@name(0,"",@altsub)) zkill ^(@altlastsub) if 1>$increment(@name("",""),-1) zkill ^("") if 1>$increment(@name(""),-1) zkill ^("")
	; . set tmp=@xfnp1$ztoldval@xfnp2 if $data(@name(0,tmp,@sub)) zkill ^(@lastsub) if 1>$increment(@name("",tmp),-1) zkill ^(tmp) if 1>$increment(@name(""),-1) zkill ^("")
	; . set tmp=@xfnp1$ztvalue@xfnp2 if '$data(@name(0,tmp,@sub)) set ^(@lastsub)="" if 1=$increment(@name("",tmp)),$increment(@name(""))
	; else  if '($data(@gbl(@fullsubprnt))#10),'($data(@gbl(@fullsub))#10),'$data(@name(0,"",@altsub)) set ^(@altlastsub)="" if 1=$increment(@name("","")),$increment(@name(""))

tt3ZKe2	; if @type1last=@lastfullsub do
	; . set tmp=@xfnp1$ztoldval@xfnp2 if $data(@name(0,tmp,@sub)) zkill ^(@lastsub) if 1>$increment(@name("",tmp),-1) zkill ^(tmp) if 1>$increment(@name(""),-1) zkill ^("")
	; . if ($data(@gbl(@fullsubprnt))#10)!$zlength($order(@gbl(@fullsub)))!$zlength($order(@gbl(@fullsub),-1)) set @name(0,"",@altsub)="" if 1=$increment(@name("","")),$increment(@name(""))
	; else  if '($data(@gbl(@fullsubprnt))#10),'$data(@gbl(@fullsub)),('($zlength($order(@gbl(@fulltrigsub)))!($zlength($order(@gbl(@fulltrigsub),-1))))),$data(@name(0,"",@altsub)) zkill ^(@altlastsub) if 1>$increment(@name("",""),-1) zkill ^("") if 1>$increment(@name(""),-1) zkill ^("")

tt3Sp0	; if @type1last=@lastfullsub do
	; . for i=@pieces set p=@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2,q=@xfnp1$@zpiece($ztvalue,@sep,i)@xfnp2 zkill @name(i,"",@altsub),@name(i,p,@sub) set @name(i,q,@sub)=""
	; else  if '($data(@gbl(@fullsub))#10) for i=@pieces set @name(i,"",@altsub)=""

tt3ZKp0	; if @type1last=@lastfullsub do
	; . if $data(@gbl(@fullsubprnt))#10!$zlength($order(@gbl(@fullsub)))!$zlength($order(@gbl(@fullsub),-1)) for i=@pieces zkill @name(i,@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2,@sub) set @name(i,"",@altsub)=""
	; . else  for i=@pieces zkill @name(i,@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2,@sub)
	; else  if '($data(@gbl(@fullsubprnt))#10),'($data(@gbl(@fullsub))#10),'($zlength($order(@gbl(@fulltrigsub)))!($zlength($order(@gbl(@fulltrigsub),-1)))) for i=@pieces zkill @name(i,"",@altsub)

tt3Sp1	; if @type1last=@lastfullsub do
	; . for i=@pieces set p=@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2,q=@xfnp1$@zpiece($ztvalue,@sep,i)@xfnp2 do
	; . . if $data(@name(i,"",@altsub)) zkill ^(@altlastsub) zkill:1>$increment(@name(-i,""),-1) ^("")
	; . . else  if $data(@name(i,p,@sub)) zkill ^(@lastsub) zkill:1>$increment(@name(-i,p),-1) ^(p)
	; . . if '$data(@name(i,q,@sub)) set ^(@lastsub)="" if $increment(@name(-i,q))
	; else  if '($data(@gbl(@fullsub))#10) for i=@pieces if '$data(@name(i,"",@altsub)) set ^(@altlastsub)="" if $increment(@name(-i,""))

tt3ZKp1	; if @type1last=@lastfullsub do
	; . if $data(@gbl(@fullsubprnt))#10!$zlength($order(@gbl(@fullsub)))!$zlength($order(@gbl(@fullsub),-1)) for i=@pieces set p=@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2 do
	; . . if $data(@name(i,p,@sub)) zkill ^(@lastsub) zkill:1>$increment(@name(-i,p),-1) ^(p)
	; . . if '$data(@name(i,"",@altsub)) set ^(@altlastsub)="" if $increment(@name(-i,""))
	; . else  for i=@pieces set p=@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2 if $data(@name(i,p,@sub)) zkill ^(@lastsub) zkill:1>$increment(@name(-i,p),-1) ^(p)
	; else  if '($data(@gbl(@fullsubprnt))#10),'($data(@gbl(@fullsub))#10),'($zlength($order(@gbl(@fulltrigsub)))!($zlength($order(@gbl(@fulltrigsub),-1)))) for i=@pieces if $data(@name(i,"",@altsub)) zkill ^(@altlastsub) zkill:1>$increment(@name(-i,""),-1) ^("")

tt3Sp2	; if @type1last=@lastfullsub do
	; . for i=@pieces set j=-i,p=@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2,q=@xfnp1$@zpiece($ztvalue,@sep,i)@xfnp2 do
	; . . if $data(@name(i,"",@altsub)) zkill ^(@altlastsub) if 1>$increment(@name(j,""),-1) zkill ^("") zkill:1>$increment(@name(j),-1) ^(j)
	; . . else  if $data(@name(i,p,@sub)) zkill ^(@lastsub) if 1>$increment(@name(j,p),-1) zkill ^(p) zkill:1>$increment(@name(j),-1) ^(j)
	; . . if '$data(@name(i,q,@sub)) set ^(@lastsub)="" if 1=$increment(@name(j,q)),$increment(@name(j))
	; else  if '($data(@gbl(@fullsub))#10) for i=@pieces set j=-i if '$data(@name(i,"",@altsub)) set ^(@altlastsub)="" if 1=$increment(@name(j,"")),$increment(@name(j))

tt3ZKp2	; if @type1last=@lastfullsub do
	; . if $data(@gbl(@fullsubprnt))#10!$zlength($order(@gbl(@fullsub)))!$zlength($order(@gbl(@fullsub),-1)) for i=@pieces set j=-i,p=@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2 do
	; . . if $data(@name(i,p,@sub)) zkill ^(@lastsub) if 1>$increment(@name(j,p),-1) zkill ^(p) zkill:1>$increment(@name(j),-1) ^(j)
	; . . if '$data(@name(i,"",@altsub)) set ^(@altlastsub)="" if 1=$increment(@name(j,"")),$increment(@name(j))
	; . else  for i=@pieces set j=-i,p=@xfnp1$@zpiece($ztoldval,@sep,i)@xfnp2 if $data(@name(i,p,@sub)) zkill ^(@lastsub) if 1>$increment(@name(j,p),-1) zkill ^(p) zkill:1>$increment(@name(j),-1) ^(j)
	; else  if '($data(@gbl(@fullsubprnt))#10),'($data(@gbl(@fullsub))#10),'($zlength($order(@gbl(@fulltrigsub)))!($zlength($order(@gbl(@fulltrigsub),-1)))) for i=@pieces if $data(@name(i,"",@altsub)) set j=-i zkill ^(@altlastsub) if 1>$increment(@name(j,""),-1) zkill ^("") zkill:1>$increment(@name(j),-1) ^(j)

ttsS0	;set:'$data(@name(@locsnum,/"#"_//sub@locsnum,@sub)) ^(@lastsubind)=""

ttsZK0	;zkill:$data(@name(@locsnum,/"#"_//sub@locsnum,@sub)) ^(@lastsubind)

ttsS1	;/set tmp="#"_sub@locsnum //if '$data(@name(@locsnum,/tmp/sub@locsnum/,@sub)) set ^(@lastsubind)="" if $increment(@name(-@locsnum,/tmp/sub@locsnum/))

ttsZK1	;/set tmp="#"_sub@locsnum //if $data(@name(@locsnum,/tmp/sub@locsnum/,@sub)) zkill ^(@lastsubind) zkill:1>$increment(@name(-@locsnum,/tmp/sub@locsnum/),-1) ^(/tmp/sub@locsnum/)

ttsS2	;/set tmp="#"_sub@locsnum //if '$data(@name(@locsnum,/tmp/sub@locsnum/,@sub)) set ^(@lastsubind)="" if 1=$increment(@name(-@locsnum,/tmp/sub@locsnum/)),$increment(@name(-@locsnum))

ttsZK2	;/set tmp="#"_sub@locsnum //if $data(@name(@locsnum,/tmp/sub@locsnum/,@sub)) zkill ^(@lastsubind) if 1>$increment(@name(-@locsnum,/tmp/sub@locsnum/),-1) zkill ^(/tmp/sub@locsnum/) zkill:1>$increment(@name(-@locsnum),-1) ^(-@locsnum)

tts2S0	;set:'$data(@name(@locsnum,@xfnp1sub@locsnum@xfnp2,@sub)) ^(@lastsubind)=""

tts2ZK0	;zkill:$data(@name(@locsnum,@xfnp1sub@locsnum@xfnp2,@sub)) ^(@lastsubind)

tts2S1	;set tmp=@xfnp1sub@locsnum@xfnp2 if '$data(@name(@locsnum,tmp,@sub)) set ^(@lastsubind)="" if $increment(@name(-@locsnum,tmp))

tts2ZK1	;set tmp=@xfnp1sub@locsnum@xfnp2 if $data(@name(@locsnum,tmp,@sub)) zkill ^(@lastsubind) zkill:1>$increment(@name(-@locsnum,tmp),-1) ^(tmp)

tts2S2	;set tmp=@xfnp1sub@locsnum@xfnp2 if '$data(@name(@locsnum,tmp,@sub)) set ^(@lastsubind)="" if 1=$increment(@name(-@locsnum,tmp)),$increment(@name(-@locsnum))

tts2ZK2	;set tmp=@xfnp1sub@locsnum@xfnp2 if $data(@name(@locsnum,tmp,@sub)) zkill ^(@lastsubind) if 1>$increment(@name(-@locsnum,tmp),-1) zkill ^(tmp) zkill:1>$increment(@name(-@locsnum),-1) ^(-@locsnum)

;	Error message texts
U228	;"-F-INVSNUM snum="_$get(snum)_" includes subscript numbers that cannot be cross referenced"
U229	;"-F-BADTRANSFORM  with type>1, force="""_$get(force)_""" is not a valid function entryref"
U230	;"-F-ALLCONST Cross referencing a subscript requires at least one non-constant subscript; No of subscripts="_nsubs
U231	;"-F-BADTEMPLATE Trigger "_$get(outstr)_" has incorrect number of / delimiters for text substitution"
U232	;"-F-BADZINTERRUPT Caller's $ZINTERRUPT handler returned control to %YDBAIM"
U233	;"-F-JOBERR Error(s) reported by JOB'd process in "_err(i)
U234	;"-F-JOBFAIL Failed to JOB process for xrefdata/xrefsub("_$get(nsubs)_","_$get(i)_")"
U235	;"-F-NULL1 Null subscripts are not permitted for type=1 global variables"
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
U254	;"-F-NOEXTREF Extended reference in "_$get(gbl)_" is not supported"
U255	;"-F-BADINVOCATION Top level invocation of "_$text(+0)_" not supported; must invoke a label"
