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
; Application Independent Metadata
;
; Compute and maintain application independent metadata, including:
; - Cross References
; (Other metadata to be added)
;
; A design point is that the triggers used to maintain Consistency between
; application global variable nodes and cross references are the most critical
; code to optimize, since that code will be executed on *every single update*
; to a cross referenced global variable. Other code should be reasonably
; efficient, but is not as critical.
;
; Cross References
;
;   Consider a global variable whose first subscript is the year a US
;   President assumed office and the second subscript is the year that President
;   left office, with values such as:
;
;	^USPresidents(1797,1801)="John||Adams" and
;   	^USPresidents(1835,1839)="John|Quincy|Adams"
;
;   - A call such as $$XREFDATA^%YDBAIM("^USPresidents",2,"|",3) would cross
;     reference the third piece of node values (last names) and with the cross
;     reference global having values such as
;     ^%ydbAIMDWn59H4eXfRmii1N5YvuC03(3,"Adams",1797,1801)="",
;     ^%ydbAIMDWn59H4eXfRmii1N5YvuC03(3,"Adams",1835,1839)="", and many others
;     including ^%ydbAIMDWn59H4eXfRmii1N5YvuC03(3,"Obama",2009,2017)="".
;
;   - Suppose only Presidents who left office in the 19th century should be
;     cross referenced. A local variable node such as cent19(2)="1801:1899" can
;     be created, and passed by reference, and
;     $$XREFDATA^%YDBAIM("^USPresidents",.cent19,"|",3) would produce the two
;     cross references as the above, but in a different global variable name
;     since the trigger signatures (see below for an explanation) would be
;     different. Unlike the first cross reference, this would not cross
;     reference Barack Obama who assumed office in 2009 and left in 2017.
;
;   - Suppose only Presidents who assumed office in the 19th century should be
;     cross referenced, a local variable cent19 would instead have the node
;     cent19(1)="1801|1899" to indicate that only first subscripts should be
;     cross referenced if they are in the 19th century, but the local variable
;     root node cent19=2 should be set to indicate that two subscripts should be
;     cross referenced. In this case, the call
;     $$XREFDATA^%YDBAIMD("^USPresidents",.cent19,"|",3) would generate a cross
;     reference that includes John Quincy Adams, but not John Adams who assumed
;     office in 1797, which is in the 18th century.
;
;   To cross reference all three names, a call such as
;   $$XREFDATA^%YDBAIM("^USPresidents",2,"|","1:3") would generate the following
;   cross references for the two President Adams:
;     ^%ydbAIMDWn59H4eXfRmii1N5YvuC03(1,"John",1797,1801)=""
;     ^%ydbAIMDWn59H4eXfRmii1N5YvuC03(1,"John",1835,1839)=""
;     ^%ydbAIMDWn59H4eXfRmii1N5YvuC03(2,"",1797,1801)=""
;     ^%ydbAIMDWn59H4eXfRmii1N5YvuC03(2,"Quincy",1835,1839)=""
;     ^%ydbAIMDWn59H4eXfRmii1N5YvuC03(3,"Adams",1797,1801)=""
;     ^%ydbAIMDWn59H4eXfRmii1N5YvuC03(3,"Adams",1835,1839)=""
;
;   Since the first President Adams record does not include a middle name, the
;   corresponding record has an empty string ("") subscript. Any region to which
;   ^%ydbAIMD* global variables are mapped should have NULL_SUBSCRIPTS set to
;   ALWAYS. Since the subscripts will include pieces of global nodes, or even
;   entire global nodes, it would be prudent to set YottaDB's maximum key size
;   (1019 bytes) for that region.
;
;  Note: subscript specifications which are not canonical numbers should be
;  quoted. So to cross reference the first piece with "|" as the separator of
;  ^%ydbocto("tables","pg_catalog","pg_attribute",*):
;    YDB>set sub=4,sub(1)="""tables""",sub(2)="""pg_catalog""",sub(3)="""pg_attribute"""
;
;    YDB>set xref=$$XREFDATA^%YDBAIM("^%ydbocto",.sub,"|",1)
;  to create the cross reference:
;    YDB>write xref
;    ^%ydbAIMDvjlGbD84bQ5u5hXGOtIe37
;    YDB>
;  Setting a value now creates the cross reference:
;    YDB>set ^%ydbocto("tables","pg_catalog","pg_attribute",100)="ABC|DEF"
;
;    YDB>write $query(@xref@(1,"ABC",""))
;    ^%ydbAIMDvjlGbD84bQ5u5hXGOtIe37(1,"ABC",100)
;    YDB>YDB
;
%YDBAIM
	; Top level entry not supported
	do etrap		; set error trap
	set $ecode=",U255,"	; top level entry not supported
	quit			; should never get here

err	; Error handler
	new io,stderr,tmp1,tmp2
	set io=$io
	set tmp1=$zpiece($ecode,",",2),tmp2=$text(@tmp1)
	set:'$zlength(tmp2) tmp2=$text(U251)
	set stderr="/proc/self/fd/2"
	open stderr
	use stderr
	write $text(+0),@$zpiece(tmp2,";",2,$zlength(tmp2,";")),!
	use io
	zshow "s":tmp1
	zhalt:"%XCMD"=$piece($get(tmp1("S",$order(tmp1("S",""""),-1))),"^",2) +$extract(tmp1,2,$zlength(tmp1))
	quit

; Set error trap. The first action the error trap does is set a failsafe error
; trap (e.g., if $zroutines is not correct). Thereafter, it jumps to the actual
; error trap to print an error message and terminate, with a return code.
;
; Note that all external entry points NEW $ETRAP and DO etrap if $ETRAP is the
; default error handler. This allows them to report the error messages for
; %YDBAIM errors, instead of just reporting that $ECODE was assigned a
; non-empty value, which the default error trap will report. If an application
; sets an error trap ($ETRAP or $ETRAP) that will be used.
etrap
	set $etrap="set $etrap=""open """"/proc/self/fd/2"""" use """"/proc/self/fd/2"""" write $zstatus,! zshow """"*"""" zhalt 1""  goto err^"_$text(+0)
	quit

; List metadata for a cross reference, all cross references for a global
; variable, or all cross references
;
; Usage: DO LSXREFDATA^%YDBAIM(lvn[,gbl])
; Parameters
; - lvn is a local variable passed by reference. In that local variable, the
;   function describes all cross references as follows:
;   - The first subscript is the cross refence global variable name, e.g.,
;     "^%ydbAIMDgBPWsnL76HLyVnlvsrvE19". The value of that node (i.e., with a
;     first subscript and no second subscript) is the application global
;     variable name, e.g., "^xyz".
;   - Nodes with second subscripts 0 through 8 have the metadata in nodes (0)
;     through (9) described below with XREFDATA().
;   Nodes of lvn other than those corresponding to reported cross references
;   remain unaltered.
; - gbl is a global variable name. There are three cases:
;   - It is an application global variable name, e.g., "^USPresidents". In lvn,
;     as described above, the function returns all cross references for that
;     global variable.
;   - It starts with "^%ydbAIMD". In lvn, the function returns information about
;     the specified cross reference.
;   - It is omitted or the empty string (""). In lvn, the function returns
;     information about all cross references.
LSXREFDATA(lvn,gbl)
	if "Write:(0=$STACK) ""Error occurred: "",$ZStatus,!"=$etrap new $etrap do etrap
	new xrefvar
	if ""=$get(gbl) do
	. set xrefvar="^%ydbAIM"
	. for  set xrefvar=$order(@xrefvar) quit:xrefvar'?1"^%ydbAIM".E  do lsxrefdata(.lvn,xrefvar)
	else  if gbl'?1"^%ydbAIMD".E do
	. set xrefvar="^%ydbAIM"
	. for  set xrefvar=$order(@xrefvar) quit:xrefvar'?1"^%ydbAIM".E!(""=xrefvar)  do:gbl=@xrefvar lsxrefdata(.lvn,xrefvar)
	else  do lsxrefdata(.lvn,gbl)
	quit

; Remove triggers and cross references for a specified global, or all globals.
; The parameters for UNXREFDATA() mirror those of XREFDATA() to simplify calling
; it to remove triggers and cross references, even though some parameters are
; not required and are therefore ignored.
;
; Usage: DO UNXREFDATA^%YDBAIM(gbl,xsub,sep,pnum,nmonl,zpiece)
; Parameters:
; - gbl is the global variable name, e.g., "^ABC" for which the specified
;   triggers are to be removed. If omitted, all xrefs and triggers for xrefs are
;   removed.
; - xsub is a specification of the subscripts in the cross reference to be
;   removed. There are four cases:
;   - xsub is unspecified or its root node is zero and there is no subtree. In
;     this case, all cross references for the specified global variable are
;     removed. In all three cases following, as the subscript specification is
;     part of the "signature" of a cross reference, the subscript specification
;     of xsub must match that of the trigger being removed.
;   - xsub has a positive integer value at the root, and no subtrees ($DATA() is
;     1): The value specifies the level (number of subscripts) of the global
;     variable for whch the cross reference is to be removed, with all
;     subscripts at each level in the signature of the cross reference. In this
;     case, the actual parameter can be a literal or a variable passed by
;     value. In both the following cases it must be passed by reference.
;   - xsub has no value at the root, but positive integer subscripts (1), (2),
;     (3), etc. ($DATA() is 10): The subscripts of the local variable specify
;     the values of the global variable subscript in the signature cross
;     referenced, using the same syntax as trigvn field of trigger definitions.
;     The last subscript defines the level of the global variable to be cross
;     referenced. Any omitted intervening subscript (e.g., if the local variable
;     has nodes (1) and (3) but not (2)), means that all subscripts at that level
;     are in the cross reference signature.
;   - xsub has both a value at the root, as well as positive integer subscripts
;     (($DATA() is 11): This is similar to the previous case, except that if the
;     number at the root exceeds the last subscript, that is the level of the
;     cross reference signature, with all global variables to be included at
;     levels beyond those of the last local variable subscript. A value at the
;     root smaller than the last subscript is ignored.
;   Other cases (e.g., non integer subscripts of xsub) raise errors.
; - sep is the piece separator for values at that node; if unspecified or the
;   empty string, the cross reference signature is entire node values.
; - pnum exists to allow the parameters of UNXREFDATA() to match those of
;   XREFDATA() and is ignored.
; - nmonly exists to allow the parameters of UNXREFDATA() to match those of
;   XREFDATA() and is ignored.
; - zpiece, if 1 means that $ZPIECE() was used as the piece separator instead
;   of $PIECE(); this is part of the trigger signature.
; - omitfix exists only to allow the parameters of UNXREFDATA() to match those
;   of XREFDATA() and is ignored.
UNXREFDATA(gbl,xsub,sep,pnum,nmonl,zpiece,omitfix)
	if ""=$etrap!("Write:(0=$STACK) ""Error occurred: "",$ZStatus,!"=$etrap) new $etrap do etrap
	new i,nsubs,xrefvar,ztout
	set ztout=$view("ztrigger_output")
	view "ztrigger_output":0
	if ""=$get(gbl) do			; remove all xrefs
	. lock +^%ydbAIMD
	. set xrefvar="^%ydbAIMD"
	. for  set xrefvar=$order(@xrefvar) quit:xrefvar'?1"^%ydbAIMD".AN!(""=xrefvar)  do unxrefdata(xrefvar)
	. lock -^%ydbAIMD
	else  do
	. set nsubs=$get(xsub,0)
	. ; If constraints specified for subscripts, ensure all refer to
	. ; subscripts that are integers in the range 1 through 31
	. do:$data(xsub)\10
	. . set i=$order(xsub(""))
	. . set:1>i!(i\1'=i)!(31<i) $ecode=",U247,"
	. . for  set i=$order(xsub(i)) quit:""=i  set:i\1'=i!(31<i) $ecode=",U247,"
	. . set i=$order(xsub(""),-1)
	. . set:i>nsubs nsubs=i
	. set:31<nsubs $ecode=",U247,"
	. if 'nsubs do		; remove all xrefs for gbl
	. . ; Xrefs are only supported for global variables
	. . set:"^"'=$zextract(gbl,1) $ecode=",U252,"
	. . lock +^%ydbAIMD(gbl)
	. . set xrefvar="^%ydbAIMD"
	. . for  set xrefvar=$order(@xrefvar) quit:xrefvar'?1"^%ydbAIMD".AN!(""=xrefvar)  do:gbl=xrefvar!(gbl=@xrefvar) unxrefdata(xrefvar)
	. . lock -^%ydbAIMD(gbl)
	. else  do unxrefdata($$XREFDATA(gbl,.xsub,$get(sep),,1,$get(zpiece),$get(omitfix,1)))
	view "ztrigger_output":ztout
	quit:$quit "" quit

; Create triggers to maintain cross references and compute cross references
; for a global variable at a specified subscript level. Concurrent execution OK.
;
; Usage: $$XREFDATA^%YDBAIM(gbl,xsub,sep,pnum,nmonly,zpiece,omitfix)
; Parameters:
; - gbl is the global variable name, e.g., "^ABC"
; - xsub is a specification of the subscripts to be cross referenced. There are
;   three cases:
;   - xsub has a positive integer value at the root, and no subtrees ($DATA() is
;     1): The value specifies the level (number of subscripts) of the global
;     variable for whch the cross reference is to be created, with all
;     subscripts at each level to be included in the cross reference. In this
;     case, the actual parameter can be a literal or a variable passed by
;     value. In other cases it must be passed by reference.
;   - xsub has no value at the root, but positive integer subscripts (1), (2),
;     (3), etc. ($DATA() is 10): The subscripts of the local variable specify
;     the values of the global variable subscript to be cross referenced, using
;     the same syntax as trigvn field of trigger definitions. The last subscript
;     defines the level of the global variable to be cross referenced. Any
;     omitted intervening subscript (e.g., if the local variable has nodes (1)
;     and (3) but not (2)), means that all subscripts at that level should be
;     included in the cross reference.
;   - xsub has both a value at the root, as well as positive integer subscripts
;     (($DATA() is 11): This is similar to the previous case, except that if the
;     number at the root exceeds the last subscript, that is the level of the
;     global variable to be cross referenced, with all global variables to be
;     included at levels beyond those of the last local variable subscript. A
;     value at the root smaller than the last subscript is ignored.
;   Other cases (e.g., non integer subscripts of xsub) raise errors.
; - sep is the piece separator for values at that node; if unspecified or the
;   empty string, the cross reference is for entire node values.
; - pnum is a semi-colon separated list of integer piece numbers for which cross
;   references should exist; ignored for xrefs of entire node values effectively
;   a no-op if pieces specified are already cross-referenced
; - nmonly, if 1, means just return the xref global variable name but don't set
;   triggers or compute xrefs
; - zpiece, if 1 means that $ZPIECE() should be used as the piece separator
;   instead of $PIECE(); AIM can have cross references for the same nodes with
;   both options; the cross references are in different global variables
; - omitfix, if 1 instructs XREFDATA() to omit from the subscripts of the cross
;   reference any subscripts of the application global that are fixed constants
;   because the code to traverse the application global using the cross
;   reference will include those known fixed subscripts when making the access.
;   If not specified, omitfix defaults to 1.
;
; Return value: name of global variable with cross reference e.g.,
; "^%ydbAIMDZzUmfwxt80MHPiWLZNtq4". The subscripts of cross reference variables
; are:
; - (pnum,value,sub[,sub]) where pnum is a positive integer for cross references
;   of pieces of nodes; and
; - (value,sub[,sub]) for cross references of entire nodes.
;
; The function is coded so that the function tries as efficiently as possible to
; determine whether the cross reference already exists. This means that it is
; reasonable to just call the function to ensure that the cross reference
; exists, without the caller first checking whether it already exists.  Since
; the function uses $PIECE(), sep is interpreted according to whether the
; process is in M mode or UTF-8 mode.  Cross references are in global variables
; such as ^%ydbAIMDZzUmfwxt80MHPiWLZNtq4, which are derived from a "trigger
; signature" derived from:
; - the name of the global variable being cross referended
; - the specification to match subscripts at each level of the global variable
; - if a piece separator is specified, then also:
;   - the piece separator
;   - if zpiece is specified, then the digit 1
;   - else if zpiece is not specified, $zchset
; The global variable name of the cross reference is derived from by prefixing
; "^%ydbAIMD" to a 128-bit MurMurHash (a non-cryptographic hash with excellent
; statistical properties) rendered into a 22-character alphanumeric string.
; The approach is to first create triggers to maintain cross references, and
; then scan the global variable to generate cross references. This way, any
; global variable changes will have cross references maintained by the triggers,
; and when returning to caller, the cross reference is complete, and will remain
; Consistent thereafter.  ^%ydbAIMD* global variables should be mapped to a
; region with NULL_SUBSCRIPTS set to EXISTING or ALWAYS.
; Just as cross references are metadata for application global variables,
; metadata for the cross reference global variables are stored in the cross
; reference global variables as follows:
; - the root node is the application global variable; subscripted nodes are:
; - (0) space separated $zut, $job, $zyrelease
; - (1) number of subscripts of the application global variable xref'd
; - (2) piece separator, "" for an xref of entire nodes
; - (3) & (4) piece numbers, in the form of a bit-map like string,
;    prefixed with "#" to prevent numeric conversion, e.g., the value for
;    pieces 2, 4 and 5 would be "#01011", "" for an xref of the entire node.
;    (3) identifies the piece numbers for which cross referencing is complete
;    whereas (4) identifies those for which triggers exist. If they are not
;    equal, it means that a process created triggers, but is still working on
;    cross referencing existing global nodes. It is also possible that the
;    process terminated, or was terminated, before completing its work.
; - (5) 1 means $ZPIECE() was used for pieces; the default of "" is $PIECE()
; - (6) SET trigger for this cross reference
; - (7) KILL trigger for this cross reference
; - (8) ZKILL trigger for this cross reference
; - (9) 1 means that omitting fixed subscripts was requested, whether or not
;    any subscripts were actually omitted
; Since the cross references themselves must have at least two subscripts, any
; node with one subscript is metadata for the cross reference.
XREFDATA(gbl,xsub,sep,pnum,nmonly,zpiece,omitfix)
	if ""=$etrap!("Write:(0=$STACK) ""Error occurred: "",$ZStatus,!"=$etrap) new $etrap do etrap
	new i,io,j,lastsub,locxsub,modflag,name,newpnum,newpstr,nsubs,constlist,omitflag,oldpstr,stderr,sub,tmp,trigdel,trigdelx,trigprefix,trigset,trigsub,trigsuffix,z,ztout
	set io=$io
	set stderr="/proc/self/fd/2" open stderr
	zshow "s":tmp do:"%XCMD"=$piece($get(tmp("S",$order(tmp("S",""""),-1))),"^",2) etrap
	; Extended references are not supported
	set:""'=$qsubscript(gbl,-1) $ecode=",U254,"
	; Xrefs are only supported for global variables
	set:"^"'=$zextract(gbl,1) $ecode=",U252,"
	set:gbl?1"^%ydbAIMD".AN $ecode=",U243,"
	; Get number of subcripts to cross reference
	set nsubs=$get(xsub,0)
	; If constraints specified for subscripts, ensure all refer to
	; subscripts that are integers in the range 1 through 31
	do:$data(xsub)\10
	. set i="" for  set i=$order(xsub(i)) quit:""=i  do:$data(xsub(i))#10
	. . set:1>i!(i\1'=i)!(31<i) $ecode=",U247,"
	. . set tmp=xsub(i),locxsub(i)=$select(":"=tmp:"*",1:tmp)
	. . set constlist(i)=$$chktrgspec(.locxsub,i)
	. set i=$order(locxsub(""),-1)
	. set:i>nsubs nsubs=i
	set:1>nsubs $ecode=",U253,"
	set:nsubs\1'=nsubs!(31<nsubs) $ecode=",U247,"
	for i=1:1:nsubs set:'$data(locxsub(i)) locxsub(i)="*"
	; Derive subscript specification for trigger definitions from parameters
	; and build string from which to derive xref variable name
	set omitfix=+$get(omitfix,1),omitflag=0
	set (name,sub,trigsub)=""
	for i=1:1:nsubs do
	. set name=name_locxsub(i)
	. set tmp="sub"_i,trigsub=trigsub_tmp_"="_locxsub(i)_","
	. if 'omitfix!'$get(constlist(i)) set lastsub=tmp,sub=sub_tmp_","
	. else  set omitflag=1		; At least one xref subscript omitted
	; Ensure at least one subscript selected for cross reference global
	set:","'=$zextract(sub,$zlength(sub)) $ecode=",U244,"
	; Remove trailing commas from building subscript lists
	set $zextract(sub,$zlength(sub))=""
	set $zextract(trigsub,$zlength(trigsub))=""
	set sep=$get(sep),tmp=$zlength(sep),zpiece=+$get(zpiece),z=$select(zpiece:"z",1:"")
	set name="^%ydbAIMD"_$zysuffix(gbl_name_$select(tmp:sep_$select(zpiece:1,1:$zchset),1:"")_$select(omitflag:1,1:""))
	quit:+$get(nmonly) name				; Caller only wants name
	if $data(@name)#10 set:gbl'=@name $ecode=",U242,"
	set ztout=$view("ztrigger_output")
	view "ztrigger_output":0
	; Common prefix for all triggers
	set trigprefix="+"_gbl_"("_trigsub_") -command="
	; Put additional trigger options like " -noisolation" here when validated
	set trigsuffix=""
	; As cross referencing does not require exclusivity, multiple processes
	; can invoke XREFDATA() with the same parameters and all will run
	; correctly to completion. The process acquires locks to ensure that
	; XREFDATA() and UNXREFDATA() do not run concurrently. So XREFDATA()
	; uses $JOB as a subscript to the lock resource, whereas UNXREFDATA()
	; does not.
	lock +(^%ydbAIMD($job),^%ydbAIMD(gbl,$job),@name@($job))
	; Determine whether to xref pieces or entire node, and act accordingly
	if $zlength(sep) do				; xref pieces
	. set:'$zlength($get(pnum)) $ecode=",U250,"
	. ; Make any trigger updates needed
	. tstart ()
	. set oldpstr=$get(@name@(4),"#"),modflag=0
	. set newpstr=$$unravel(pnum)
	. for i=2:1:$zlength(newpstr) set:+$zextract(oldpstr,i)'=+$zextract(newpstr,i) ($zextract(newpstr,i),modflag)=1
	. set:modflag&($zlength(oldpstr)>$zlength(newpstr)) $zextract(newpstr,i+1,$zlength(oldpstr))=$zextract(oldpstr,i+1,$zlength(oldpstr))
	. do:modflag
	. . set newpnum=$$ravel(newpstr)
 	. . set trigset=trigprefix_"set -xecute=""for i=1:1:$zlength($ztupdate,"""","""") set p=$piece($ztupdate,"""","""",i) zkill:$data("_name_"(p,$"_z_"piece($ztoldval,$ztdelim,p),"_sub_"))#10 ^("_lastsub_") set:'$data("_name_"(p,$"_z_"piece($ztvalue,$ztdelim,p),"_sub_")) ^("_lastsub_")="""""""""" -pieces="_newpnum_" -"_z_"delim="_$zwrite(sep)_trigsuffix
	. . set trigdel=trigprefix_"kill -xecute=""set p="_name_"(2) for i=2:1:$zlength("_name_"(4)) if +$zextract("_name_"(4),i) set j=i-1 kill:$data("_name_"(j,$"_z_"piece($ztoldval,p,j),"_sub_")) ^("_lastsub_")"""_trigsuffix
	. . set trigdelx=trigprefix_"zkill -xecute=""set p="_name_"(2) for i=2:1:$zlength("_name_"(4)) if +$zextract("_name_"(4),i) set j=i-1 zkill:$data("_name_"(j,$"_z_"piece($ztoldval,p,j),"_sub_"))#10 ^("_lastsub_")"""_trigsuffix
	. . for i=6:1:8 if $zlength($get(@name@(i)))&$ztrigger("item","-"_$zextract(^(i),2,$zlength(^(i))))
	. . set @name=gbl,@name@(4)=newpstr,^(5)=z,^(6)=trigset,^(7)=trigdel,^(8)=trigdelx,^(9)=omitfix
	. . if $ztrigger("item",trigset)&$ztrigger("item",trigdel)&$ztrigger("item",trigdelx)
	. tcommit
	. ; Cross reference existing nodes, if needed. Note that even if this
	. ; process set triggers, another concurrent process might have
	. ; cross referenced the pieces this process wants xref'd.
	. set oldpstr=$get(@name@(3),"#"),modflag=0
	. for i=2:1:$zlength(newpstr) if +$zextract(newpstr,i)&'+$zextract(oldpstr,i) set modflag=1 quit
	. do:modflag
	. . do xrefdata(name,gbl,nsubs,.locxsub,sep,newpstr,zpiece,omitfix,.constlist)
	. . ; Update medatadata
	. . tstart ()
	. . set @name=gbl,@name@(0)=$zut_" "_$job_" "_$zyrelease,^(1)=nsubs,^(2)=sep
	. . set oldpstr=$get(^(3),"#")
	. . for i=2:1:$zlength(newpstr) set $zextract(oldpstr,i)=$select(+$zextract(newpstr,i):1,1:+$zextract(oldpstr,i))
	. . set ^(3)=oldpstr
	. . tcommit
	else  do:'$data(@name@(0))		; No piece sep; xref entire node
	. set trigset=trigprefix_"set -xecute=""zkill:$data("_name_"(0,$ztoldval,"_sub_"))#10 ^("_lastsub_") set:'$data("_name_"(0,$ztvalue,"_sub_")) ^("_lastsub_")="""""""""""_trigsuffix
	. set trigdel=trigprefix_"kill -xecute=""kill:$data("_name_"(0,$ztoldval,"_sub_")) ^("_lastsub_")"""_trigsuffix
	. set trigdelx=trigprefix_"zkill -xecute=""zkill:$data("_name_"(0,$ztoldval,"_sub_"))#10 ^("_lastsub_")"""_trigsuffix
	. tstart ()
	. if $ztrigger("item",trigset)&$ztrigger("item",trigdel)&$ztrigger("item",trigdelx)
	. set @name=gbl,@name@(6)=trigset,^(7)=trigdel,^(8)=trigdelx,^(9)=omitfix
	. tcommit
	. do xrefdata(name,gbl,nsubs,.locxsub,"","","",omitfix,.constlist)
	. ; Add metadata to indicate completion
	. tstart ()
	. set @name@(0)=$zut_" "_$job_" "_$zyrelease,^(1)=nsubs,(^(2),^(3),^(4),^(5))=""
	. tcommit
	; Release locks that block UNXREFDATA()
	lock -(^%ydbAIMD($job),^%ydbAIMD(gbl,$job),@name@($job))
	view "ztrigger_output":ztout
	quit name

; The functions below are intended only to be called internally.

; Checks for whether the single parameter meets one of the following trigger
; specifications supported by Application Independent Metadata:
; - A specific value, e.g., 100.1 or "PATIENT" with * (the default if not
;   specified) to indicate that all subscripts at that level should be matched.
;   A current limitation is that literal strings should not contain semicolons
;   or colons.
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
	. . set lower=$piece(subpiece,":",1),upper=$piece(subpiece,":",2)
	. . ; if upper=lower, this piece is really a constant value
	. . if upper=lower set piecelen=1,$piece(locxsub(n),";",i)=lower
	. . else  set:lower]]upper $ecode=",U246,"
	. else  set okflag=0,$ecode=",U245,"
	quit 1=i&(1=piecelen)

; Output metadata for a specific xref variable.
lsxrefdata:(lvn,xref)
	new i
	set lvn(xref)=@xref
	for i=0:1:9 set lvn(xref,i)=@xref@(i)
	quit

; This label is the inverse of the unravel() function. ravel() takes a bit-map
; like piece number string, e.g., "#0010100111", and composes from it the
; compact form suitable for input to a trigger specification, e.g, "3;5;8:10"
; for the example above.
ravel:(pstr)
	new i,j,newpspec,nextp
	set newpspec=""
	set i=2 for  do  quit:i>$zlength(pstr)
	. if $zextract(pstr,i) do
	. . set nextp=i-1 if $increment(i)
	. . for j=i:1 quit:j>$zlength(pstr)!('$zextract(pstr,j))  set:$increment(i) $zpiece(nextp,":",2)=j-1
	. . set newpspec=newpspec_$select($zlength(newpspec):";",1:"")_nextp
	. else  if $increment(i)
	quit newpspec

; This label takes a piece number specification, e.g., "3;5;8:10" and returns
; the bit-map like piece specification, preceded by "#", e.g., "#0010100111" for
; the example above.  and determines if there are new pieces to xref. Pieces are
; separated by semi-colons. Each piece can be a number, or two colon separated
; numbers, with the first number smaller than the second. Unraveled piece
; numbers are in a bit-map like string, prefixed with "#".
unravel:(pnum)
	new i,newpstr,nextp,nextp1,nextpl,nextplen,nextpu
	set modflag=0,newpstr="#"
	for i=1:1:$zlength(pnum,";") do
	. set nextp=$zpiece(pnum,";",i),nextplen=$zlength(nextp,":")
	. if 1=nextplen do
	. . set:nextp\1'=nextp!(nextp<=0) $ecode=",U248,"
	. . set nextp1=nextp+1
	. . set:nextp1>$zlength(newpstr) newpstr=newpstr_$ztranslate($justify("",nextp1-$zlength(newpstr))," ",0)
	. . set $zextract(newpstr,nextp1)=1
	. else  if 2=nextplen do
	. . set nextpl=$zpiece(nextp,":",1),nextpu=$zpiece(nextp,":",2)
	. . set:nextpl\1'=nextpl!(nextpl<=0!(nextpu\1'=nextpu!(nextpu<=0!(nextpu<=nextpl)))) $ecode=",U248,"
	. . set nextp1=nextpu+1
	. . set:nextp1>$zlength(newpstr) newpstr=newpstr_$ztranslate($justify("",nextp1-$zlength(newpstr))," ",0)
	. . for j=nextpl+1:1:nextp1 set $zextract(newpstr,j)=1
	. else  set $ecode=",U249,"
	quit newpstr

; Given the name of a cross reference global, this function removes the triggers
; used to maintain the xref, and then deletes the cross reference global
; variable.
unxrefdata:(xrefgbl)
	do:$data(@xrefgbl)
	. new i,trig
	. lock +@xrefgbl
	. tstart ()
	. for i=6:1:8 set trig=@xrefgbl@(i) if $ztrigger("item","-"_$zextract(trig,2,$zlength(trig)))
	. kill @xrefgbl
	. tcommit
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
; The following code has some built-in assumptions about the frequencey of
; occurrence of different options, and actual application data might imply
; a different ordering of tests for the various code paths. Fortunately, this
; code is executed only for the initial creation of a cross reference, and not
; in the triggers that maintain cross references as globals are updated.
xrefdata:(name,gblref,nsubs,locxsub,sep,pstr,zpiece,omitfix,constlist)
	new i,j,k,lastsub,nodelen1,nodeval,piece1,piece2,sublist,sublvl,thisrange,thissub,thissubz,tmp,xflag,xref
	set thissub=""
	; If nsubs>1 it means call the function recursively for the next
	; subscript level. Where the specification is to not match all
	; subscripts at this level, each subscript will need to be checked
	; as to whether it should be cross referenced.
	set sublvl=$order(locxsub(""),-1)-nsubs+1
	if nsubs>1 do
	. if "*"=locxsub(sublvl) for  set thissub=$order(@gblref@(thissub)) quit:""=thissub  do:$data(@gblref@(thissub))\10 xrefdata(name,$select($qlength(gblref):$zextract(gblref,1,$zlength(gblref)-1)_",",1:gblref_"(")_$zwrite(thissub)_")",nsubs-1,.locxsub,sep,pstr,zpiece,omitfix,.constlist)
	. else  for  set thissub=$order(@gblref@(thissub)) quit:""=thissub  do
	. . set thissubz=$zwrite(thissub)
	. . if zpiece do
	. . . for i=1:1:$zlength(locxsub(sublvl),";") do
	. . . . set thisrange=$zpiece(locxsub(sublvl),";",i)
	. . . . set piece1=$zpiece(thisrange,":",1)
	. . . . set piece2=$select(1=$zlength(thisrange,":"):piece1,1:$zpiece(thisrange,":",2))
	. . else  do
	. . . for i=1:1:$length(locxsub(sublvl),";") do
	. . . . set thisrange=$piece(locxsub(sublvl),";",i)
	. . . . set piece1=$piece(thisrange,":",1)
	. . . . set piece2=$select(1=$length(thisrange,":"):piece1,1:$piece(thisrange,":",2))
	. . do:thissubz=piece1!(thissubz=piece2!(thissubz]]piece1&(piece2]]thissubz)))&($data(@gblref@(thissub))\10) xrefdata(name,$select($qlength(gblref):$zextract(gblref,1,$zlength(gblref)-1)_",",1:gblref_"(")_thissubz_")",nsubs-1,.locxsub,sep,pstr,zpiece,omitfix,.constlist)
	; if nsubs=1 (this else clause) then cross reference those
	; subscripts that the specification says to cross reference.
	else  for  set thissub=$order(@gblref@(thissub)) quit:""=thissub  do
	. set thissubz=$zwrite(thissub)
	. tstart ()
	. set xflag=0
	. if "*"=locxsub(sublvl) set xflag=1
	. else  do
	. . if zpiece do
	. . . for i=1:1:$zlength(locxsub(sublvl),";") quit:xflag  do
	. . . . set thisrange=$zpiece(locxsub(sublvl),";",i)
	. . . . set piece1=$zpiece(thisrange,":",1)
	. . . . set piece2=$select(1=$zlength(thisrange,":"):piece1,1:$zpiece(thisrange,":",2))
	. . . . set:thissubz=piece1!(thissubz=piece2!(thissubz]]piece1&(piece2]]thissubz))) xflag=1
	. . else  do
	. . . for i=1:1:$length(locxsub(sublvl),";") quit:xflag  do
	. . . . set thisrange=$piece(locxsub(sublvl),";",i)
	. . . . set piece1=$piece(thisrange,":",1)
	. . . . set piece2=$select(1=$length(thisrange,":"):piece1,1:$piece(thisrange,":",2))
	. . . . set:thissubz=piece1!(thissubz=piece2!(thissubz]]piece1&(piece2]]thissubz))) xflag=1
	. do:xflag&($data(@gblref@(thissub))#10)
	. . set nodeval=@gblref@(thissub),sublist=""
	. . for i=1:1:$qlength($reference) do:'omitflag!'$get(constlist(i),0)
	. . . set lastsub=$qsubscript($reference,i)
	. . . set sublist=sublist_","_$zwrite(lastsub)
	. . set:'$zlength(sublist) $ecode=",U244,"
	. . if $zlength(sep) do
	. . . set nodelen1=1+$select(zpiece:$zlength(nodeval,sep),1:$length(nodeval,sep))
	. . . set tmp=$zlength(pstr),k=$select(tmp>nodelen1:nodelen1,1:tmp)
	. . . for i=2:1:k do:+$zextract(pstr,i)
	. . . . set j=i-1
	. . . . set xref=name_"("_j_","_$zwrite($select(zpiece:$zpiece(nodeval,sep,j),1:$piece(nodeval,sep,j)))_sublist_")"
	. . . . set:'$data(@xref)#10 ^(lastsub)=""
	. . else  do
	. . . set xref=name_"(0,"_$zwrite(nodeval)_sublist_")"
	. . . set:'$data(@xref)#10 ^(lastsub)=""
	. tcommit
	quit

;	Error message texts
U242	;"-F-OUTOFDESIGN """_name_""" already used to xref """_@name_""" cannot reuse for """_gbl_""""
U243	;"-F-ALREADYXREF """_gbl_""" is already a cross reference global variable"
U244	;"-F-NEEDSUB Need at least one subscript for cross reference"
U245	;"-F-INVPIECE Range """_trgpiece_""" has invalid number of pieces: "_piecelen
U246	;"-F-INVRANGE upper """_upper_""" is less than lower """_lower_""" in range specification"
U247	;"-F-INVSUB Subscript """_$get(i,nsubs)_""" is not an integer 1 through 31"
U248	;"-F-INVPIECE Piece """_nextp_""" is invalid piece specification"
U249	;"-F-INVPSEPLEN Piece number "_i_", "_nextp_" has "_nextplen_" "":"" separated pieces, invalid"
U250	;"-F-NOPIECE Piece separator """_sep_""" specified, but no piece numbers"
U251	;"-F-UNKNERR """_tmp1_""" is not a recognized error code"
U252	;"-F-NOTAGBL Variable """_gbl_""" is not a global variable"
U253	;"-F-NOSUBS Need at least 1 subscript to cross reference; nsubs="_nsubs
U254	;"-F-NOEXTREF Extended reference in "_var_" is not supported"
U255	;"-F-BADINVOCATION Top level invocation of "_$text(+0)_" not supported; must invoke a label"
