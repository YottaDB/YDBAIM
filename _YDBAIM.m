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
; Application Independent Metadata (AIM)
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
;     ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(3,"Adams",1797,1801)="",
;     ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(3,"Adams",1835,1839)="", and many others
;     including ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(3,"Obama",2009,2017)="".
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
;     ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(1,"John",1797,1801)=""
;     ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(1,"John",1835,1839)=""
;     ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(2,"",1797,1801)=""
;     ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(2,"Quincy",1835,1839)=""
;     ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(3,"Adams",1797,1801)=""
;     ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(3,"Adams",1835,1839)=""
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
; Statistics
;
;   The optional parameter stat can be used to instruct AIM that the application
;   wishes to compute and maintain statistics. For example, a call to
;   $$XREFDATA^%YDBAIM("^USPresidents",2,"|","1:3",,,,2) would compute not only
;   the node ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(1,"John",1825,1829)="" for John
;   Quincy Adams, but also the node ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(1,"John")=4
;   indicating that "John" appears 4 times as the first piece, and also the node
;   ^%ydbAIMDf1x7fSMuGT4HtAEXAx0g65(11)=135 indicating that the metadata for
;   ^USPresidents has 135 (45*3) global variable nodes (as of today, that sample
;   datatset cannot include Joe Biden as he is still in office, and the
;   subscripts of ^USPresidents in the example are the years the President took
;   and relinquished the office).
;     - stat=0 - no statistics
;     - stat=1 - stat
;
; Note that $data(@name@(SUB))#10 constructs where SUB is an integer are found
; throughout the code because these root nodes are metadata about the metadata
; and subtrees then contain the stored metadata of application globals.
;
; Number after the %YDBAIM label is a version number of the metadata format.
%YDBAIM;1
	; Top level entry not supported
	do etrap		; set error trap
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
;   $ETRAP) that will be used.
; - Capture existing locks in currlck so that the error trap can release
;   locks acquired by %YDBAIM. In non-error code paths, acquired locks are
;   released without the need to refer to captured locks.
etrap
	set $etrap="set $etrap=""open """"/proc/self/fd/2"""" use """"/proc/self/fd/2"""" write $zstatus,! zshow """"*"""" zhalt $piece($zstatus,"""","""",1)""  goto err^"_$text(+0)
	quit

err	; Error handler
	new errcode,errtxt,i,io,stderr,tmp1,tmp2,usestderr
	set io=$io
	set usestderr=1
	zshow "d":tmp1
	set tmp2="" for  set tmp2=$order(tmp1("D",tmp2)) quit:'usestderr!'$zlength(tmp2)  set:tmp1("D",tmp2)?.E1" TERMINAL ".E usestderr=0
	trollback:$tlevel&$data(tlevel) tlevel
	set errcode=$zpiece($ecode,",",2),errtxt=$text(@errcode)
	set stderr="/proc/self/fd/2"
	open stderr
	use:usestderr stderr
	if $zlength(errtxt) write $text(+0),@$zpiece(errtxt,";",2,$zlength(errtxt,";")),!
	else  for i=1:1:$length($zstatus,"%YDB-") write "%YDB-",$piece($zstatus,"%YDB-",i),!
	set $ecode=""
	zshow "s":tmp1
	view:$data(ztout) "ztrigger_output":ztout
	use io
	do unsnaplck(.currlck)
	zhalt:"%XCMD"=$piece($get(tmp1("S",$order(tmp1("S",""""),-1))),"^",2) +$extract(errcode,2,$zlength(errcode))
	do etrap
	zgoto 1

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
;   - Nodes with positive integer second subscripts have metadata about the
;     metadata. These are described below with XREFDATA().
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
	new currlck,tlevel,xrefvar
	set tlevel=$tlevel
	do snaplck(.currlck)
	if '$zlength($get(gbl)) do
	. set gbl="" for  set gbl=$order(^%ydbAIMDxref(gbl)) quit:'$zlength(gbl)  do
	. . set xrefvar="" for  set xrefvar=$order(^%ydbAIMDxref(gbl,xrefvar)) quit:'$zlength(xrefvar)  do lsxrefdata(.lvn,xrefvar)
	else  if gbl'?1"^%ydbAIMD".E do
	. set xrefvar="" for  set xrefvar=$order(^%ydbAIMDxref(gbl,xrefvar)) quit:'$zlength(xrefvar)  do lsxrefdata(.lvn,xrefvar)
	else  do lsxrefdata(.lvn,gbl)
	quit

; Remove triggers and cross references for a specified global, or all globals.
; The parameters for UNXREFDATA() mirror those of XREFDATA() to simplify calling
; it to remove triggers and cross references, even though some parameters are
; not required and are therefore ignored.
;
; Usage: DO UNXREFDATA^%YDBAIM(gbl,xsub,sep,pnum,nmonly,zpiece)
; Quick summary:
;  - UNXREFDATA() deletes all metadata
;  - UNXREFDATA(gbl) where gbl is an application global name deletes all AIM
;    metadata for that application global.
;  - UNXREFDATA(aimgbl) where aimgbl is an AIM metadata global variable remove
;    that metadata
; Triggers associated with maintaining any requested metadata are removed when
; the metadata is removed.
;
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
;     (($DATA() is 11): This is similar to the previous case, except that
;     should the number at the root exceed the last subscript, the value at the
;     root is the level of the cross reference signature, with all global
;     variables to be included at levels beyond those of the last local
;     variable subscript. A value at the root smaller than the last subscript
;     is ignored.
;   Other cases (e.g., non integer subscripts of xsub) raise errors.
; - sep is the piece separator for values at that node; if unspecified or the
;   empty string, the cross reference signature is entire node values.
; - pnum exists to allow the parameters of UNXREFDATA() to match those of
;   XREFDATA() and is ignored. Note that it is not possible to remove the cross
;   reference of one piece of a node.
; - nmonly exists to allow the parameters of UNXREFDATA() to match those of
;   XREFDATA() and is ignored.
; - zpiece, if 1 means that $ZPIECE() was used as the piece separator instead
;   of $PIECE(); this is part of the trigger signature.
; - omitfix exists only to allow the parameters of UNXREFDATA() to match those
;   of XREFDATA() and is ignored.
; - stat exists only to allow the parameters of UNXREFDATA() to match and is
;   ignored
UNXREFDATA(gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat)
	if '$zlength($etrap)!("Write:(0=$STACK) ""Error occurred: "",$ZStatus,!"=$etrap) new $etrap do etrap
	new currlck,i,nsubs,tlevel,xrefvar
	set tlevel=$tlevel
	do snaplck(.currlck)
	set gbl=$get(gbl)
	if gbl?1"^%ydbAIMD".22AN,$data(@gbl) set xrefvar=gbl,gbl=@xrefvar do unxrefdata(xrefvar)
	else  if '$zlength(gbl) do			; remove all xrefs
	. lock +^%ydbAIMD
	. set gbl="" for  set gbl=$order(^%ydbAIMDxref(gbl)) quit:'$zlength(gbl)  do
	. . set xrefvar="" for  set xrefvar=$order(^%ydbAIMDxref(gbl,xrefvar)) quit:'$zlength(xrefvar)  do unxrefdata(xrefvar)
	. lock -^%ydbAIMD
	else  do
	. ; Xrefs are only supported for global variables
	. set:(gbl'?1"^"1(1"%",1AN).AN)!(32<$zlength(gbl)) $ecode=",U252,"
	. set nsubs=$get(xsub,0)
	. ; If constraints specified for subscripts, ensure all refer to
	. ; subscripts that are integers in the range 1 through 31
	. do:$data(xsub)\10
	. . set i=$order(xsub(""))
	. . set:1>i!(i\1'=i)!(31<i) $ecode=",U247,"
	. . for  set i=$order(xsub(i)) quit:'$zlength(i)  set:i\1'=i!(31<i) $ecode=",U247,"
	. . set i=$order(xsub(""),-1)
	. . set:i>nsubs nsubs=i
	. set:31<nsubs $ecode=",U247,"
	. if 'nsubs do		; remove all xrefs for gbl
	. . lock +^%ydbAIMD(gbl)
	. . set xrefvar="" for  set xrefvar=$order(^%ydbAIMDxref(gbl,xrefvar)) quit:'$zlength(xrefvar)  do unxrefdata(xrefvar)
	. . lock -^%ydbAIMD(gbl)
	. else  do unxrefdata($$XREFDATA(gbl,.xsub,$get(sep),,1,$get(zpiece),$get(omitfix,1)))
	quit:$quit "" quit

; Create triggers to maintain cross references and compute cross references
; for a global variable at a specified subscript level. Concurrent execution OK.
;
; Usage: $$XREFDATA^%YDBAIM(gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat)
; Parameters:
; - gbl is the global variable name, e.g., "^ABC"
; - xsub is a specification of the subscripts to be cross referenced. There are
;   three cases:
;   - xsub has a positive integer value at the root, and no subtrees (i.e.,
;     $DATA() is 1): The value specifies the level (number of subscripts) of
;     the global variable for whch the cross reference is to be created, with
;     all subscripts at each level to be included in the cross reference. In
;     this case, the actual parameter can be a literal or a variable passed by
;     value. In other cases it must be passed by reference.
;   - xsub has no value at the root, but positive integer subscripts (1), (2),
;     (3), etc. (i.e., $DATA() is 10): The subscripts of the local variable
;     specify the values of the global variable subscript to be cross
;     referenced, using the same syntax as trigvn field of trigger
;     definitions. The last subscript defines the level of the global variable
;     to be cross referenced. Any omitted intervening subscript (e.g., if the
;     local variable has nodes (1) and (3) but not (2)), means that all
;     subscripts at that level should be included in the cross reference.
;   - xsub has both a value at the root, as well as positive integer subscripts
;     (i.e., ($DATA() is 11): This is similar to the previous case, except that
;     if the value at the root exceeds the last subscript, that is the level of
;     the global variable to be cross referenced. For example, if the local
;     variable has nodes (1) and (3) but the value at the root is 5, five
;     subscripts of the global variable will be cross referenced. A value at
;     the root smaller than the last subscript is ignored, so with the
;     subscripts above and a value of 2 at the root, three subscripts will be
;     cross referenced.
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
; - stat if 1 or 2 says the metadata should include statistics, as described
;   above under "Statistics".
;
; Return value: name of global variable with cross reference e.g.,
; "^%ydbAIMDZzUmfwxt80MHPiWLZNtq4". The subscripts of cross reference variables
; are:
; - (pnum,value,sub[,sub]) where pnum is a positive integer for cross references
;   of pieces of nodes; and
; - (0,value,sub[,sub]) for cross references of entire nodes.
;
; The function is coded so that the function tries as efficiently as possible to
; determine whether the cross reference already exists. This means that it is
; reasonable to just call the function to ensure that the cross reference
; exists, without the caller first checking whether it already exists.  Since
; the function uses $PIECE(), sep is interpreted according to whether the
; process is in M mode or UTF-8 mode.  Cross references are in global variables
; such as ^%ydbAIMDZzUmfwxt80MHPiWLZNtq4, which are derived from a "trigger
; signature" derived from:
; - the name of the global variable being cross referenced
; - the specification to match subscripts at each level of the global variable
; - if a piece separator is specified, then also:
;   - the piece separator
;   - Whether a piece separator is ASCII or not (digit 0 if yes)
;   - if zpiece is specified, then the digit 1
;   - if the separator is not ASCII and zpiece is not specified, $zchset
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
; - the root node is the application global variable name; subscripted nodes are:
; - (0) space separated $zut, $job, $zyrelease, metadata format version number
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
; - (10) if 1 or 2 means that statistics are maintained, as specified by the
;    stat parameter
; - (11) if stat is 2, this contains the total number of nodes being tracked
; Since the cross references themselves must have at least two subscripts, any
; node with one subscript is metadata for the cross reference.
;
; Nodes of ^%ydbAIMDxref(gbl,aimgbl) are metadata on metadata, where gbl is an
; application global and aimgbl is the AIM global.
XREFDATA(gbl,xsub,sep,pnum,nmonly,zpiece,omitfix,stat)
	if '$zlength($etrap)!("Write:(0=$STACK) ""Error occurred: "",$ZStatus,!"=$etrap) new $etrap do etrap
	new asciisep,currlck,gblind,i,io,j,lastsub,lastsubind,lastvarsub,lf,locxsub,modflag,name,nameind,newpnum,newpstr,pieces
	new nsubs,nullsub,constlist,omitflag,oldpstr,stderr,sub,subary,suffix,tlevel,tmp,totcntind,trigdel,trigdelx,trigprefix
	new trigset,trigsub,valcntind,xrefind,z,zlsep,ztout
	set tlevel=$tlevel	; tlevel is required by error trap to rollback
	do snaplck(.currlck)
	set io=$io
	set stderr="/proc/self/fd/2" open stderr
	set lf=$char(10)
	zshow "s":tmp do:"%XCMD"=$piece($get(tmp("S",$order(tmp("S",""""),-1))),"^",2) etrap
	set:'$data(gbl) $ecode=",U252,"
	; Extended references are not supported
	set:""'=$qsubscript(gbl,-1) $ecode=",U254,"
	; Xrefs are only supported for global variables other than AIM global variables
	set:gbl'?1"^"1(1"%",1AN).AN!(32<$zlength(gbl)) $ecode=",U252,"
	set:gbl?1"^%ydbAIMD".AN $ecode=",U243,"
	; Get number of subcripts to cross reference
	set nsubs=$get(xsub,0)
	; If constraints specified for subscripts, ensure all refer to
	; subscripts that are integers in the range 1 through 31
	do:$data(xsub)\10
	. set i="" for  set i=$order(xsub(i)) quit:'$zlength(i)  do:$data(xsub(i))#10
	. . set:1>i!(i\1'=i)!(31<i) $ecode=",U247,"
	. . set tmp=xsub(i),locxsub(i)=$select(":"=tmp:"*",1:tmp)
	. . set constlist(i)=$$chktrgspec(.locxsub,i)
	. set i=$order(locxsub(""),-1)
	. set:i>nsubs nsubs=i
	set:1>nsubs $ecode=",U253,"
	set:nsubs\1'=nsubs!(31<nsubs) $ecode=",U247,"
	for i=1:1:nsubs set:'$data(locxsub(i)) locxsub(i)="*",constlist(i)=0
	; Derive subscript specification for trigger definitions from parameters
	; and build string from which to derive xref variable name
	set omitfix=+$get(omitfix,1),omitflag=0
	set (name,sub,trigsub)=""
	for i=1:1:nsubs do
	. set name=name_locxsub(i)
	. set tmp="sub"_i,trigsub=trigsub_tmp_"="_locxsub(i)_","
	. if omitfix&constlist(i) set omitflag=1
	. else  set lastsub=tmp,sub=sub_tmp_","
	; Ensure at least one subscript selected for cross reference global
	; Remove trailing commas from building subscript lists
	set $zextract(sub,$zlength(sub))=""
	set $zextract(trigsub,$zlength(trigsub))=""
	set sep=$get(sep),zlsep=$zlength(sep),zpiece=+$get(zpiece),z=$select(zpiece:"z",1:"")
	set asciisep=1
	for i=1:1:zlsep set:$zascii($zextract(sep,i))>127 asciisep=0 quit:'asciisep
	set suffix=$zysuffix(gbl_name_$select(zlsep:sep_$select(asciisep:0,zpiece:1,1:$zchset),1:"")_$select(omitflag:1,1:""))
	set name="^%ydbAIMD"_suffix
	; Flagging this error needs to be deferred as the error handler may
	; need name to be set.
	set:'$data(lastsub) $ecode=",U244,"
	; Quit if caller only wants the variable name. Note that asking for a
	; name requires that XREFDATA() be called as a function that returns
	; a value, as calling it as a routine asking for a name is a
	; meaningless operation that is likely an application program bug.
	quit:+$get(nmonly) name
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
	. . ; $data()#10 needed as nodes can have subtrees which are cross references of application globals
	. . for i=6:1:8 if $data(@name@(i))#10 set tmp=^(i) if $ztrigger("item","-"_$zextract(tmp,2,$zlength(tmp)))
	. . if 'stat do
	. . . set trigset=trigprefix_"set -name=%ydb"_suffix_"S -xecute="_$$exptempl("ttSp0")
	. . . set trigdel=trigprefix_"kill -name=%ydb"_suffix_"K -xecute="_$$exptempl("ttKp0")
	. . . set trigdelx=trigprefix_"zkill -name=%ydb"_suffix_"Z -xecute="_$$exptempl("ttZKp0")
	. . else  if 1=stat do
	. . . set trigset=trigprefix_"set -name=%ydb"_suffix_"S -xecute="_$$exptempl("ttSp1")
	. . . set trigdel=trigprefix_"kill -xecute="_$$exptempl("ttKp1")
	. . . set trigdelx=trigprefix_"zkill -xecute="_$$exptempl("ttZKp1")
	. . else  if 2=stat do
	. . . set trigset=trigprefix_"set -name=%ydb"_suffix_"S -xecute="_$$exptempl("ttSp2")
	. . . set trigdel=trigprefix_"kill -name=%ydb"_suffix_"K -xecute="_$$exptempl("ttKp2")
	. . . set trigdelx=trigprefix_"zkill -name="_suffix_"Z -xecute="_$$exptempl("ttZKp2")
	. . else  set $ecode=",U241,"
	. . set @name=gbl,@name@(4)=newpstr,^(5)=z,^(6)=trigset,^(7)=trigdel,^(8)=trigdelx,^(9)=omitfix,^(10)=stat
	. . set:'($ztrigger("item",trigset)&$ztrigger("item",trigdel)&$ztrigger("item",trigdelx)) $ecode=",U239,"
	. . set ^%ydbAIMDxref(gbl,name)=""
	. tcommit
	. view "ztrigger_output":ztout
	. ; Cross reference existing nodes, if needed. Note that even if this
	. ; process set triggers, another concurrent process might have
	. ; cross referenced the pieces this process wants xref'd.
	. set oldpstr=$get(@name@(3),"#"),modflag=0
	. for i=2:1:$zlength(newpstr) if +$zextract(newpstr,i)&'+$zextract(oldpstr,i) set modflag=1 quit
	. do:modflag
	. . do xrefdata(nsubs)
	. . ; Update metadata to indicate completion
	. . tstart ():transactionid="batch"
	. . set @name=gbl,@name@(0)=$zut_" "_$job_" "_$zyrelease_" "_$zpiece($text(%YDBAIM),";",2),^(1)=nsubs,^(2)=sep
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
	. do:'$data(@name@(0))#10
	. . for i=6:1:8 if $data(@name@(i))#10 set tmp=^(i) if $ztrigger("item","-"_$zextract(tmp,2,$zlength(tmp)))
	. . if 'stat do
	. . . set trigset=trigprefix_"set -xecute="_$$exptempl("ttSe0")
	. . . set trigdel=trigprefix_"kill -xecute="_$$exptempl("ttKe0")
	. . . set trigdelx=trigprefix_"zkill -xecute="_$$exptempl("ttZKe0")
	. . else  if 1=stat do
	. . . set trigset=trigprefix_"set -xecute="_$$exptempl("ttSe1")
	. . . set trigdel=trigprefix_"kill -xecute="_$$exptempl("ttKe1")
	. . . set trigdelx=trigprefix_"zkill -xecute="_$$exptempl("ttZKe1")
	. . else  if 2=stat do
	. . . set trigset=trigprefix_"set -xecute="_$$exptempl("ttSe2")
	. . . set trigdel=trigprefix_"kill -xecute="_$$exptempl("ttKe2")
	. . . set trigdelx=trigprefix_"zkill -xecute="_$$exptempl("ttZKe2")
	. . else  set $ecode=",U241,"
	. . set:'($ztrigger("item",trigset)&$ztrigger("item",trigdel)&$ztrigger("item",trigdelx)) $ecode=",U239,"
	. . set @name=gbl,@name@(6)=trigset,^(7)=trigdel,^(8)=trigdelx,^(9)=omitfix,^(10)=stat
	. . set ^%ydbAIMDxref(gbl,name)=""
	. tcommit
	. view "ztrigger_output":ztout
	. set newpstr="" do xrefdata(nsubs)
	. ; Add metadata to indicate completion
	. tstart ():transactionid="batch"
	. set @name@(0)=$zut_" "_$job_" "_$zyrelease_" "_$zpiece($text(%YDBAIM),";",2),^(1)=nsubs,(^(2),^(3),^(4),^(5))=""
	. tcommit
	; Release locks that block UNXREFDATA()
	lock -(^%ydbAIMD($job),^%ydbAIMD(gbl,$job),@name@($job))
	quit:$quit name quit

; The functions below are intended only to be called internally.

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

; Expand trigger template into -xecute string for trigger Note that this relies
; on the (per standard) behavior of $text() to report empty source code lines
; as a single space.
; Note: exptempl() uses the line just before the label to get list of variables
; to replace.  A variable name ending in $ means $ZWRITE() its value when
; expanding.
; Uses local variables from XREFDATA(): lastsub,name,pieces,sep$,sub,z
exptempl:(lab)
	new i,j,len,line,multiline,outstr,rep,str,tmp,var,vars,zflag
	set tmp=$text(@lab),len=$zlength(tmp,";"),line=$zpiece(tmp,";",2,len)
	set str=line
	for i=1:1 set tmp=$text(@lab+i) quit:" "=tmp  set len=$zlength(tmp,";"),line=$zpiece(tmp,";",2,len),str=str_$char(10,9)_line
	set multiline=i-1
	set vars=$zpiece($text(exptempl+-1),": ",2)
	for i=1:1:$zlength(vars,",") set tmp=$zpiece(vars,",",i),var=$zpiece(tmp,"$",1),zflag=$zlength(tmp,"$")-1 do:$data(@var)
	. set rep="@"_var,len=$zlength(str,rep),outstr=$zpiece(str,rep,1)
	. for j=2:1:len do
	. . set outstr=outstr_$select(zflag:$zwrite(@var),1:@var)_$zpiece(str,rep,j)
	. set str=outstr
	quit $select(multiline:"<<"_$char(10,9)_outstr_$c(10),1:$zwrite(outstr))

; Output metadata for a specific xref variable.
lsxrefdata:(lvn,xref)
	tstart ():transactionid="batch"
	do:$data(@xref)
	. new s
	. set lvn(xref)=@xref
	. set s="" for  set:$data(@xref@(s))#10 lvn(xref,s)=^(s) set s=$order(^(s)) quit:'$zlength(s)
	tcommit
	quit

; Create indirection strings to be used by xrefdata()
; Uses or references local variables passed to or defined in XREFDATA():
;   constlist, gbl, name, lastvarsub, nameind, nsubs, omitflags, sep, subary,
;   totcntind, valcntind, xrefind
mkindxrefdata:
	new i,tmp
	set gblind(1)=gbl_"("_$select(constlist(1):locxsub(1),1:"subary(1)")
	for i=2:1:nsubs set gblind(i)=gblind(i-1)_","_$select(constlist(i):locxsub(i),1:"subary("_i_")")
	for i=1:1:nsubs set gblind(i)=gblind(i)_")"
	set xrefind=name_"("_$select($zlength(sep):"k,pieceval",1:"0,nodeval")
	for i=1:1:nsubs do
	. if constlist(i) do
	. . if 'omitfix set cflag=1,lastvarsub=i,lastsubind=locxsub(i),xrefind=xrefind_","_lastsubind
	. else  set lastvarsub=i,lastsubind="subary("_i_")",xrefind=xrefind_","_lastsubind
	set xrefind=xrefind_")"
	if $zlength(sep) set nameind=name_"(-k,pieceval)",valcntind=name_"(-k)"
	else  set nameind=name_"("""",nodeval)",valcntind=name_"("""")"
	set totcntind=name_"(11)"
	quit

; ravel() takes a bit-map like piece number string, e.g., "#0010100111", and
; composes from it the form sultable for the index values of a for loop,
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
unxrefdata:(xrefgbl)
	do:$data(@xrefgbl)
	. new i,trig,ztout
	. ; The M lock protects against concurrent XREFDATA() and UNXREFDATA()
	. ; and the transaction ensures Consistency of the metadata about the
	. ; metadata.
	. lock +@xrefgbl
	. set ztout=$view("ztrigger_output")
	. view "ztrigger_output":0
	. tstart ():transactionid="batch"
	. for i=6:1:8 if $data(@xrefgbl@(i))#10 set trig=^(i) if $ztrigger("item","-"_$zextract(trig,2,$zlength(trig)))
	. kill @xrefgbl
	. zkill ^%ydbAIMDxref(gbl,xrefgbl)
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
; Uses or references variables defined in XREFDATA():
;   constlist, gbl, lastvarsub, locxsub, name, nameind, newpstr, nullsub,
;   omitfix, stat, totcntind, valcntind, xref, zpiece
xrefdata:(nsubs)
	new flag,i,j,k,nodelen1,nodeval,nranges,piece1,piece2,pieceval,rangeend,rangeflag,sublvl,thisrange,tmp
	; If nsubs>1 it means call the function recursively for the next
	; subscript level.
	set flag=nullsub
	set sublvl=$order(locxsub(""),-1)-nsubs+1
	set subary(sublvl)=""
	if nsubs>1 do
	. if "*"=locxsub(sublvl) for  do:flag  set flag=1,subary(sublvl)=$order(@gblind(sublvl)) quit:'$zlength(subary(sublvl))
	. . do:$data(@gblind(sublvl))\10 xrefdata(nsubs-1)
	. else  if $get(constlist(sublvl),0) do:$data(@gblind(sublvl))\10 xrefdata(nsubs-1) quit
	. else  do
	. . set nranges=$select(zpiece:$zlength(locxsub(sublvl),";"),1:$length(locxsub(sublvl),";"))
	. . for i=1:1:nranges do
	. . . if zpiece do
	. . . . set thisrange=$zpiece(locxsub(sublvl),";",i)
	. . . . set piece1=$zpiece(thisrange,":",1),piece2=$zpiece(thisrange,":",$zlength(thisrange,":"))
	. . . else  do
	. . . . set thisrange=$piece(locxsub(sublvl),";",i)
	. . . . set piece1=$piece(thisrange,":",1),piece2=$piece(thisrange,":",$length(thisrange,":"))
	. . . if $zlength(piece1) set subary(sublvl)=$zwrite(piece1,1),flag=1
	. . . else  set subary(sublvl)="",flag=nullsub
	. . . set rangeend=$select($zlength(piece2):$zwrite(piece2,1),1:""),rangeflag=$zlength(rangeend)
	. . . for  do:flag  set flag=1,(subary(sublvl),tmp)=$order(@gblind(sublvl)) quit:'$zlength(tmp)!(rangeflag&(tmp]]rangeend))
	. . . . do:$data(@gblind(sublvl))\10 xrefdata(nsubs-1)
	; if nsubs=1 (the else command below) then cross reference those
	; subscripts that the specification says to cross reference.  do with
	; postconditional to cross reference "" subscripts.
	else  if constlist(sublvl) do
	. tstart ():transactionid="batch"
	. set nodeval=$get(@gblind(sublvl))
	. if $zlength(sep) do
	. . set nodelen1=$zlength(newpstr)
	. . for i=2:1:nodelen1 do:+$zextract(newpstr,i)
	. . . set k=i-1,pieceval=$select(zpiece:$zpiece(nodeval,sep,k),1:$piece(nodeval,sep,k))
	. . . do:'$data(@xrefind)
	. . . . set ^($select(lastvarsub=sublvl:lastsubind,1:@lastsubind))=""
	. . . . if stat,$increment(@nameind),(2=stat),$increment(@totcntind),1=@nameind,$increment(@valcntind)
	. else  do:'$data(@xrefind)
	. . set ^($select(lastvarsub=sublvl:lastsubind,1:@lastsubind))=""
	. . if stat,$increment(@nameind),(2=stat),$increment(@totcntind),1=@nameind,$increment(@valcntind)
	. tcommit
	else  do
	. set nranges=$select(zpiece:$zlength(locxsub(sublvl),";"),1:$length(locxsub(sublvl),";"))
	. for i=1:1:nranges do
	. . if "*"=locxsub(sublvl) do
	. . . set (piece1,piece2)="",flag=nullsub
	. . else  do
	. . . if zpiece do
	. . . . set thisrange=$zpiece(locxsub(sublvl),";",i)
	. . . . set piece1=$zpiece(thisrange,":",1),piece2=$zpiece(thisrange,":",$zlength(thisrange,":"))
	. . . else  do
	. . . . set thisrange=$piece(locxsub(sublvl),";",i)
	. . . . set piece1=$piece(thisrange,":",1),piece2=$piece(thisrange,":",$length(thisrange,":"))
	. . . if $zlength(piece1) set subary(sublvl)=$zwrite(piece1,1),flag=1
	. . . else  set subary(sublvl)="",flag=nullsub
	. . set rangeend=$select($zlength(piece2):$zwrite(piece2,1),1:""),rangeflag=$zlength(rangeend)
	. . for  do:flag  set flag=1,(subary(sublvl),tmp)=$order(@gblind(sublvl)) quit:'$zlength(tmp)!(rangeflag&(tmp]]rangeend))
	. . . tstart ():transactionid="batch"
	. . . do:$data(@gblind(sublvl))#10
	. . . . set nodeval=@gblind(sublvl)
	. . . . if $zlength(sep) do
	. . . . . set nodelen1=$zlength(newpstr)
	. . . . . for j=2:1:nodelen1 do:+$zextract(newpstr,j)
	. . . . . . set k=j-1,pieceval=$select(zpiece:$zpiece(nodeval,sep,k),1:$piece(nodeval,sep,k))
	. . . . . . if '$data(@xrefind)#10 set ^(@lastsubind)="" if stat,$increment(@nameind),(2=stat),$increment(@totcntind),1=@nameind,$increment(@valcntind)
	. . . . else  if '$data(@xrefind)#10 set ^(@lastsubind)="" if stat,$increment(@nameind),(2=stat),$increment(@totcntind),1=@nameind,$increment(@valcntind)
	. . . tcommit
	quit

; Templates for triggers
; The labeling convention is as follows:
; - tt for trigger template
; - trigger command: S, ZK, K
; - e for entire node, p for pieces of node
; - statistics level: 0, 1 or 2
; Each template must end in a blank line so that exptempl() knows the end of a template
ttSe0	;zkill @name(0,$ztoldval,@sub) set @name(0,$ztvalue,@sub)=""

ttKe0	;kill @name(0,$ztoldval,@sub)

ttZKe0	;zkill @name(0,$ztoldval,@sub)

ttSe1	;if $data(@name(0,$ztoldval,@sub))#10 zkill ^(@lastsub) zkill:1>$increment(@name("",$ztoldval),-1) ^($ztoldval)
	;if '$data(@name(0,$ztvalue,@sub)) set ^(@lastsub)="" if $increment(@name("",$ztvalue))

ttKe1	;if $data(@name(0,$ztoldval,@sub)) kill ^(@lastsub) zkill:1>$increment(@name("",$ztoldval),-1) ^($ztoldval)

ttZKe1	;if $data(@name(0,$ztoldval,@sub))#10 zkill ^(@lastsub) zkill:1>$increment(@name("",$ztoldval),-1) ^($ztoldval)

ttSe2	;if $data(@name(0,$ztoldval,@sub))#10 zkill ^(@lastsub) zkill:1>$increment(@name(11),-1) ^(11) if 1>$increment(@name("",$ztoldval),-1) zkill ^($ztoldval) zkill:1>$increment(@name(""),-1) ^("")
	;if '$data(@name(0,$ztvalue,@sub)) set ^(@lastsub)="" if $increment(@name(11)),(1=$increment(@name("",$ztvalue))),$increment(@name(""))

ttKe2	;if $data(@name(0,$ztoldval,@sub)) kill ^(@lastsub) zkill:1>$increment(@name(11),-1) ^(11) if 1>$increment(@name("",$ztoldval),-1) zkill ^($ztoldval) zkill:1>$increment(@name(""),-1) ^("")

ttZKe2	;if $data(@name(0,$ztoldval,@sub))#10 zkill ^(@lastsub) zkill:1>$increment(@name(11),-1) ^(11) if 1>$increment(@name("",$ztoldval),-1) zkill ^($ztoldval) if 1>$increment(@name(""),-1) zkill ^("")

ttSp0	;for i=@pieces set p=$@zpiece($ztoldval,@sep,i),q=$@zpiece($ztvalue,@sep,i) do
	;. if p'=q zkill @name(i,p,@sub) set @name(i,q,@sub)=""
	;. else  set:'$zlength(q) @name(i,"",@sub)=""

ttKp0	;set p=@name(2),b=^(4),q=$zlength(b) for i=2:1:q if +$zextract(b,i) set j=i-1 kill @name(j,$@zpiece($ztoldval,p,j),@sub)

ttZKp0	;set p=@name(2),b=^(4),q=$zlength(b) for i=2:1:q if +$zextract(b,i) set j=i-1 zkill @name(j,$@zpiece($ztoldval,p,j),@sub)

ttSp1	;for i=@pieces set p=$@zpiece($ztoldval,@sep,i),q=$@zpiece($ztvalue,@sep,i),j=-i do
	;. if p'=q do
	;. . if $data(@name(i,p,@sub))#10 zkill ^(@lastsub) zkill:1>$increment(@name(j,p),-1) ^(p)
	;. . if '($data(@name(i,q,@sub))#10) set ^(@lastsub)="" if $increment(@name(j,q))
	;. else  if '$zlength(q),'($data(@name(i,"",@sub))#10) set ^(@lastsub)="" if $increment(@name(j,""))

ttKp1	;set p=@name(2),b=^(4),q=$zlength(b) for i=2:1:q if +$zextract(b,i) set j=i-1,k=-j,x=$@zpiece($ztoldval,p,j) kill @name(j,x,@sub) zkill:1>$increment(@name(k,x),-1) ^(x)

ttZKp1	;set p=@name(2),b=^(4),q=$zlength(b) for i=2:1:q if +$zextract(b,i) set j=i-1,k=-j,x=$@zpiece($ztoldval,p,j) zkill @name(j,x,@sub) zkill:1>$increment(@name(k,x),-1) ^(x)

ttSp2	;for i=@pieces set p=$@zpiece($ztoldval,@sep,i),q=$@zpiece($ztvalue,@sep,i),j=-i do
	;. if p'=q do
	;. . if $data(@name(i,p,@sub))#10 zkill ^(@lastsub) zkill:1>$increment(@name(11),-1) ^(11) if 1>$increment(@name(j,p),-1) zkill ^(p) if 1>$increment(@name(j),-1) zkill ^(j)
	;. . if '($data(@name(i,q,@sub))#10) set ^(@lastsub)="" if $increment(@name(11)),(1=$increment(@name(j,q))),$increment(@name(j))
	;. else  if '$zlength(q),'($data(@name(i,"",@sub))#10) set ^(@lastsub)="" if $increment(@name(11)),(1=$increment(@name(j,""))),$increment(@name(j))

ttKp2	;set p=@name(2) for i=2:1:$zlength(@name(4)) if +$zextract(@name(4),i) set j=i-1,mj=-j,q=$@zpiece($ztoldval,p,j) if $data(@name(j,q,@sub)) kill ^(@lastsub) zkill:1>$increment(@name(11),-1) ^(11) if 1>$increment(@name(mj,q),-1) zkill ^(q) zkill:1>$increment(@name(mj),-1) ^(mj)

ttZKp2	;set p=@name(2) for i=2:1:$zlength(@name(4)) if +$zextract(@name(4),i) set j=i-1,mj=-j,q=$@zpiece($ztoldval,p,j) if $data(@name(j,q,@sub))#10 zkill ^(@lastsub) zkill:1>$increment(@name(11),-1) ^(11) if 1>$increment(@name(mj,q),-1) zkill ^(q) zkill:1>$increment(@name(mj),-1) ^(mj)

;	Error message texts
U238	;"-F-NOPSEP Piece numbers "_pnum_" specified, but piece separator not specifed"
U239	;"-F-SETZTRIGGERFAIL Out of design condition - setting $ZTRIGGER() failed"
U240	;"-F-CANTADDSTAT stat="_stat_" and "_name_"(10)="_+$get(@name@(10))_" - adding statistics to existing metadata not supported"
U241	;"-F-INVSTAT """_stat_""" is invalid stat; must be 0, 1, or 2"
U242	;"-F-OUTOFDESIGN """_name_""" already used to xref """_@name_""" cannot reuse for """_gbl_""""
U243	;"-F-ALREADYXREF """_gbl_""" is already a cross reference global variable"
U244	;"-F-NEEDSUB Need at least one subscript for cross reference"
U245	;"-F-INVPIECE Range """_subpiece_""" has invalid number of pieces: "_piecelen
U246	;"-F-INVRANGE upper """_upper_""" is less than lower """_lower_""" in range specification"
U247	;"-F-INVSUB Subscript """_$get(i,nsubs)_""" is not an integer 1 through 31"
U248	;"-F-INVPIECE Piece """_nextp_""" is invalid piece specification"
U249	;"-F-INVPNUMSEP Range specification "_k_" ("_$zwrite(nextp)_") has "_nextplen_" "":"" separated pieces, invalid"
U250	;"-F-NOPIECE Piece separator """_sep_""" specified, but no piece numbers"
U251	;"-F-INCONSISTENTNULL Regions "_tmp_" for global variable "_gbl_" are inconsistent with regard to null subscripts"
U252	;"-F-NOTAGBL Variable """_$get(gbl)_""" is not a valid global variable name"
U253	;"-F-NOSUBS Need at least 1 subscript to cross reference; nsubs="_nsubs
U254	;"-F-NOEXTREF Extended reference in "_gbl_" is not supported"
U255	;"-F-BADINVOCATION Top level invocation of "_$text(+0)_" not supported; must invoke a label"
