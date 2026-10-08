#!/bin/bash
#################################################################
#								#
# Copyright (c) 2021-2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

# Assumes script_dir env var is set to point to the path ".../YDBAIM/tests"
# "tbash" passes $ydb_dist in as $1; keep any inherited value when invoked without it.
export ydb_dist="${1:-$ydb_dist}"

# Run Bash tests
# Note: We originally had the following simple line.
#	$script_dir/bash-tests/ydbaim_test.sh >& bash_test_output.txt
# But for whatever weird reason, this did not work in the pipeline (passed when run locally/interactively).
# Some output sent to stderr and/or stdout got lost in the bash_test_output.txt file.
# A workaround that was found is to use "|&" and use cat to redirect to the file.
# Hence the seemingly non-intuitive usage below.
$script_dir/bash-tests/ydbaim_test.sh |& cat > bash_test_output.txt

# Check that the bash tests match the reference file.
#
# AIM reports an error as LABEL+offset^%YDBAIM, and that offset moves whenever _YDBAIM.m gains or
# loses a line anywhere above the label - which says nothing about behaviour. Normalize the offset
# away on both sides so this reference survives an edit to AIM rather than needing a refresh with
# every one. The reference is stored already normalized; the sed on it is therefore a no-op and is
# there only so the two sides are demonstrably filtered the same way.
sed -E 's/([A-Za-z0-9%]+)\+[0-9]+\^%YDBAIM/\1+NN^%YDBAIM/g' $script_dir/bash-tests/bash_test_output.ref > bash_test_output_ref_norm.txt
sed -E 's/([A-Za-z0-9%]+)\+[0-9]+\^%YDBAIM/\1+NN^%YDBAIM/g' bash_test_output.txt > bash_test_output_norm.txt
diff bash_test_output_ref_norm.txt bash_test_output_norm.txt
exit $?

