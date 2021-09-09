#!/bin/bash
#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

# Assumes script_dir env var is set to point to the path ".../YDBAIM/tests"

# Run Bash tests
# Note: We originally had the following simple line.
#	$script_dir/bash-tests/ydbaim_test.sh >& bash_test_output.txt
# But for whatever weird reason, this did not work in the pipeline (passed when run locally/interactively).
# Some output sent to stderr and/or stdout got lost in the bash_test_output.txt file.
# A workaround that was found is to use "|&" and use cat to redirect to the file.
# Hence the seemingly non-intuitive usage below.
$script_dir/bash-tests/ydbaim_test.sh |& cat > bash_test_output.txt

# Check that the bash tests match the reference file
diff $script_dir/bash-tests/bash_test_output.ref bash_test_output.txt
exit $?

