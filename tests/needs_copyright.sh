#!/bin/sh

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

set -euv

# Determines whether a file should need a copyright by its name
# Returns 0 if it needs a copyright and 1 otherwise.
! [ "$1" = README.md ]
