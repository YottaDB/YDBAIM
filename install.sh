#!/bin/bash
#################################################################
#                               #
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.  #
# All rights reserved.                      #
#                               #
#   This source code contains the intellectual property #
#   of its copyright holder(s), and is made available   #
#   under a license.  If you do not know the terms of   #
#   the license, please stop and do not read further.   #
#                               #
#################################################################
set -e
cd "$(dirname "$(readlink -f "$0")")"
rm -rf build
mkdir build && cd build
cmake -DM_UTF8_MODE=0 .. 
make && make install
cd ..
rm -rf build
mkdir build && cd build
cmake -DM_UTF8_MODE=1 ..
make && make install
cd ..
