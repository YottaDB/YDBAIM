#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
FROM yottadb/yottadb-base:latest-master
WORKDIR /YDBAIM
ENV DEBIAN_FRONTEND=noninteractive
RUN ln -fs /usr/share/zoneinfo/US/Eastern /etc/localtime
RUN apt-get update -qq && apt-get install -y -qq build-essential cmake git curl pkg-config libicu-dev
ADD CMakeLists.txt _YDBAIM.m ./
ADD tests tests
RUN mkdir /build && cd /build && cmake /YDBAIM && make && make install
ENTRYPOINT ./tests/test.sh
