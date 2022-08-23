#################################################################
#								#
# Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
# Prelim
cmake_minimum_required(VERSION 3.14)
include(FetchContent)
FetchContent_Declare(YDBCMake
	GIT_REPOSITORY	https://gitlab.com/YottaDB/Tools/YDBCMake.git
	GIT_TAG		ec3ed1781a8a9ebeee67cd524029da5176183e8b
)
FetchContent_MakeAvailable(YDBCMake)
message(STATUS "YDBCMake Source Directory: ${ydbcmake_SOURCE_DIR}")
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${ydbcmake_SOURCE_DIR}/ydbcmake/")

project(YDBAIM M)

# Find YottaDB and print Install Destination
find_package(YOTTADB REQUIRED)
message(STATUS "Install Location: ${YOTTADB_PLUGIN_PREFIX}")

# Add library
add_ydb_library(_ydbaim SOURCES _YDBAIM.m)

# Install library
install_ydb_library(_ydbaim)

# Install Sources
install(FILES _YDBAIM.m
	DESTINATION ${YOTTADB_PLUGIN_PREFIX}/r/)
