#################################################################
#								#
# Copyright (c) 2021-2025 YottaDB LLC and/or its subsidiaries.	#
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
# The GIT_TAG is, at the time of this writing, the latest commit in YDBCMake.
# It is fixed at that point to ensure reproducible builds.
# Keep this periodically up to date with future YDBCMake commits.
FetchContent_Declare(YDBCMake
	GIT_REPOSITORY	https://gitlab.com/YottaDB/Tools/YDBCMake.git
	GIT_TAG		1bf25f1a14c82e8348a024fb105a2e44e0645f9d
)
FetchContent_MakeAvailable(YDBCMake)
message(STATUS "YDBCMake Source Directory: ${ydbcmake_SOURCE_DIR}")
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${ydbcmake_SOURCE_DIR}/ydbcmake/")

# Get user presentable version number
# Algorithm is:
# - Major = XREFDATA + XREFSUB
# - Minor = %YDBAIM
file(STRINGS _YDBAIM.m XREFDATA REGEX "^XREFDATA\\(")
file(STRINGS _YDBAIM.m XREFSUB REGEX "^XREFSUB\\(")
file(STRINGS _YDBAIM.m _YDBAIM REGEX "^%YDBAIM")
string(REGEX MATCH ";([0-9]+)" _ ${XREFDATA})
set(XREFDATA_VER ${CMAKE_MATCH_1})
string(REGEX MATCH ";([0-9]+)" _ ${XREFSUB})
set(XREFSUB_VER ${CMAKE_MATCH_1})
string(REGEX MATCH ";([0-9]+)" _ ${_YDBAIM})
set(AIM_MINOR_VERSION ${CMAKE_MATCH_1})
math(EXPR AIM_MAJOR_VERSION "${XREFDATA_VER} + ${XREFSUB_VER}")

project(YDBAIM
	LANGUAGES M
	VERSION "${AIM_MAJOR_VERSION}.${AIM_MINOR_VERSION}")

configure_file(_ydbaim.manifest.json.in _ydbaim.manifest.json @ONLY)

# Find YottaDB and print install destination & current version
find_package(YOTTADB REQUIRED)
message(STATUS "Install Location: ${YOTTADB_PLUGIN_PREFIX}")
message(STATUS "YDBAIM Version: ${YDBAIM_VERSION}")

# Add library
add_ydb_library(_ydbaim SOURCES _YDBAIM.m)

# Install library
install_ydb_library(_ydbaim)

# Install Sources
install(FILES _YDBAIM.m
	DESTINATION ${YOTTADB_PLUGIN_PREFIX}/r/)

# Install Manifest file
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/_ydbaim.manifest.json DESTINATION ${YOTTADB_M_PLUGIN_DIR})
if(ydb_icu_version)
	install(FILES ${CMAKE_CURRENT_BINARY_DIR}/_ydbaim.manifest.json DESTINATION ${YOTTADB_M_PLUGIN_DIR}/utf8)
endif()
