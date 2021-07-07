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

# Prelim
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/ydbcmake/")
project(YDBAIM C M) # C needed for ld
cmake_minimum_required(VERSION 3.0.0)

# Find YottaDB and set Install Destination
find_package(YOTTADB REQUIRED)
include_directories("${YOTTADB_INCLUDE_DIRS}")
set(CMAKE_INSTALL_PREFIX ${YOTTADB_PLUGIN_DIR})
message(STATUS "Install Location: ${YOTTADB_PLUGIN_DIR}")

# CMake Build Type (req b/c it's C code (we need ld from the C toolkit))
if(NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE Release)
endif()
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

# Sources
set(source_files _YDBAIM.m)

# .so file for plugin
add_library(_ydbaim SHARED ${source_files})
SET_TARGET_PROPERTIES(_ydbaim PROPERTIES PREFIX "")

# Install shared library
install(TARGETS _ydbaim DESTINATION ${CMAKE_INSTALL_PREFIX})
