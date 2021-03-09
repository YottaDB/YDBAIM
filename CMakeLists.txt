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
cmake_minimum_required(VERSION 2.8)

# Find YottaDB and set Install Destination
find_package(YOTTADB REQUIRED)
include_directories("${YOTTADB_INCLUDE_DIRS}")
set(CMAKE_INSTALL_PREFIX ${YOTTADB_PLUGIN_DIR})

# CMake Build Type (req b/c it's C code (we need ld from the C toolkit))
if(NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE Release)
endif()
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

# Sources
set(source_files _YDBAIM.m _YDBAIMTEST.m)

# .so file for plugin
add_library(_YDBAIM SHARED ${source_files})
SET_TARGET_PROPERTIES(_YDBAIM PROPERTIES PREFIX "")

# Install sources
install(FILES ${source_files} DESTINATION r)

# Install shared library
install(TARGETS _YDBAIM DESTINATION o)

# This is so ugly, but that's what we do now to install UTF-8 versions of the
# code. We need a better way, but we don't have one right now.
if(EXISTS ${YOTTADB_INCLUDE_DIRS}/utf8)
  find_program(PKGCONFIG NAMES pkg-config)
  if(PKGCONFIG)
    execute_process(
      COMMAND ${PKGCONFIG} --modversion icu-io
      OUTPUT_VARIABLE icu_version
      RESULT_VARIABLE icu_failed
      OUTPUT_STRIP_TRAILING_WHITESPACE
      )
    if(icu_failed)
      message(FATAL_ERROR "Command\n ${PKGCONFIG} --modversion icu-io\nfailed (${icu_failed}).")
    elseif("x${icu_version}" MATCHES "^x([0-9]+\\.[0-9]+)")
      set(ydb_icu_version "${CMAKE_MATCH_1}")
    else()
      message(FATAL_ERROR "Command\n ${PKGCONFIG} --modversion icu-io\nproduced unrecognized output:\n ${icu_version}")
    endif()
  else()
    message(FATAL_ERROR "Unable to find 'pkg-config'.  Set PKGCONFIG in CMake cache.")
  endif()

  find_program(LOCALECFG NAMES locale)
  if(LOCALECFG)
    execute_process(
      COMMAND ${LOCALECFG} -a
      OUTPUT_VARIABLE locale_list
      RESULT_VARIABLE locale_failed
      OUTPUT_STRIP_TRAILING_WHITESPACE
      )
    if(locale_failed)
      message(FATAL_ERROR "Command\n ${LOCALECFG} -a\nfailed (${locale_failed}).")
    endif()
    STRING(REGEX REPLACE "\n" ";" locale_list "${locale_list}")
    foreach(lc ${locale_list})
      string(TOLOWER "${lc}" lc_lower)
      if("x${lc_lower}" MATCHES "^x[a-zA-Z_]+\\.?utf-?8")
    set(LC_ALL ${lc})
    message("-- Setting locale to ${LC_ALL}")
    break()
      endif()
    endforeach(lc)
    if("${LC_ALL}" STREQUAL "")
      message("Locale undefined. Expect to see NONUTF8LOCALE during M routine compilation: ${locale_list}\n")
    endif()
  else()
    message(FATAL_ERROR "Unable to find 'locale'.  Set LOCALECFG in CMake cache.")
  endif()
  set(CMAKE_M_COMPILER ${mumps_dir}/utf8/mumps)
  set(ydb_chset "UTF-8")

  # .so file for plugin
  add_library(_YDBAIMUTF8 SHARED ${source_files})
  SET_TARGET_PROPERTIES(_YDBAIMUTF8 PROPERTIES PREFIX "")
  SET_TARGET_PROPERTIES(_YDBAIMUTF8 PROPERTIES OUTPUT_NAME "_YDBAIM")

  # Install utf-8 .so
  install(TARGETS _YDBAIMUTF8 DESTINATION o/utf8)
endif()
