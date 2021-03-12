# YottaDB Application Independent Metadata
Application Independent Metadata (AIM) provides an API for YottaDB to manage
metadata (cross references, statistics) for an application. Metadata is
available in a standard schema.

# Installation
Since this is a plug-in for [YottaDB](https://gitlab.com/YottaDB/DB/YDB),
YottaDB must be installed first.

To install, you need `cmake`, `make`, `cc`, and `ld` commands. After
downloading this repository, you can install as follows:

```
cd <project directory>
mkdir build && cd build
cmake -DM_UTF8_MODE=0 .. && make && sudo make install && echo && make clean && cmake -DM_UTF8_MODE=1 .. && make && sudo make install
```

Here is some sample output:
```
root@d4b472e59995:/ydbaim/build# cmake -DM_UTF8_MODE=0 .. && make && sudo make install && echo && make clean && cmake -DM_UTF8_MODE=1 .. && make && sudo make install
-- The C compiler identification is GNU 9.3.0
-- Check for working C compiler: /usr/bin/cc
-- Check for working C compiler: /usr/bin/cc -- works
-- Detecting C compiler ABI info
-- Detecting C compiler ABI info - done
-- Detecting C compile features
-- Detecting C compile features - done
-- Found YOTTADB: /opt/yottadb/current/libyottadb.so
-- Install Location: /opt/yottadb/current/plugin/o
-- Build type: Release
-- Configuring done
-- Generating done
-- Build files have been written to: /ydbaim/build
Scanning dependencies of target _ydbaim
[ 33%] Building M object CMakeFiles/_ydbaim.dir/_YDBAIM.m.o
[ 66%] Building M object CMakeFiles/_ydbaim.dir/_YDBAIMTEST.m.o
[100%] Linking M shared library _ydbaim.so
[100%] Built target _ydbaim
[100%] Built target _ydbaim
Install the project...
-- Install configuration: "Release"
-- Installing: /opt/yottadb/current/plugin/o/_ydbaim.so

-- Install Location: /opt/yottadb/current/plugin/o/utf8
-- Build type: Release
-- Configuring done
-- Generating done
-- Build files have been written to: /ydbaim/build
[ 33%] Building M object CMakeFiles/_ydbaim.dir/_YDBAIM.m.o
[ 66%] Building M object CMakeFiles/_ydbaim.dir/_YDBAIMTEST.m.o
[100%] Linking M shared library _ydbaim.so
[100%] Built target _ydbaim
[100%] Built target _ydbaim
Install the project...
-- Install configuration: "Release"
-- Installing: /opt/yottadb/current/plugin/o/utf8/_ydbaim.so
```
