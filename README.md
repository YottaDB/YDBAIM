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
cmake ..
make
sudo make install
```

Here is some sample output:
```
root@806474820c41:/aim/build# cmake ..
-- The C compiler identification is GNU 9.3.0
-- Check for working C compiler: /usr/bin/cc
-- Check for working C compiler: /usr/bin/cc -- works
-- Detecting C compiler ABI info
-- Detecting C compiler ABI info - done
-- Detecting C compile features
-- Detecting C compile features - done
-- Found YOTTADB: /opt/yottadb/current/libyottadb.so
-- Build type: RelWithDebInfo
-- Setting locale to C.UTF-8
-- Configuring done
-- Generating done
-- Build files have been written to: /aim/build
root@806474820c41:/aim/build# make
Scanning dependencies of target _YDBAIMUTF8
[ 16%] Building M object CMakeFiles/_YDBAIMUTF8.dir/_YDBAIM.m.o
[ 33%] Building M object CMakeFiles/_YDBAIMUTF8.dir/_YDBAIMTEST.m.o
[ 50%] Linking M shared library _YDBAIM.so
[ 50%] Built target _YDBAIMUTF8
Scanning dependencies of target _YDBAIM
[ 66%] Building M object CMakeFiles/_YDBAIM.dir/_YDBAIM.m.o
[ 83%] Building M object CMakeFiles/_YDBAIM.dir/_YDBAIMTEST.m.o
[100%] Linking M shared library _YDBAIM.so
[100%] Built target _YDBAIM

root@806474820c41:/aim/build# sudo make install
[ 50%] Built target _YDBAIMUTF8
[100%] Built target _YDBAIM
Install the project...
-- Install configuration: "RelWithDebInfo"
-- Installing: /opt/yottadb/current/plugin/r/_YDBAIM.m
-- Installing: /opt/yottadb/current/plugin/r/_YDBAIMTEST.m
-- Installing: /opt/yottadb/current/plugin/o/_YDBAIM.so
-- Installing: /opt/yottadb/current/plugin/o/utf8/_YDBAIM.so
```
