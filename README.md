# YottaDB Application Independent Metadata
Application Independent Metadata (AIM) provides an API for YottaDB to manage
metadata (cross references, statistics) for an application. Metadata is
available in a standard schema.

# Installation
Since this is a plug-in for [YottaDB](https://gitlab.com/YottaDB/DB/YDB),
YottaDB must be installed first.

YDBAIM requires YottaDB r1.32 or higher.

To install, you need `cmake`, `make`, and `ld` commands. After
downloading this repository, you can install as follows:

```
cd <project directory>
mkdir build && cd build
cmake .. && make && sudo make install
```

Here is some sample output:
```
-- YDBCMake Source Directory: /build/_deps/ydbcmake-src
-- Found YOTTADB: /usr/local/lib/yottadb/r134/libyottadb.so
-- Install Location: /usr/local/lib/yottadb/r134
-- Configuring done
-- Generating done
-- Build files have been written to: /build
Scanning dependencies of target _ydbaimutf8
[ 25%] Building M object CMakeFiles/_ydbaimutf8.dir/_YDBAIM.m.o
[ 50%] Linking M shared library utf8/_ydbaim.so
[ 50%] Built target _ydbaimutf8
Scanning dependencies of target _ydbaimM
[ 75%] Building M object CMakeFiles/_ydbaimM.dir/_YDBAIM.m.o
[100%] Linking M shared library M/_ydbaim.so
[100%] Built target _ydbaimM
[ 50%] Built target _ydbaimutf8
[100%] Built target _ydbaimM
Install the project...
-- Install configuration: ""
-- Installing: /usr/local/lib/yottadb/r134/plugin/o/_ydbaim.so
-- Installing: /usr/local/lib/yottadb/r134/plugin/o/utf8/_ydbaim.so
-- Installing: /usr/local/lib/yottadb/r134/plugin/r/_YDBAIM.m
```

# Testing
To run tests, run `./test.sh` in the `tests` subdirectory after installation.

## Contributing
To contribute or help with further development, [fork the repository](https://docs.gitlab.com/ee/gitlab-basics/fork-project.html), clone your fork to a local copy and begin contributing!

Please also set up the pre-commit script to automatically enforce some coding conventions. Creating a symbolic link to YDBOcto/pre-commit will be enough for the setup. Assuming you are in the top-level directory of your local copy, the following will work:

```sh
ln -s ../../pre-commit .git/hooks
```

Note that this script will require `tcsh`.
