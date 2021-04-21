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
sudo -E ./install.sh
```

Here is some sample output:
```
# sudo -E ./install.sh
-- The C compiler identification is GNU 9.3.0
-- Check for working C compiler: /usr/bin/cc
-- Check for working C compiler: /usr/bin/cc -- works
-- Detecting C compiler ABI info
-- Detecting C compiler ABI info - done
-- Detecting C compile features
-- Detecting C compile features - done
-- Found YOTTADB: /usr/library/V989_R131/dbg/libyottadb.so
-- Install Location: /usr/library/V989_R131/dbg/plugin/o
-- Build type: Release
-- Configuring done
-- Generating done
-- Build files have been written to: /home/sam/work/gitlab/YDBAIM/build
Scanning dependencies of target _ydbaim
[ 33%] Building M object CMakeFiles/_ydbaim.dir/_YDBAIM.m.o
[ 66%] Building M object CMakeFiles/_ydbaim.dir/_YDBAIMTEST.m.o
[100%] Linking M shared library _ydbaim.so
[100%] Built target _ydbaim
[100%] Built target _ydbaim
Install the project...
-- Install configuration: "Release"
-- Installing: /usr/library/V989_R131/dbg/plugin/o/_ydbaim.so
-- The C compiler identification is GNU 9.3.0
-- Setting locale to C.UTF-8
-- Check for working C compiler: /usr/bin/cc
-- Check for working C compiler: /usr/bin/cc -- works
-- Detecting C compiler ABI info
-- Detecting C compiler ABI info - done
-- Detecting C compile features
-- Detecting C compile features - done
-- Found YOTTADB: /usr/library/V989_R131/dbg/libyottadb.so
-- Install Location: /usr/library/V989_R131/dbg/plugin/o/utf8
-- Build type: Release
-- Configuring done
-- Generating done
-- Build files have been written to: /home/sam/work/gitlab/YDBAIM/build
Scanning dependencies of target _ydbaim
[ 33%] Building M object CMakeFiles/_ydbaim.dir/_YDBAIM.m.o
[ 66%] Building M object CMakeFiles/_ydbaim.dir/_YDBAIMTEST.m.o
[100%] Linking M shared library _ydbaim.so
[100%] Built target _ydbaim
[100%] Built target _ydbaim
Install the project...
-- Install configuration: "Release"
-- Installing: /usr/library/V989_R131/dbg/plugin/o/utf8/_ydbaim.so
```

## Contributing
To contribute or help with further development, [fork the repository](https://docs.gitlab.com/ee/gitlab-basics/fork-project.html), clone your fork to a local copy and begin contributing!

Please also set up the pre-commit script to automatically enforce some coding conventions. Creating a symbolic link to YDBOcto/pre-commit will be enough for the setup. Assuming you are in the top-level directory of your local copy, the following will work:

```sh
ln -s ../../pre-commit .git/hooks
```

Note that this script will require `tcsh`.
