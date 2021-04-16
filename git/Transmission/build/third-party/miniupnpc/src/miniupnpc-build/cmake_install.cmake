# Install script for directory: /home/pnotz17/Transmission/build/third-party/miniupnpc/src/miniupnpc

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/home/pnotz17/Transmission/build/third-party/miniupnpc")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "RelWithDebInfo")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "0")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

# Set default install directory permissions.
if(NOT DEFINED CMAKE_OBJDUMP)
  set(CMAKE_OBJDUMP "/bin/objdump")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/home/pnotz17/Transmission/build/third-party/miniupnpc/src/miniupnpc-build/libminiupnpc.a")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/miniupnpc" TYPE FILE FILES
    "/home/pnotz17/Transmission/build/third-party/miniupnpc/src/miniupnpc/miniupnpc.h"
    "/home/pnotz17/Transmission/build/third-party/miniupnpc/src/miniupnpc/miniwget.h"
    "/home/pnotz17/Transmission/build/third-party/miniupnpc/src/miniupnpc/upnpcommands.h"
    "/home/pnotz17/Transmission/build/third-party/miniupnpc/src/miniupnpc/igd_desc_parse.h"
    "/home/pnotz17/Transmission/build/third-party/miniupnpc/src/miniupnpc/upnpreplyparse.h"
    "/home/pnotz17/Transmission/build/third-party/miniupnpc/src/miniupnpc/upnperrors.h"
    "/home/pnotz17/Transmission/build/third-party/miniupnpc/src/miniupnpc/upnpdev.h"
    "/home/pnotz17/Transmission/build/third-party/miniupnpc/src/miniupnpc/miniupnpctypes.h"
    "/home/pnotz17/Transmission/build/third-party/miniupnpc/src/miniupnpc/portlistingparse.h"
    "/home/pnotz17/Transmission/build/third-party/miniupnpc/src/miniupnpc/miniupnpc_declspec.h"
    )
endif()

if(CMAKE_INSTALL_COMPONENT)
  set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
else()
  set(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
file(WRITE "/home/pnotz17/Transmission/build/third-party/miniupnpc/src/miniupnpc-build/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
