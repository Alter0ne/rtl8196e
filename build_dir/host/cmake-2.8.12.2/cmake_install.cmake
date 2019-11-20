# Install script for directory: /home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2

# Set the install prefix
IF(NOT DEFINED CMAKE_INSTALL_PREFIX)
  SET(CMAKE_INSTALL_PREFIX "/home/alter0ne/rtk_openwrt_sdk/staging_dir/host")
ENDIF(NOT DEFINED CMAKE_INSTALL_PREFIX)
STRING(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
IF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  IF(BUILD_TYPE)
    STRING(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  ELSE(BUILD_TYPE)
    SET(CMAKE_INSTALL_CONFIG_NAME "")
  ENDIF(BUILD_TYPE)
  MESSAGE(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
ENDIF(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)

# Set the component getting installed.
IF(NOT CMAKE_INSTALL_COMPONENT)
  IF(COMPONENT)
    MESSAGE(STATUS "Install component: \"${COMPONENT}\"")
    SET(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  ELSE(COMPONENT)
    SET(CMAKE_INSTALL_COMPONENT)
  ENDIF(COMPONENT)
ENDIF(NOT CMAKE_INSTALL_COMPONENT)

# Install shared libraries without execute permission?
IF(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  SET(CMAKE_INSTALL_SO_NO_EXE "1")
ENDIF(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/doc/cmake-2.8" TYPE FILE FILES "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Copyright.txt")
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  FILE(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/cmake-2.8" TYPE DIRECTORY PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ DIR_PERMISSIONS OWNER_READ OWNER_EXECUTE OWNER_WRITE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE FILES
    "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Modules"
    "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Templates"
    REGEX "/[^/]*\\.sh[^/]*$" PERMISSIONS OWNER_READ OWNER_EXECUTE OWNER_WRITE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
ENDIF(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")

IF(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  INCLUDE("/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Utilities/KWIML/cmake_install.cmake")
  INCLUDE("/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Source/kwsys/cmake_install.cmake")
  INCLUDE("/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Utilities/cmzlib/cmake_install.cmake")
  INCLUDE("/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Utilities/cmcurl/cmake_install.cmake")
  INCLUDE("/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Utilities/cmcompress/cmake_install.cmake")
  INCLUDE("/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Utilities/cmbzip2/cmake_install.cmake")
  INCLUDE("/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Utilities/cmlibarchive/cmake_install.cmake")
  INCLUDE("/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Utilities/cmexpat/cmake_install.cmake")
  INCLUDE("/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Source/CursesDialog/form/cmake_install.cmake")
  INCLUDE("/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Source/cmake_install.cmake")
  INCLUDE("/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Utilities/cmake_install.cmake")
  INCLUDE("/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Tests/cmake_install.cmake")
  INCLUDE("/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Docs/cmake_install.cmake")

ENDIF(NOT CMAKE_INSTALL_LOCAL_ONLY)

IF(CMAKE_INSTALL_COMPONENT)
  SET(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
ELSE(CMAKE_INSTALL_COMPONENT)
  SET(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
ENDIF(CMAKE_INSTALL_COMPONENT)

FILE(WRITE "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/${CMAKE_INSTALL_MANIFEST}" "")
FOREACH(file ${CMAKE_INSTALL_MANIFEST_FILES})
  FILE(APPEND "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/${CMAKE_INSTALL_MANIFEST}" "${file}\n")
ENDFOREACH(file)
