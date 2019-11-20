# CMake generated Testfile for 
# Source directory: /home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Utilities
# Build directory: /home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Utilities
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
ADD_TEST(CMake.HTML "/usr/bin/xmllint" "--valid" "--noout" "--nonet" "--path" "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Utilities/xml/xhtml1" "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Docs/cmake.html" "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Docs/cmake-policies.html" "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Docs/cmake-properties.html" "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Docs/cmake-variables.html" "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Docs/cmake-modules.html" "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Docs/cmake-commands.html" "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Docs/cmake-compatcommands.html" "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Docs/ctest.html" "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Docs/cpack.html" "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Docs/ccmake.html")
ADD_TEST(CMake.DocBook "/usr/bin/xmllint" "--valid" "--noout" "--nonet" "--path" "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Utilities/xml/docbook-4.5" "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Docs/cmake.docbook" "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Docs/ctest.docbook" "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Docs/cpack.docbook" "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Docs/ccmake.docbook")
SUBDIRS(Doxygen)
SUBDIRS(KWStyle)
