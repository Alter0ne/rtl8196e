cmake_minimum_required(VERSION 2.8)

# CTestConfig.cmake settings:
set(CTEST_PROJECT_NAME "SmallAndFast")

# Intentionally leave out other upload-related CTestConfig.cmake settings
# so that the ctest_submit call below fails with an error message...
#
set(CTEST_DROP_METHOD "xmlrpc")

# Settings:
set(CTEST_USE_LAUNCHERS 1)

# Emit these compiler warnings:
set(ENV{CXXFLAGS} "$ENV{CXXFLAGS} -Wall")

set(CTEST_SITE                          "alter0ne-PC")
set(CTEST_BUILD_NAME                    "CTestTestLaunchers-xmlrpc")

set(CTEST_SOURCE_DIRECTORY              "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Tests/CTestTest/SmallAndFast")
set(CTEST_BINARY_DIRECTORY              "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Tests/CTestTestFailedSubmits/xmlrpc")
set(CTEST_CVS_COMMAND                   "CVSCOMMAND-NOTFOUND")
set(CTEST_CMAKE_GENERATOR               "Unix Makefiles")
set(CTEST_CMAKE_GENERATOR_TOOLSET       "")
set(CTEST_BUILD_CONFIGURATION           "$ENV{CMAKE_CONFIG_TYPE}")
set(CTEST_COVERAGE_COMMAND              "/usr/bin/gcov")
set(CTEST_NOTES_FILES                   "${CTEST_SCRIPT_DIRECTORY}/${CTEST_SCRIPT_NAME}")

CTEST_EMPTY_BINARY_DIRECTORY(${CTEST_BINARY_DIRECTORY})

CTEST_START(Experimental)

# explicitly do not use CTEST_UPDATE - avoid network activity

CTEST_CONFIGURE(BUILD "${CTEST_BINARY_DIRECTORY}"
  OPTIONS "-DCTEST_USE_LAUNCHERS:BOOL=${CTEST_USE_LAUNCHERS};-DSAF_INTENTIONAL_COMPILE_ERROR:BOOL=ON;-DSAF_INTENTIONAL_COMPILE_WARNING:BOOL=ON"
  RETURN_VALUE res)
CTEST_BUILD(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
CTEST_TEST(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
CTEST_COVERAGE(BUILD "${CTEST_BINARY_DIRECTORY}"  RETURN_VALUE res)

# ok to call ctest_submit - still avoids network activity because there is
# not a valid drop location given above...
CTEST_SUBMIT(RETURN_VALUE res)

# Add coverage for the new APPEND arg to ctest_start:
#
CTEST_START(Experimental APPEND)
