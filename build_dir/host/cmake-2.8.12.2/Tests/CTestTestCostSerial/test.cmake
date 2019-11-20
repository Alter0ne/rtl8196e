cmake_minimum_required(VERSION 2.1)

# Settings:
set(CTEST_DASHBOARD_ROOT                "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Tests/CTestTest")
set(CTEST_SITE                          "alter0ne-PC")
set(CTEST_BUILD_NAME                    "CTestTest-Linux-g++-CostSerial")

set(CTEST_SOURCE_DIRECTORY              "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Tests/CTestTestCostSerial")
set(CTEST_BINARY_DIRECTORY              "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Tests/CTestTestCostSerial")
set(CTEST_CVS_COMMAND                   "CVSCOMMAND-NOTFOUND")
set(CTEST_CMAKE_GENERATOR               "Unix Makefiles")
set(CTEST_CMAKE_GENERATOR_TOOLSET       "")
set(CTEST_BUILD_CONFIGURATION           "$ENV{CMAKE_CONFIG_TYPE}")
set(CTEST_COVERAGE_COMMAND              "/usr/bin/gcov")
set(CTEST_NOTES_FILES                   "${CTEST_SCRIPT_DIRECTORY}/${CTEST_SCRIPT_NAME}")

#CTEST_EMPTY_BINARY_DIRECTORY(${CTEST_BINARY_DIRECTORY})

# Remove old cost data file if it exists
if(EXISTS "${CTEST_BINARY_DIRECTORY}/Testing/Temporary/CTestCostData.txt")
  file(REMOVE "${CTEST_BINARY_DIRECTORY}/Testing/Temporary/CTestCostData.txt")
endif()

CTEST_START(Experimental)
CTEST_CONFIGURE(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
CTEST_BUILD(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
CTEST_TEST(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
# Run test set a second time to make sure they run in same specified order
CTEST_TEST(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
