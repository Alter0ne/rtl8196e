cmake_minimum_required(VERSION 2.8.9)
project(CTestTestMemcheckDummyBCNoLogfile)
include(CTest)

add_test(NAME RunCMake COMMAND "${CMAKE_COMMAND}" --version)


