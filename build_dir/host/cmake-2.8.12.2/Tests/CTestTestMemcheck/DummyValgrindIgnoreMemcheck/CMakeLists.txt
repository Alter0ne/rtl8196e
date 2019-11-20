cmake_minimum_required(VERSION 2.8.9)
project(CTestTestMemcheckDummyValgrindIgnoreMemcheck)
include(CTest)

add_test(NAME RunCMake COMMAND "${CMAKE_COMMAND}" --version)

add_test(NAME RunCMakeAgain COMMAND "${CMAKE_COMMAND}" --version)
