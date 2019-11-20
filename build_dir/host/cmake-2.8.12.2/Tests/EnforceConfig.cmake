# Choose a configuration with which to drive CTest tests.
if(CTEST_CONFIGURATION_TYPE)
  set(CTestTest_CONFIG "${CTEST_CONFIGURATION_TYPE}")
else()
  set(CTestTest_CONFIG "Debug")
endif()

# Choose a configuration that was built if none is given.
if(NOT CTEST_CONFIGURATION_TYPE)
  set(CTEST_CMD "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/bin/ctest")
  get_filename_component(CTEST_DIR "${CTEST_CMD}" PATH)
  get_filename_component(CTEST_EXE "${CTEST_CMD}" NAME)
  foreach(cfg Release Debug MinSizeRel RelWithDebInfo)
    if(NOT CTEST_CONFIGURATION_TYPE)
      if(EXISTS "${CTEST_DIR}/${cfg}/${CTEST_EXE}")
        set(CTEST_CONFIGURATION_TYPE ${cfg})
      endif()
    endif()
  endforeach()
  if(NOT CTEST_CONFIGURATION_TYPE)
    set(CTEST_CONFIGURATION_TYPE NoConfig)
  endif()
  message("Guessing configuration ${CTEST_CONFIGURATION_TYPE}")
endif()

# Fake a user home directory to avoid polluting the real one.
# But provide original ENV{HOME} value in ENV{CTEST_REAL_HOME} for tests that
# need access to the real HOME directory.
set(ENV{CTEST_REAL_HOME} "$ENV{HOME}")
set(ENV{HOME} "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Tests/CMakeFiles/TestHome")

