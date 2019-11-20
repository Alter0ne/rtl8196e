if(NOT EXISTS "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/install_manifest.txt")
  message(FATAL_ERROR "Cannot find install manifest: \"/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/install_manifest.txt\"")
endif()

file(READ "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/install_manifest.txt" files)
string(REGEX REPLACE "\n" ";" files "${files}")
foreach(file ${files})
  message(STATUS "Uninstalling \"$ENV{DESTDIR}${file}\"")
  if(EXISTS "$ENV{DESTDIR}${file}")
    exec_program(
      "/home/alter0ne/rtk_openwrt_sdk/build_dir/host/cmake-2.8.12.2/Bootstrap.cmk/cmake" ARGS "-E remove \"$ENV{DESTDIR}${file}\""
      OUTPUT_VARIABLE rm_out
      RETURN_VALUE rm_retval
      )
    if("${rm_retval}" STREQUAL 0)
    else()
      message(FATAL_ERROR "Problem when removing \"$ENV{DESTDIR}${file}\"")
    endif()
  else()
    message(STATUS "File \"$ENV{DESTDIR}${file}\" does not exist.")
  endif()
endforeach()
