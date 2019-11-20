#-*-makefile-*-   ; force emacs to enter makefile-mode
# ----------------------------------------------------
# %CopyrightBegin%
#
# Copyright Ericsson AB 2009-2013. All Rights Reserved.
#
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
#
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
#
# %CopyrightEnd%

# The version.
#
# Note that it is important that the version is
# explicitly expressed here. Some applications need to
# be able to check this value *before* configure has
# been run and generated otp_ded.mk
DED_MK_VSN = 1
# ----------------------------------------------------
# Variables needed for building Dynamic Erlang Drivers
# ----------------------------------------------------
DED_CC = gcc
DED_GCC = yes
DED_LD = gcc
DED_LDFLAGS = -shared -Wl,-Bsymbolic
DED__NOWARN_NOTHR_CFLAGS = -O2 -I/home/alter0ne/rtk_openwrt_sdk/staging_dir/host/include -I/home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_R16B02/erts/x86_64-pc-linux-gnu   -fno-tree-copyrename -I/home/alter0ne/rtk_openwrt_sdk/staging_dir/host/include -D_GNU_SOURCE -fPIC
DED__NOTHR_CFLAGS = -Wall -Wstrict-prototypes -Wmissing-prototypes -Wdeclaration-after-statement -O2 -I/home/alter0ne/rtk_openwrt_sdk/staging_dir/host/include -I/home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_R16B02/erts/x86_64-pc-linux-gnu   -fno-tree-copyrename -I/home/alter0ne/rtk_openwrt_sdk/staging_dir/host/include -D_GNU_SOURCE -fPIC
DED__NOWARN_CFLAGS =  -DUSE_THREADS -D_THREAD_SAFE -D_REENTRANT -DPOSIX_THREADS -D_POSIX_THREAD_SAFE_FUNCTIONS -O2 -I/home/alter0ne/rtk_openwrt_sdk/staging_dir/host/include -I/home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_R16B02/erts/x86_64-pc-linux-gnu   -fno-tree-copyrename -I/home/alter0ne/rtk_openwrt_sdk/staging_dir/host/include -D_GNU_SOURCE -fPIC
DED_THR_DEFS = -DUSE_THREADS  -D_THREAD_SAFE -D_REENTRANT -DPOSIX_THREADS -D_POSIX_THREAD_SAFE_FUNCTIONS -D_GNU_SOURCE
DED_EMU_THR_DEFS =  -DUSE_THREADS -D_THREAD_SAFE -D_REENTRANT -DPOSIX_THREADS -D_POSIX_THREAD_SAFE_FUNCTIONS
DED_WARN_FLAGS = -Wall -Wstrict-prototypes -Wmissing-prototypes -Wdeclaration-after-statement
DED_CFLAGS = -Werror=return-type  -Wall -Wstrict-prototypes -Wmissing-prototypes -Wdeclaration-after-statement  -DUSE_THREADS -D_THREAD_SAFE -D_REENTRANT -DPOSIX_THREADS -D_POSIX_THREAD_SAFE_FUNCTIONS -O2 -I/home/alter0ne/rtk_openwrt_sdk/staging_dir/host/include -I/home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_R16B02/erts/x86_64-pc-linux-gnu   -fno-tree-copyrename -I/home/alter0ne/rtk_openwrt_sdk/staging_dir/host/include -D_GNU_SOURCE -fPIC
DED_LIBS = -lutil -ldl -lm  
DED_EXT = so
ERLANG_OSTYPE = unix
PRIVDIR = ../priv
OBJDIR = $(PRIVDIR)/obj/$(TARGET)
LIBDIR = $(PRIVDIR)/lib/$(TARGET)
DED_SYS_INCLUDE = -I/home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_R16B02/erts/emulator/beam -I/home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_R16B02/erts/include -I/home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_R16B02/erts/include/x86_64-pc-linux-gnu -I/home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_R16B02/erts/include/internal -I/home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_R16B02/erts/include/internal/x86_64-pc-linux-gnu -I/home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_R16B02/erts/emulator/sys/unix
DED_INCLUDES = $(DED_SYS_INCLUDE)
