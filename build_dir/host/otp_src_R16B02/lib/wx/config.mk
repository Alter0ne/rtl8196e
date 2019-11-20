#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2009. All Rights Reserved.
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
#
INSIDE_ERLSRC	 = true
SYS_TYPE	 = x86_64-pc-linux-gnu
CAN_BUILD_DRIVER = true

VSN = $(WX_VSN)

ifeq ($(INSIDE_ERLSRC), true)

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

RELSYSDIR = $(RELEASE_PATH)/lib/wx-$(VSN)

else
INSTALLDIR=/home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_R16B02/lib
endif

