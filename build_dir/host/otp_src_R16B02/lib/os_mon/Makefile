#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

#
# Macros
#
ifeq ($(findstring win32,$(TARGET)),win32)
SUB_DIRECTORIES = src c_src mibs doc/src
else
SUB_DIRECTORIES = src c_src mibs doc/src
endif

include vsn.mk
VSN = $(OS_MON_VSN)

SPECIAL_TARGETS = 

#
# Default Subdir Targets
#
include $(ERL_TOP)/make/otp_subdir.mk

