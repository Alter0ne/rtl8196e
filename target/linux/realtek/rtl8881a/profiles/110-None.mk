#
# Copyright (C) 2006-2008 OpenWrt.org
#
# This is free software, licensed under the GNU General Public License v2.
# See /LICENSE for more information.
#

define Profile/None
  NAME:=No WiFi
  PACKAGES:=-wpad-mini
endef

define Profile/None/Description
	Realtek SOC,Package set without WiFi support
endef

$(eval $(call Profile,None))
