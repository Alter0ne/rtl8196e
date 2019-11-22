#
# Copyright (C) 2007-2013 OpenWrt.org
#
# This is free software, licensed under the GNU General Public License v2.
# See /LICENSE for more information.
#

include $(TOPDIR)/rules.mk

PKG_NAME:=dhcpcd
PKG_VERSION:=6.1.0
PKG_RELEASE:=1

PKG_SOURCE:=$(PKG_NAME)-$(PKG_VERSION).tar.bz2
PKG_SOURCE_URL:=ftp://roy.marples.name/pub/dhcpcd
PKG_MD5SUM:=6070040c57492925af9ac6aed980de2a

PKG_INSTALL:=1

include $(INCLUDE_DIR)/package.mk

define Package/dhcpcd
  SECTION:=net
  CATEGORY:=Network
  SUBMENU:=IP Addresses and Names
  TITLE:=DHCP client for automatically configuring IPv4 networking
  DEPENDS:=+librt
  URL:=http://roy.marples.name/projects/dhcpcd
endef

define Package/dhcpcd/description
	Simple configuration: supports executions of a script when the
	IP address changes.
endef

define Package/dhcpcd/install
	$(INSTALL_DIR) $(1)/sbin $(1)/etc $(1)/etc/dhcpcd.enter-hook
	$(INSTALL_BIN) $(PKG_INSTALL_DIR)/usr/sbin/dhcpcd $(1)/sbin/
	$(INSTALL_BIN) $(PKG_INSTALL_DIR)/usr/lib/dhcpcd-run-hooks $(1)/sbin/
	$(INSTALL_CONF) $(PKG_INSTALL_DIR)/etc/dhcpcd.conf $(1)/etc/dhcpcd.conf
	$(INSTALL_CONF) $(PKG_INSTALL_DIR)/usr/lib/dhcpcd-hooks/* $(1)/etc/dhcpcd.enter-hook/
endef

$(eval $(call BuildPackage,dhcpcd))

