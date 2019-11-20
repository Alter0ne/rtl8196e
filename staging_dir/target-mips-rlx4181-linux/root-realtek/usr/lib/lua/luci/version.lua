local pcall, dofile, _G = pcall, dofile, _G

module "luci.version"

if pcall(dofile, "/etc/openwrt_release") and _G.DISTRIB_DESCRIPTION then
	distname    = ""
	distversion = _G.DISTRIB_DESCRIPTION
else
	distname    = "OpenWrt Firmware"
	distversion = "Barrier Breaker (unknown)"
end

luciname    = "LuCI 0.12 Branch"
luciversion = "0.12+git-16.038.38474-0d510b2"
