--[[

LuCI uShare
(c) 2008 Yanira <forum-2008@email.de>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

$Id$

]]--

module("luci.controller.ushare", package.seeall)

function index()
	if not nixio.fs.access("/etc/config/ushare") then
		return
	end

	local page

	page = entry({"admin", "services", "ushare"}, cbi("ushare"), _("uShare"), 60)
	page.dependent = true
end
