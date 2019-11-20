-- sasl.lua v0.4
-- Copyright (C) 2008-2010 Tobias Markmann
--
--    All rights reserved.
--
--    Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
--
--        * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--        * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
--        * Neither the name of Tobias Markmann nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
--
--    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


local pairs, ipairs = pairs, ipairs;
local t_insert = table.insert;
local type = type
local setmetatable = setmetatable;
local assert = assert;
local require = require;

module "sasl"

--[[
Authentication Backend Prototypes:

state = false : disabled
state = true : enabled
state = nil : non-existant
]]

local method = {};
method.__index = method;
local mechanisms = {};
local backend_mechanism = {};

-- register a new SASL mechanims
function registerMechanism(name, backends, f)
	assert(type(name) == "string", "Parameter name MUST be a string.");
	assert(type(backends) == "string" or type(backends) == "table", "Parameter backends MUST be either a string or a table.");
	assert(type(f) == "function", "Parameter f MUST be a function.");
	mechanisms[name] = f
	for _, backend_name in ipairs(backends) do
		if backend_mechanism[backend_name] == nil then backend_mechanism[backend_name] = {}; end
		t_insert(backend_mechanism[backend_name], name);
	end
end

-- create a new SASL object which can be used to authenticate clients
function new(realm, profile)
	local mechanisms = profile.mechanisms;
	if not mechanisms then
		mechanisms = {};
		for backend, f in pairs(profile) do
			if backend_mechanism[backend] then
				for _, mechanism in ipairs(backend_mechanism[backend]) do
					mechanisms[mechanism] = true;
				end
			end
		end
		profile.mechanisms = mechanisms;
	end
	return setmetatable({ profile = profile, realm = realm, mechs = mechanisms }, method);
end

-- get a fresh clone with the same realm and profile
function method:clean_clone()
	return new(self.realm, self.profile)
end

-- get a list of possible SASL mechanims to use
function method:mechanisms()
	return self.mechs;
end

-- select a mechanism to use
function method:select(mechanism)
	if not self.selected and self.mechs[mechanism] then
		self.selected = mechanism;
		return true;
	end
end

-- feed new messages to process into the library
function method:process(message)
	--if message == "" or message == nil then return "failure", "malformed-request" end
	return mechanisms[self.selected](self, message);
end

-- load the mechanisms
require "util.sasl.plain"     .init(registerMechanism);
require "util.sasl.digest-md5".init(registerMechanism);
require "util.sasl.anonymous" .init(registerMechanism);
require "util.sasl.scram"     .init(registerMechanism);

return _M;
