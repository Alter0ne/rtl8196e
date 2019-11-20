-- Prosody IM
-- Copyright (C) 2008-2010 Matthew Wild
-- Copyright (C) 2008-2010 Waqas Hussain
-- 
-- This project is MIT/X11 licensed. Please see the
-- COPYING file in the source package for more information.
--

local _G = _G;
local setmetatable, rawget, rawset, io, error, dofile, type, pairs, table =
      setmetatable, rawget, rawset, io, error, dofile, type, pairs, table;
local format, math_max = string.format, math.max;

local fire_event = prosody and prosody.events.fire_event or function () end;

local envload = require"util.envload".envload;
local lfs = require "lfs";
local path_sep = package.config:sub(1,1);

module "configmanager"

local parsers = {};

local config_mt = { __index = function (t, k) return rawget(t, "*"); end};
local config = setmetatable({ ["*"] = { } }, config_mt);

-- When host not found, use global
local host_mt = { __index = function(_, k) return config["*"][k] end }

function getconfig()
	return config;
end

function get(host, key, _oldkey)
	if key == "core" then
		key = _oldkey; -- COMPAT with code that still uses "core"
	end
	return config[host][key];
end
function _M.rawget(host, key, _oldkey)
	if key == "core" then
		key = _oldkey; -- COMPAT with code that still uses "core"
	end
	local hostconfig = rawget(config, host);
	if hostconfig then
		return rawget(hostconfig, key);
	end
end

local function set(config, host, key, value)
	if host and key then
		local hostconfig = rawget(config, host);
		if not hostconfig then
			hostconfig = rawset(config, host, setmetatable({}, host_mt))[host];
		end
		hostconfig[key] = value;
		return true;
	end
	return false;
end

function _M.set(host, key, value, _oldvalue)
	if key == "core" then
		key, value = value, _oldvalue; --COMPAT with code that still uses "core"
	end
	return set(config, host, key, value);
end

-- Helper function to resolve relative paths (needed by config)
do
	function resolve_relative_path(parent_path, path)
		if path then
			-- Some normalization
			parent_path = parent_path:gsub("%"..path_sep.."+$", "");
			path = path:gsub("^%.%"..path_sep.."+", "");
			
			local is_relative;
			if path_sep == "/" and path:sub(1,1) ~= "/" then
				is_relative = true;
			elseif path_sep == "\\" and (path:sub(1,1) ~= "/" and (path:sub(2,3) ~= ":\\" and path:sub(2,3) ~= ":/")) then
				is_relative = true;
			end
			if is_relative then
				return parent_path..path_sep..path;
			end
		end
		return path;
	end	
end

-- Helper function to convert a glob to a Lua pattern
local function glob_to_pattern(glob)
	return "^"..glob:gsub("[%p*?]", function (c)
		if c == "*" then
			return ".*";
		elseif c == "?" then
			return ".";
		else
			return "%"..c;
		end
	end).."$";
end

function load(filename, format)
	format = format or filename:match("%w+$");

	if parsers[format] and parsers[format].load then
		local f, err = io.open(filename);
		if f then
			local new_config = setmetatable({ ["*"] = { } }, config_mt);
			local ok, err = parsers[format].load(f:read("*a"), filename, new_config);
			f:close();
			if ok then
				config = new_config;
				fire_event("config-reloaded", {
					filename = filename,
					format = format,
					config = config
				});
			end
			return ok, "parser", err;
		end
		return f, "file", err;
	end

	if not format then
		return nil, "file", "no parser specified";
	else
		return nil, "file", "no parser for "..(format);
	end
end

function save(filename, format)
end

function addparser(format, parser)
	if format and parser then
		parsers[format] = parser;
	end
end

-- _M needed to avoid name clash with local 'parsers'
function _M.parsers()
	local p = {};
	for format in pairs(parsers) do
		table.insert(p, format);
	end
	return p;
end

-- Built-in Lua parser
do
	local pcall, setmetatable = _G.pcall, _G.setmetatable;
	local rawget = _G.rawget;
	parsers.lua = {};
	function parsers.lua.load(data, config_file, config)
		local env;
		-- The ' = true' are needed so as not to set off __newindex when we assign the functions below
		env = setmetatable({
			Host = true, host = true, VirtualHost = true,
			Component = true, component = true,
			Include = true, include = true, RunScript = true }, {
				__index = function (t, k)
					return rawget(_G, k);
				end,
				__newindex = function (t, k, v)
					set(config, env.__currenthost or "*", k, v);
				end
		});
		
		rawset(env, "__currenthost", "*") -- Default is global
		function env.VirtualHost(name)
			if rawget(config, name) and rawget(config[name], "component_module") then
				error(format("Host %q clashes with previously defined %s Component %q, for services use a sub-domain like conference.%s",
					name, config[name].component_module:gsub("^%a+$", { component = "external", muc = "MUC"}), name, name), 0);
			end
			rawset(env, "__currenthost", name);
			-- Needs at least one setting to logically exist :)
			set(config, name or "*", "defined", true);
			return function (config_options)
				rawset(env, "__currenthost", "*"); -- Return to global scope
				for option_name, option_value in pairs(config_options) do
					set(config, name or "*", option_name, option_value);
				end
			end;
		end
		env.Host, env.host = env.VirtualHost, env.VirtualHost;
		
		function env.Component(name)
			if rawget(config, name) and rawget(config[name], "defined") and not rawget(config[name], "component_module") then
				error(format("Component %q clashes with previously defined Host %q, for services use a sub-domain like conference.%s",
					name, name, name), 0);
			end
			set(config, name, "component_module", "component");
			-- Don't load the global modules by default
			set(config, name, "load_global_modules", false);
			rawset(env, "__currenthost", name);
			local function handle_config_options(config_options)
				rawset(env, "__currenthost", "*"); -- Return to global scope
				for option_name, option_value in pairs(config_options) do
					set(config, name or "*", option_name, option_value);
				end
			end
	
			return function (module)
					if type(module) == "string" then
						set(config, name, "component_module", module);
						return handle_config_options;
					end
					return handle_config_options(module);
				end
		end
		env.component = env.Component;
		
		function env.Include(file)
			if file:match("[*?]") then
				local path_pos, glob = file:match("()([^"..path_sep.."]+)$");
				local path = file:sub(1, math_max(path_pos-2,0));
				local config_path = config_file:gsub("[^"..path_sep.."]+$", "");
				if #path > 0 then
					path = resolve_relative_path(config_path, path);
				else
					path = config_path;
				end
				local patt = glob_to_pattern(glob);
				for f in lfs.dir(path) do
					if f:sub(1,1) ~= "." and f:match(patt) then
						env.Include(path..path_sep..f);
					end
				end
			else
				local file = resolve_relative_path(config_file:gsub("[^"..path_sep.."]+$", ""), file);
				local f, err = io.open(file);
				if f then
					local ret, err = parsers.lua.load(f:read("*a"), file, config);
					if not ret then error(err:gsub("%[string.-%]", file), 0); end
				end
				if not f then error("Error loading included "..file..": "..err, 0); end
				return f, err;
			end
		end
		env.include = env.Include;
		
		function env.RunScript(file)
			return dofile(resolve_relative_path(config_file:gsub("[^"..path_sep.."]+$", ""), file));
		end
		
		local chunk, err = envload(data, "@"..config_file, env);
		
		if not chunk then
			return nil, err;
		end
		
		local ok, err = pcall(chunk);
		
		if not ok then
			return nil, err;
		end
		
		return true;
	end
	
end

return _M;
