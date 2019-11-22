--[[

Luci statistics - interface plugin diagram definition
(c) 2008 Freifunk Leipzig / Jo-Philipp Wich <xm@leipzig.freifunk.net>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

$Id$

]]--

module("luci.statistics.rrdtool.definitions.interface", package.seeall)

function rrdargs( graph, plugin, plugin_instance )

	--
	-- traffic diagram
	--
	local traffic = {

		-- draw this diagram for each data instance
		per_instance = true,
		title = "%H: Transfer on %di",
		vlabel = "Bytes/s",

		-- diagram data description
		data = {
			-- defined sources for data types, if ommitted assume a single DS named "value" (optional)
			sources = {
				if_octets = { "tx", "rx" }
			},

			-- special options for single data lines
			options = {
				if_octets__tx = {
					total = true,		-- report total amount of bytes
					color = "00ff00",	-- tx is green
					title = "Bytes (TX)"
				},

				if_octets__rx = {
					flip  = true,		-- flip rx line
					total = true,		-- report total amount of bytes
					color = "0000ff",	-- rx is blue
					title = "Bytes (RX)"
				}
			}
		}
	}


	--
	-- packet diagram
	--
	local packets = {

		-- draw this diagram for each data instance
		per_instance = true,
		title = "%H: Packets on %di",
		vlabel = "Packets/s",

		-- diagram data description
		data = {
			-- data type order
			types = { "if_packets", "if_errors" },

			-- defined sources for data types
			sources = {
				if_packets = { "tx", "rx" },
				if_errors  = { "tx", "rx" }
			},

			-- special options for single data lines
			options = {
				-- processed packets (tx DS)
				if_packets__tx = {
					overlay = true,		-- don't summarize
					total   = true,		-- report total amount of bytes
					color   = "00ff00",	-- processed tx is green
					title	= "Processed (tx)"
				},

				-- processed packets (rx DS)
				if_packets__rx = {
					overlay = true,		-- don't summarize
					flip    = true,		-- flip rx line
					total   = true,		-- report total amount of bytes
					color   = "0000ff",	-- processed rx is blue
					title	= "Processed (rx)"
				},

				-- packet errors (tx DS)
				if_errors__tx = {
					overlay = true,		-- don't summarize
					total   = true,		-- report total amount of packets
					color   = "ff5500",	-- tx errors are orange
					title	= "Errors (tx)"
				},

				-- packet errors (rx DS)
				if_errors__rx = {
					overlay = true,		-- don't summarize
					flip    = true,		-- flip rx line
					total   = true,		-- report total amount of packets
					color   = "ff0000",	-- rx errors are red
					title	= "Errors (rx)"
				}
			}
		}
	}

	return { traffic, packets }
end
