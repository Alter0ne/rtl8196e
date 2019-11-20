#!/bin/sh
#checktunnel.sh,v 1.3 2004/02/11 04:52:55 blanchet Exp
#
#
# This source code copyright (c) Hexago Inc. 2002-2004.
#
# This program is free software; you can redistribute it and/or modify it 
# under the terms of the GNU General Public License (GPL) Version 2, 
# June 1991 as published by the Free  Software Foundation.
#
# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY;  without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License 
# along with this program; see the file GPL_LICENSE.txt. If not, write 
# to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307 USA
#
#TAG : Display tunnel information
# <TSP_TUNNEL_INTERFACE> <TSP_CLIENT_ADDRESS_IPV4> <TSP_SERVER_ADDRESS_IPV4> <TSP_CLIENT_ADDRESS_IPV6> <TSP_SERVER_ADDRESS_IPV6> 
# <TSP_TUNNEL_PREFIXLEN> <TSP_HOME_INTERFACE>  <TSP_PREFIX> <TSP_PREFIXLEN>  <TSP_TUNNEL_MODE>
#
#
# Program localization 

cat <<EOT
Host type: [$TSP_HOST_TYPE]

Tunnel end-points:
Local interface: [$TSP_TUNNEL_INTERFACE]

Client:	v4[$TSP_CLIENT_ADDRESS_IPV4]
	v6[$TSP_CLIENT_ADDRESS_IPV6/$TSP_TUNNEL_PREFIXLEN]

Server:	v4[$TSP_SERVER_ADDRESS_IPV4]
	v6[$TSP_SERVER_ADDRESS_IPV6/$TSP_TUNNEL_PREFIXLEN]

Tunnel mode: [$TSP_TUNNEL_MODE]

EOT

if [ X"${TSP_HOST_TYPE}" = X"ROUTER" -o X"${TSP_HOST_TYPE}" = X"router" ]; then

cat <<EOT
Routing information:
Home network interface: [$TSP_HOME_INTERFACE]
  (for prefix advertisement)

Prefix: [$TSP_PREFIX/$TSP_PREFIXLEN]

EOT
fi

return 0
