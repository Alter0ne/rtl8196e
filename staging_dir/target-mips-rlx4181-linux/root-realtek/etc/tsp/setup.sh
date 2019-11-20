#!/bin/sh
#linux.sh,v 1.11 2004/06/23 21:29:20 jpicard Exp
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
# Note: IPV6 support and tun Support must be enabled before calling this script.
# This is done by tspc in plateforme/linux/tsp_tun.c


LANGUAGE=C

if [ -z $TSP_VERBOSE ]; then
   TSP_VERBOSE=0
fi

KillProcess()
{
   if [ ! -z $TSP_VERBOSE ]; then
      if [ $TSP_VERBOSE -ge 2 ]; then
         echo killing $*
      fi
   fi
   PID=`ps axww | grep $1 | grep -v grep | awk '{ print $1;}'`
   echo $PID
   if [ ! -z $PID ]; then
      kill $PID
   fi
}

Display()
{
   if [ -z $TSP_VERBOSE ]; then
      return;
   fi
   if [ $TSP_VERBOSE -lt $1 ]; then
      return;
   fi
   shift
   echo "$*"
}

Exec()
{
   if [ ! -z $TSP_VERBOSE ]; then
      if [ $TSP_VERBOSE -ge 2 ]; then
         echo $*
      fi
   fi
   $* # Execute command
   if [ $? -ne 0 ]; then
      echo "Error while executing $1"
      echo "   Command: $*"
      exit 1
   fi
}

ExecNoCheck()
{
   if [ ! -z $TSP_VERBOSE ]; then
      if [ $TSP_VERBOSE -ge 2 ]; then
         echo $*
      fi
   fi
   $* # Execute command
}

# Program localization 

Display 1 "--- Start of configuration script. ---"
Display 1 "Script: " `basename $0`

ifconfig=/sbin/ifconfig
route=/sbin/route
ipconfig=/sbin/ip
rtadvd=/usr/sbin/radvd
sysctl=/sbin/sysctl
rtadvdconfigfilename=tsprtadvd.conf
rtadvdconfigfile=$TSP_HOME_DIR/$rtadvdconfigfilename

if [ -z $TSP_HOME_DIR ]; then
   echo "TSP_HOME_DIR variable not specified!;"
   exit 1
fi

if [ ! -d $TSP_HOME_DIR ]; then
   echo "Error : directory $TSP_HOME_DIR does not exist"
   exit 1
fi
#

if [ -z $TSP_HOST_TYPE ]; then
   echo Error: TSP_HOST_TYPE not defined.
   exit 1
fi

if [ X"${TSP_HOST_TYPE}" = X"host" ] || [ X"${TSP_HOST_TYPE}" = X"router" ]; then
   #
   # Configured tunnel config (IPv6) 
   Display 1 "$TSP_TUNNEL_INTERFACE setup"
   if [ X"${TSP_TUNNEL_MODE}" = X"v6v4" ]; then
      Display 1 "Setting up link to $TSP_SERVER_ADDRESS_IPV4"
      if [ -x $ipconfig ]; then
	 ExecNoCheck $ipconfig tunnel del $TSP_TUNNEL_INTERFACE
         Exec $ipconfig tunnel add $TSP_TUNNEL_INTERFACE mode sit ttl 64 remote $TSP_SERVER_ADDRESS_IPV4
      else
         Exec $ifconfig $TSP_TUNNEL_INTERFACE tunnel ::$TSP_SERVER_ADDRESS_IPV4
      fi
   fi

   Exec $ifconfig $TSP_TUNNEL_INTERFACE up

   PREF=`echo $TSP_CLIENT_ADDRESS_IPV6 | sed "s/:0*/:/g" |cut -d : -f1-2`
   OLDADDR=`$ifconfig $TSP_TUNNEL_INTERFACE | grep "inet6.* $PREF" | sed -e "s/^.*inet6 addr: //" -e "s/ Scope.*\$//"`
   if [ ! -z $OLDADDR ]; then
      Display 1 "Removing old IPv6 address $OLDADDR"
      Exec $ifconfig $TSP_TUNNEL_INTERFACE inet6 del $OLDADDR
   fi
   Display 1 "This host is: $TSP_CLIENT_ADDRESS_IPV6/$TSP_TUNNEL_PREFIXLEN"
   Exec $ifconfig $TSP_TUNNEL_INTERFACE add $TSP_CLIENT_ADDRESS_IPV6/$TSP_TUNNEL_PREFIXLEN
   Exec $ifconfig $TSP_TUNNEL_INTERFACE mtu 1280
   # 
   # Default route  
   Display 1 "Adding default route"
   ExecNoCheck $route -A inet6 del 2000::/3 2>/dev/null  # delete old gw route
   Exec $route -A inet6 add 2000::/3 dev $TSP_TUNNEL_INTERFACE
fi

# Router configuration if required
if [ X"${TSP_HOST_TYPE}" = X"router" ]; then
   Display 1 "Router configuration"
   Display 1 "Kernel setup"
   if [ X"${TSP_PREFIXLEN}" != X"64" ]; then
      #Better way on linux to avoid loop with the remaining /48?
      ExecNoCheck $route -A inet6 del $TSP_PREFIX::/$TSP_PREFIXLEN dev $TSP_HOME_INTERFACE 2>/dev/null
      Exec $route -A inet6 add $TSP_PREFIX::/$TSP_PREFIXLEN dev $TSP_HOME_INTERFACE
   fi
   Exec $sysctl -q -w net.ipv6.conf.all.forwarding=1 # ipv6_forwarding enabled
   Display 1 "Adding prefix to $TSP_HOME_INTERFACE"
   OLDADDR=`$ifconfig $TSP_HOME_INTERFACE | grep "inet6.* $PREF" | sed -e "s/^.*inet6 addr: //" -e "s/ Scope.*\$//"`
   if [ ! -z $OLDADDR ]; then
      Display 1 "Removing old IPv6 address $OLDADDR"
      Exec $ifconfig $TSP_HOME_INTERFACE inet6 del $OLDADDR
   fi
   Exec $ifconfig $TSP_HOME_INTERFACE add $TSP_PREFIX::1/64
#   # Router advertisement configuration 
#   Display 1 "Create new $rtadvdconfigfile"
#   echo "##### rtadvd.conf made by TSP ####" > "$rtadvdconfigfile"
#   echo "interface $TSP_HOME_INTERFACE" >> "$rtadvdconfigfile"
#   echo "{" >> "$rtadvdconfigfile"
#   echo " AdvSendAdvert on;" >> "$rtadvdconfigfile"
#   echo " prefix $TSP_PREFIX::/64" >> "$rtadvdconfigfile"
#   echo " {" >> "$rtadvdconfigfile"
#   echo " AdvOnLink on;" >> "$rtadvdconfigfile"
#   echo " AdvAutonomous on;" >> "$rtadvdconfigfile"
#   echo " };" >> "$rtadvdconfigfile"
#   echo "};" >> "$rtadvdconfigfile"
#   echo "" >> "$rtadvdconfigfile"
#   /etc/init.d/radvd stop
#   if [ -f $rtadvdconfigfile ]; then
#      KillProcess $rtadvdconfigfile
#      Exec $rtadvd -u radvd -C $rtadvdconfigfile
#      Display 1 "Starting radvd: $rtadvd -u radvd -C $rtadvdconfigfile"
#   else
#      echo "Error : file $rtadvdconfigfile not found"
#      exit 1
#   fi
fi

Display 1 "--- End of configuration script. ---"

exit 0

#---------------------------------------------------------------------
