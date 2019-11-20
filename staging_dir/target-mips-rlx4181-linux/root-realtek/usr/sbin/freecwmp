#!/bin/sh
# Copyright (C) 2011-2012 Luka Perkov <freecwmp@lukaperkov.net>

. /lib/functions.sh
. /usr/share/libubox/jshn.sh
. /usr/share/shflags/shflags.sh
. /usr/share/freecwmp/defaults

# define a 'name' command-line string flag
DEFINE_boolean 'newline' false 'do not output the trailing newline' 'n'
DEFINE_boolean 'value' false 'output values only' 'v'
DEFINE_boolean 'empty' false 'output empty parameters' 'e'
DEFINE_boolean 'last' false 'output only last line ; for parameters that tend to have huge output' 'l'
DEFINE_boolean 'debug' false 'give debug output' 'd'
DEFINE_boolean 'dummy' false 'echo system commands' 'D'
DEFINE_boolean 'force' false 'force getting values for certain parameters' 'f'
DEFINE_string 'url' '' 'file to download [download only]' 'u'
DEFINE_string 'size' '' 'size of file to download [download only]' 's'

FLAGS_HELP=`cat << EOF
USAGE: $0 [flags] command [parameter] [values]
command:
  get [value|notification|tags|all]
  set [value|notification|tag]
  download
  factory_reset
  reboot
EOF`

FLAGS "$@" || exit 1
eval set -- "${FLAGS_ARGV}"

if [ ${FLAGS_help} -eq ${FLAGS_TRUE} ]; then
	exit 1
fi

if [ ${FLAGS_newline} -eq ${FLAGS_TRUE} ]; then
	ECHO_newline='-n'
fi

case "$1" in
	set)
		if [ "$2" = "notification" ]; then
			__arg1="$3"
			__arg2="$4"
			action="set_notification"
		elif [ "$2" = "tag" ]; then
			__arg1="$3"
			__arg2="$4"
			action="set_tag"
		elif [ "$2" = "value" ]; then
			__arg1="$3"
			__arg2="$4"
			action="set_value"
		else
			__arg1="$2"
			__arg2="$3"
			action="set_value"
		fi
		;;
	get)
		if [ "$2" = "notification" ]; then
			__arg1="$3"
			action="get_notification"
		elif [ "$2" = "tags" ]; then
			__arg1="$3"
			action="get_tags"
		elif [ "$2" = "value" ]; then
			__arg1="$3"
			action="get_value"
		elif [ "$2" = "all" ]; then
			__arg1="$3"
			action="get_all"
		else
			__arg1="$2"
			action="get_value"
		fi
		;;
	add)
		if [ "$2" = "object" ]; then
			__arg1="$3"
			action="add_object"
		fi
		;;
	download)
		action="download"
		;;
	factory_reset)
		action="factory_reset"
		;;
	reboot)
		action="reboot"
		;;
esac

if [ -z "$action" ]; then
	echo invalid action \'$1\'
	exit 1
fi

if [ ${FLAGS_debug} -eq ${FLAGS_TRUE} ]; then
	echo "[debug] started at \"`date`\""
fi

get_value_functions=""
set_value_functions=""
add_object_functions=""

load_script() {
	. $1 
}

load_function() {
	get_value_functions="$get_value_functions get_$1"
	set_value_functions="$set_value_functions set_$1"
	add_object_functions="$add_object_functions add_$1"
}

handle_scripts() {
	local section="$1"
	config_get prefix "$section" "prefix"
	config_list_foreach "$section" 'location' load_script
	config_list_foreach "$section" 'function' load_function
}

config_load freecwmp
config_foreach handle_scripts "scripts"

if [ "$action" = "get_value" -o "$action" = "get_all" ]; then
	if [ ${FLAGS_force} -eq ${FLAGS_FALSE} ]; then
		__tmp_arg="Device."
		# TODO: don't check only string length ; but this is only used
		#       for getting correct prefix of CWMP parameter anyway
		if [  ${#__arg1} -lt ${#__tmp_arg} ]; then
			echo "CWMP parameters usualy begin with 'InternetGatewayDevice.' or 'Device.'     "
			echo "if you want to force script execution with provided parameter use '-f' flag."
			exit -1
		fi
	fi
	for function_name in $get_value_functions
	do
		$function_name "$__arg1"
	done
fi

if [ "$action" = "set_value" ]; then
	for function_name in $set_value_functions
	do
		$function_name "$__arg1" "$__arg2"
	done
fi

if [ "$action" = "add_object" ]; then
	for function_name in $add_object_functions
	do
		$function_name "$__arg1"
	done
fi

if [ "$action" = "get_notification" -o "$action" = "get_all" ]; then
	freecwmp_get_parameter_notification "x_notification" "$__arg1"
	freecwmp_notification_output "$__arg1" "$x_notification"
fi

if [ "$action" = "set_notification" ]; then
	freecwmp_set_parameter_notification "$__arg1" "$__arg2"
	/sbin/uci -q ${UCI_CONFIG_DIR:+-c $UCI_CONFIG_DIR} commit
fi

if [ "$action" = "get_tags" -o "$action" = "get_all" ]; then
	freecwmp_get_parameter_tags "x_tags" "$__arg1"
	freecwmp_tags_output "$__arg1" "$x_tags"
fi

if [ "$action" = "set_tag" ]; then
	freecwmp_set_parameter_tag "$__arg1" "$__arg2"
	/sbin/uci -q ${UCI_CONFIG_DIR:+-c $UCI_CONFIG_DIR} commit
fi

if [ "$action" = "download" ]; then

	rm /tmp/freecwmp_download 2> /dev/null
	wget -O /tmp/freecwmp_download "${FLAGS_url}" > /dev/null 2>&1

	dl_size=`ls -l /tmp/freecwmp_download | awk '{ print $5 }'`
	if [ ! "$dl_size" -eq "${FLAGS_size}" ]; then
		rm /tmp/freecwmp_download 2> /dev/null
		exit 1
	fi
fi

if [ "$action" = "factory_reset" ]; then
	if [ ${FLAGS_dummy} -eq ${FLAGS_TRUE} ]; then
		echo "# factory_reset"
	else
		jffs2_mark_erase "rootfs_data"
		sync
		reboot
	fi
fi

if [ "$action" = "reboot" ]; then
	/sbin/uci -q ${UCI_CONFIG_DIR:+-c $UCI_CONFIG_DIR} set freecwmp.@local[0].event="boot"
	/sbin/uci -q ${UCI_CONFIG_DIR:+-c $UCI_CONFIG_DIR} commit

	if [ ${FLAGS_dummy} -eq ${FLAGS_TRUE} ]; then
		echo "# reboot"
	else
		sync
		reboot
	fi
fi

if [ ${FLAGS_debug} -eq ${FLAGS_TRUE} ]; then
	echo "[debug] exited at \"`date`\""
fi
