[ -n "${FLAGS_VERSION:-}" ] && return 0
FLAGS_VERSION='1.0.3'

FLAGS_TRUE=0
FLAGS_FALSE=1
FLAGS_ERROR=2

FLAGS_RESERVED='ARGC ARGV ERROR FALSE HELP PARENT RESERVED TRUE VERSION'

_flags_debug() { echo "flags:DEBUG $@" >&2; }
_flags_warn() { echo "flags:WARN $@" >&2; }
_flags_error() { echo "flags:ERROR $@" >&2; }
_flags_fatal() { echo "flags:FATAL $@" >&2; }

if [ -n "${ZSH_VERSION:-}" ]; then
  setopt |grep "^shwordsplit$" >/dev/null
  if [ $? -ne ${FLAGS_TRUE} ]; then
    _flags_fatal 'zsh shwordsplit option is required for proper zsh operation'
    exit ${FLAGS_ERROR}
  fi
  if [ -z "${FLAGS_PARENT:-}" ]; then
    _flags_fatal "zsh does not pass \$0 through properly. please declare' \
\"FLAGS_PARENT=\$0\" before calling shFlags"
    exit ${FLAGS_ERROR}
  fi
fi


__FLAGS_GETOPT_VERS_STD=0
__FLAGS_GETOPT_VERS_ENH=1
__FLAGS_GETOPT_VERS_BSD=2

getopt >/dev/null 2>&1
case $? in
  0) __FLAGS_GETOPT_VERS=${__FLAGS_GETOPT_VERS_STD} ;;
  2)
    if [ "`getopt --version`" = '-- ' ]; then
      __FLAGS_GETOPT_VERS=${__FLAGS_GETOPT_VERS_STD}
    else
      __FLAGS_GETOPT_VERS=${__FLAGS_GETOPT_VERS_ENH}
    fi
    ;;
  *)
    _flags_fatal 'unable to determine getopt version'
    exit ${FLAGS_ERROR}
    ;;
esac

__FLAGS_OPTSTR_SHORT=0
__FLAGS_OPTSTR_LONG=1

__FLAGS_NULL='~'

__FLAGS_INFO_DEFAULT='default'
__FLAGS_INFO_HELP='help'
__FLAGS_INFO_SHORT='short'
__FLAGS_INFO_TYPE='type'

__FLAGS_LEN_SHORT=0
__FLAGS_LEN_LONG=1

__FLAGS_TYPE_NONE=0
__FLAGS_TYPE_BOOLEAN=1
__FLAGS_TYPE_FLOAT=2
__FLAGS_TYPE_INTEGER=3
__FLAGS_TYPE_STRING=4

__flags_constants=`set |awk -F= '/^FLAGS_/ || /^__FLAGS_/ {print $1}'`
for __flags_const in ${__flags_constants}; do
  case ${__flags_const} in
    FLAGS_HELP) continue ;;
    FLAGS_PARENT) continue ;;
  esac
  if [ -z "${ZSH_VERSION:-}" ]; then
    readonly ${__flags_const}
  else
    case ${ZSH_VERSION} in
      [123].*) readonly ${__flags_const} ;;
      *) readonly -g ${__flags_const} ;;
    esac
  fi
done
unset __flags_const __flags_constants


__flags_boolNames=' '
__flags_longNames=' '
__flags_shortNames=' '
__flags_columns=''
__flags_opts=''

_flags_define()
{
  if [ $# -lt 4 ]; then
    flags_error='DEFINE error: too few arguments'
    flags_return=${FLAGS_ERROR}
    _flags_error "${flags_error}"
    return ${flags_return}
  fi

  _flags_type_=$1
  _flags_name_=$2
  _flags_default_=$3
  _flags_help_=$4
  _flags_short_=${5:-${__FLAGS_NULL}}

  _flags_return_=${FLAGS_TRUE}


  echo " ${FLAGS_RESERVED} " |grep " ${_flags_name_} " >/dev/null
  if [ $? -eq 0 ]; then
    flags_error="flag name (${_flags_name_}) is reserved"
    _flags_return_=${FLAGS_ERROR}
  fi

  if [ ${_flags_return_} -eq ${FLAGS_TRUE} \
      -a ${__FLAGS_GETOPT_VERS} -ne ${__FLAGS_GETOPT_VERS_ENH} \
      -a "${_flags_short_}" = "${__FLAGS_NULL}" ]
  then
    flags_error="short flag required for (${_flags_name_}) on this platform"
    _flags_return_=${FLAGS_ERROR}
  fi

  if [ ${_flags_return_} -eq ${FLAGS_TRUE} ]; then
    if _flags_itemInList "${_flags_name_}" \
        ${__flags_longNames} ${__flags_boolNames}
    then
      flags_error="flag name ([no]${_flags_name_}) already defined"
      _flags_warn "${flags_error}"
      _flags_return_=${FLAGS_FALSE}
    fi
  fi

  if [ ${_flags_return_} -eq ${FLAGS_TRUE} \
      -a "${_flags_short_}" != "${__FLAGS_NULL}" ]
  then
    if _flags_itemInList "${_flags_short_}" ${__flags_shortNames}; then
      flags_error="flag short name (${_flags_short_}) already defined"
      _flags_warn "${flags_error}"
      _flags_return_=${FLAGS_FALSE}
    fi
  fi

  if [ ${_flags_return_} -eq ${FLAGS_TRUE} ]; then
    case ${_flags_type_} in
      ${__FLAGS_TYPE_BOOLEAN})
        if _flags_validateBoolean "${_flags_default_}"; then
          case ${_flags_default_} in
            true|t|0) _flags_default_=${FLAGS_TRUE} ;;
            false|f|1) _flags_default_=${FLAGS_FALSE} ;;
          esac
        else
          flags_error="invalid default flag value '${_flags_default_}'"
          _flags_return_=${FLAGS_ERROR}
        fi
        ;;

      ${__FLAGS_TYPE_FLOAT})
        if _flags_validateFloat "${_flags_default_}"; then
          :
        else
          flags_error="invalid default flag value '${_flags_default_}'"
          _flags_return_=${FLAGS_ERROR}
        fi
        ;;

      ${__FLAGS_TYPE_INTEGER})
        if _flags_validateInteger "${_flags_default_}"; then
          :
        else
          flags_error="invalid default flag value '${_flags_default_}'"
          _flags_return_=${FLAGS_ERROR}
        fi
        ;;

      ${__FLAGS_TYPE_STRING}) ;;

      *)
        flags_error="unrecognized flag type '${_flags_type_}'"
        _flags_return_=${FLAGS_ERROR}
        ;;
    esac
  fi

  if [ ${_flags_return_} -eq ${FLAGS_TRUE} ]; then
    eval "FLAGS_${_flags_name_}='${_flags_default_}'"
    eval "__flags_${_flags_name_}_${__FLAGS_INFO_TYPE}=${_flags_type_}"
    eval "__flags_${_flags_name_}_${__FLAGS_INFO_DEFAULT}=\
\"${_flags_default_}\""
    eval "__flags_${_flags_name_}_${__FLAGS_INFO_HELP}=\"${_flags_help_}\""
    eval "__flags_${_flags_name_}_${__FLAGS_INFO_SHORT}='${_flags_short_}'"

    __flags_longNames="${__flags_longNames}${_flags_name_} "
    __flags_shortNames="${__flags_shortNames}${_flags_short_} "
    [ ${_flags_type_} -eq ${__FLAGS_TYPE_BOOLEAN} ] && \
        __flags_boolNames="${__flags_boolNames}no${_flags_name_} "
  fi

  flags_return=${_flags_return_}
  unset _flags_default_ _flags_help_ _flags_name_ _flags_return_ _flags_short_ \
      _flags_type_
  [ ${flags_return} -eq ${FLAGS_ERROR} ] && _flags_error "${flags_error}"
  return ${flags_return}
}

_flags_genOptStr()
{
  _flags_optStrType_=$1

  _flags_opts_=''

  for _flags_flag_ in ${__flags_longNames}; do
    _flags_type_=`_flags_getFlagInfo ${_flags_flag_} ${__FLAGS_INFO_TYPE}`
    case ${_flags_optStrType_} in
      ${__FLAGS_OPTSTR_SHORT})
        _flags_shortName_=`_flags_getFlagInfo \
            ${_flags_flag_} ${__FLAGS_INFO_SHORT}`
        if [ "${_flags_shortName_}" != "${__FLAGS_NULL}" ]; then
          _flags_opts_="${_flags_opts_}${_flags_shortName_}"
          [ ${_flags_type_} -ne ${__FLAGS_TYPE_BOOLEAN} ] && \
              _flags_opts_="${_flags_opts_}:"
        fi
        ;;

      ${__FLAGS_OPTSTR_LONG})
        _flags_opts_="${_flags_opts_:+${_flags_opts_},}${_flags_flag_}"
        [ ${_flags_type_} -ne ${__FLAGS_TYPE_BOOLEAN} ] && \
            _flags_opts_="${_flags_opts_}:"
        ;;
    esac
  done

  echo "${_flags_opts_}"
  unset _flags_flag_ _flags_opts_ _flags_optStrType_ _flags_shortName_ \
      _flags_type_
  return ${FLAGS_TRUE}
}

_flags_getFlagInfo()
{
  _flags_name_=$1
  _flags_info_=$2

  _flags_nameVar_="__flags_${_flags_name_}_${_flags_info_}"
  _flags_strToEval_="_flags_value_=\"\${${_flags_nameVar_}:-}\""
  eval "${_flags_strToEval_}"
  if [ -n "${_flags_value_}" ]; then
    flags_return=${FLAGS_TRUE}
  else
    _flags_typeVar_="__flags_${_flags_name_}_${__FLAGS_INFO_TYPE}"
    _flags_strToEval_="_flags_type_=\"\${${_flags_typeVar_}:-}\""
    eval "${_flags_strToEval_}"
    if [ "${_flags_type_}" = "${__FLAGS_TYPE_STRING}" ]; then
      flags_return=${FLAGS_TRUE}
    else
      flags_return=${FLAGS_ERROR}
      flags_error="invalid flag name (${_flags_nameVar_})"
    fi
  fi

  echo "${_flags_value_}"
  unset _flags_info_ _flags_name_ _flags_strToEval_ _flags_type_ _flags_value_ \
      _flags_nameVar_ _flags_typeVar_
  [ ${flags_return} -eq ${FLAGS_ERROR} ] && _flags_error "${flags_error}"
  return ${flags_return}
}

_flags_itemInList()
{
  _flags_str_=$1
  shift

  echo " ${*:-} " |grep " ${_flags_str_} " >/dev/null
  if [ $? -eq 0 ]; then
    flags_return=${FLAGS_TRUE}
  else
    flags_return=${FLAGS_FALSE}
  fi

  unset _flags_str_
  return ${flags_return}
}

_flags_columns()
{
  if [ -z "${__flags_columns}" ]; then
    if eval stty size >/dev/null 2>&1; then
      set -- `stty size`
      __flags_columns=$2
    elif eval tput cols >/dev/null 2>&1; then
      set -- `tput cols`
      __flags_columns=$1
    else
      __flags_columns=80
    fi
  fi
  echo ${__flags_columns}
}

_flags_validateBoolean()
{
  _flags_bool_=$1

  flags_return=${FLAGS_TRUE}
  case "${_flags_bool_}" in
    true|t|0) ;;
    false|f|1) ;;
    *) flags_return=${FLAGS_FALSE} ;;
  esac

  unset _flags_bool_
  return ${flags_return}
}

_flags_validateFloat()
{
  _flags_float_=$1

  if _flags_validateInteger ${_flags_float_}; then
    flags_return=${FLAGS_TRUE}
  else
    flags_return=${FLAGS_TRUE}
    case ${_flags_float_} in
      -*)
        _flags_test_=`expr "${_flags_float_}" : '\(-[0-9][0-9]*\.[0-9][0-9]*\)'`
        ;;
      *)
        _flags_test_=`expr "${_flags_float_}" : '\([0-9][0-9]*\.[0-9][0-9]*\)'`
        ;;
    esac
    [ "${_flags_test_}" != "${_flags_float_}" ] && flags_return=${FLAGS_FALSE}
  fi

  unset _flags_float_ _flags_test_
  return ${flags_return}
}

_flags_validateInteger()
{
  _flags_int_=$1

  flags_return=${FLAGS_TRUE}
  case ${_flags_int_} in
    -*)
      _flags_test_=`expr "${_flags_int_}" : '\(-[0-9][0-9]*\)'`
      ;;
    *)
      _flags_test_=`expr "${_flags_int_}" : '\([0-9][0-9]*\)'`
      ;;
  esac
  [ "${_flags_test_}" != "${_flags_int_}" ] && flags_return=${FLAGS_FALSE}

  unset _flags_int_ _flags_test_
  return ${flags_return}
}

_flags_getoptStandard()
{
  flags_return=${FLAGS_TRUE}
  _flags_shortOpts_=`_flags_genOptStr ${__FLAGS_OPTSTR_SHORT}`

  for _flags_opt_ in "$@"; do
    _flags_match_=`echo "x${_flags_opt_}x" |sed 's/ //g'`
    if [ "${_flags_match_}" != "x${_flags_opt_}x" ]; then
      flags_error='the available getopt does not support spaces in options'
      flags_return=${FLAGS_ERROR}
      break
    fi
  done

  if [ ${flags_return} -eq ${FLAGS_TRUE} ]; then
    __flags_opts=`getopt ${_flags_shortOpts_} $@ 2>&1`
    _flags_rtrn_=$?
    if [ ${_flags_rtrn_} -ne ${FLAGS_TRUE} ]; then
      _flags_warn "${__flags_opts}"
      flags_error='unable to parse provided options with getopt.'
      flags_return=${FLAGS_ERROR}
    fi
  fi

  unset _flags_match_ _flags_opt_ _flags_rtrn_ _flags_shortOpts_
  return ${flags_return}
}

_flags_getoptEnhanced()
{
  flags_return=${FLAGS_TRUE}
  _flags_shortOpts_=`_flags_genOptStr ${__FLAGS_OPTSTR_SHORT}`
  _flags_boolOpts_=`echo "${__flags_boolNames}" \
      |sed 's/^ *//;s/ *$//;s/ /,/g'`
  _flags_longOpts_=`_flags_genOptStr ${__FLAGS_OPTSTR_LONG}`

  __flags_opts=`getopt \
      -o ${_flags_shortOpts_} \
      -l "${_flags_longOpts_},${_flags_boolOpts_}" \
      -- "$@" 2>&1`
  _flags_rtrn_=$?
  if [ ${_flags_rtrn_} -ne ${FLAGS_TRUE} ]; then
    _flags_warn "${__flags_opts}"
    flags_error='unable to parse provided options with getopt.'
    flags_return=${FLAGS_ERROR}
  fi

  unset _flags_boolOpts_ _flags_longOpts_ _flags_rtrn_ _flags_shortOpts_
  return ${flags_return}
}

_flags_parseGetopt()
{
  _flags_argc_=$1
  shift

  flags_return=${FLAGS_TRUE}

  if [ ${__FLAGS_GETOPT_VERS} -ne ${__FLAGS_GETOPT_VERS_ENH} ]; then
    set -- $@
  else
    eval set -- "$@"
  fi

  FLAGS_ARGC=`expr $# - 1 - ${_flags_argc_}`

  while true; do
    _flags_opt_=$1
    _flags_arg_=${2:-}
    _flags_type_=${__FLAGS_TYPE_NONE}
    _flags_name_=''

    case "${_flags_opt_}" in
      --) shift; break ;;

      --*)
        _flags_opt_=`expr "${_flags_opt_}" : '--\(.*\)'`
        _flags_len_=${__FLAGS_LEN_LONG}
        if _flags_itemInList "${_flags_opt_}" ${__flags_longNames}; then
          _flags_name_=${_flags_opt_}
        else
          if _flags_itemInList "${_flags_opt_}" ${__flags_boolNames}; then
            _flags_name_=`expr "${_flags_opt_}" : 'no\(.*\)'`
            _flags_type_=${__FLAGS_TYPE_BOOLEAN}
            _flags_arg_=${__FLAGS_NULL}
          fi
        fi
        ;;

      -*)
        _flags_opt_=`expr "${_flags_opt_}" : '-\(.*\)'`
        _flags_len_=${__FLAGS_LEN_SHORT}
        if _flags_itemInList "${_flags_opt_}" ${__flags_shortNames}; then
          _flags_pos_=`echo "${__flags_shortNames}" \
              |awk 'BEGIN{RS=" ";rn=0}$0==e{rn=NR}END{print rn}' \
                  e=${_flags_opt_}`
          _flags_name_=`echo "${__flags_longNames}" \
              |awk 'BEGIN{RS=" "}rn==NR{print $0}' rn="${_flags_pos_}"`
        fi
        ;;
    esac

    if [ -z "${_flags_name_}" ]; then
      flags_error="unrecognized option (${_flags_opt_})"
      flags_return=${FLAGS_ERROR}
      break
    fi

    [ ${_flags_type_} -eq ${__FLAGS_TYPE_NONE} ] && \
        _flags_type_=`_flags_getFlagInfo \
            "${_flags_name_}" ${__FLAGS_INFO_TYPE}`
    case ${_flags_type_} in
      ${__FLAGS_TYPE_BOOLEAN})
        if [ ${_flags_len_} -eq ${__FLAGS_LEN_LONG} ]; then
          if [ "${_flags_arg_}" != "${__FLAGS_NULL}" ]; then
            eval "FLAGS_${_flags_name_}=${FLAGS_TRUE}"
          else
            eval "FLAGS_${_flags_name_}=${FLAGS_FALSE}"
          fi
        else
          _flags_strToEval_="_flags_val_=\
\${__flags_${_flags_name_}_${__FLAGS_INFO_DEFAULT}}"
          eval "${_flags_strToEval_}"
          if [ ${_flags_val_} -eq ${FLAGS_FALSE} ]; then
            eval "FLAGS_${_flags_name_}=${FLAGS_TRUE}"
          else
            eval "FLAGS_${_flags_name_}=${FLAGS_FALSE}"
          fi
        fi
        ;;

      ${__FLAGS_TYPE_FLOAT})
        if _flags_validateFloat "${_flags_arg_}"; then
          eval "FLAGS_${_flags_name_}='${_flags_arg_}'"
        else
          flags_error="invalid float value (${_flags_arg_})"
          flags_return=${FLAGS_ERROR}
          break
        fi
        ;;

      ${__FLAGS_TYPE_INTEGER})
        if _flags_validateInteger "${_flags_arg_}"; then
          eval "FLAGS_${_flags_name_}='${_flags_arg_}'"
        else
          flags_error="invalid integer value (${_flags_arg_})"
          flags_return=${FLAGS_ERROR}
          break
        fi
        ;;

      ${__FLAGS_TYPE_STRING})
        eval "FLAGS_${_flags_name_}='${_flags_arg_}'"
        ;;
    esac

    if [ "${_flags_name_}" = 'help' ]; then
      if [ ${FLAGS_help} -eq ${FLAGS_TRUE} ]; then
        flags_help
        flags_error='help requested'
        flags_return=${FLAGS_FALSE}
        break
      fi
    fi

    shift
    [ ${_flags_type_} != ${__FLAGS_TYPE_BOOLEAN} ] && shift
  done

  FLAGS_ARGV=''
  while [ $# -gt 0 ]; do
    FLAGS_ARGV="${FLAGS_ARGV:+${FLAGS_ARGV} }'$1'"
    shift
  done

  unset _flags_arg_ _flags_len_ _flags_name_ _flags_opt_ _flags_pos_ \
      _flags_strToEval_ _flags_type_ _flags_val_
  return ${flags_return}
}


DEFINE_boolean() { _flags_define ${__FLAGS_TYPE_BOOLEAN} "$@"; }

DEFINE_float()   { _flags_define ${__FLAGS_TYPE_FLOAT} "$@"; }
DEFINE_integer() { _flags_define ${__FLAGS_TYPE_INTEGER} "$@"; }
DEFINE_string()  { _flags_define ${__FLAGS_TYPE_STRING} "$@"; }

FLAGS()
{
  [ -z "${__flags_help_type:-}" ] && \
      DEFINE_boolean 'help' false 'show this help' 'h'

  if [ $# -gt 0 ]; then
    if [ ${__FLAGS_GETOPT_VERS} -ne ${__FLAGS_GETOPT_VERS_ENH} ]; then
      _flags_getoptStandard "$@"
    else
      _flags_getoptEnhanced "$@"
    fi
    flags_return=$?
  else
    __flags_opts='--'
    flags_return=${FLAGS_TRUE}
  fi

  if [ ${flags_return} -eq ${FLAGS_TRUE} ]; then
    _flags_parseGetopt $# "${__flags_opts}"
    flags_return=$?
  fi

  [ ${flags_return} -eq ${FLAGS_ERROR} ] && _flags_fatal "${flags_error}"
  return ${flags_return}
}

flags_getoptInfo()
{
  _flags_debug "uname -a: `uname -a`"
  _flags_debug "PATH: ${PATH}"

  if [ -n "${BASH_VERSION:-}" ]; then
    _flags_debug 'shell: bash'
    _flags_debug "BASH_VERSION: ${BASH_VERSION}"
  elif [ -n "${ZSH_VERSION:-}" ]; then
    _flags_debug 'shell: zsh'
    _flags_debug "ZSH_VERSION: ${ZSH_VERSION}"
  fi

  getopt >/dev/null
  _flags_getoptReturn=$?
  _flags_debug "getopt return: ${_flags_getoptReturn}"
  _flags_debug "getopt --version: `getopt --version 2>&1`"

  unset _flags_getoptReturn
}

flags_getoptIsEnh()
{
  test ${__FLAGS_GETOPT_VERS} -eq ${__FLAGS_GETOPT_VERS_ENH}
}

flags_getoptIsStd()
{
  test ${__FLAGS_GETOPT_VERS} -eq ${__FLAGS_GETOPT_VERS_STD}
}

flags_help()
{
  if [ -n "${FLAGS_HELP:-}" ]; then
    echo "${FLAGS_HELP}" >&2
  else
    echo "USAGE: ${FLAGS_PARENT:-$0} [flags] args" >&2
  fi
  if [ -n "${__flags_longNames}" ]; then
    echo 'flags:' >&2
    for flags_name_ in ${__flags_longNames}; do
      flags_flagStr_=''
      flags_boolStr_=''

      flags_default_=`_flags_getFlagInfo \
          "${flags_name_}" ${__FLAGS_INFO_DEFAULT}`
      flags_help_=`_flags_getFlagInfo \
          "${flags_name_}" ${__FLAGS_INFO_HELP}`
      flags_short_=`_flags_getFlagInfo \
          "${flags_name_}" ${__FLAGS_INFO_SHORT}`
      flags_type_=`_flags_getFlagInfo \
          "${flags_name_}" ${__FLAGS_INFO_TYPE}`

      [ "${flags_short_}" != "${__FLAGS_NULL}" ] \
          && flags_flagStr_="-${flags_short_}"

      if [ ${__FLAGS_GETOPT_VERS} -eq ${__FLAGS_GETOPT_VERS_ENH} ]; then
        [ "${flags_short_}" != "${__FLAGS_NULL}" ] \
            && flags_flagStr_="${flags_flagStr_},"
        [ ${flags_type_} -eq ${__FLAGS_TYPE_BOOLEAN} ] \
            && flags_boolStr_='[no]'
        flags_flagStr_="${flags_flagStr_}--${flags_boolStr_}${flags_name_}:"
      fi

      case ${flags_type_} in
        ${__FLAGS_TYPE_BOOLEAN})
          if [ ${flags_default_} -eq ${FLAGS_TRUE} ]; then
            flags_defaultStr_='true'
          else
            flags_defaultStr_='false'
          fi
          ;;
        ${__FLAGS_TYPE_FLOAT}|${__FLAGS_TYPE_INTEGER})
          flags_defaultStr_=${flags_default_} ;;
        ${__FLAGS_TYPE_STRING}) flags_defaultStr_="'${flags_default_}'" ;;
      esac
      flags_defaultStr_="(default: ${flags_defaultStr_})"

      flags_helpStr_="  ${flags_flagStr_}  ${flags_help_} ${flags_defaultStr_}"
      flags_helpStrLen_=`expr "${flags_helpStr_}" : '.*'`
      flags_columns_=`_flags_columns`
      if [ ${flags_helpStrLen_} -lt ${flags_columns_} ]; then
        echo "${flags_helpStr_}" >&2
      else
        echo "  ${flags_flagStr_}  ${flags_help_}" >&2
        flags_emptyStr_="`echo \"x${flags_flagStr_}x\" \
            |awk '{printf "%"length($0)-2"s", ""}'`"
        flags_helpStr_="  ${flags_emptyStr_}  ${flags_defaultStr_}"
        flags_helpStrLen_=`expr "${flags_helpStr_}" : '.*'`
        if [ ${__FLAGS_GETOPT_VERS} -eq ${__FLAGS_GETOPT_VERS_STD} \
            -o ${flags_helpStrLen_} -lt ${flags_columns_} ]; then
          echo "${flags_helpStr_}" >&2
        else
          echo "    ${flags_defaultStr_}" >&2
        fi
      fi
    done
  fi

  unset flags_boolStr_ flags_default_ flags_defaultStr_ flags_emptyStr_ \
      flags_flagStr_ flags_help_ flags_helpStr flags_helpStrLen flags_name_ \
      flags_columns_ flags_short_ flags_type_
  return ${FLAGS_TRUE}
}

flags_reset()
{
  for flags_name_ in ${__flags_longNames}; do
    flags_strToEval_="unset FLAGS_${flags_name_}"
    for flags_type_ in \
        ${__FLAGS_INFO_DEFAULT} \
        ${__FLAGS_INFO_HELP} \
        ${__FLAGS_INFO_SHORT} \
        ${__FLAGS_INFO_TYPE}
    do
      flags_strToEval_=\
"${flags_strToEval_} __flags_${flags_name_}_${flags_type_}"
    done
    eval ${flags_strToEval_}
  done

  __flags_boolNames=' '
  __flags_longNames=' '
  __flags_shortNames=' '

  unset flags_name_ flags_type_ flags_strToEval_
}
