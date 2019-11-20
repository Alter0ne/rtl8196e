%% -------------------------------------------------------------------
%% This is a generated file.
%% -------------------------------------------------------------------

-module(diameter_gen_doic_rfc7683).

-compile({parse_transform, diameter_exprecs}).

-compile(nowarn_unused_function).

-export_records(['diameter_doic_OC-Supported-Features',
		 'diameter_doic_OC-OLR']).

-record('diameter_doic_OC-Supported-Features',
	{'OC-Feature-Vector' = [], 'AVP' = []}).

-record('diameter_doic_OC-OLR',
	{'OC-Sequence-Number', 'OC-Report-Type',
	 'OC-Reduction-Percentage' = [],
	 'OC-Validity-Duration' = [], 'AVP' = []}).

-export([name/0, id/0, vendor_id/0, vendor_name/0,
	 decode_avps/3, encode_avps/3, grouped_avp/4, msg_name/2,
	 msg_header/1, rec2msg/1, msg2rec/1, name2rec/1,
	 avp_name/2, avp_arity/1, avp_arity/2, avp_header/1,
	 avp/4, enumerated_avp/3, empty_value/2, dict/0]).

-include_lib("diameter/include/diameter.hrl").

-include_lib("diameter/include/diameter_gen.hrl").

name() -> diameter_gen_doic_rfc7683.

id() -> erlang:error(badarg).

vendor_id() -> erlang:error(undefined).

vendor_name() -> erlang:error(undefined).

msg_name(_, _) -> ''.

msg_header(_) -> erlang:error(badarg).

rec2msg(_) -> erlang:error(badarg).

msg2rec(_) -> erlang:error(badarg).

name2rec('OC-Supported-Features') ->
    'diameter_doic_OC-Supported-Features';
name2rec('OC-OLR') -> 'diameter_doic_OC-OLR';
name2rec(T) -> msg2rec(T).

avp_name(622, undefined) ->
    {'OC-Feature-Vector', 'Unsigned64'};
avp_name(623, undefined) -> {'OC-OLR', 'Grouped'};
avp_name(627, undefined) ->
    {'OC-Reduction-Percentage', 'Unsigned32'};
avp_name(626, undefined) ->
    {'OC-Report-Type', 'Enumerated'};
avp_name(624, undefined) ->
    {'OC-Sequence-Number', 'Unsigned64'};
avp_name(621, undefined) ->
    {'OC-Supported-Features', 'Grouped'};
avp_name(625, undefined) ->
    {'OC-Validity-Duration', 'Unsigned32'};
avp_name(_, _) -> 'AVP'.

avp_arity('OC-Supported-Features') ->
    [{'OC-Feature-Vector', {0, 1}}, {'AVP', {0, '*'}}];
avp_arity('OC-OLR') ->
    [{'OC-Sequence-Number', 1}, {'OC-Report-Type', 1},
     {'OC-Reduction-Percentage', {0, 1}},
     {'OC-Validity-Duration', {0, 1}}, {'AVP', {0, '*'}}];
avp_arity(_) -> erlang:error(badarg).

avp_arity('OC-Supported-Features',
	  'OC-Feature-Vector') ->
    {0, 1};
avp_arity('OC-Supported-Features', 'AVP') -> {0, '*'};
avp_arity('OC-OLR', 'OC-Sequence-Number') -> 1;
avp_arity('OC-OLR', 'OC-Report-Type') -> 1;
avp_arity('OC-OLR', 'OC-Reduction-Percentage') ->
    {0, 1};
avp_arity('OC-OLR', 'OC-Validity-Duration') -> {0, 1};
avp_arity('OC-OLR', 'AVP') -> {0, '*'};
avp_arity(_, _) -> 0.

avp_header('OC-Feature-Vector') -> {622, 0, undefined};
avp_header('OC-OLR') -> {623, 0, undefined};
avp_header('OC-Reduction-Percentage') ->
    {627, 0, undefined};
avp_header('OC-Report-Type') -> {626, 0, undefined};
avp_header('OC-Sequence-Number') -> {624, 0, undefined};
avp_header('OC-Supported-Features') ->
    {621, 0, undefined};
avp_header('OC-Validity-Duration') ->
    {625, 0, undefined};
avp_header(_) -> erlang:error(badarg).

avp(T, Data, 'OC-Feature-Vector', Opts) ->
    diameter_types:'Unsigned64'(T, Data, Opts);
avp(T, Data, 'OC-OLR', Opts) ->
    grouped_avp(T, 'OC-OLR', Data, Opts);
avp(T, Data, 'OC-Reduction-Percentage', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(T, Data, 'OC-Report-Type', _) ->
    enumerated_avp(T, 'OC-Report-Type', Data);
avp(T, Data, 'OC-Sequence-Number', Opts) ->
    diameter_types:'Unsigned64'(T, Data, Opts);
avp(T, Data, 'OC-Supported-Features', Opts) ->
    grouped_avp(T, 'OC-Supported-Features', Data, Opts);
avp(T, Data, 'OC-Validity-Duration', Opts) ->
    diameter_types:'Unsigned32'(T, Data, Opts);
avp(_, _, _, _) -> erlang:error(badarg).

enumerated_avp(decode, 'OC-Report-Type',
	       <<0, 0, 0, 0>>) ->
    0;
enumerated_avp(encode, 'OC-Report-Type', 0) ->
    <<0, 0, 0, 0>>;
enumerated_avp(decode, 'OC-Report-Type',
	       <<0, 0, 0, 1>>) ->
    1;
enumerated_avp(encode, 'OC-Report-Type', 1) ->
    <<0, 0, 0, 1>>;
enumerated_avp(_, _, _) -> erlang:error(badarg).

empty_value('OC-Supported-Features', Opts) ->
    empty_group('OC-Supported-Features', Opts);
empty_value('OC-OLR', Opts) ->
    empty_group('OC-OLR', Opts);
empty_value('OC-Report-Type', _) -> <<0, 0, 0, 0>>;
empty_value(Name, Opts) -> empty(Name, Opts).

dict() ->
    [1,
     {avp_types,
      [{"OC-Feature-Vector", 622, "Unsigned64", []},
       {"OC-OLR", 623, "Grouped", []},
       {"OC-Reduction-Percentage", 627, "Unsigned32", []},
       {"OC-Report-Type", 626, "Enumerated", []},
       {"OC-Sequence-Number", 624, "Unsigned64", []},
       {"OC-Supported-Features", 621, "Grouped", []},
       {"OC-Validity-Duration", 625, "Unsigned32", []}]},
     {avp_vendor_id, []}, {codecs, []}, {command_codes, []},
     {custom_types, []}, {define, []},
     {enum,
      [{"OC-Report-Type",
	[{"HOST_REPORT", 0}, {"REALM_REPORT", 1}]}]},
     {grouped,
      [{"OC-Supported-Features", 621, [],
	[["OC-Feature-Vector"], {'*', ["AVP"]}]},
       {"OC-OLR", 623, [],
	[{{"OC-Sequence-Number"}}, {{"OC-Report-Type"}},
	 ["OC-Reduction-Percentage"], ["OC-Validity-Duration"],
	 {'*', ["AVP"]}]}]},
     {import_avps, []}, {import_enums, []},
     {import_groups, []}, {inherits, []}, {messages, []},
     {name, "diameter_gen_doic_rfc7683"},
     {prefix, "diameter_doic"}].


