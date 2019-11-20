%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%
%% Tests of traffic between two Diameter nodes, one client, one server.
%%

-module(diameter_traffic_SUITE).

-export([suite/0,
         all/0,
         groups/0,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% testcases
-export([start/1,
         start_services/1,
         add_transports/1,
         result_codes/1,
         send_ok/1,
         send_nok/1,
         send_eval/1,
         send_bad_answer/1,
         send_protocol_error/1,
         send_arbitrary/1,
         send_unknown/1,
         send_unknown_mandatory/1,
         send_noreply/1,
         send_unsupported/1,
         send_unsupported_app/1,
         send_error_bit/1,
         send_unsupported_version/1,
         send_long_avp_length/1,
         send_short_avp_length/1,
         send_zero_avp_length/1,
         send_invalid_avp_length/1,
         send_invalid_reject/1,
         send_unrecognized_mandatory/1,
         send_long/1,
         send_nopeer/1,
         send_noapp/1,
         send_discard/1,
         send_any_1/1,
         send_any_2/1,
         send_all_1/1,
         send_all_2/1,
         send_timeout/1,
         send_error/1,
         send_detach/1,
         send_encode_error/1,
         send_destination_1/1,
         send_destination_2/1,
         send_destination_3/1,
         send_destination_4/1,
         send_destination_5/1,
         send_destination_6/1,
         send_bad_option_1/1,
         send_bad_option_2/1,
         send_bad_filter_1/1,
         send_bad_filter_2/1,
         send_bad_filter_3/1,
         send_bad_filter_4/1,
         send_multiple_filters_1/1,
         send_multiple_filters_2/1,
         send_multiple_filters_3/1,
         send_anything/1,
         outstanding/1,
         remove_transports/1,
         empty/1,
         stop_services/1,
         stop/1]).

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/6, pick_peer/7,
         prepare_request/5, prepare_request/6,
         prepare_retransmit/5,
         handle_answer/6, handle_answer/7,
         handle_error/6,
         handle_request/3]).

-include("diameter.hrl").
-include("diameter_gen_base_rfc3588.hrl").
-include("diameter_gen_base_accounting.hrl").
%% The listening transports use RFC 3588 dictionaries, the client
%% transports use either 3588 or 6733. (So can't use the record
%% definitions in the latter case.)

%% ===========================================================================

-define(util, diameter_util).

-define(A, list_to_atom).
-define(L, atom_to_list).

%% Don't use is_record/2 since dictionary hrl's aren't included.
%% (Since they define conflicting reqcords with the same names.)
-define(is_record(Rec, Name), (Name == element(1, Rec))).

-define(ADDR, {127,0,0,1}).

-define(CLIENT, "CLIENT").
-define(SERVER, "SERVER").
-define(REALM, "erlang.org").
-define(HOST(Host, Realm), Host ++ [$.|Realm]).

-define(EXTRA, an_extra_argument).

%% Sequence mask for End-to-End and Hop-by-Hop identifiers.
-define(CLIENT_MASK, {1,26}).  %% 1 in top 6 bits

%% How to construct messages, as record or list.
-define(ENCODINGS, [list, record]).

%% How to send answers, in a diameter_packet or not.
-define(CONTAINERS, [pkt, msg]).

%% Which common dictionary to use in the clients.
-define(RFCS, [rfc3588, rfc6733]).

-record(group,
        {client_encoding,
         client_dict0,
         server_encoding,
         server_container}).

%% Not really what we should be setting unless the message is sent in
%% the common application but diameter doesn't care.
-define(APP_ID, ?DIAMETER_APP_ID_COMMON).

%% An Application-ID the server doesn't support.
-define(BAD_APP, 42).

%% A common match when receiving answers in a client.
-define(answer_message(SessionId, ResultCode),
        ['answer-message',
         {'Session-Id', SessionId},
         {'Origin-Host', _},
         {'Origin-Realm', _},
         {'Result-Code', ResultCode}
         | _]).
-define(answer_message(ResultCode),
        ?answer_message(_, ResultCode)).

%% Config for diameter:start_service/2.
-define(SERVICE(Name),
        [{'Origin-Host', Name ++ "." ++ ?REALM},
         {'Origin-Realm', ?REALM},
         {'Host-IP-Address', [?ADDR]},
         {'Vendor-Id', 12345},
         {'Product-Name', "OTP/diameter"},
         {'Auth-Application-Id', [?DIAMETER_APP_ID_COMMON]},
         {'Acct-Application-Id', [?DIAMETER_APP_ID_ACCOUNTING]},
         {restrict_connections, false},
         {spawn_opt, [{min_heap_size, 5000}]}
         | [{application, [{dictionary, D},
                           {module, ?MODULE},
                           {answer_errors, callback}]}
            || D <- [diameter_gen_base_rfc3588,
                     diameter_gen_base_accounting,
                     diameter_gen_base_rfc6733,
                     diameter_gen_acct_rfc6733]]]).

-define(SUCCESS,
        ?'DIAMETER_BASE_RESULT-CODE_SUCCESS').
-define(COMMAND_UNSUPPORTED,
        ?'DIAMETER_BASE_RESULT-CODE_COMMAND_UNSUPPORTED').
-define(TOO_BUSY,
        ?'DIAMETER_BASE_RESULT-CODE_TOO_BUSY').
-define(APPLICATION_UNSUPPORTED,
        ?'DIAMETER_BASE_RESULT-CODE_APPLICATION_UNSUPPORTED').
-define(INVALID_HDR_BITS,
        ?'DIAMETER_BASE_RESULT-CODE_INVALID_HDR_BITS').
-define(INVALID_AVP_BITS,
        ?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_BITS').
-define(AVP_UNSUPPORTED,
        ?'DIAMETER_BASE_RESULT-CODE_AVP_UNSUPPORTED').
-define(UNSUPPORTED_VERSION,
        ?'DIAMETER_BASE_RESULT-CODE_UNSUPPORTED_VERSION').
-define(REALM_NOT_SERVED,
        ?'DIAMETER_BASE_RESULT-CODE_REALM_NOT_SERVED').
-define(UNABLE_TO_DELIVER,
        ?'DIAMETER_BASE_RESULT-CODE_UNABLE_TO_DELIVER').
-define(INVALID_AVP_LENGTH,
        ?'DIAMETER_BASE_RESULT-CODE_INVALID_AVP_LENGTH').

-define(EVENT_RECORD,
        ?'DIAMETER_BASE_ACCOUNTING-RECORD-TYPE_EVENT_RECORD').
-define(AUTHORIZE_ONLY,
        ?'DIAMETER_BASE_RE-AUTH-REQUEST-TYPE_AUTHORIZE_ONLY').
-define(AUTHORIZE_AUTHENTICATE,
        ?'DIAMETER_BASE_RE-AUTH-REQUEST-TYPE_AUTHORIZE_AUTHENTICATE').

-define(LOGOUT,
        ?'DIAMETER_BASE_TERMINATION-CAUSE_LOGOUT').
-define(BAD_ANSWER,
        ?'DIAMETER_BASE_TERMINATION-CAUSE_BAD_ANSWER').
-define(USER_MOVED,
        ?'DIAMETER_BASE_TERMINATION-CAUSE_USER_MOVED').

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [start, start_services, add_transports, result_codes]
        ++ [{group, ?util:name([R,D,A,C]), P} || R <- ?ENCODINGS,
                                                 D <- ?RFCS,
                                                 A <- ?ENCODINGS,
                                                 C <- ?CONTAINERS,
                                                 P <- [[], [parallel]]]
        ++ [outstanding, remove_transports, empty, stop_services, stop].

groups() ->
    Ts = tc(),
    [{?util:name([R,D,A,C]), [], Ts} || R <- ?ENCODINGS,
                                        D <- ?RFCS,
                                        A <- ?ENCODINGS,
                                        C <- ?CONTAINERS].

init_per_group(Name, Config) ->
    [R,D,A,C] = ?util:name(Name),
    G = #group{client_encoding = R,
               client_dict0 = dict0(D),
               server_encoding = A,
               server_container = C},
    [{group, G} | Config].

end_per_group(_, _) ->
    ok.

init_per_testcase(Name, Config) ->
    [{testcase, Name} | Config].

end_per_testcase(_, _) ->
    ok.

%% Testcases to run when services are started and connections
%% established.
tc() ->
    [send_ok,
     send_nok,
     send_eval,
     send_bad_answer,
     send_protocol_error,
     send_arbitrary,
     send_unknown,
     send_unknown_mandatory,
     send_noreply,
     send_unsupported,
     send_unsupported_app,
     send_error_bit,
     send_unsupported_version,
     send_long_avp_length,
     send_short_avp_length,
     send_zero_avp_length,
     send_invalid_avp_length,
     send_invalid_reject,
     send_unrecognized_mandatory,
     send_long,
     send_nopeer,
     send_noapp,
     send_discard,
     send_any_1,
     send_any_2,
     send_all_1,
     send_all_2,
     send_timeout,
     send_error,
     send_detach,
     send_encode_error,
     send_destination_1,
     send_destination_2,
     send_destination_3,
     send_destination_4,
     send_destination_5,
     send_destination_6,
     send_bad_option_1,
     send_bad_option_2,
     send_bad_filter_1,
     send_bad_filter_2,
     send_bad_filter_3,
     send_bad_filter_4,
     send_multiple_filters_1,
     send_multiple_filters_2,
     send_multiple_filters_3,
     send_anything].

%% ===========================================================================
%% start/stop testcases

start(_Config) ->
    ok = diameter:start().

start_services(_Config) ->
    ok = diameter:start_service(?SERVER, ?SERVICE(?SERVER)),
    ok = diameter:start_service(?CLIENT, [{sequence, ?CLIENT_MASK}
                                          | ?SERVICE(?CLIENT)]).

add_transports(Config) ->
    LRef = ?util:listen(?SERVER,
                        tcp,
                        [{capabilities_cb, fun capx/2},
                         {spawn_opt, [{min_heap_size, 8096}]},
                         {applications, apps(rfc3588)}]),
    Cs = [?util:connect(?CLIENT,
                        tcp,
                        LRef,
                        [{id, Id},
                         {capabilities, [{'Origin-State-Id', origin(Id)}]},
                         {applications, apps(D)}])
          || A <- ?ENCODINGS,
             C <- ?CONTAINERS,
             D <- ?RFCS,
             Id <- [{A,C}]],
    %% The server uses the client's Origin-State-Id to decide how to
    %% answer.
    ?util:write_priv(Config, "transport", [LRef | Cs]).

apps(D0) ->
    D = dict0(D0),
    [acct(D), D].

%% Ensure there are no outstanding requests in request table.
outstanding(_Config) ->
    [] = [T || T <- ets:tab2list(diameter_request),
               is_atom(element(1,T))].

remove_transports(Config) ->
    [LRef | Cs] = ?util:read_priv(Config, "transport"),
    [?util:disconnect(?CLIENT, C, ?SERVER, LRef) || C <- Cs].

stop_services(_Config) ->
    ok = diameter:stop_service(?CLIENT),
    ok = diameter:stop_service(?SERVER).

%% Ensure even transports have been removed from request table.
empty(_Config) ->
    [] = ets:tab2list(diameter_request).

stop(_Config) ->
    ok = diameter:stop().

capx(_, #diameter_caps{origin_host = {OH,DH}}) ->
    io:format("connection: ~p -> ~p~n", [DH,OH]),
    ok.

%% ===========================================================================

%% Ensure that result codes have the expected values.
result_codes(_Config) ->
    {2001, 3001, 3002, 3003, 3004, 3007, 3008, 3009, 5001, 5011, 5014}
        = {?SUCCESS,
           ?COMMAND_UNSUPPORTED,
           ?UNABLE_TO_DELIVER,
           ?REALM_NOT_SERVED,
           ?TOO_BUSY,
           ?APPLICATION_UNSUPPORTED,
           ?INVALID_HDR_BITS,
           ?INVALID_AVP_BITS,
           ?AVP_UNSUPPORTED,
           ?UNSUPPORTED_VERSION,
           ?INVALID_AVP_LENGTH}.

%% Send an ACR and expect success.
send_ok(Config) ->
    Req = ['ACR', {'Accounting-Record-Type', ?EVENT_RECORD},
                  {'Accounting-Record-Number', 1}],

    ['ACA', _SessionId, {'Result-Code', ?SUCCESS} | _]
        = call(Config, Req).

%% Send an accounting ACR that the server answers badly to.
send_nok(Config) ->
    Req = ['ACR', {'Accounting-Record-Type', ?EVENT_RECORD},
                  {'Accounting-Record-Number', 0}],

    ?answer_message(?INVALID_AVP_BITS)
        = call(Config, Req).

%% Send an ACR and expect success.
send_eval(Config) ->
    Req = ['ACR', {'Accounting-Record-Type', ?EVENT_RECORD},
                  {'Accounting-Record-Number', 3}],

    ['ACA', _SessionId, {'Result-Code', ?SUCCESS} | _]
        = call(Config, Req).

%% Send an accounting ACR that the server tries to answer with an
%% inappropriate header, resulting in no answer being sent and the
%% request timing out.
send_bad_answer(Config) ->
    Req = ['ACR', {'Accounting-Record-Type', ?EVENT_RECORD},
                  {'Accounting-Record-Number', 2}],
    {timeout, _} = call(Config, Req).

%% Send an ACR that the server callback answers explicitly with a
%% protocol error.
send_protocol_error(Config) ->
    Req = ['ACR', {'Accounting-Record-Type', ?EVENT_RECORD},
                  {'Accounting-Record-Number', 4}],

    ?answer_message(?TOO_BUSY)
        = call(Config, Req).

%% Send an ASR with an arbitrary non-mandatory AVP and expect success
%% and the same AVP in the reply.
send_arbitrary(Config) ->
    Req = ['ASR', {'AVP', [#diameter_avp{name = 'Product-Name',
                                         value = "XXX"}]}],
    ['ASA', _SessionId, {'Result-Code', ?SUCCESS} | Avps]
        = call(Config, Req),
    {'AVP', [#diameter_avp{name = 'Product-Name',
                           value = "XXX"}]}
        = lists:last(Avps).

%% Send an unknown AVP (to some client) and check that it comes back.
send_unknown(Config) ->
    Req = ['ASR', {'AVP', [#diameter_avp{code = 999,
                                         is_mandatory = false,
                                         data = <<17>>}]}],
    ['ASA', _SessionId, {'Result-Code', ?SUCCESS} | Avps]
        = call(Config, Req),
    {'AVP', [#diameter_avp{code = 999,
                           is_mandatory = false,
                           data = <<17>>}]}
        = lists:last(Avps).

%% Ditto but set the M flag.
send_unknown_mandatory(Config) ->
    Req = ['ASR', {'AVP', [#diameter_avp{code = 999,
                                         is_mandatory = true,
                                         data = <<17>>}]}],
    ['ASA', _SessionId, {'Result-Code', ?AVP_UNSUPPORTED} | Avps]
        = call(Config, Req),
    [#'diameter_base_Failed-AVP'{'AVP' = As}]
        = proplists:get_value('Failed-AVP', Avps),
    [#diameter_avp{code = 999,
                   is_mandatory = true,
                   data = <<17>>}]
        = As.

%% Send an STR that the server ignores.
send_noreply(Config) ->
    Req = ['STR', {'Termination-Cause', ?BAD_ANSWER}],
    {timeout, _} = call(Config, Req).

%% Send an unsupported command and expect 3001.
send_unsupported(Config) ->
    Req = ['STR', {'Termination-Cause', ?BAD_ANSWER}],
    ?answer_message(?COMMAND_UNSUPPORTED)
        = call(Config, Req).

%% Send an unsupported application and expect 3007.
send_unsupported_app(Config) ->
    Req = ['STR', {'Termination-Cause', ?BAD_ANSWER}],
    ?answer_message(?APPLICATION_UNSUPPORTED)
        = call(Config, Req).

%% Send a request with the E bit set and expect 3008.
send_error_bit(Config) ->
    Req = ['STR', {'Termination-Cause', ?BAD_ANSWER}],
    ?answer_message(?INVALID_HDR_BITS)
        = call(Config, Req).

%% Send a bad version and check that we get 5011.
send_unsupported_version(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    ['STA', _SessionId, {'Result-Code', ?UNSUPPORTED_VERSION} | _]
        = call(Config, Req).

%% Send a request containing an AVP length > data size.
send_long_avp_length(Config) ->
    send_invalid_avp_length(Config).

%% Send a request containing an AVP length < data size.
send_short_avp_length(Config) ->
    send_invalid_avp_length(Config).

%% Send a request containing an AVP whose advertised length is < 8.
send_zero_avp_length(Config) ->
    send_invalid_avp_length(Config).

%% Send a request containing an AVP length that doesn't match the
%% AVP's type.
send_invalid_avp_length(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],

    ['STA', _SessionId,
            {'Result-Code', ?INVALID_AVP_LENGTH},
            _OriginHost,
            _OriginRealm,
            _UserName,
            _Class,
            _ErrorMessage,
            _ErrorReportingHost,
            {'Failed-AVP', [#'diameter_base_Failed-AVP'{'AVP' = [_]}]}
          | _]
        = call(Config, Req).

%% Send a request containing 5xxx errors that the server rejects with
%% 3xxx.
send_invalid_reject(Config) ->
    Req = ['STR', {'Termination-Cause', ?USER_MOVED}],

    ?answer_message(?TOO_BUSY)
        = call(Config, Req).

%% Send an STR containing a known AVP, but one that's not allowed and
%% sets the M-bit.
send_unrecognized_mandatory(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],

    ['STA', _SessionId, {'Result-Code', ?AVP_UNSUPPORTED} | _]
        = call(Config, Req).

%% Send something long that will be fragmented by TCP.
send_long(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'User-Name', [lists:duplicate(1 bsl 20, $X)]}],
    ['STA', _SessionId, {'Result-Code', ?SUCCESS} | _]
        = call(Config, Req).

%% Send something for which pick_peer finds no suitable peer.
send_nopeer(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    {error, no_connection} = call(Config, Req, [{extra, [?EXTRA]}]).

%% Send something on an unconfigured application.
send_noapp(_Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    {error, no_connection} = diameter:call(?CLIENT, unknown_alias, Req).

%% Send something that's discarded by prepare_request.
send_discard(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    {error, unprepared} = call(Config, Req).

%% Send with a disjunctive filter.
send_any_1(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    {error, no_connection} = call(Config, Req, [{filter, {any, []}}]).
send_any_2(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Host', [?HOST(?SERVER, "unknown.org")]}],
    ?answer_message(?UNABLE_TO_DELIVER)
        = call(Config, Req, [{filter, {any, [host, realm]}}]).

%% Send with a conjunctive filter.
send_all_1(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    Realm = lists:foldr(fun(C,A) -> [C,A] end, [], ?REALM),
    ['STA', _SessionId, {'Result-Code', ?SUCCESS} | _]
        = call(Config, Req, [{filter, {all, [{host, any},
                                             {realm, Realm}]}}]).
send_all_2(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Host', [?HOST(?SERVER, "unknown.org")]}],
    {error, no_connection}
        = call(Config, Req, [{filter, {all, [host, realm]}}]).

%% Timeout before the server manages an answer.
send_timeout(Config) ->
    Req = ['RAR', {'Re-Auth-Request-Type', ?AUTHORIZE_ONLY}],
    {timeout, _} = call(Config, Req, [{timeout, 1000}]).

%% Explicitly answer with an answer-message and ensure that we
%% received the Session-Id.
send_error(Config) ->
    Req = ['RAR', {'Re-Auth-Request-Type', ?AUTHORIZE_AUTHENTICATE}],
    ?answer_message(SId, ?TOO_BUSY)
        = call(Config, Req),
    true = undefined /= SId.

%% Send a request with the detached option and receive it as a message
%% from handle_answer instead.
send_detach(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    Ref = make_ref(),
    ok = call(Config, Req, [{extra, [{self(), Ref}]}, detach]),
    Ans = receive {Ref, T} -> T end,
    ['STA', _SessionId, {'Result-Code', ?SUCCESS} | _]
        = Ans.

%% Send a request which can't be encoded and expect {error, encode}.
send_encode_error(Config) ->
    {error, encode} = call(Config, ['STR']).  %% No Termination-Cause

%% Send with filtering and expect success.
send_destination_1(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Host', [?HOST(?SERVER, ?REALM)]}],
    ['STA', _SessionId, {'Result-Code', ?SUCCESS} | _]
        = call(Config, Req, [{filter, {all, [host, realm]}}]).
send_destination_2(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    ['STA', _SessionId, {'Result-Code', ?SUCCESS} | _]
        = call(Config, Req, [{filter, {all, [host, realm]}}]).

%% Send with filtering on and expect failure when specifying an
%% unknown host or realm.
send_destination_3(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Realm', "unknown.org"}],
    {error, no_connection}
        = call(Config, Req, [{filter, {all, [host, realm]}}]).
send_destination_4(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Host', [?HOST(?SERVER, "unknown.org")]}],
    {error, no_connection}
        = call(Config, Req, [{filter, {all, [host, realm]}}]).

%% Send without filtering and expect an error answer when specifying
%% an unknown host or realm.
send_destination_5(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Realm', "unknown.org"}],
    ?answer_message(?REALM_NOT_SERVED)
        = call(Config, Req).
send_destination_6(Config) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT},
                  {'Destination-Host', [?HOST(?SERVER, "unknown.org")]}],
    ?answer_message(?UNABLE_TO_DELIVER)
        = call(Config, Req).

%% Specify an invalid option and expect failure.
send_bad_option_1(Config) ->
    send_bad_option(Config, x).
send_bad_option_2(Config) ->
    send_bad_option(Config, {extra, false}).

send_bad_option(Config, Opt) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    try call(Config, Req, [Opt]) of
        T -> erlang:error({?MODULE, ?LINE, T})
    catch
        error: _ -> ok
    end.

%% Specify an invalid filter and expect no matching peers.
send_bad_filter_1(Config) ->
    send_bad_filter(Config, {all, none}).
send_bad_filter_2(Config) ->
    send_bad_filter(Config, {host, x}).
send_bad_filter_3(Config) ->
    send_bad_filter(Config, {eval, fun() -> true end}).
send_bad_filter_4(Config) ->
    send_bad_filter(Config, {eval, {?MODULE, not_exported, []}}).

send_bad_filter(Config, F) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    {error, no_connection} = call(Config, Req, [{filter, F}]).

%% Specify multiple filter options and expect them be conjunctive.
send_multiple_filters_1(Config) ->
    Fun = fun(#diameter_caps{}) -> true end,
    ['STA', _SessionId, {'Result-Code', ?SUCCESS} | _]
        = send_multiple_filters(Config, [host, {eval, Fun}]).
send_multiple_filters_2(Config) ->
    E = {erlang, is_tuple, []},
    {error, no_connection}
        = send_multiple_filters(Config, [realm, {neg, {eval, E}}]).
send_multiple_filters_3(Config) ->
    E1 = [fun(#diameter_caps{}, ok) -> true end, ok],
    E2 = {erlang, is_tuple, []},
    E3 = {erlang, is_record, [diameter_caps]},
    E4 = [{erlang, is_record, []}, diameter_caps],
    ['STA', _SessionId, {'Result-Code', ?SUCCESS} | _]
        = send_multiple_filters(Config, [{eval, E} || E <- [E1,E2,E3,E4]]).

send_multiple_filters(Config, Fs) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    call(Config, Req, [{filter, F} || F <- Fs]).

%% Ensure that we can pass a request in any form to diameter:call/4,
%% only the return value from the prepare_request callback being
%% significant.
send_anything(Config) ->
    ['STA', _SessionId, {'Result-Code', ?SUCCESS} | _]
        = call(Config, anything).

%% ===========================================================================

call(Config, Req) ->
    call(Config, Req, []).

call(Config, Req, Opts) ->
    Name = proplists:get_value(testcase, Config),
    #group{client_encoding = ReqEncoding,
           client_dict0 = Dict0}
        = Group
        = proplists:get_value(group, Config),
    diameter:call(?CLIENT,
                  dict(Req, Dict0),
                  msg(Req, ReqEncoding, Dict0),
                  [{extra, [{Name, Group}, now()]} | Opts]).

origin({A,C}) ->
    2*codec(A) + container(C);

origin(N) ->
    {codec(N band 2), container(N rem 2)}.

%% Map booleans, but the readable atoms are part of (constructed)
%% group names, so it's good that they're readable.

codec(record) -> 0;
codec(list)   -> 1;
codec(0) -> record;
codec(_) -> list.

container(pkt) -> 0;
container(msg) -> 1;
container(0) -> pkt;
container(_) -> msg.

msg([H|_] = Msg, record = E, diameter_gen_base_rfc3588)
  when H == 'ACR';
       H == 'ACA' ->
    msg(Msg, E, diameter_gen_base_accounting);
msg([H|_] = Msg, record = E, diameter_gen_base_rfc6733)
  when H == 'ACR';
       H == 'ACA' ->
    msg(Msg, E, diameter_gen_acct_rfc6733);
msg([H|T], record, Dict) ->
    Dict:'#new-'(Dict:msg2rec(H), T);
msg(Msg, _, _) ->
    Msg.

dict0(D) ->
    ?A("diameter_gen_base_" ++ ?L(D)).

dict(Msg, Dict0)
  when 'ACR' == hd(Msg);
       'ACA' == hd(Msg);
       ?is_record(Msg, diameter_base_accounting_ACR);
       ?is_record(Msg, diameter_base_accounting_ACA) ->
    acct(Dict0);
dict(_, Dict0) ->
    Dict0.

acct(diameter_gen_base_rfc3588) ->
    diameter_gen_base_accounting;
acct(diameter_gen_base_rfc6733) ->
    diameter_gen_acct_rfc6733.

%% Set only values that aren't already.
set(_, [H|T], Vs) ->
    [H | Vs ++ T];
set(#group{client_dict0 = Dict0} = _Group, Rec, Vs) ->
    Dict = dict(Rec, Dict0),
    lists:foldl(fun({F,_} = FV, A) ->
                        set(Dict, Dict:'#get-'(F, A), FV, A)
                end,
                Rec,
                Vs).

set(Dict, E, FV, Rec)
  when E == undefined;
       E == [] ->
    Dict:'#set-'(FV, Rec);
set(_, _, _, Rec) ->
    Rec.

%% ===========================================================================
%% diameter callbacks

%% peer_up/3

peer_up(_SvcName, _Peer, State) ->
    State.

%% peer_down/3

peer_down(_SvcName, _Peer, State) ->
    State.

%% pick_peer/6-7

pick_peer(Peers, _, ?CLIENT, _State, {Name, Group}, _)
  when Name /= send_detach ->
    find(Group, Peers).

pick_peer(_Peers, _, ?CLIENT, _State, {send_nopeer, _}, _, ?EXTRA) ->
    false;

pick_peer(Peers, _, ?CLIENT, _State, {send_detach, Group}, _, {_,_}) ->
    find(Group, Peers).

find(#group{server_encoding = A, server_container = C}, Peers) ->
    Id = {A,C},
    [P] = [P || P <- Peers, id(Id, P)],
    {ok, P}.

id(Id, {Pid, _Caps}) ->
    [{ref, _}, {type, _}, {options, Opts} | _]
        = diameter:service_info(?CLIENT, Pid),
    lists:member({id, Id}, Opts).

%% prepare_request/5-6

prepare_request(_Pkt, ?CLIENT, {_Ref, _Caps}, {send_discard, _}, _) ->
    {discard, unprepared};

prepare_request(Pkt, ?CLIENT, {_Ref, Caps}, {Name, Group}, _) ->
    {send, prepare(Pkt, Caps, Name, Group)}.

prepare_request(Pkt, ?CLIENT, {_Ref, Caps}, {send_detach, Group}, _, _) ->
    {eval_packet, {send, prepare(Pkt, Caps, Group)}, [fun log/2, detach]}.

log(#diameter_packet{bin = Bin} = P, T)
  when is_binary(Bin) ->
    io:format("~p: ~p~n", [T,P]).

%% prepare/4

prepare(Pkt, Caps, N, #group{client_dict0 = Dict0} = Group)
  when N == send_long_avp_length;
       N == send_short_avp_length;
       N == send_zero_avp_length ->
    Req = prepare(Pkt, Caps, Group),
    %% Second last AVP in our STR is Auth-Application-Id of type
    %% Unsigned32: set AVP Length to a value other than 12 and place
    %% it last in the message (so as not to mess with Termination-Cause).
    #diameter_packet{header = #diameter_header{length = L},
                     bin = B}
        = E
        = diameter_codec:encode(Dict0, Pkt#diameter_packet{msg = Req}),
    Offset = L - 24,  %% to Auth-Application-Id
    <<H:Offset/binary,
      Hdr:5/binary, 12:24, Data:4/binary,
      T:12/binary>>
        = B,
    AL = case N of
             send_long_avp_length  -> 13;
             send_short_avp_length -> 11;
             send_zero_avp_length  -> 0
         end,
    E#diameter_packet{bin = <<H/binary,
                              T/binary,
                              Hdr/binary, AL:24, Data/binary>>};

prepare(Pkt, Caps, N, #group{client_dict0 = Dict0} = Group)
  when N == send_invalid_avp_length;
       N == send_invalid_reject ->
    Req = prepare(Pkt, Caps, Group),
    %% Second last AVP in our STR is Auth-Application-Id of type
    %% Unsigned32: send data of length 8.
    #diameter_packet{header = #diameter_header{length = L},
                     bin = B0}
        = E
        = diameter_codec:encode(Dict0, Pkt#diameter_packet{msg = Req}),
    Offset = L - 7 - 12,  %% to AVP Length
    <<H0:Offset/binary, 12:24, T:16/binary>> = B0,
    <<V, L:24, H/binary>> = H0,  %% assert
    E#diameter_packet{bin = <<V, (L+4):24, H/binary, 16:24, 0:32, T/binary>>};

prepare(Pkt, Caps, send_unrecognized_mandatory, #group{client_dict0 = Dict0}
                                                = Group) ->
    Req = prepare(Pkt, Caps, Group),
    #diameter_packet{bin = <<V, Len:24, T/binary>>}
        = E
        = diameter_codec:encode(Dict0, Pkt#diameter_packet{msg = Req}),
    {Code, Flags, undefined} = Dict0:avp_header('Proxy-State'),
    Avp = <<Code:32, Flags, 8:24>>,
    E#diameter_packet{bin = <<V, (Len+8):24, T/binary, Avp/binary>>};

prepare(Pkt, Caps, send_unsupported, #group{client_dict0 = Dict0} = Group) ->
    Req = prepare(Pkt, Caps, Group),
    #diameter_packet{bin = <<H:5/binary, _CmdCode:3/binary, T/binary>>}
        = E
        = diameter_codec:encode(Dict0, Pkt#diameter_packet{msg = Req}),
    E#diameter_packet{bin = <<H/binary, 42:24, T/binary>>};

prepare(Pkt, Caps, send_unsupported_app, #group{client_dict0 = Dict0}
                                         = Group) ->
    Req = prepare(Pkt, Caps, Group),
    #diameter_packet{bin = <<H:8/binary, _ApplId:4/binary, T/binary>>}
        = E
        = diameter_codec:encode(Dict0, Pkt#diameter_packet{msg = Req}),
    E#diameter_packet{bin = <<H/binary, ?BAD_APP:32, T/binary>>};

prepare(Pkt, Caps, send_error_bit, Group) ->
    #diameter_packet{header = Hdr} = Pkt,
    Pkt#diameter_packet{header = Hdr#diameter_header{is_error = true},
                        msg = prepare(Pkt, Caps, Group)};

prepare(Pkt, Caps, send_unsupported_version, Group) ->
    #diameter_packet{header = Hdr} = Pkt,
    Pkt#diameter_packet{header = Hdr#diameter_header{version = 42},
                        msg = prepare(Pkt, Caps, Group)};

prepare(Pkt, Caps, send_anything, Group) ->
    Req = ['STR', {'Termination-Cause', ?LOGOUT}],
    prepare(Pkt#diameter_packet{msg = Req}, Caps, Group);

prepare(Pkt, Caps, _Name, Group) ->
    prepare(Pkt, Caps, Group).

%% prepare/3

prepare(#diameter_packet{msg = Req}, Caps, Group)
  when ?is_record(Req, diameter_base_accounting_ACR);
       'ACR' == hd(Req) ->
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, DR}}
        = Caps,

    set(Group, Req, [{'Session-Id', diameter:session_id(OH)},
                     {'Origin-Host',  OH},
                     {'Origin-Realm', OR},
                     {'Destination-Realm', DR}]);

prepare(#diameter_packet{msg = Req}, Caps, Group)
  when ?is_record(Req, diameter_base_ASR);
       'ASR' == hd(Req) ->
    #diameter_caps{origin_host  = {OH, DH},
                   origin_realm = {OR, DR}}
        = Caps,
    set(Group, Req, [{'Session-Id', diameter:session_id(OH)},
                     {'Origin-Host',  OH},
                     {'Origin-Realm', OR},
                     {'Destination-Host',  DH},
                     {'Destination-Realm', DR},
                     {'Auth-Application-Id', ?APP_ID}]);

prepare(#diameter_packet{msg = Req}, Caps, Group)
  when ?is_record(Req, diameter_base_STR);
       'STR' == hd(Req) ->
    #diameter_caps{origin_host  = {OH, _},
                   origin_realm = {OR, DR}}
        = Caps,
    set(Group, Req, [{'Session-Id', diameter:session_id(OH)},
                     {'Origin-Host',  OH},
                     {'Origin-Realm', OR},
                     {'Destination-Realm', DR},
                     {'Auth-Application-Id', ?APP_ID}]);

prepare(#diameter_packet{msg = Req}, Caps, Group)
  when ?is_record(Req, diameter_base_RAR);
       'RAR' == hd(Req) ->
    #diameter_caps{origin_host  = {OH, DH},
                   origin_realm = {OR, DR}}
        = Caps,
    set(Group, Req, [{'Session-Id', diameter:session_id(OH)},
                     {'Origin-Host',  OH},
                     {'Origin-Realm', OR},
                     {'Destination-Host',  DH},
                     {'Destination-Realm', DR},
                     {'Auth-Application-Id', ?APP_ID}]).

%% prepare_retransmit/5

prepare_retransmit(_Pkt, false, _Peer, _Name, _Group) ->
    discard.

%% handle_answer/6-7

handle_answer(Pkt, Req, ?CLIENT, Peer, {Name, Group}, _) ->
    answer(Pkt, Req, Peer, Name, Group).

handle_answer(Pkt, Req, ?CLIENT, Peer, {send_detach = Name, Group}, _, X) ->
    {Pid, Ref} = X,
    Pid ! {Ref, answer(Pkt, Req, Peer, Name, Group)}.

answer(Pkt, Req, _Peer, Name, #group{client_dict0 = Dict0}) ->
    #diameter_packet{header = H, msg = Ans, errors = Es} = Pkt,
    ApplId = app(Req, Name, Dict0),
    #diameter_header{application_id = ApplId} = H,  %% assert
    Dict = dict(Ans, Dict0),
    [R | Vs] = Dict:'#get-'(answer(Ans, Es, Name)),
    [Dict:rec2msg(R) | Vs].

answer(Rec, [_|_], N)
  when N == send_long_avp_length;
       N == send_short_avp_length;
       N == send_zero_avp_length;
       N == send_invalid_avp_length;
       N == send_invalid_reject ->
    Rec;
answer(Rec, [], _) ->
    Rec.

app(_, send_unsupported_app, _) ->
    ?BAD_APP;
app(Req, _, Dict0) ->
    Dict = dict(Req, Dict0),
    Dict:id().

%% handle_error/6

handle_error(timeout = Reason, _Req, ?CLIENT, _Peer, _, Time) ->
    Now = now(),
    {Reason, {Time, Now, timer:now_diff(Now, Time)}};

handle_error(Reason, _Req, ?CLIENT, _Peer, _, _Time) ->
    {error, Reason}.

%% handle_request/3

%% Note that diameter will set Result-Code and Failed-AVPs if
%% #diameter_packet.errors is non-null.

handle_request(#diameter_packet{header = H, msg = M}, ?SERVER, {_Ref, Caps}) ->
    #diameter_header{end_to_end_id = EI,
                     hop_by_hop_id = HI}
        = H,
    {V,B} = ?CLIENT_MASK,
    V = EI bsr B,  %% assert
    V = HI bsr B,  %%
    #diameter_caps{origin_state_id = {_,[Id]}} = Caps,
    answer(origin(Id), request(M, Caps)).

answer(T, {Tag, Action, Post}) ->
    {Tag, answer(T, Action), Post};
answer({A,C}, {reply, Ans}) ->
    answer(C, {reply, msg(Ans, A, diameter_gen_base_rfc3588)});
answer(pkt, {reply, Ans})
  when not is_record(Ans, diameter_packet) ->
    {reply, #diameter_packet{msg = Ans}};
answer(_, T) ->
    T.

%% send_nok
request(#diameter_base_accounting_ACR{'Accounting-Record-Number' = 0},
        _) ->
    {eval_packet, {protocol_error, ?INVALID_AVP_BITS}, [fun log/2, invalid]};

%% send_bad_answer
request(#diameter_base_accounting_ACR{'Session-Id' = SId,
                                      'Accounting-Record-Type' = RT,
                                      'Accounting-Record-Number' = 2 = RN},
        #diameter_caps{origin_host = {OH, _},
                       origin_realm = {OR, _}}) ->
    Ans = ['ACA', {'Result-Code', ?SUCCESS},
                  {'Session-Id', SId},
                  {'Origin-Host', OH},
                  {'Origin-Realm', OR},
                  {'Accounting-Record-Type', RT},
                  {'Accounting-Record-Number', RN}],

    {reply, #diameter_packet{header = #diameter_header{is_error = true},%% NOT
                             msg = Ans}};

%% send_eval
request(#diameter_base_accounting_ACR{'Session-Id' = SId,
                                      'Accounting-Record-Type' = RT,
                                      'Accounting-Record-Number' = 3 = RN},
        #diameter_caps{origin_host = {OH, _},
                       origin_realm = {OR, _}}) ->
    Ans = ['ACA', {'Result-Code', ?SUCCESS},
                  {'Session-Id', SId},
                  {'Origin-Host', OH},
                  {'Origin-Realm', OR},
                  {'Accounting-Record-Type', RT},
                  {'Accounting-Record-Number', RN}],
    {eval, {reply, Ans}, {erlang, now, []}};

%% send_ok
request(#diameter_base_accounting_ACR{'Session-Id' = SId,
                                      'Accounting-Record-Type' = RT,
                                      'Accounting-Record-Number' = 1 = RN},
        #diameter_caps{origin_host = {OH, _},
                       origin_realm = {OR, _}}) ->
    {reply, ['ACA', {'Result-Code', ?SUCCESS},
                    {'Session-Id', SId},
                    {'Origin-Host', OH},
                    {'Origin-Realm', OR},
                    {'Accounting-Record-Type', RT},
                    {'Accounting-Record-Number', RN}]};

%% send_protocol_error
request(#diameter_base_accounting_ACR{'Accounting-Record-Number' = 4},
        #diameter_caps{origin_host = {OH, _},
                       origin_realm = {OR, _}}) ->
    Ans = ['answer-message', {'Result-Code', ?TOO_BUSY},
                             {'Origin-Host', OH},
                             {'Origin-Realm', OR}],
    {reply, Ans};

request(#diameter_base_ASR{'Session-Id' = SId,
                           'AVP' = Avps},
        #diameter_caps{origin_host = {OH, _},
                       origin_realm = {OR, _}}) ->
    {reply, ['ASA', {'Result-Code', ?SUCCESS},
                    {'Session-Id', SId},
                    {'Origin-Host', OH},
                    {'Origin-Realm', OR},
                    {'AVP', Avps}]};

%% send_invalid_reject
request(#diameter_base_STR{'Termination-Cause' = ?USER_MOVED},
        _Caps) ->
    {protocol_error, ?TOO_BUSY};

%% send_noreply
request(#diameter_base_STR{'Termination-Cause' = T},
        _Caps)
  when T /= ?LOGOUT ->
    discard;

%% send_destination_5
request(#diameter_base_STR{'Destination-Realm' = R},
        #diameter_caps{origin_realm = {OR, _}})
  when R /= undefined, R /= OR ->
    {protocol_error, ?REALM_NOT_SERVED};

%% send_destination_6
request(#diameter_base_STR{'Destination-Host' = [H]},
        #diameter_caps{origin_host = {OH, _}})
  when H /= OH ->
    {protocol_error, ?UNABLE_TO_DELIVER};

request(#diameter_base_STR{'Session-Id' = SId},
        #diameter_caps{origin_host  = {OH, _},
                       origin_realm = {OR, _}}) ->
    {reply, ['STA', {'Result-Code', ?SUCCESS},
                    {'Session-Id', SId},
                    {'Origin-Host', OH},
                    {'Origin-Realm', OR}]};

%% send_error/send_timeout
request(#diameter_base_RAR{}, _Caps) ->
    receive after 2000 -> {protocol_error, ?TOO_BUSY} end.
