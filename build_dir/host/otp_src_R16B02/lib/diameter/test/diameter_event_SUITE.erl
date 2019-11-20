%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
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
%% Tests of events sent as a consequence of diameter:subscribe/1.
%% Watchdog events are dealt with more extensively in the watchdog
%% suite.
%%

-module(diameter_event_SUITE).

-export([suite/0,
         all/0,
         init_per_testcase/2,
         end_per_testcase/2]).

%% testcases
-export([start/1,
         start_server/1,
         up/1,
         down/1,
         cea_timeout/1,
         stop/1]).

-include("diameter.hrl").

%% ===========================================================================

-define(util, diameter_util).

-define(ADDR, {127,0,0,1}).
-define(REALM, "REALM").

-define(SERVER, "SERVER.SERVER-REALM").
-define(CLIENT, "CLIENT.CLIENT-REALM").

-define(DICT_COMMON, ?DIAMETER_DICT_COMMON).
-define(DICT_ACCT,   ?DIAMETER_DICT_ACCOUNTING).

-define(SERVER_CAPX_TMO, 6000).

%% Config for diameter:start_service/2.
-define(SERVICE(Host, Dicts),
        [{'Origin-Host', Host},
         {'Origin-Realm', realm(Host)},
         {'Host-IP-Address', [?ADDR]},
         {'Vendor-Id', 12345},
         {'Product-Name', "OTP/diameter"},
         {'Acct-Application-Id', [D:id() || D <- Dicts]}
         | [{application, [{dictionary, D},
                           {module, #diameter_callback{}}]}
            || D <- Dicts]]).

%% Diameter Result-Code's:
-define(NO_COMMON_APP, 5010).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [start,
     start_server,
     up,
     down,
     cea_timeout,
     stop].

init_per_testcase(Name, Config) ->
    [{name, Name} | Config].

end_per_testcase(_, _) ->
    ok.

%% ===========================================================================
%% start/stop testcases

start(_Config) ->
    ok = diameter:start().

start_server(Config) ->
    diameter:subscribe(?SERVER),
    ok = diameter:start_service(?SERVER, ?SERVICE(?SERVER, [?DICT_COMMON])),
    LRef = ?util:listen(?SERVER, tcp, [{capabilities_cb, fun capx_cb/2},
                                       {capx_timeout, ?SERVER_CAPX_TMO}]),
    [PortNr] = ?util:lport(tcp, LRef),
    ?util:write_priv(Config, portnr, PortNr),
    start = event(?SERVER).

%% Connect with matching capabilities and expect the connection to
%% come up.
up(Config) ->
    {Svc, Ref} = connect(Config, []),
    start = event(Svc),
    {up, Ref, {_,_Caps}, _Config, #diameter_packet{}} = event(Svc),
    {watchdog, Ref, _, {initial, okay}, _} = event(Svc).

%% Connect with non-matching capabilities and expect CEA from the peer
%% to indicate as much and then for the transport to be restarted
%% (after reconnect_timer).
down(Config) ->
    {Svc, Ref} = connect(Config, [{capabilities, [{'Acct-Application-Id',
                                                   [?DICT_ACCT:id()]}]},
                                  {applications, [?DICT_ACCT]},
                                  {reconnect_timer, 5000}]),
    start = event(Svc),
    {closed, Ref, {'CEA', ?NO_COMMON_APP, _, #diameter_packet{}}, _}
        = event(Svc),
    {reconnect, Ref, _} = event(Svc).

%% Connect with matching capabilities but have the server delay its
%% CEA and cause the client to timeout.
cea_timeout(Config) ->
    {Svc, Ref} = connect(Config, [{capx_timeout, ?SERVER_CAPX_TMO div 2},
                                  {reconnect_timer, 2*?SERVER_CAPX_TMO}]),
    start = event(Svc),
    {closed, Ref, {'CEA', timeout}, _} = event(Svc).

stop(_Config) ->
    ok = diameter:stop().

%% ----------------------------------------

%% Keep the server from sending CEA until the client has timed out.
capx_cb(_, #diameter_caps{origin_host = {_, "cea_timeout-" ++ _}}) ->
    receive after ?SERVER_CAPX_TMO -> ok end;

%% Or not.
capx_cb(_, _Caps) ->
    ok.

%% ----------------------------------------

%% Use the testcase name to construct Origin-Host of the client so
%% that the server can match on it in capx_cb/2.
connect(Config, Opts) ->
    Pre = atom_to_list(proplists:get_value(name, Config)),
    Name = Pre ++ uniq() ++ ?CLIENT,
    diameter:subscribe(Name),
    ok = start_service(Name, ?SERVICE(Name, [?DICT_COMMON, ?DICT_ACCT])),
    {ok, Ref} = diameter:add_transport(Name, opts(Config, Opts)),
    {Name, Ref}.

uniq() ->
    {MS,S,US} = now(),
    lists:flatten(io_lib:format("-~p-~p-~p-", [MS,S,US])).

event(Name) ->
    receive #diameter_event{service = Name, info = T} -> T end.

start_service(Name, Opts) ->
    diameter:start_service(Name, [{monitor, self()} | Opts]).

opts(Config, Opts) ->
    PortNr = ?util:read_priv(Config, portnr),

    {connect, [{transport_module, diameter_tcp},
               {transport_config, [{ip, ?ADDR}, {port, 0},
                                   {raddr, ?ADDR}, {rport, PortNr}]}
               | Opts]}.

realm(Host) ->
    tl(lists:dropwhile(fun(C) -> C /= $. end, Host)).
