%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
-module(trans_normal_sup).

-behaviour(supervisor).

%% External exports
-export([start/2]).

%% Internal exports
-export([init/1]).

start(_, _) ->
    supervisor:start_link({local, trans_normal_sup}, trans_normal_sup, []),
    exit(normal).

init([]) ->
    SupFlags = {one_for_one, 4, 3600},
    Config = {transient,
	      {transient, start_link, []},
	      transient, 2000, worker, [transient]},
    {ok, {SupFlags, [Config]}}.
