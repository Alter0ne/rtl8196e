%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2009. All Rights Reserved.
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
%% Description: Callback module for the runtime_tools application.
%%  ----------------------------------------------------------------------------

-module(runtime_tools).
-behaviour(application).

-export([start/2,stop/1]).


%% -----------------------------------------------------------------------------
%% Callback functions for the runtime_tools application
%% -----------------------------------------------------------------------------
start(_,AutoModArgs) ->
    case supervisor:start_link({local,runtime_tools_sup},
			       runtime_tools_sup,
			       AutoModArgs) of
	{ok, Pid} ->
	    {ok, Pid, []};
	Error ->
	    Error
    end.

stop(_) ->
    ok.
%% -----------------------------------------------------------------------------






