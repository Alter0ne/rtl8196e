%%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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
%%%-------------------------------------------------------------------
%%% File    : bb.hrl
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Description : Typed record declaration for basic blocks 
%%%
%%% Created : 20 Dec 2007 by Per Gustafsson <pergu@it.uu.se>
%%%-------------------------------------------------------------------

-record(bb, {code=[] :: [_]}).

-type bb() :: #bb{}.
