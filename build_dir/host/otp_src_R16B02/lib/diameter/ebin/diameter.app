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

{application, diameter,
 [{description, "Diameter protocol"},
  {vsn, "1.4.3"},
  {modules, [diameter,diameter_app,diameter_callback,diameter_capx,diameter_config,diameter_codec,diameter_dict,diameter_lib,diameter_misc_sup,diameter_peer,diameter_peer_fsm,diameter_peer_fsm_sup,diameter_reg,diameter_service,diameter_service_sup,diameter_session,diameter_stats,diameter_sup,diameter_sync,diameter_traffic,diameter_types,diameter_watchdog,diameter_watchdog_sup,diameter_etcp,diameter_etcp_sup,diameter_tcp,diameter_tcp_sup,diameter_sctp,diameter_sctp_sup,diameter_transport,diameter_transport_sup,diameter_gen_base_rfc3588,diameter_gen_base_rfc6733,diameter_gen_base_accounting,diameter_gen_acct_rfc6733,diameter_gen_relay]},
  {registered, [diameter_config,diameter_peer,diameter_reg,diameter_stats,diameter_sync]},
  {applications, [stdlib, kernel]},
  {env, []},
  {mod, {diameter_app, []}}
 ]}.
