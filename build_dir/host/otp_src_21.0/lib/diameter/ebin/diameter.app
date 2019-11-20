%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

{application, diameter,
 [{description, "Diameter protocol"},
  {vsn, "2.1.5"},
  {modules, [
      diameter,diameter_app,diameter_callback,diameter_capx,diameter_config,diameter_config_sup,diameter_codec,diameter_gen,diameter_lib,diameter_misc_sup,diameter_peer,diameter_peer_fsm,diameter_peer_fsm_sup,diameter_reg,diameter_service,diameter_service_sup,diameter_session,diameter_stats,diameter_sup,diameter_sync,diameter_traffic,diameter_types,diameter_watchdog,diameter_watchdog_sup,diameter_etcp,diameter_etcp_sup,diameter_tcp,diameter_tcp_sup,diameter_sctp,diameter_sctp_sup,diameter_transport,diameter_transport_sup,diameter_gen_base_rfc3588,diameter_gen_base_rfc6733,diameter_gen_base_accounting,diameter_gen_acct_rfc6733,diameter_gen_doic_rfc7683,diameter_gen_relay
      %,diameter_codegen,diameter_exprecs,diameter_dict_scanner,diameter_dict_util,diameter_make,diameter_dict_parser
      %,diameter_dbg,diameter_info
      ]},
  {registered, [diameter_config,diameter_peer,diameter_reg,diameter_stats,diameter_sync]},
  {applications, [
      stdlib,
      kernel
      %, ssl
      %, syntax-tools
      %, runtime-tools
      ]},
  {env, []},
  {mod, {diameter_app, []}},
  {runtime_dependencies, [
      "erts-10.0",
      "stdlib-2.4",
      "kernel-3.2",
      "ssl-9.0"
      %, "syntax-tools-1.6.18"
      %, "runtime-tools-1.8.16"
      ]}
  %%
  %% Note that ssl is only required if configured on TCP transports,
  %% and syntax-tools and runtime-tools are only required if the
  %% dictionary compiler and debug modules (respectively) are
  %% needed/wanted at runtime, which they typically aren't. These
  %% modules are the two commented lines in the 'modules' tuple.
  %%
 ]}.
