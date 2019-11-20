%% -------------------------------------------------------------------
%% This is a generated file.
%% -------------------------------------------------------------------

-hrl_name('diameter_gen_doic_rfc7683.hrl').


%%% -------------------------------------------------------
%%% Grouped AVP records:
%%% -------------------------------------------------------

-record('diameter_doic_OC-Supported-Features',
	{'OC-Feature-Vector' = [], 'AVP' = []}).

-record('diameter_doic_OC-OLR',
	{'OC-Sequence-Number', 'OC-Report-Type',
	 'OC-Reduction-Percentage' = [],
	 'OC-Validity-Duration' = [], 'AVP' = []}).


%%% -------------------------------------------------------
%%% ENUM Macros:
%%% -------------------------------------------------------

-define('DIAMETER_DOIC_OC-REPORT-TYPE_HOST_REPORT', 0).
-define('DIAMETER_DOIC_OC-REPORT-TYPE_REALM_REPORT', 1).

