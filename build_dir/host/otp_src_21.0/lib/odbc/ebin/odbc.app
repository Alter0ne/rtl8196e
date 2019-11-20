{application, odbc,
 [{description, "Erlang ODBC application"},
  {vsn, "2.12.1"},
  {modules, [
	     odbc,
	     odbc_app,
	     odbc_sup
            ]},
  {registered, [
		odbc_sup
	       ]},
  {applications, [kernel, stdlib]},
  {env,[]},
  {mod, {odbc_app, []}},
  {runtime_dependencies, ["stdlib-2.0","kernel-3.0","erts-6.0"]}]}.

