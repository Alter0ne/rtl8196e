{application, parsetools,
 [{description, "XLATETOOLS  CXC 138 xx"},
  {vsn, "2.1.7"},
  {modules, [leex,
             yecc,
	     yeccparser,
	     yeccscan
	    ]
  },
  {registered,[]},
  {applications, [kernel,stdlib]},
  {env, [{file_util_search_methods,[{"", ""}, {"ebin", "esrc"}, {"ebin", "src"}]}
	]
  },
  {runtime_dependencies, ["stdlib-2.5","kernel-3.0","erts-6.0"]}
 ]
}. 
 
















