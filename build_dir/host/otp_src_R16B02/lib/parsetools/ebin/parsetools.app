{application, parsetools,
 [{description, "XLATETOOLS  CXC 138 xx"},
  {vsn, "2.0.10"},
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
  }
 ]
}. 
 
















