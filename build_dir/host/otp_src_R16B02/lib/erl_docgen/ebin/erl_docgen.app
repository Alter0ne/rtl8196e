{application, erl_docgen,
 [{description, "Misc tools for building documentation"},
  {vsn, "0.3.4.1"},
  {modules, [docgen_otp_specs,
  	     docgen_edoc_xml_cb,
	     docgen_xmerl_xml_cb
	    ]
  },
  {registered,[]},
  {applications, [kernel,stdlib]},
  {env, []
  }
 ]
}.
