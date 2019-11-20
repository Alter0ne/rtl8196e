{application, asn1,
 [{description, "The Erlang ASN1 compiler version 2.0.3"},
  {vsn, "2.0.3"},
  {modules, [
	asn1rt,
        asn1rt_nif
             ]},
  {registered, [
	asn1_ns,
	asn1db
		]},
  {env, []},
  {applications, [kernel, stdlib]}
  ]}.
