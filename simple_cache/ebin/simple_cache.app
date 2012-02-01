{application, simple_cache,
 [{descriptions, "A simple caching system"},
  {vsn, "0.1.0"},
  {modules, [
	     sc_app,
	     sc_sup,
	     sc_element_sup,
	     sc_store,
	     sc_element,
	     sc_event
	    ]},
  {registered, [sc_sup]},
  {applications, [kernel, sasl, stdlib]},
  {mod, {sc_app, []}}
]}.
