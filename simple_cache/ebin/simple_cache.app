{application, simple_cache,
 [{descriptions, "A simple caching system"},
  {vsn, "0.1.0"},
  {modules, [
	     sc_app,
	     sc_sup
	    ]},
  {registered, [sc_sup]},
  {applications, [kernel, stdlib]},
  {mod, {sc_app, []}}
]}.
