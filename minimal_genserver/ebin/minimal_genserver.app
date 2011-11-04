%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, mimimal_genserver,
 [{description, "Utter minimal do nothing server for Erlang and OTP in action"},
  {vsn, "0.1.0"},
  {modules, [minimal_genserver]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {minimal_genserver, []}}
]}.
