{application, erling_server,
 [{description, "erling_server"},
  {vsn, "0.01"},
  {modules, [
    erling_server,
    erling_server_app,
    erling_server_sup,
    erling_server_web,
    erling_server_deps
  ]},
  {registered, []},
  {mod, {erling_server_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
