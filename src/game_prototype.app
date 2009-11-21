{application, game_prototype,
 [{description, "game_prototype"},
  {vsn, "0.1"},
  {modules, [
    game_prototype,
    game_prototype_app,
    game_prototype_sup,
    game_prototype_deps,
    game_prototype_resource
  ]},
  {registered, []},
  {mod, {game_prototype_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
