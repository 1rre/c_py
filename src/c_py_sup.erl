-module(c_py_sup).
-behaviour(application).
-export([start/2, get_pid/0, stop/1]).

start(_Type, _Args) ->
  {ok, PID} = gen_server:start_link(c_py_app, [], []),
  register(translator, PID),
  {ok, PID}.

get_pid() -> whereis(translator).

stop(_State) -> ok.

