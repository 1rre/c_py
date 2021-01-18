-module(c_py_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_,_) -> c_py_sup:start_link().

stop(_) -> ok.
