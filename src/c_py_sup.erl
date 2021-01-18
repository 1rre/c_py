-module(c_py_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link(c_py_sup, []).

init(Args) -> 
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  ChildSpecs = [#{id => translator,
                    start => {gen_server, start_link, [c_py, Args, []]},
                    restart => permanent,
                    type => worker,
                    modules => [c_py]}],
  {ok, {SupFlags, ChildSpecs}}.
