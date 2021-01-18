-module(c_py_app).
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2]).

init(_) -> {ok, nil}.
handle_call({translate, C89Code}, _Sender, _nil) ->
  {reply, translate(C89Code), nil}.
handle_cast({translate, C89Code}, _nil) ->
  {noreply, translate(C89Code), nil}.

% Translate a whole python program
translate(C89Code) -> 
  {ok, Tokens, _} = c_lexer:string(C89Code),
  {ok, Result} = c_parser:parse(Tokens),
  translate(Result, [], 0).

% A main function (treated differently as we need to define the entry point)
translate([{function, {_,{_,_, "main"}, Args, Block}} | Code], Globals, Indent) ->
  Args_Py = lists:join(", ", lists:flatten([Arg || {_,{_,_,Arg}} <- Args])),
  translate(Code, Globals, Indent) ++
  "def main(" ++
  Args_Py ++
  "):\n" ++
  lists:flatten([lists:duplicate(Indent+2, $ )
   ++ "global "
   ++ X
   ++ "\n" || X <- proplists:get_keys(Globals)]) ++
  translate(Block, Globals, Indent+2) ++
  "\nif __name__ == \"__main__\":\n  import sys\n  ret=main(" ++
  Args_Py ++
  ")\n  sys.exit(ret)\n";

% Function definition
translate([{function, {_,{_,_, Name}, Args, Block}} | Code], Globals, Indent) ->
  Args_Py = lists:join(", ", lists:flatten([Arg || {_,{_,_,Arg}} <- Args])),
  "def " ++
  Name ++
  "(" ++
  Args_Py ++
  "):\n" ++
  lists:flatten([lists:duplicate(Indent+2, $ )
   ++ "global "
   ++ X
   ++ "\n" || X <- proplists:get_keys(Globals)]) ++
  translate(Block, Globals, Indent+2) ++
  translate(Code, Globals, Indent);

% Declaration of global variable
translate([{declaration, {_,{_, Line, Name},{_,_, Value}}} | Code], Globals, 0) ->
  X = proplists:is_defined(Name, Globals),
  if X -> error("Global " ++
               Name ++
               " on line " ++
               Line ++
               " already declared on line " ++
               proplists:lookup(Name, Globals));
    true -> Name ++
            " = " ++
            Value ++
            "\n" ++
            translate(Code, [{Name, Line} | Globals], 0)
  end;

translate([{declaration, {_,{_, Line, Name}, nil}} | Code], Globals, 0) -> 
  X = proplists:is_defined(Name, Globals), 
  if X -> error("Global " ++
               Name ++
               " on line " ++
               Line ++
               " already declared on line " ++
               proplists:lookup(Name, Globals));
    true -> Name ++
            " = 0\n" ++
            translate(Code, [{Name, Line} | Globals], 0)
  end;  

% Declaration of local variable
translate([{declaration, {_,{_,_, Name},{_,_, Value}}} | Code], Globals, Indent) ->
  lists:duplicate(Indent, $ ) ++
  Name ++
  " = " ++
  Value ++
  "\n" ++
  translate(Code, Globals, Indent); 

translate([{declaration, {_,{_,_, Name}, nil}} | Code], Globals, Indent) ->
  lists:duplicate(Indent, $ ) ++
  Name ++
  " = 0\n" ++
  translate(Code, Globals, Indent);

% Function calls (Including operators)
% Standalone if statement
translate([{call, {{'if',_}, [Args, Block]}} | Code], Globals, Indent) ->
  lists:duplicate(Indent, $ ) ++
  "if (" ++
  translate_call(Args) ++
  "):\n" ++
  translate(Block, Globals, Indent + 2) ++
  translate(Code, Globals, Indent);

% If/Else statement
translate([{call, {{'if',_}, [Args, Block, Elif]}} | Code], Globals, Indent) ->
  lists:duplicate(Indent, $ ) ++
  "if (" ++
  translate_call(Args) ++
  "):\n" ++
  translate(Block, Globals, Indent + 2) ++
  lists:duplicate(Indent, $ ) ++
  "else:\n" ++
  translate(Elif, Globals, Indent + 2) ++
  translate(Code, Globals, Indent);

% While Loop
translate([{call, {{while,_}, [Call, Block]}} | Code], Globals, Indent) ->
  lists:duplicate(Indent, $ ) ++
  "while " ++
  translate_call(Call) ++
  "\n" ++
  translate(Block, Globals, Indent + 2) ++
  translate(Code, Globals, Indent);

% Return void
translate([{call, {{return,_}, []}} | Code], Globals, Indent) ->
  lists:duplicate(Indent, $ ) ++
  "return\n" ++
  translate(Code, Globals, Indent);

% Return value
translate([{call, {{return,_}, [Return]}} | Code], Globals, Indent) ->
  lists:duplicate(Indent, $ ) ++
  "return " ++
  translate_call(Return) ++
  "\n" ++
  translate(Code, Globals, Indent);

% Assignment
translate([{call, {{'++',_},[{_,_,B}]}} | Code], Globals, Indent) ->
  lists:duplicate(Indent, $ ) ++
  B ++
  " = " ++
  B ++
  " + 1\n"
  ++ translate(Code, Globals, Indent);

translate([{call, {{'=',_}, [{_,_,A}, B]}} | Code], Globals, Indent) ->
  lists:duplicate(Indent, $ ) ++
  A ++
  " = " ++
  translate_call(B) ++
  "\n"
  ++ translate(Code, Globals, Indent);

% Empty list
translate([],_,_) -> "";

% Other input (Prototypes etc.)
translate([_|Code], Globals, Indent) -> translate(Code, Globals, Indent).

% Translate operator calls
translate_call({call, {{ident, _, Fn}, Args}}) ->
  Fn ++
  "(" ++
  lists:map(fun(X) -> translate_call(X) end, Args) ++
  ")";

translate_call({call, {{'&&', _}, [A, B]}}) ->
  "(" ++
  translate_call(A) ++
  " and " ++
  translate_call(B) ++
  ")";

translate_call({call, {{'||', _}, [A, B]}}) ->
  "(" ++
  translate_call(A) ++
  " or " ++
  translate_call(B) ++
  ")";

translate_call({call, {{Op, _}, [A, B]}}) ->
  "(" ++
  translate_call(A) ++
  " " ++
  atom_to_list(Op) ++
  " " ++
  translate_call(B) ++
  ")";

translate_call({_,_,Value}) -> Value.
