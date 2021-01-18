Nonterminals global_lines global_line invocation function declaration args arg block lines line if_statement return_statement while_loop for_loop init_clause cond_expression assignment value function_call if_block unary_minus assignment_operator operator.
Terminals return if else while for type ident int '{' '}' '(' ')' ',' ';' '=' '++' '--' '+=' '-=' '*=' '/=' '%=' '==' '>' '<' '>=' '<=' '&&' '||' '-' '+' '*' '/' '%' '&' '|' '^'.

Rootsymbol global_lines.

Left  100 assignment_operator.
Unary 400 unary_minus.
Left  200 '+'.
Left  200 '-'.
Left  300 '*'.
Left  300 '/'.
Left  300 '%'.
Left  300 '&'.
Left  300 '|'.
Left  300 '^'.

global_lines -> global_line global_lines : ['$1' | '$2'].
global_lines -> global_line : ['$1'].

global_line -> invocation : '$1'.
global_line -> function : '$1'.
global_line -> declaration : '$1'.

invocation -> type ident '(' args ')' ';' : {invocation, {'$1', '$2', '$4'}}.
invocation -> type ident '(' ')' ';' : {invocation, {'$1', '$2', []}}.

function -> type ident '(' args ')' block : {function, {'$1', '$2', '$4', '$6'}}.
function -> type ident '(' ')' block : {function, {'$1', '$2', [], '$5'}}.

declaration -> type ident ';' : {declaration, {'$1', '$2', nil}}.
declaration -> type ident '=' value ';' : {declaration, {'$1', '$2', '$4'}}.

args -> arg ',' args : ['$1' | '$3'].
args -> arg : ['$1'].

arg -> type ident : {'$1', '$2'}.
arg -> value : '$1'.

block -> '{' lines '}' : '$2'.
block -> '{' '}' : [].

lines -> line lines : ['$1' | '$2'].
lines -> line : ['$1'].

line -> if_statement : '$1'.
line -> declaration : '$1'.
line -> return_statement : '$1'.
line -> while_loop : '$1'.
line -> for_loop : '$1'.
line -> value ';' : '$1'.

if_statement -> if '(' value ')' if_block else if_block : {call, {'$1', ['$3', '$5', '$7']}}.
if_statement -> if '(' value ')' if_block : {call, {'$1', ['$3', '$5']}}.

if_block -> block : '$1'.
if_block -> line : ['$1'].

return_statement -> return ';' : {call, {'$1', []}}.
return_statement -> return value ';' : {call, {'$1', ['$2']}}.

while_loop -> while '(' value ')' if_block : {call, {'$1', ['$3', '$5']}}.

for_loop -> for '(' init_clause cond_expression assignment ')' if_block : {call, {'$1', ['$3', '$4', '$5', '$7']}}.
for_loop -> for '(' init_clause cond_expression ')' if_block : {call, {'$1', ['$3', '$4', nil, '$6']}}.

init_clause -> declaration : '$1'.
init_clause -> ';' : nil.

cond_expression -> value ';' : '$1'.
cond_expression -> ';' : nil.

assignment -> function_call : '$1'.

value -> ident : '$1'.
value -> int : '$1'.
value -> function_call : '$1'.
value -> '(' value ')' : '$2'.

function_call -> ident '(' args ')' : {call, {'$1', '$3'}}.
function_call -> unary_minus : '$1'.
function_call -> value operator value : {call, {'$2', ['$1', '$3']}}.
function_call -> ident assignment_operator: {call, {'$2', ['$1']}}.
function_call -> value '*' value : {call, {'$2', ['$1', '$3']}}.
function_call -> value '/' value : {call, {'$2', ['$1', '$3']}}.
function_call -> value '%' value : {call, {'$2', ['$1', '$3']}}.
function_call -> value '&' value : {call, {'$2', ['$1', '$3']}}.
function_call -> value '|' value : {call, {'$2', ['$1', '$3']}}.
function_call -> value '^' value : {call, {'$2', ['$1', '$3']}}.
function_call -> value '+' value : {call, {'$2', ['$1', '$3']}}.
function_call -> value '-' value : {call, {'$2', ['$1', '$3']}}.

unary_minus -> '-' value : {call, {'$1', [{int, element(2, '$2'), "0"}, '$2']}}.

assignment_operator -> '++' : '$1'.
assignment_operator -> '--' : '$1'.
assignment_operator -> '+=' : '$1'.
assignment_operator -> '-=' : '$1'.
assignment_operator -> '*=' : '$1'.
assignment_operator -> '/=' : '$1'.
assignment_operator -> '%=' : '$1'.

operator -> '='  : '$1'.
operator -> '&&' : '$1'.
operator -> '||' : '$1'.
operator -> '==' : '$1'.
operator -> '<' : '$1'.
operator -> '<=' : '$1'.
operator -> '>' : '$1'.
operator -> '>=' : '$1'.

