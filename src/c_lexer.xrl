Definitions.

INT   = [0-9]+
WS    = (\s|\t|\n|\r)
TYPE  = (int)|(void)
IDENT = (_|[A-Z]|[a-z])(_|[A-Z]|[a-z]|[0-9])*

Rules.

{TYPE}          : {token, {type, TokenLine, TokenChars}}.
{INT}           : {token, {int, TokenLine, TokenChars}}.
if              : {token, {'if', TokenLine}}.
else            : {token, {'else', TokenLine}}.
while           : {token, {'while', TokenLine}}.
for             : {token, {'for', TokenLine}}.
return          : {token, {'return', TokenLine}}.
\:              : {token, {':', TokenLine}}.
\?              : {token, {'?', TokenLine}}.
,               : {token, {',', TokenLine}}.
\{              : {token, {'{', TokenLine}}.
\}              : {token, {'}', TokenLine}}.
\(              : {token, {'(', TokenLine}}.
\)              : {token, {')', TokenLine}}.
\[              : {token, {'[', TokenLine}}.
\]              : {token, {']', TokenLine}}.
;               : {token, {';', TokenLine}}.
=               : {token, {'=', TokenLine}}.
\+\+            : {token, {'++', TokenLine}}.
\-\-            : {token, {'--', TokenLine}}.
\+\=            : {token, {'+-', TokenLine}}.
\-\=            : {token, {'-=', TokenLine}}.
\*\=            : {token, {'*=', TokenLine}}.
\/\=            : {token, {'/=', TokenLine}}.
\%\=            : {token, {'%=', TokenLine}}.
\*              : {token, {'*', TokenLine}}.
\+              : {token, {'+', TokenLine}}.
\/              : {token, {'/', TokenLine}}.
\%              : {token, {'%', TokenLine}}.
-               : {token, {'-', TokenLine}}.
&&              : {token, {'&&', TokenLine}}.
\|\|            : {token, {'||', TokenLine}}.
==              : {token, {'==', TokenLine}}.
<               : {token, {'<', TokenLine}}.
>               : {token, {'>', TokenLine}}.
<=              : {token, {'<=', TokenLine}}.
>=              : {token, {'>=', TokenLine}}.
&               : {token, {'&', TokenLine}}.
\|              : {token, {'|', TokenLine}}.
\^              : {token, {'^', TokenLine}}.
{IDENT}         : {token, {ident, TokenLine, TokenChars}}.
{WS}+           : skip_token.


Erlang code.


