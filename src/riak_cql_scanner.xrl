%% Copyright (c) 2013
%% Basho Technologies, Inc. <dev@basho.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
Definitions.

EOL = [\r\n]
WS = [\s\n\t\r\v\f]
WS_NONL = [\s\t\r\v\f]
UNPRINTABLE = [\x00-\x19]
DIGIT = [0-9]
OCTAL_DIGIT = [0-7]
HEX_DIGIT = [0-9a-fA-F]
LETTER = [a-zA-Z_]
ALNUM = [a-zA-Z0-9_]
ESCAPE = [abfnrtv\\\?\'\"]
SYMBOL = ([{},+-?*$]|\+)
SIGN = (-|\+)
FLOAT = [fF]

Rules.

{UNPRINTABLE}+                                             : skip_token.  %% remove unprintables
{WS}+                                                      : skip_token.  %% remove whitespace
#.*({EOL}+|)                                               : skip_token.  %% sh-style comments
//.*({EOL}+|)                                              : skip_token.  %% C style single-line comments
/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/                  : skip_token.  %% C-style block comments
{LETTER}{ALNUM}*                                           : {token, select_id_type(TokenChars, TokenLine)}.
\"([^\"]|\\\")*\"                                          : {token, {string, TokenLine, unquote(TokenChars)}}.
\'([^\']|\\\')*\'                                          : {token, {string, TokenLine, unquote(TokenChars)}}.
(0|{SIGN}?[1-9]{DIGIT}*)                                   : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{SYMBOL}                                                   : {token, {list_to_atom(TokenChars), TokenLine}}.

Erlang code.
%% Copyright (c) 2013
%% Basho Technologies, Inc. <dev@basho.com>
%
%% Copyright (c) 2013
%% Sean Cribbs
%%   https://github.com/seancribbs/epb
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% @doc Lexical analysis for Protocol Buffers definitions
%% Field types
-define(TYPE, ["map", "set", "register", "counter"]).

%% Other keywords
-define(KEYWORD, ["keys", "values", "count", "type"]).

-export([file/1, main/1]).

%% Flip through the reserved words and create tokens of the proper type.
select_id_type(T, Line) ->
    select_id_type(T, Line, [{type, ?TYPE},
                             {keyword, ?KEYWORD}]).

%% When none of the keywords match, it's a regular identifier
select_id_type(T, Line, []) ->
    {identifier, Line, T};
%% special-casing other keywords so they come out as terminals
select_id_type(T, Line, [{keyword, K}|Rest]) ->
    case lists:member(T, K) of
        true ->
            {list_to_atom(T), Line};
        false ->
            select_id_type(T, Line, Rest)
    end;
select_id_type(T, Line, [{NT, Toks}|Rest]) ->
    case lists:member(T, Toks) of
        true ->
            {NT, Line, list_to_atom(T)};
        false ->
            select_id_type(T, Line, Rest)
    end.

unquote(Str) ->
    %% Quote is at beginning and end of the string, strip them off.
    Stripped = lists:sublist(Str, 2, length(Str)-2),
    unescape(Stripped, []).

unescape([], Acc) ->
    lists:reverse(Acc);
unescape([$\\|Rest0], Acc) ->
    {Char, Rest} = unescape_char(Rest0),
    unescape(Rest, [Char|Acc]);
unescape([C|Rest], Acc) ->
    unescape(Rest, [C|Acc]).

unescape_char([$n|T]) -> {$\n, T};
unescape_char([$r|T]) -> {$\r, T};
unescape_char([$t|T]) -> {$\t, T};
unescape_char([$v|T]) -> {$\v, T};
unescape_char([$b|T]) -> {$\b, T};
unescape_char([$f|T]) -> {$\f, T};
unescape_char([$e|T]) -> {$\e, T};
unescape_char([$s|T]) -> {$\s, T};
unescape_char([$d|T]) -> {$\d, T};
unescape_char([C|T]) ->  {C, T}.



file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    string(unicode:characters_to_list(Bin)).

main([Filename]) ->
    io:format("~p~n", [file(Filename)]).
