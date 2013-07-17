%% Copyright (c) 2013
%% Basho Technologies, Inc. <dev@basho.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the \"Software\"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%

Nonterminals
   commands command scope
   .

Terminals
    '{' '}' ','
    type identifier increment decrement
    .

Rootsymbol commands.
commands -> command : ['$1'].
commands -> command ',' commands : ['$1' | '$3'].
command -> scope : {update, '$1'}.
command -> type identifier increment : {update, {unpack('$2'), unpack('$1')}, {increment, 1}}.
command -> type identifier decrement: {update, {unpack('$2'), unpack('$1')}, {decrement, 1}}.
scope -> '{' commands '}' : '$2'.

Erlang code.
%% Copyright (c) 2013
%% Basho Technologies, Inc. <dev@basho.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the \"Software\"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%

-export([file/1, main/1, string/1]).

unpack({_,_,V}) -> V.

%line({_,L,_}) -> L;
%line({_,L}) -> L.

string(String) ->
    case riak_cql_scanner:string(String) of
        {ok, Toks, _} ->
            parse(Toks);
        Other -> Other
    end.

file(Filename) ->
    case riak_cql_scanner:file(Filename) of
        {ok, Toks, _} ->
            parse(Toks);
        Other -> Other
    end.

main([Filename]) ->
    case file(Filename) of
        {ok, AST} ->
            io:format("~p~n", [AST]);
        Other ->
            io:format("ERROR: ~p~n", [Other]),
            halt(1)
    end.
