%% -------------------------------------------------------------------
%%
%% etoml: A parser and generator for TOML language 
%% (https://github.com/mojombo/toml)
%%
%% Copyright (c) 2013 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc TOML language parser 

-module(etoml).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([parse/1, parse2/1]).
-export_type([basic_type/0, element/0, block/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type basic_type() :: binary() | boolean() | integer() | float() |
					     calendar:datetime().

-type element() :: basic_type() | [basic_type()].					    

-type block() :: {Key::binary(), Value::element()|block()}.


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Parses a TOML `(https://github.com/mojombo/toml)' binary
-spec parse(binary()|string()) -> 
	[block()] | {error, Error}
	when Error :: {invalid_key, integer()} | {invalid_group, integer()} | 
				  {invalid_date, integer()} | {invalid_number, integer()} | 
				  {invalid_array, integer()} | {invalid_string, integer()} | 
				  {duplicated_key, binary()}.

parse(Msg) ->
	try
		case parse2(Msg) of
			{ok, List} -> {ok, join(List, [])};
			{error, Error} -> {error, Error}
		end
	catch 
		throw:TError -> {error, TError}
	end.


%% @doc Parses a TOML `(https://github.com/mojombo/toml)' binary as raw
-spec parse2(binary()|string()) -> 
	[{Keys::[binary()], Value::element()}] | {error, Error}
	when Error :: {invalid_key, integer()} | {invalid_group, integer()} | 
				  {invalid_date, integer()} | {invalid_number, integer()} | 
				  {invalid_array, integer()} | {invalid_string, integer()}.

parse2(Msg) when is_binary(Msg) ->
	parse2(binary_to_list(Msg));

parse2(Msg) when is_list(Msg) ->
	try 
		{Rest, Line} = parse_space(Msg, 1),
		parse(Rest, Line, [], [])
	catch
		throw:TError -> {error, TError}
	end.


%% ===================================================================
%% Private
%% ===================================================================

%% @private
join([], Acc) -> lists:reverse(Acc);
join([{Keys, Val}|Rest], Acc) -> 
	join(Rest, join(Keys, Val, Acc)).

%% @private
join([Key], Val, Acc) ->
	case lists:keymember(Key, 1, Acc) of
		false -> [{Key, Val}|Acc];
		true -> throw({duplicated_key, Key})
	end;
join([Key|Rest], Val, Acc) ->
	case lists:keytake(Key, 1, Acc) of
		false -> [{Key, join(Rest, Val, [])}|Acc];
		{value, {_, Acc1}, AccRest} -> [{Key, join(Rest, Val, Acc1)}|AccRest]
	end.

%% @private
parse([], _Line, _Names, Data) ->
	{ok, lists:reverse(Data)};
parse([$[|Rest], Line, _Names, Data) ->
	{Rest1, Line1} = parse_space(Rest, Line),
	{Names1, {Rest2, Line2}} = parse_group(Rest1, Line1, [], []),	
	parse(Rest2, Line2, Names1, Data);
parse(Rest, Line, Names, Data) ->
	% io:format("Parsing ~p: ~p\n\n", [Line, Rest]),
	{{Key, Value}, {Rest1, Line1}} = parse_key(Rest, Line),
	Keys = lists:reverse([Key|Names]),
	parse(Rest1, Line1, Names, [{Keys, Value}|Data]).

%% @private
parse_key(Rest, Line) ->
	case parse_text(Rest, Line, []) of
		{Key, {[$=|Rest1], Line1}} ->
			{Rest2, Line2} = parse_space(Rest1, Line1),
			{Value, {Rest3, Line3}} = parse_value(Rest2, Line2),
			{{list_to_binary(Key), Value}, {Rest3, Line3}};
		_ ->
			throw({invalid_key, Line})
	end.

%% @private
parse_value([$"|Rest], Line) ->	
	parse_string(Rest, Line, []);
parse_value([$[|Rest], Line) ->	
	{Rest1, Line1} = parse_space(Rest, Line),
	parse_array(Rest1, Line1, []);
parse_value(Rest, Line) ->
	case parse_text(Rest, Line, []) of
		{"true", Pos} -> {true, Pos};
		{"false", Pos} -> {false, Pos};
		{[_,_,_,_,$-|_]=Date, Pos} -> {parse_date(Date, Line), Pos};
		{Number, Pos} -> {parse_number(Number, Line), Pos}
	end.

%% @private
parse_string("\\0"++Rest, Line, Val) ->
	parse_string(Rest, Line, [0|Val]);
parse_string("\\t"++Rest, Line, Val) ->
	parse_string(Rest, Line, [9|Val]);
parse_string("\\n"++Rest, Line, Val) ->
	parse_string(Rest, Line, [10|Val]);
parse_string("\\r"++Rest, Line, Val) ->
	parse_string(Rest, Line, [13|Val]);
parse_string("\\\""++Rest, Line, Val) ->
	parse_string(Rest, Line, [$"|Val]);
parse_string("\\\\"++Rest, Line, Val) ->
	parse_string(Rest, Line, [92|Val]);
parse_string([$"|Rest], Line, Val) ->
	{list_to_binary(lists:reverse(Val)), parse_space(Rest, Line)};
parse_string([L|Rest], Line, Val) ->
	parse_string(Rest, Line, [L|Val]);
parse_string([], Line, _) ->
	throw({invalid_string, Line}).

%% @private
parse_array(Rest, Line, Acc) ->
	case parse_value(Rest, Line) of
		{<<>>, {[$[|Rest1], Line1}} ->
			{Value, {Rest2, Line2}} = parse_array(Rest1, Line1, []),
			parse_array(Rest2, Line2, [Value|Acc]);
		{<<>>, {[$,|_], _}} ->
			throw({invalid_array, Line});
		{Value, {[$,|Rest1], Line1}} ->
			{Rest2, Line2} = parse_space(Rest1, Line1),
			parse_array(Rest2, Line2, [Value|Acc]);
		{<<>>, {[$]|Rest1], Line1}} ->
			{lists:reverse(Acc), parse_space(Rest1, Line1)};
		{Value, {[$]|Rest1], Line1}} ->
			{lists:reverse([Value|Acc]), parse_space(Rest1, Line1)};
		_ ->
			throw({invalid_array, Line})
	end.

%% @private
parse_date([A1, A2, A3, A4, $-, M1, M2, $-, D1, D2, $T, H1, H2, $:, I1, I2, $:,
	        S1, S2, $Z], _Line)
			when is_integer(A1), is_integer(A2), is_integer(A3), is_integer(A4),
			     is_integer(M1), is_integer(M2), is_integer(D1), is_integer(D2),
			     is_integer(H1), is_integer(H2), is_integer(I1), is_integer(I2),
   			     is_integer(S1), is_integer(S2) ->
   	{
   		{
   			(A1-48)*1000+(A2-48)*100+(A3-48)*10+(A4-48), 
   			(M1-48)*10+(M2-48), (D1-48)*10+(D2-48)
   		},
   		{
   			(H1-48)*10+(H2-48), (I1-48)*10+(I2-48), (S1-48)*10+(S2-48)
   		}
	};
parse_date(_, Line) ->
	throw({invalid_date, Line}).

%% @private
parse_number(String, Line) ->
	case catch list_to_integer(String) of
		{'EXIT', _} -> 
			case catch list_to_float(String) of
				{'EXIT', _} -> throw({invalid_number, Line});
				Float -> Float
			end;
		Integer ->
			Integer
	end.

%% @private
parse_space([9|Rest], Line) -> parse_space(Rest, Line);
parse_space([32|Rest], Line) -> parse_space(Rest, Line);
parse_space([10|Rest], Line) -> parse_space(Rest, Line+1);
parse_space([13, 10|Rest], Line) -> parse_space(Rest, Line+1);
parse_space([$#|Rest], Line) -> parse_comment(Rest, Line);
parse_space(Rest, Line) -> {Rest, Line}. 

%% @private
parse_comment([10|Rest], Line) -> parse_space(Rest, Line+1);
parse_comment([13, 10|Rest], Line) -> parse_space(Rest, Line+1);
parse_comment([_|Rest], Line) -> parse_comment(Rest, Line);
parse_comment([], Line) -> {[], Line}.

%% @private
parse_text([], Line, Acc) ->
	{lists:reverse(Acc), {[], Line}};
parse_text([L|_]=Rest, Line, Acc) when L=:=9; L=:=32; L=:=$=; L=:=$,; L=:=$]->
	{lists:reverse(Acc), parse_space(Rest, Line)};
parse_text([10|Rest], Line, Acc) -> 
	{lists:reverse(Acc), parse_space(Rest, Line+1)};
parse_text([13, 10|Rest], Line, Acc) -> 
	{lists:reverse(Acc), parse_space(Rest, Line+1)};
parse_text([L|Rest], Line, Acc) ->
	parse_text(Rest, Line, [L|Acc]).

%% @private
parse_group([$.|Rest], Line, Acc1, Acc2) ->
	parse_group(Rest, Line, [], [list_to_binary(lists:reverse(Acc1))|Acc2]);
parse_group([$]|Rest], Line, Acc1, Acc2) ->
	{[list_to_binary(lists:reverse(Acc1))|Acc2], parse_space(Rest, Line)};
parse_group([L|_], Line, _Acc1, _Acc2) when L=:=9; L=:=32; L=:=10; L=:=13 ->
	throw({invalid_group, Line});
parse_group([L|Rest], Line, Acc1, Acc2) ->
	parse_group(Rest, Line, [L|Acc1], Acc2).



%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).

parser_test() ->
	{ok,[
		{<<"title">>, <<"TOML Example">>},
     	{<<"owner">>, [
     		{<<"dob">>, {{1979,5,27},{7,32,0}}},
       		{<<"bio">>, <<"GitHub Cofounder & CEO\nLikes tater tots and beer.">>},
       		{<<"organization">>, <<"GitHub">>},
       		{<<"name">>, <<"Tom Preston-Werner">>}
       	]},
     	{<<"database">>, [
     		{<<"enabled">>, true},
       		{<<"connection_max">>, 5000},
       		{<<"ports">>, [8001,8001,8002]},
       		{<<"server">>, <<"192.168.1.1">>}]},
     		{<<"servers">>, [
     			{<<"beta">>, [
     				{<<"dc">>, <<"eqdc10">>},
     				{<<"ip">>, <<"10.0.0.2">>}
     			]},
       			{<<"alpha">>, [
       				{<<"dc">>, <<"eqdc10">>},
       				{<<"ip">>, <<"10.0.0.1">>}
       			]}
       		]},
     	{<<"clients">>, [
      		{<<"hosts">>, [<<"alpha">>,<<"omega">>]},
       		{<<"data">>, [
       			[<<"gamma">>, <<"delta">>],
       			[1,2]
       		]}
       	]}
    ]} = parse(test_msg()).

parser2_test() ->
	{ok,[{[<<"title">>],<<"TOML Example">>},
    	 {[<<"owner">>,<<"name">>],<<"Tom Preston-Werner">>},
     	{[<<"owner">>,<<"organization">>],<<"GitHub">>},
     	{[<<"owner">>,<<"bio">>],
      	<<"GitHub Cofounder & CEO\nLikes tater tots and beer.">>},
     	{[<<"owner">>,<<"dob">>],{{1979,5,27},{7,32,0}}},
     	{[<<"database">>,<<"server">>],<<"192.168.1.1">>},
     	{[<<"database">>,<<"ports">>],[8001,8001,8002]},
     	{[<<"database">>,<<"connection_max">>],5000},
     	{[<<"database">>,<<"enabled">>],true},
     	{[<<"servers">>,<<"alpha">>,<<"ip">>],<<"10.0.0.1">>},
     	{[<<"servers">>,<<"alpha">>,<<"dc">>],<<"eqdc10">>},
     	{[<<"servers">>,<<"beta">>,<<"ip">>],<<"10.0.0.2">>},
     	{[<<"servers">>,<<"beta">>,<<"dc">>],<<"eqdc10">>},
     	{[<<"clients">>,<<"data">>],
      		[[<<"gamma">>,<<"delta">>],[1,2]]},
     	{[<<"clients">>,<<"hosts">>],[<<"alpha">>,<<"omega">>]}]} =
    parse2(test_msg()).

speed_test() ->
	Msg = test_msg(),
	Now = now(),
	N = 10000,
	speed_test(Msg, N),
	Diff = timer:now_diff(now(), Now) / 1000000,
	?debugFmt("~p passes/sec (~p Mbyes/sec)\n", 
				[round(N/Diff), round(N*length(Msg)/Diff/1024/1024)]).

speed_test(_, 0) -> 0;
speed_test(Msg, N) -> {ok, _} = parse(Msg), speed_test(Msg, N-1).

test_msg() ->
"# This is a TOML document. Boom.

title = \"TOML Example\"

[owner]
name = \"Tom Preston-Werner\"
organization = \"GitHub\"
bio = \"GitHub Cofounder & CEO\nLikes tater tots and beer.\"
dob = 1979-05-27T07:32:00Z # First class dates? Why not?

[database]
server = \"192.168.1.1\"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true

[servers]

  # You can indent as you please. Tabs or spaces. TOML don't care.
  [servers.alpha]
  ip = \"10.0.0.1\"
  dc = \"eqdc10\"

  [servers.beta]
  ip = \"10.0.0.2\"
  dc = \"eqdc10\"

[clients]
data = [ [\"gamma\", \"delta\"], [1, 2] ] # just an update to make sure parsers support it

# Line breaks are OK when inside arrays
hosts = [
  \"alpha\",
  \"omega\"
]
".

-endif.




