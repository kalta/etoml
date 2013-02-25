%% -------------------------------------------------------------------
%%
%% etoml: A parser and generator for TOML language (https://github.com/mojombo/toml)
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

%% @doc TOML language parser and generator
%% @see https://github.com/mojombo/toml

-module(etoml).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
% -include_lib("eunit/include/eunit.hrl").

-export([t/0, parse/1]).
-compile([export_all]).

t1() ->
	Now = now(),
	N = 10000,
	t1(N),
	Diff = timer:now_diff(now(), Now) / 1000000,
	io:format("~p passes/sec\n", [round(N/Diff)]).



t1(0) -> 0;
t1(N) -> {ok, _} = t(), t1(N-1).


% LF -> 10 \n
% CR -> 13 \r

t2() ->
	A = [
		{[a,b], val_ab},
		{[a,c], cal_ac},
		{[a,c,d], {val_acd}}
	],
	join(A, []).


t() ->
	Msg = <<"
# This is a TOML document. Boom.

title = \"TOML Example\"
[owner]
name =  \"Tom Preston-Werner\"
organization = \"GitHub\"
bio = \"GitHub Cofounder & CEO\nLikes tater tots and beer.\"
dob = 1979-05-27T07:32:00Z # First class dates? Why not?

[database]
server = \"192.168.1.1\"

ports = [ 8001, 8001, 8002 ]
connection_max = 5000.1
enabled = true

[servers]

  # You can indent as you please. Tabs or spaces. TOML don't care.
  [servers.alpha]
  ip = \"10.0.0.1\"
  dc = \"eqdc10\"

  [servers.beta]
  ip = \"10.0.0.2\"
  ip = 1
  dc = \"eqdc10\"
[clients]
data = [ [\"gamma\", \"delta\"], [1, 2] ] # just an update to make sure parsers support it
	">>,
	parse(Msg).



%% ===================================================================
%% Public
%% ===================================================================

parse(Msg) ->
	case parse2(Msg) of
		{ok, List} -> {ok, join(List, [])};
		{error, Error} -> {error, Error}
	end.


join([], Acc) -> lists:reverse(Acc);
join([{Keys, Val}|Rest], Acc) -> 
	join(Rest, join(Keys, Val, Acc)).

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




parse2(Msg) when is_binary(Msg) ->
	parse2(binary_to_list(Msg));

parse2(Msg) when is_list(Msg) ->
	try 
		{Rest, Line} = parse_space(Msg, 1),
		parse(Rest, Line, [], [])
	catch
		throw:Error -> {error, Error}
	end.






	
parse([], _Line, _Names, Data) ->
	{ok, lists:reverse(Data)};
parse([$[|Rest], Line, _Names, Data) ->
	{Rest1, Line1} = parse_space(Rest, Line),
	{Names1, {Rest2, Line2}} = parse_names(Rest1, Line1),	
	parse(Rest2, Line2, Names1, Data);
parse(Rest, Line, Names, Data) ->
	% io:format("Parsing ~p: ~p\n\n", [Line, Rest]),
	{{Key, Value}, {Rest1, Line1}} = parse_key(Rest, Line),
	Keys = lists:reverse([Key|Names]),
	parse(Rest1, Line1, Names, [{Keys, Value}|Data]).


parse_key(Rest, Line) ->
	case parse_text(Rest, Line) of
		{Key, {[$=|Rest1], Line1}} ->
			{Rest2, Line2} = parse_space(Rest1, Line1),
			{Value, {Rest3, Line3}} = parse_value(Rest2, Line2),
			{{list_to_binary(Key), Value}, {Rest3, Line3}};
		_ ->
			throw({invalid_key, Line})
	end.


parse_value([$"|Rest], Line) ->	
	parse_string(Rest, Line, []);
parse_value([$[|Rest], Line) ->	
	{Rest1, Line1} = parse_space(Rest, Line),
	parse_array(Rest1, Line1, []);
parse_value(Rest, Line) ->
	case parse_text(Rest, Line) of
		{"true", Pos} -> {true, Pos};
		{"false", Pos} -> {false, Pos};
		{[_,_,_,_,$-|_]=Date, Pos} -> {parse_date(Date, Line), Pos};
		{Number, Pos} -> {parse_number(Number, Line), Pos}
	end.


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
	throw({unfinished_string, Line}).


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

parse_space([9|Rest], Line) -> parse_space(Rest, Line);
parse_space([32|Rest], Line) -> parse_space(Rest, Line);
parse_space([10|Rest], Line) -> parse_space(Rest, Line+1);
parse_space([13, 10|Rest], Line) -> parse_space(Rest, Line+1);
parse_space([$#|Rest], Line) -> parse_comment(Rest, Line);
parse_space(Rest, Line) -> {Rest, Line}. 


parse_comment([10|Rest], Line) -> parse_space(Rest, Line+1);
parse_comment([13, 10|Rest], Line) -> parse_space(Rest, Line+1);
parse_comment([_|Rest], Line) -> parse_comment(Rest, Line);
parse_comment([], Line) -> {[], Line}.


parse_text(Rest, Line) ->
	parse_text(Rest, Line, []).

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

parse_names(Rest, Line) ->
	parse_names(Rest, Line, [], []).

parse_names([$.|Rest], Line, Acc1, Acc2) ->
	parse_names(Rest, Line, [], [list_to_binary(lists:reverse(Acc1))|Acc2]);
parse_names([$]|Rest], Line, Acc1, Acc2) ->
	{[list_to_binary(lists:reverse(Acc1))|Acc2], parse_space(Rest, Line)};
parse_names([L|_], Line, _Acc1, _Acc2) when L=:=9; L=:=32; L=:=10; L=:=13 ->
	throw({invalid_name, Line});
parse_names([L|Rest], Line, Acc1, Acc2) ->
	parse_names(Rest, Line, [L|Acc1], Acc2).


