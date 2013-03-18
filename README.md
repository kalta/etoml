TOML language Erlang parser
===========================

A pure Erlang parser for [TOML](https://github.com/mojombo/toml).

* It implements all defined elements (according to version 
[f68d014bfd](https://github.com/mojombo/toml/commit/f68d014bfd4a84a64fb5f6a7c1a83a4162415d4b)). 
* It is fast. It can parse about 15Mbytes/sec on an Core2 Duo Macbook.
* It detects many errors, and returns the error type and line number. 


Usage
-----

```
git clone git@github.com:kalta/etoml.git
cd etoml
make
make test

$ erl -pa ebin
Erlang R15B02 (erts-5.9.2) [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.9.2  (abort with ^G)
1> etoml:parse("key=1").
{ok,[{<<"key">>,1}]}
```

You can also include etoml in your application as a rebar dependency in your 
`rebar.config` file:

```erlang
{deps, [{etoml, ".*",{git, "git@github.com:kalta/etoml.git", master}}]}.
```



Example
-------

The usual TOML example

```toml
# This is a TOML document. Boom.

title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
organization = "GitHub"
bio = "GitHub Cofounder & CEO\nLikes tater tots and beer."
dob = 1979-05-27T07:32:00Z # First class dates? Why not?

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true

[servers]

  # You can indent as you please. Tabs or spaces. TOML don't care.
  [servers.alpha]
  ip = "10.0.0.1"
  dc = "eqdc10"

  [servers.beta]
  ip = "10.0.0.2"
  dc = "eqdc10"

[clients]
data = [ ["gamma", "delta"], [1, 2] ] # just an update to make sure parsers support it

# Line breaks are OK when inside arrays
hosts = [
  "alpha",
  "omega"
]
```

is parsed calling `etoml:parse/1` as:

```erlang
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
    ]}
```

etoml generates an intermediate parse result that can also be useful. If `etoml:parse2/1`
is used instead of `etoml:parse/1` the result is:

```erlang
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
     	{[<<"clients">>,<<"data">>], [[<<"gamma">>,<<"delta">>],[1,2]]},
     	{[<<"clients">>,<<"hosts">>],[<<"alpha">>,<<"omega">>]}]}
```
