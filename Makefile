REBAR = ./rebar

all: app

app:
	erlc src/etoml.erl -o ebin

launch: 
	erl -name -pa ebin 
