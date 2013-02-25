
.PHONY: all compile clean eunit test doc

all: compile

compile:
	./rebar compile

clean:
	./rebar clean
	rm -f doc/edoc-info
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/*.css

eunit:
	./rebar eunit

test: eunit

doc:
	./rebar doc