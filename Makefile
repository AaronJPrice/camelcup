all: compile local

compile:
	rebar3 compile

local:
	erl -pa _build/default/lib/*/ebin/ -s camelcup

ct:
	rebar3 ct
