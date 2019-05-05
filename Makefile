all: compile local

compile:
	rebar3 compile

local:
	erl -pa _build/default/lib/*/ebin/ -config config/sys.config -s camelcup
