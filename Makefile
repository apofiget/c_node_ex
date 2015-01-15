all: compile

compile: get-deps
	@(if test ! -d "priv"; then mkdir priv ; fi)
	@rebar compile

get-deps:
	@rebar get-deps

clean:
	@rebar clean
	rm -rf log/
	rm -rf logs/
	rm -f erl_crash.dump

tests:
	@rebar ct

start:
	ERL_LIBS=deps erl -pa ebin -sname c_node_ex -setcookie COOKIE

.PHONY: compile
