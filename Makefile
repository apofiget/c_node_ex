all: compile

compile: get-deps
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
