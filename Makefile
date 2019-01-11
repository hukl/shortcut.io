.PHONY: test docker_chatty_image docker_db_setup_image docker_demo_image


default:
	rebar3 compile

test:
	rebar3 compile && ETEST_ARGS="-config config/test.config" _build/default/lib/etest/bin/etest-runner

test_debug:
	rebar3 compile && ETEST_ARGS="-config config/test.config" _build/default/lib/etest/bin/etest-runner

shell:
	ERL_FLAGS=" -config config/development.config" rebar3 shell

dev:
	rebar3 release && _build/default/rel/development/bin/development console

dialyzer:
	rebar3 dialyzer
