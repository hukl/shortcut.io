.PHONY: test docker_chatty_image docker_db_setup_image docker_demo_image


default:
	rebar3 compile

test:
	rebar3 compile && ETEST_ARGS="-config config/test.config" WITH_COVERAGE=true ETEST_BUILD_DIR="_build/default/lib/scio" _build/default/lib/etest/bin/etest-runner

test_debug:
	rebar3 compile && ETEST_ARGS="-config config/test.config" _build/default/lib/etest/bin/etest-runner

shell:
	ERL_FLAGS=" -config config/development.config" rebar3 shell

dev:
	rebar3 release -n development && _build/default/rel/development/bin/development console

release:
	rebar3 release -n production

dialyzer:
	rebar3 dialyzer

schema_dump:
	pg_dump -h localhost -U shortcut -c -s shortcut_dev > priv/db/schema.sql

schema_load: schema_load_dev schema_load_test

schema_load_dev:
	psql -h localhost -d shortcut_dev < priv/db/schema.sql

schema_load_test:
	psql -h localhost -d shortcut_test < priv/db/schema.sql

schema_load_production:
	psql -h localhost -d shortcut_production < priv/db/schema.sql
