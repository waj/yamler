-module(yaml_schema_ruby_test).
-include_lib("eunit/include/eunit.hrl").

bool_load_test() ->
  ?assertEqual({ok, [true]}, yaml:load(<<"--- true\n...\n">>, [{schema, yaml_schema_ruby}])),
  ?assertEqual({ok, [false]}, yaml:load(<<"--- false\n...\n">>, [{schema, yaml_schema_ruby}])),
  ?assertEqual({ok, [123]}, yaml:load(<<"--- 123\n...\n">>, [{schema, yaml_schema_ruby}])),
  ?assertEqual({ok, [83]}, yaml:load(<<"--- 0123\n...\n">>, [{schema, yaml_schema_ruby}])),
  ?assertEqual({ok, ["019"]}, yaml:load(<<"--- 019\n...\n">>, [{schema, yaml_schema_ruby}])).

bool_dump_test() ->
  ?assertEqual(<<"--- true\n...\n">>, yaml:dump(true, [{schema, yaml_schema_ruby}])),
  ?assertEqual(<<"--- false\n...\n">>, yaml:dump(false, [{schema, yaml_schema_ruby}])).

string_dump_test() ->
  ?assertEqual(<<"--- foo\n...\n">>, yaml:dump("foo", [{schema, yaml_schema_ruby}])),
  ?assertEqual(<<"--- '123'\n...\n">>, yaml:dump("123", [{schema, yaml_schema_ruby}])),
  ?assertEqual(<<"--- 'true'\n...\n">>, yaml:dump("true", [{schema, yaml_schema_ruby}])),
  ?assertEqual(<<"--- 'false'\n...\n">>, yaml:dump("false", [{schema, yaml_schema_ruby}])).
