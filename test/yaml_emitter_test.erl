-module(yaml_emitter_test).
-include_lib("eunit/include/eunit.hrl").

-define(DOC_PRE, [{stream_start, {}}, {document_start, {}}]).
-define(DOC_POST, [{document_end, {}}, {stream_end, {}}]).
-define(DOC(Elements), ?DOC_PRE ++ Elements ++ ?DOC_POST).
-define(SEQ(Elements), [{sequence_start, {null, null, any}}] ++ Elements ++ [{sequence_end}]).
-define(MAP(Elements), [{mapping_start, {null, null, any}}] ++ Elements ++ [{mapping_end}]).

scalar_test() ->
  ?assertEqual(?DOC([{scalar, {null, null, <<"foo">>, any}}]), yaml_emitter:emit("foo")),
  ?assertEqual(?DOC([{scalar, {null, null, <<"foo">>, any}}]), yaml_emitter:emit(<<"foo">>)),
  ?assertEqual(?DOC([{scalar, {null, null, <<"123">>, any}}]), yaml_emitter:emit(123)),
  ?assertEqual(?DOC([{scalar, {null, null, <<"123.456">>, any}}]), yaml_emitter:emit(123.456)),
  ?assertEqual(?DOC([{scalar, {null, null, <<"">>}, any}]), yaml_emitter:emit([])).

list_test() ->
  ?assertEqual(?DOC(?SEQ([])), yaml_emitter:emit({sequence, []})),
  ?assertEqual(?DOC(?SEQ([{scalar, {null, null, <<"foo">>, any}}, {scalar, {null, null, <<"bar">>}}])),
    yaml_emitter:emit(["foo", "bar"])).

map_test() ->
  ?assertEqual(?DOC(?MAP([])), yaml_emitter:emit({map, []})),
  ?assertEqual(?DOC(?MAP([
    {scalar, {null, null, <<"foo">>, any}},
    {scalar, {null, null, <<"123">>, any}},
    {scalar, {null, null, <<"bar">>, any}},
    {scalar, {null, null, <<"456">>, any}}
  ])), yaml_emitter:emit([{"foo", 123}, {"bar", 456}])).

ruby_scalar_test() ->
  ?assertEqual(?DOC([{scalar, {null, null, <<"foo">>, any}}]), yaml_emitter:emit("foo", [{schema, yaml_schema_ruby}])),
  ?assertEqual(?DOC([{scalar, {null, null, <<"123">>, single_quoted}}]), yaml_emitter:emit("123", [{schema, yaml_schema_ruby}])),
  ?assertEqual(?DOC([{scalar, {null, null, <<"123">>, single_quoted}}]), yaml_emitter:emit(<<"123">>, [{schema, yaml_schema_ruby}])),
  ?assertEqual(?DOC([{scalar, {null, null, <<":foo">>, any}}]), yaml_emitter:emit(foo, [{schema, yaml_schema_ruby}])).
