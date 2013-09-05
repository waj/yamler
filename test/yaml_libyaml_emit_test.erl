-module(yaml_libyaml_emit_test).
-include_lib("eunit/include/eunit.hrl").

-define(DOC_PRE, [{stream_start, {}}, {document_start, {}}]).
-define(DOC_POST, [{document_end, {}}, {stream_end, {}}]).
-define(DOC(Elements), ?DOC_PRE ++ Elements ++ ?DOC_POST).
-define(SEQ(Elements), [{sequence_start, {null, null, any}}] ++ Elements ++ [{sequence_end}]).
-define(MAP(Elements), [{mapping_start, {null, null, any}}] ++ Elements ++ [{mapping_end}]).

scalar_test_() ->
  [
    fun() ->
      Yaml = yaml_libyaml:libyaml_emit(?DOC([Scalar])),
      ?assertEqual(Expected, Yaml)
    end ||
    {Scalar, Expected} <- [
      {{scalar, {null, null, <<"foo">>, any}}, <<"--- foo\n...\n">>},
      {{scalar, {null, null, <<"123">>, single_quoted}}, <<"--- '123'\n...\n">>},
      {{scalar, {null, <<"!foo">>, <<"foo">>, any}}, <<"--- !foo foo\n...\n">>}
    ]
  ].

sequence_test_() ->
  [
    fun() ->
      Yaml = yaml_libyaml:libyaml_emit(?DOC(?SEQ(Elements))),
      ?assertEqual(Expected, Yaml)
    end ||
    {Elements, Expected} <- [
      {[], <<"--- []\n...\n">>},
      {[
        {scalar, {null, null, <<"foo">>, any}},
        {scalar, {null, null, <<"bar">>, any}}
      ], <<"---\n- foo\n- bar\n...\n">>},
      {?SEQ(?SEQ([{scalar, {null, null, <<"foo">>, any}}])), <<"---\n- - - foo\n...\n">>}
    ]
  ].

map_test_() ->
  [
    fun() ->
      Yaml = yaml_libyaml:libyaml_emit(?DOC(?MAP(Elements))),
      ?assertEqual(Expected, Yaml)
    end ||
    {Elements, Expected} <- [
      {[], <<"--- {}\n...\n">>},
      {[
        {scalar, {null, null, <<"foo">>, any}},
        {scalar, {null, null, <<"bar">>, any}}
      ], <<"---\nfoo: bar\n...\n">>},
      {?MAP([]) ++ ?MAP([]), <<"---\n{}: {}\n...\n">>}
    ]
  ].

map_with_tag_test() ->
  Yaml = yaml_libyaml:libyaml_emit(?DOC([{mapping_start, {null, <<"!foo">>, any}}, {mapping_end}])),
  ?assertEqual(<<"--- !foo {}\n...\n">>, Yaml).

nested_list(0, L) -> L;
nested_list(N, L) -> nested_list(N - 1, [L]).

large_object_test() ->
  X = yaml_libyaml:libyaml_emit(yaml_emitter:emit(nested_list(10000, []))),
  ?assert(erlang:byte_size(X) > 10000).

error_test_() ->
  [
    fun() ->
      {error, Error} = yaml_libyaml:libyaml_emit(Doc),
      ?assertEqual(Expected, Error)
    end ||
    {Expected, Doc} <- [
      {invalid_argument, foo},
      {empty_event_list, []},
      {{invalid_event, foo}, [foo]},
      {{invalid_event, {foo}}, [{foo}]},
      {{invalid_event, {scalar}}, [{scalar}]},
      {{invalid_event, {scalar, foo}}, [{scalar, foo}]},
      {{invalid_event, {scalar, {}}}, [{scalar, {}}]},
      {{invalid_event, {scalar, {null, bar, <<"">>, any}}}, [{scalar, {null, bar, <<"">>, any}}]},
      {{invalid_event, {scalar, {null, null, null, any}}}, [{scalar, {null, null, null, any}}]},
      {{invalid_event, {scalar, {null, null, <<"">>, foo}}}, [{scalar, {null, null, <<"">>, foo}}]},
      {{invalid_event, {mapping_start}}, [{mapping_start}]},
      {{invalid_event, {mapping_start, foo}}, [{mapping_start, foo}]},
      {{invalid_event, {mapping_start, {}}}, [{mapping_start, {}}]},
      {{invalid_event, {mapping_start, {null, foo, any}}}, [{mapping_start, {null, foo, any}}]},
      {{invalid_event, {mapping_start, {null, null, foo}}}, [{mapping_start, {null, null, foo}}]},
      {missing_stream_start, [{document_start, {}}]},
      {{invalid_event, {document_end, {}}, <<"expected DOCUMENT-START or STREAM-END">>}, [{stream_start, {}}, {document_end, {}}]}
    ]
  ].
