-module(yaml_emitter).
-export([emit/1, emit/2]).

-record(state, {events :: [yaml_libyaml:event()], schema, schema_state}).

-define(APPEND(State, Element), State#state{events = [Element | State#state.events]}).

emit(Object) -> emit(Object, yaml_schema_failsafe, []).

emit(Object, Options) ->
  Schema = proplists:get_value(schema, Options, yaml_schema_failsafe),
  emit(Object, Schema, Options).

emit(Object, Schema, Options) ->
  SchemaState = Schema:init(Options),
  State = emit_object(Object, #state{events = [{document_start, {}}, {stream_start, {}}], schema = Schema, schema_state = SchemaState}),
  lists:reverse([{stream_end, {}}, {document_end, {}} | State#state.events]).

marshal(Object, #state{schema = Schema, schema_state = SchemaState}) ->
  Schema:marshal(Object, SchemaState).

emit_object({scalar, Scalar}, State) -> emit_object({scalar, Scalar, null, any}, State);
emit_object({scalar, Scalar, Tag, Style}, State) -> ?APPEND(State, {scalar, {null, Tag, Scalar, Style}});
emit_object({sequence, Sequence}, State) -> emit_object({sequence, Sequence, null}, State);
emit_object({sequence, Sequence, Tag}, State) -> emit_sequence(Sequence, ?APPEND(State, {sequence_start, {null, Tag, any}}));
emit_object({map, Map}, State) -> emit_object({map, Map, null}, State);
emit_object({map, Map, Tag}, State) -> emit_map(Map, ?APPEND(State, {mapping_start, {null, Tag, any}}));
emit_object(Object, State) -> emit_object(marshal(Object, State), State).

emit_sequence([], State) ->
  ?APPEND(State, {sequence_end});
emit_sequence([H | T], State) ->
  emit_sequence(T, emit_object(H, State)).

emit_map([], State) ->
  ?APPEND(State, {mapping_end});
emit_map([{Key, Value} | T], State) ->
  emit_map(T, emit_object(Value, emit_object(Key, State))).
