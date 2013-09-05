/**
 * author: Daniel Goertzen <daniel.goertzen@gmail.com>
 * contributor: Sergei Lebedev <superbobry@gmail.com>
 * copyright: 2012 Daniel Goertzen
 * license: See file /LICENSE
 */

#include "erl_nif.h"
#include <yaml.h>

#define ATOM(s)         enif_make_atom(env, s)
#define BOOL(v)         ATOM(v ? "true" : "false")
#define INT(n)          enif_make_int(env, n)
#define ULONG(ln)       enif_make_ulong(env, ln)
#define TUPLE2(x, y)    enif_make_tuple2(env, x, y)
#define TUPLE3(x, y, z) enif_make_tuple3(env, x, y, z)
#define TUPLE4(x, y, z, a) enif_make_tuple4(env, x, y, z, a)
#define BINARY(s)       (!s ? ATOM("null") : MEMORY(s, strlen((const char *) s)))
#define MEMORY(s, len)  enif_wrap_binary(env, s, len)
#define ENUM(table, value) ATOM(table[value])

#define UNUSED  __attribute__((unused))

static const char *encodings[] = {"any", "utf8", "utf16le", "utf16be"};
static const char *breaks[] UNUSED = {"any", "cr", "ln", "crln"};
static const char *error_types[] = {
    "no", "memory",
    "reader", "scanner", "parser",
    "composer", "writer", "emitter"
};
static const char *scalar_styles[] = {
    "any", "plain", "single_quoted", "double_quoted", "literal", "folded"
};
static const char *sequence_styles[] = {"any", "block", "flow"};
static const char *mapping_styles[]  = {"any", "block", "flow"};
static const char *event_types[] = {
    "no",
    "stream_start", "stream_end",
    "document_start", "document_end",
    "alias", "scalar",
    "sequence_start", "sequence_end",
    "mapping_start", "mapping_end"
};


static inline ERL_NIF_TERM
enif_wrap_binary(ErlNifEnv *env, const unsigned char *cstr, const size_t len)
{
    unsigned char *bin;
    ERL_NIF_TERM term;

    if (!cstr)
        term = ATOM("null");
    else {
        bin = enif_make_new_binary(env, len, &term);
        memcpy(bin, cstr, len);
    }

    return term;
}


static ERL_NIF_TERM
version_directive_to_term(ErlNifEnv *env, const yaml_version_directive_t *version)
{
    if (!version)
        return ATOM("null");
    else
        return TUPLE2(INT(version->major), INT(version->minor));
}


static inline ERL_NIF_TERM
tag_directive_to_term(ErlNifEnv *env, yaml_tag_directive_t *tag)
{
    return TUPLE2(BINARY(tag->handle),
                  BINARY(tag->prefix));
}

static inline ERL_NIF_TERM
mark_to_term(ErlNifEnv *env, const yaml_mark_t *mark)
{
    if (!mark)
        return ATOM("null");
    else
        return TUPLE3(ULONG(mark->index), ULONG(mark->line), ULONG(mark->column));
}


static ERL_NIF_TERM event_to_term(ErlNifEnv *env, const yaml_event_t *event)
{
    const yaml_event_type_t type = event->type;
    ERL_NIF_TERM term;

    switch (type) {
    case YAML_STREAM_START_EVENT:
        term = ENUM(encodings, event->data.stream_start.encoding);
        break;
    case YAML_DOCUMENT_START_EVENT:
        term = enif_make_list(env, 0);

        // iterate backwards so list ends up in right order
        for (yaml_tag_directive_t *tag_directive = event->data.document_start.tag_directives.end;
             event->data.document_start.tag_directives.start != tag_directive;
             tag_directive--) {
            term = enif_make_list_cell(
                env, tag_directive_to_term(env, tag_directive), term);
        }

        term = TUPLE3(version_directive_to_term(env, event->data.document_start.version_directive),
                      term,
                      BOOL(event->data.document_start.implicit));
        break;
    case YAML_DOCUMENT_END_EVENT:
        term = BOOL(event->data.document_end.implicit);
        break;
    case YAML_ALIAS_EVENT:
        term = BINARY(event->data.alias.anchor);
        break;
    case YAML_SCALAR_EVENT:
        term = TUPLE4(
            BINARY(event->data.scalar.anchor),
            BINARY(event->data.scalar.tag),
            MEMORY(event->data.scalar.value, event->data.scalar.length),
            /* FIXME(Sergei): why is this commented out?
               BOOL(event->data.scalar.plain_implicit),
               BOOL(event->data.scalar.quoted_implicit), */
            ENUM(scalar_styles, event->data.scalar.style));
        break;
    case YAML_SEQUENCE_START_EVENT:
        term = TUPLE3(
            BINARY(event->data.sequence_start.anchor),
            BINARY(event->data.sequence_start.tag),
            /* FIXME(Sergei): why is this commented out?
              BOOL(event->data.sequence_start.implicit), */
            ENUM(sequence_styles, event->data.sequence_start.style));
        break;
    case YAML_MAPPING_START_EVENT:
        term = TUPLE3(
            BINARY(event->data.mapping_start.anchor),
            BINARY(event->data.mapping_start.tag),
            /* FIXME(Sergei): same for this.
               BOOL(event->data.mapping_start.implicit), */
            ENUM(mapping_styles, event->data.mapping_start.style));
        break;
    default:
        term = ATOM("null");
        break;
    }

    return TUPLE4(ENUM(event_types, type), term,
                  mark_to_term(env, &event->start_mark),
                  mark_to_term(env, &event->end_mark));
}



static ERL_NIF_TERM
binary_to_libyaml_event_stream_rev(ErlNifEnv* env,
                                   int argc UNUSED,
                                   const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM term = enif_make_list(env, 0);
    ERL_NIF_TERM status;
    ErlNifBinary bin;
    yaml_parser_t parser;
    yaml_event_t event;
    int done = 0;
    char msg[256] = {0};

    if (!enif_inspect_binary(env, argv[0], &bin))
        return enif_make_badarg(env);

    yaml_parser_initialize(&parser);
    yaml_parser_set_input_string(&parser, bin.data, bin.size);

    while (!done) {
        if (!yaml_parser_parse(&parser, &event))
            goto parser_error;

        term = enif_make_list_cell(env, event_to_term(env, &event), term);
        done = event.type == YAML_STREAM_END_EVENT;
        yaml_event_delete(&event);
    }

    status = ATOM("ok");
    goto parser_done;

parser_error:
    switch (parser.error) {
    case YAML_MEMORY_ERROR:
        snprintf(msg, sizeof(msg), "Memory error: Not enough memory for parsing");
        break;
    case YAML_READER_ERROR:
        if (parser.problem_value != -1)
            snprintf(msg, sizeof(msg), "Reader error: %s: #%X at %zu",
                     parser.problem,
                     parser.problem_value, parser.problem_offset);
        else
            snprintf(msg, sizeof(msg), "Reader error: %s at %zu",
                     parser.problem,
                     parser.problem_offset);
        break;
    case YAML_SCANNER_ERROR:
        if (parser.context)
            snprintf(msg, sizeof(msg), "Scanner error: %s at line %zu, column %zu\n"
                     "%s at line %zu, column %zu",
                     parser.context,
                     parser.context_mark.line + 1, parser.context_mark.column + 1,
                     parser.problem, parser.problem_mark.line + 1,
                     parser.problem_mark.column + 1);
        else
            snprintf(msg, sizeof(msg), "Scanner error: %s at line %zu, column %zu",
                     parser.problem,
                     parser.problem_mark.line + 1,
                     parser.problem_mark.column + 1);
        break;
    case YAML_PARSER_ERROR:
        if (parser.context)
            snprintf(msg, sizeof(msg), "Parser error: %s at line %zu, column %zu\n"
                     "%s at line %zu, column %zu",
                     parser.context,
                     parser.context_mark.line + 1, parser.context_mark.column + 1,
                     parser.problem, parser.problem_mark.line + 1,
                     parser.problem_mark.column + 1);
        else
            snprintf(msg, sizeof(msg), "Parser error: %s at line %zu, column %zu",
                     parser.problem,
                     parser.problem_mark.line + 1,
                     parser.problem_mark.column + 1);
        break;
    case YAML_NO_ERROR:
    case YAML_COMPOSER_ERROR:
    case YAML_WRITER_ERROR:
    case YAML_EMITTER_ERROR:
        break;
    }

    status = ATOM("error");
    term   = TUPLE2(ENUM(error_types, parser.error),
                    MEMORY((const unsigned char *) msg, strlen(msg)));
    goto parser_done;

parser_done:
    yaml_parser_delete(&parser);
    return TUPLE2(status, term);
}

static void
initialize_scalar_event(ErlNifEnv* env, const ERL_NIF_TERM *elements, yaml_event_t *event)
{
  int argc;
  const ERL_NIF_TERM *args;
  yaml_char_t *tag = NULL;
  ErlNifBinary tagBin;
  ErlNifBinary bin;
  yaml_scalar_style_t style;

  enif_get_tuple(env, elements[1], &argc, &args);

  if (enif_is_binary(env, args[1])) {
    enif_inspect_binary(env, args[1], &tagBin);
    tag = (yaml_char_t*) strndup((const char *)tagBin.data, tagBin.size);
  }

  if (enif_compare(args[3], ATOM("single_quoted")) == 0) {
    style = YAML_SINGLE_QUOTED_SCALAR_STYLE;
  } else {
    style = YAML_ANY_SCALAR_STYLE;
  }

  enif_inspect_binary(env, args[2], &bin);
  yaml_scalar_event_initialize(event, NULL, tag, (yaml_char_t*)bin.data, bin.size, tag == NULL ? 1 : 0, tag == NULL ? 1 : 0, style);

  if (tag != NULL) free(tag);
}

static void
term_to_event(ErlNifEnv* env, ERL_NIF_TERM term, yaml_event_t *event)
{
  int arity;
  const ERL_NIF_TERM *elements;

  enif_get_tuple(env, term, &arity, &elements);
  if (arity == 0 || !enif_is_atom(env, elements[0])) {
    printf("Todo mal!!\n"); fflush(stdout);
  }

  if (enif_compare(elements[0], ATOM("stream_start")) == 0) {
    yaml_stream_start_event_initialize(event, YAML_ANY_ENCODING);
  } else if (enif_compare(elements[0], ATOM("stream_end")) == 0) {
    yaml_stream_end_event_initialize(event);
  } else if (enif_compare(elements[0], ATOM("document_start")) == 0) {
    yaml_document_start_event_initialize(event, NULL, NULL, NULL, 0);
  } else if (enif_compare(elements[0], ATOM("document_end")) == 0) {
    yaml_document_end_event_initialize(event, 0);
  } else if (enif_compare(elements[0], ATOM("scalar")) == 0) {
    initialize_scalar_event(env, elements, event);
  } else if (enif_compare(elements[0], ATOM("sequence_start")) == 0) {
    yaml_sequence_start_event_initialize(event, NULL, NULL, 1, YAML_ANY_SEQUENCE_STYLE);
  } else if (enif_compare(elements[0], ATOM("sequence_end")) == 0) {
    yaml_sequence_end_event_initialize(event);
  } else if (enif_compare(elements[0], ATOM("mapping_start")) == 0) {
    yaml_mapping_start_event_initialize(event, NULL, NULL, 1, YAML_ANY_MAPPING_STYLE);
  } else if (enif_compare(elements[0], ATOM("mapping_end")) == 0) {
    yaml_mapping_end_event_initialize(event);
  } else {
    printf("???\n"); fflush(stdout);
  }
}

typedef struct {
  unsigned char *buffer;
  size_t capacity;
  size_t size;
} output_t;

int binary_emitter(void *data, unsigned char *buffer, size_t size) {
  output_t *output = data;
  if (output->size + size > output->capacity) {
    while (output->size + size > output->capacity)
      output->capacity *= 2;
    output->buffer = realloc(output->buffer, output->capacity);
  }

  memcpy(output->buffer + output->size, buffer, size);
  output->size += size;

  return 1;
}

static ERL_NIF_TERM
libyaml_emit(ErlNifEnv* env, int argc UNUSED, const ERL_NIF_TERM argv[])
{
  yaml_emitter_t emitter;
  yaml_event_t event;
  ERL_NIF_TERM list = argv[0];
  ERL_NIF_TERM event_term;
  output_t output;
  ERL_NIF_TERM result;

  output.capacity = 1024;
  output.size = 0;
  output.buffer = (unsigned char*) malloc(output.capacity);

  yaml_emitter_initialize(&emitter);
  yaml_emitter_set_output(&emitter, binary_emitter, &output);

  while (enif_get_list_cell(env, list, &event_term, &list)) {
    term_to_event(env, event_term, &event);
    if (!yaml_emitter_emit(&emitter, &event)) {
      printf("%s\n", emitter.problem); fflush(stdout);
    }
  }

  yaml_emitter_flush(&emitter);
  yaml_emitter_delete(&emitter);
  result = MEMORY(output.buffer, output.size);

  free(output.buffer);
  return result;
}


static ErlNifFunc nif_funcs[] = {
    {"binary_to_libyaml_event_stream_rev", 1, binary_to_libyaml_event_stream_rev},
    {"libyaml_emit", 1, libyaml_emit}
};


ERL_NIF_INIT(yaml_libyaml, nif_funcs, NULL, NULL, NULL, NULL)
