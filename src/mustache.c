/*
 * Copyright (c) 2011, Vicent Marti
 * Copyright (c) 2020, Jeremy Williams
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <mruby.h>
#include <mruby/string.h>
#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/value.h>
#include <mruby/array.h>
#include <mruby/hash.h>

#include "crustache.h"
#include "buffer.h"

#include <string.h>
#include <stdlib.h>
#include <ctype.h>

static void template_free(mrb_state* mrb, void *template);

struct mrb_data_type const mrb_template_type = { "Mustache::Template", template_free };

static struct RClass* parse_error;
static struct RClass* render_error;

static int template_setvar(mrb_state* mrb, crustache_var *variable, mrb_value rb_obj) {
  variable->data.mrb = mrb;
  switch (mrb_type(rb_obj)) {
  case MRB_TT_ARRAY:
    variable->type = CRUSTACHE_VAR_LIST;
    variable->data.u.obj = rb_obj;
    variable->size = RARRAY_LEN(rb_obj);
    break;

  case MRB_TT_HASH:
    variable->type = CRUSTACHE_VAR_CONTEXT;
    variable->data.u.obj = rb_obj;
    break;

  case MRB_TT_STRING:
    variable->type = CRUSTACHE_VAR_STR;
    variable->data.u.cstr = RSTRING_PTR(rb_obj);
    variable->size = RSTRING_LEN(rb_obj);
    break;

  case MRB_TT_FALSE:
    variable->type = CRUSTACHE_VAR_FALSE;
    break;

  default:
    if (mrb_respond_to(mrb, rb_obj, mrb_intern_lit(mrb, "call"))) {
      variable->type = CRUSTACHE_VAR_LAMBDA;
      variable->data.u.obj = rb_obj;
      break;
    }

    variable->type = CRUSTACHE_VAR_CONTEXT;
    variable->data.u.obj = rb_obj;
  }

  return 0;
}

static int api_partial(
  crustache_template **partial,
  const char *partial_name,
  size_t name_len
) {
  //mrb_raise(mrb, render_error, "Partials not currently supported");
  return -1;
}

static int api_context_get(
  crustache_var *var,
  crustache_var_data ctx,
  const char *key,
  size_t key_size
) {
  mrb_state* mrb = ctx.mrb;
  mrb_value obj = ctx.u.obj;
  mrb_value rb_key, rb_val;

  rb_key = mrb_str_new(mrb, key, (long)key_size);
  rb_val = mrb_nil_value();

  if (mrb_hash_p(obj)) {
    rb_val = mrb_hash_get(mrb, obj, rb_key);
    if (mrb_nil_p(rb_val))
      rb_val = mrb_hash_get(mrb, obj, mrb_symbol_value(mrb_intern_str(mrb, rb_key)));
  } else {
    mrb_sym method = mrb_intern_str(mrb, rb_key);

    if (mrb_respond_to(mrb, obj, method)) {
      rb_val = mrb_funcall(mrb, obj, RSTRING_CSTR(mrb, rb_key), 0);
    } else if (mrb_respond_to(mrb, obj, mrb_intern_lit(mrb, "[]"))) {
      rb_val = mrb_funcall(mrb, obj, "[]", 1, key);
    }
  }

  if (mrb_nil_p(rb_val)) { /* not found */
    return -1;
  }

  return template_setvar(mrb, var, rb_val);
}

static int api_list_get(
  crustache_var *var,
  crustache_var_data list,
  size_t i
) {
  mrb_state* mrb = list.mrb;
  mrb_value rb_array = list.u.obj;
  mrb_check_type(mrb, rb_array, MRB_TT_ARRAY);
  return template_setvar(mrb, var, mrb_ary_entry(rb_array, (long)i));
}

static int api_lambda(
  crustache_var *var,
  crustache_var_data lambda,
  const char *raw_template,
  size_t raw_size
) {
  mrb_state* mrb = lambda.mrb;
  mrb_value rb_lambda = lambda.u.obj;
  mrb_value rb_val, rb_tmpl;

  rb_tmpl = mrb_str_new(mrb, raw_template, (long)raw_size);
  rb_val = mrb_funcall(mrb, rb_lambda, "call", 1, rb_tmpl);
  mrb_check_type(mrb, rb_val, MRB_TT_STRING);

  return template_setvar(mrb, var, rb_val);
}

static void template_free(mrb_state* mrb, void *template) {
  (void)mrb;
  crustache_free((crustache_template *)template);
}

static void template_parser_error(mrb_state* mrb, crustache_template *template, int error) {
  const char *error_line;
  size_t line_len, line_n, col_n;

  error_line = crustache_error_syntaxline(&line_n, &col_n, &line_len, template);
  crustache_free(template);

  mrb_raisef(
    mrb,
    parse_error,
    "%s (line %d, col %d)\n\t%.*s\n\t%*s\n",
    crustache_strerror(error), (int)line_n, (int)col_n,
    (int)line_len, error_line,
    (int)col_n, "^"
  );
}

static void template_render_error(mrb_state* mrb, crustache_template *template, int error) {
  char error_node[256];
  crustache_error_rendernode(error_node, sizeof(error_node), template);
  mrb_raisef(mrb, render_error, "%s (%s)", crustache_strerror(error), error_node);
}

static mrb_value mustache_template_new(mrb_state* mrb, mrb_value self) {
  crustache_template *tmpl;
  int error;

  crustache_api default_api = {
    api_context_get,
    api_list_get,
    api_lambda,
    NULL,
    api_partial,
    0
  };

  mrb_value rb_raw_template;
  mrb_get_args(mrb, "S", &rb_raw_template);

  error = crustache_new(&tmpl,
    &default_api,
    RSTRING_PTR(rb_raw_template),
    RSTRING_LEN(rb_raw_template));

  if (error < 0)
    template_parser_error(mrb, tmpl, error);

  return mrb_obj_value(
    Data_Wrap_Struct(mrb, mrb_class_ptr(self), &mrb_template_type, tmpl)
  );
}

static mrb_value mustache_template_render(mrb_state* mrb, mrb_value self) {
  crustache_var ctx;
  struct buf *output_buf;

  int error;
  mrb_value result;

  mrb_value variable;
  mrb_get_args(mrb, "o", &variable);
  crustache_template* tmpl = DATA_PTR(self);
  template_setvar(mrb, &ctx, variable);
  output_buf = bufnew(128);

  error = crustache_render(output_buf, tmpl, &ctx);
  if (error < 0)
    template_render_error(mrb, tmpl, error);

  result = mrb_str_new(mrb, output_buf->data, output_buf->size);
  bufrelease(output_buf);

  return result;
}

void mrb_mruby_mustache_gem_init(mrb_state* mrb) {
  struct RClass* m = mrb_define_module(mrb, "Mustache");
  parse_error = mrb_define_class_under(mrb, m, "ParserError", mrb->eStandardError_class);
  render_error = mrb_define_class_under(mrb, m, "RenderError", mrb->eStandardError_class);

  struct RClass* c = mrb_define_class_under(mrb, m, "Template", mrb->object_class);
  mrb_define_class_method(mrb, c, "new", mustache_template_new, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, c, "render", mustache_template_render, MRB_ARGS_REQ(1));
}

void mrb_mruby_mustache_gem_final(mrb_state* mrb) {
}
