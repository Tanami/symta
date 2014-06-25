#include <dlfcn.h>
#include <stdarg.h>

#include "runtime.h"

#define VIEW(dst,base,start,size) \
  ALLOC_BASIC(dst, base, 1); \
  dst = ADD_TAG(dst, T_VIEW); \
  VIEW_REF4(dst,0) = (uint32_t)(start); \
  VIEW_REF4(dst,1) = (uint32_t)(size);
#define VIEW_START(o) VIEW_REF4(o,0)
#define VIEW_SIZE(o) VIEW_REF4(o,1)
#define VIEW_REF(o,start,i) *((void**)VIEW_GET(o,-1) + start + (i))

#define LIST_SIZE(o) OBJECT_CODE(o)

#define IS_TEXT(o) (GET_TAG(o) == T_DATA && DATA_TAG(o) == T_TEXT)
#define TEXT_SIZE(o) UNFIXNUM(DATA_REF4(o,0))
#define TEXT_DATA(o) ((char*)&DATA_REF1(o,4))

#define DATA_SIZE(o) ((uintptr_t)methods[0].types[DATA_TAG(o)])

#define C_ANY(o,arg_index,meta)

#define C_FIXNUM(o,arg_index,meta) \
  if (GET_TAG(o) != T_FIXNUM) \
    api->bad_type(REGS_ARGS(P), "integer", arg_index, meta)

#define C_TEXT(o,arg_index,meta) \
  if (GET_TAG(o) != T_FIXTEXT && !IS_TEXT(o)) \
    api->bad_type(REGS_ARGS(P), "text", arg_index, meta)

#define C_LIST(o,arg_index,meta) \
  if (GET_TAG(o) != T_LIST) \
    api->bad_type(REGS_ARGS(P), "cons", arg_index, meta)

#define C_CONS(o,arg_index,meta) \
  if (GET_TAG(o) != T_DATA || DATA_TAG(o) != T_CONS) \
    api->bad_type(REGS_ARGS(P), "cons", arg_index, meta)


#define BUILTIN_CLOSURE(dst,code) { ALLOC_CLOSURE(dst, code, 0); }
#define getVal(x) ((uintptr_t)(x)&~TAG_MASK)

#define MAX_TYPES (1024)
#define MAX_METHODS (8*1024)

static char *lib_path;

static api_t apis[2]; // one for each heap

typedef struct {
  char *name;
  void **types;
} method_t;

static int methods_used;
static method_t methods[MAX_METHODS];
static int types_used;
static char *typenames[MAX_TYPES];

static void *undefined;

static void **resolve_method(api_t *api, char *name) {
  int i, j;
  for (i = 0; i < methods_used; i++) {
    if (!strcmp(methods[i].name, name)) {
      return methods[i].types;
    }
  }
  if (methods_used == MAX_METHODS) {
    fprintf(stderr, "methods table overflow\n");
    abort();
  }
  ++methods_used;

  for (j = 0; j < types_used; j++) methods[i].types[j] = undefined;

  methods[i].name = strdup(name);
  return methods[i].types;
}

static int resolve_type(api_t *api, char *name) {
  int i, j;
  for (i = 0; i < types_used; i++)
    if (!strcmp(typenames[i], name))
      return i;
  if (types_used == MAX_TYPES) {
    fprintf(stderr, "typenames table overflow\n");
    abort();
  }
  ++types_used;
  typenames[i] = strdup(name);

  for (j = 0; j < methods_used; j++) methods[j].types[i] = undefined;

  return i;
}

static void set_method(api_t *api, void *method, void *type, void *handler) {
  LIFT(method,(uintptr_t)(type),handler);
}

static void set_type_size_and_name(struct api_t *api, intptr_t tag, intptr_t size, void *name) {
  methods[0].types[tag] = (void*)size;
  methods[1].types[tag] = name;
}

static void *tag_of(void *o) {
  uintptr_t tag = GET_TAG(o);
  if (tag == T_DATA) {
    tag = DATA_TAG(o);
  }
  return methods[1].types[tag];
}

static void fatal(char *fmt, ...) {
   va_list ap;
   va_start(ap,fmt);
   vfprintf(stderr, fmt, ap);
   va_end(ap);
   abort();
}

static void bad_type(REGS, char *expected, int arg_index, char *name) {
  PROLOGUE;
  int i, nargs = (int)UNFIXNUM(NARGS(E));
  printf("arg %d isnt %s, in: %s", arg_index, expected, name);
  for (i = 0; i < nargs; i++) printf(" %s", print_object(getArg(i)));
  printf("\n");
  abort();
}

static void bad_call(REGS, void *method) {
  PROLOGUE;
  int i, nargs = (int)UNFIXNUM(NARGS(E));
  printf("bad call: %s", print_object(getArg(0)));
  printf(" %s", print_object(method));
  for (i = 1; i < nargs; i++) printf(" %s", print_object(getArg(i)));
  printf("\n");
  abort();
}

static char *print_object_r(api_t *api, char *out, void *o);

// FIXME: use heap instead
static char print_buffer[1024*1024*2];
char* print_object_f(api_t *api, void *object) {
  print_object_r(api, print_buffer, object);
  return print_buffer;
}


#define MOD_ADLER 65521
uint32_t hash(uint8_t *data, int len) {
  uint32_t a = 1, b = 0;
  int index;
  for (index = 0; index < len; ++index) {
    a = (a + data[index]) % MOD_ADLER;
    b = (b + a) % MOD_ADLER;
  } 
  return (b << 16) | a;
}

#define BUILTIN_CHECK_NARGS(expected,tag,name) \
  if (NARGS(E) != FIXNUM(expected)) { \
    void *meta, *ttag, *t; \
    LIST_ALLOC(meta, 1); \
    TEXT(t, name); \
    LIST_REF(meta,0) = t; \
    if (tag) { \
      TEXT(ttag, tag); \
    } else { \
      ttag = Void; \
    } \
    return api->handle_args(REGS_ARGS(P), E, FIXNUM(expected), FIXNUM(0), ttag, meta); \
  }
#define BUILTIN_CHECK_VARARGS(expected,tag,name) \
  if (NARGS(E) < FIXNUM(expected)) { \
    void *meta, *ttag, *t; \
    LIST_ALLOC(meta, 1); \
    TEXT(t, name); \
    LIST_REF(meta,0) = t; \
    if (tag) { \
      TEXT(ttag, tag); \
    } else { \
      ttag = Void; \
    } \
    return api->handle_args(REGS_ARGS(P), E, -FIXNUM(expected), FIXNUM(0), ttag, meta); \
  }

#define BUILTIN0(sname, name) \
  static void *b_##name(REGS) { \
  PROLOGUE; \
  void *A, *R; \
  BUILTIN_CHECK_NARGS(0,0,sname);
#define BUILTIN1(sname,name,a_check,a) \
  static void *b_##name(REGS) { \
  PROLOGUE; \
  void *A, *R, *a; \
  BUILTIN_CHECK_NARGS(1,0,sname); \
  a = getArg(0); \
  a_check(a, 0, sname);
#define BUILTIN2(sname,name,a_check,a,b_check,b) \
  static void *b_##name(REGS) { \
  PROLOGUE; \
  void *A, *R, *a, *b; \
  BUILTIN_CHECK_NARGS(2,0,sname); \
  a = getArg(0); \
  a_check(a, 0, sname); \
  b = getArg(1); \
  b_check(b, 1, sname);
#define BUILTIN3(sname,name,a_check,a,b_check,b,c_check,c) \
  static void *b_##name(REGS) { \
  PROLOGUE; \
  void *A, *R, *a, *b,*c; \
  BUILTIN_CHECK_NARGS(3,0,sname); \
  a = getArg(0); \
  a_check(a, 0, sname); \
  b = getArg(1); \
  b_check(b, 1, sname); \
  c = getArg(2); \
  c_check(c, 2, sname);
#define BUILTIN_VARARGS(sname,name) \
  static void *b_##name(REGS) { \
  PROLOGUE; \
  void *A, *R; \
  BUILTIN_CHECK_VARARGS(0,0,sname);
#define RETURNS(r) Top = Base; return (void*)(r); }
#define RETURNS_VOID Top = Base; }

static int texts_equal(void *a, void *b) {
  intptr_t al, bl;
  if (GET_TAG(a) == T_FIXTEXT || GET_TAG(b) == T_FIXTEXT) return a == b;
  al = TEXT_SIZE(a);
  bl = TEXT_SIZE(b);
  return al == bl && !memcmp(TEXT_DATA(a), TEXT_DATA(b), UNFIXNUM(al));
}

BUILTIN2("void is",void_is,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a == b))
BUILTIN2("void isnt",void_isnt,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a != b))
BUILTIN1("void end",void_end,C_ANY,o)
RETURNS(FIXNUM(1))
BUILTIN2("void get",void_get,C_ANY,o,C_ANY,key)
RETURNS(Void)

BUILTIN2("text is",text_is,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(IS_TEXT(b) ? texts_equal(a,b) : 0))
BUILTIN2("text isnt",text_isnt,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(IS_TEXT(b) ? !texts_equal(a,b) : 1))
BUILTIN1("text size",text_size,C_ANY,o)
RETURNS(TEXT_SIZE(o))
BUILTIN2("text {}",text_get,C_ANY,o,C_FIXNUM,index)
  char t[2];
  if ((uintptr_t)CLOSURE_REF4(o,0) <= (uintptr_t)index) {
    printf("index out of bounds\n");
    TEXT(P, "{}");
    bad_call(REGS_ARGS(P),P);
  }
  t[0] = DATA_REF1(o,4+UNFIXNUM(index));
  t[1] = 0;
  TEXT(R,t);
RETURNS(R)
BUILTIN1("text end",text_end,C_ANY,o)
RETURNS(FIXNUM(1))
BUILTIN1("text hash",text_hash,C_ANY,o)
RETURNS(FIXNUM(hash(&DATA_REF1(o,4), UNFIXNUM(DATA_REF4(o,0)))))

BUILTIN2("text is",fixtext_is,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(GET_TAG(b) == T_FIXTEXT ? texts_equal(a,b) : 0))
BUILTIN2("text isnt",fixtext_isnt,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(GET_TAG(b) == T_FIXTEXT ? !texts_equal(a,b) : 1))
BUILTIN1("text size",fixtext_size,C_ANY,o)
  uint64_t x = (uint64_t)o;
  int i = 3;
  int l = 0;
  while (0x7F<<i) {
    i += 7;
    l++;
  }
RETURNS(FIXNUM(l))
BUILTIN2("text {}",fixtext_get,C_ANY,o,C_FIXNUM,index)
  char t[20];
  uint64_t c;
  int i = UNFIXNUM(index);
  if (i >= 8) {
bounds_error:
    printf("index out of bounds\n");
    TEXT(P, "{}");
    bad_call(REGS_ARGS(P),P);
  }
  c = ((uint64_t)o>>(i*7))&(0x7F<<TAG_BITS);
  if (!c) goto bounds_error;
RETURNS(ADD_TAG(c,T_FIXTEXT))
BUILTIN1("text end",fixtext_end,C_ANY,o)
RETURNS(FIXNUM(1))
BUILTIN1("text hash",fixtext_hash,C_ANY,o)
RETURNS(FIXNUM(((uint64_t)o&(((uint64_t)1<<32)-1))^((uint64_t)o>>32)))
BUILTIN1("text code",fixtext_code,C_ANY,o)
RETURNS(FIXNUM((uint64_t)o>>TAG_BITS))
BUILTIN_VARARGS("text _",fixtext)
  fprintf(stderr, "FIXME: fixtext _\n");
  abort();
RETURNS_VOID

BUILTIN2("view is",view_is,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a == b))
BUILTIN2("view isnt",view_isnt,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a != b))
BUILTIN1("view size",view_size,C_ANY,o)
RETURNS((uintptr_t)VIEW_SIZE(o))
BUILTIN2("view {}",view_get,C_ANY,o,C_FIXNUM,index)
  uint32_t start = VIEW_START(o);
  uint32_t size = VIEW_SIZE(o);
  if (size <= (uint32_t)(uintptr_t)index) {
    printf("index out of bounds\n");
    TEXT(R, "{}");
    bad_call(REGS_ARGS(P),R);
  }
RETURNS(VIEW_REF(o, start, UNFIXNUM(index)))
BUILTIN3("view {!}",view_set,C_ANY,o,C_FIXNUM,index,C_ANY,value)
  uint32_t start = VIEW_START(o);
  uint32_t size = VIEW_SIZE(o);
  void *p;
  if (size <= (uint32_t)(uintptr_t)index) {
    printf("view {!}: index out of bounds\n");
    TEXT(P, "{!}");
    bad_call(REGS_ARGS(P),P);
  }
  start += UNFIXNUM(index);
  p = &VIEW_REF(o, 0, 0);
  LIFT(p,start,value);
  R = 0;
RETURN(R)
RETURNS(Void)
BUILTIN1("view end",view_end,C_ANY,o)
RETURNS(FIXNUM(0))
BUILTIN1("view head",view_head,C_ANY,o)
RETURNS(VIEW_REF(o, VIEW_START(o), 0))
BUILTIN1("view tail",view_tail,C_ANY,o)
  uint32_t size = UNFIXNUM(VIEW_SIZE(o));
  if (size == 1) R = Empty;
  else {
    uint32_t start = VIEW_START(o);
    A = o;
    VIEW(R, &VIEW_REF(A,0,0), start+1, FIXNUM(size-1));
  }
RETURN(R)
RETURNS(0)
BUILTIN2("view add",view_add,C_ANY,o,C_ANY,x)
  void **p, **q;
  int size = (int)UNFIXNUM(VIEW_SIZE(o));
  LIST_ALLOC(R, size+1);
  p = &LIST_REF(R,0);
  *p++ = x;
  q = &VIEW_REF(o,VIEW_START(o),0);
  while(size-- > 0) *p++ = *q++;
RETURN(R)
RETURNS(0)

BUILTIN2("list is",list_is,C_ANY,a,C_ANY,b)
  intptr_t size = UNFIXNUM(LIST_SIZE(a));
RETURNS(FIXNUM(a == b))
BUILTIN2("list isnt",list_isnt,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a != b))
BUILTIN1("list size",list_size,C_ANY,o)
RETURNS(LIST_SIZE(o))
BUILTIN2("list {}",list_get,C_ANY,o,C_FIXNUM,index)
  if ((uintptr_t)LIST_SIZE(o) <= (uintptr_t)index) {
    printf("index out of bounds\n");
    TEXT(P, "{}");
    bad_call(REGS_ARGS(P),P);
  }
RETURNS(LIST_REF(o, UNFIXNUM(index)))
BUILTIN3("list {!}",list_set,C_ANY,o,C_FIXNUM,index,C_ANY,value)
  void **p;
  intptr_t i;
  if ((uintptr_t)LIST_SIZE(o) <= (uintptr_t)index) {
    printf("list {!}: index out of bounds\n");
    TEXT(P, "{!}");
    bad_call(REGS_ARGS(P),P);
  }
  p = (void*)((uintptr_t)o - T_LIST);
  LIFT(p,UNFIXNUM(index),value);
  R = 0;
RETURN(R)
RETURNS(Void)
BUILTIN1("list end",list_end,C_ANY,o)
RETURNS(FIXNUM(LIST_SIZE(o) == 0))
BUILTIN1("list head",list_head,C_ANY,o)
RETURNS(LIST_REF(o,0))
BUILTIN1("list tail",list_tail,C_ANY,o)
  intptr_t size = UNFIXNUM(LIST_SIZE(o));
  if (size == 1) R = Empty;
  else {
    VIEW(R, &LIST_REF(o,0), 1, FIXNUM(size-1));
  }
RETURN(R)
RETURNS(0)
BUILTIN2("list add",list_add,C_ANY,o,C_ANY,x)
  void **p, **q;
  intptr_t s = UNFIXNUM(LIST_SIZE(o));
  A = x;
  LIST_ALLOC(R, s+1);
  p = &LIST_REF(R,0);
  *p++ = A;
  q = &LIST_REF(o,0);
  while(s-- > 0) *p++ = *q++;
RETURN(R)
RETURNS(0)
BUILTIN_VARARGS("list _",list)
  fprintf(stderr, "FIXME: list _\n");
  abort();
RETURNS_VOID

BUILTIN1("integer neg",integer_neg,C_ANY,o)
RETURNS(-(intptr_t)o)
BUILTIN2("integer +",integer_add,C_ANY,a,C_FIXNUM,b)
RETURNS((intptr_t)a + (intptr_t)b)
BUILTIN2("integer -",integer_sub,C_ANY,a,C_FIXNUM,b)
RETURNS((intptr_t)a - (intptr_t)b)
BUILTIN2("integer *",integer_mul,C_ANY,a,C_FIXNUM,b)
RETURNS(UNFIXNUM(a) * (intptr_t)b)
BUILTIN2("integer /",integer_div,C_ANY,a,C_FIXNUM,b)
RETURNS(FIXNUM((intptr_t)a / (intptr_t)b))
BUILTIN2("integer %",integer_rem,C_ANY,a,C_FIXNUM,b)
RETURNS((intptr_t)a % (intptr_t)b)
BUILTIN2("integer is",integer_is,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a == b))
BUILTIN2("integer isnt",integer_isnt,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a != b))
BUILTIN2("integer <",integer_lt,C_ANY,a,C_FIXNUM,b)
RETURNS(FIXNUM((intptr_t)a < (intptr_t)b))
BUILTIN2("integer >",integer_gt,C_ANY,a,C_FIXNUM,b)
RETURNS(FIXNUM((intptr_t)a > (intptr_t)b))
BUILTIN2("integer <<",integer_lte,C_ANY,a,C_FIXNUM,b)
RETURNS(FIXNUM((intptr_t)a <= (intptr_t)b))
BUILTIN2("integer >>",integer_gte,C_ANY,a,C_FIXNUM,b)
RETURNS(FIXNUM((intptr_t)a <= (intptr_t)b))
BUILTIN2("integer mask",integer_mask,C_ANY,a,C_FIXNUM,b)
RETURNS((uintptr_t)a & (uintptr_t)b)
BUILTIN2("integer ior",integer_ior,C_ANY,a,C_FIXNUM,b)
RETURNS((uintptr_t)a | (uintptr_t)b)
BUILTIN2("integer xor",integer_xor,C_ANY,a,C_FIXNUM,b)
RETURNS((uintptr_t)a ^ (uintptr_t)b)
BUILTIN2("integer shl",integer_shl,C_ANY,a,C_FIXNUM,b)
RETURNS((intptr_t)a<<UNFIXNUM(b))
BUILTIN2("integer shr",integer_shr,C_ANY,a,C_FIXNUM,b)
RETURNS(((intptr_t)a>>UNFIXNUM(b))&~(TAG_MASK>>1))
BUILTIN2("integer x",integer_x,C_ANY,size,C_ANY,init)
  void **p;
  intptr_t s = UNFIXNUM(size);
  if (s < 0) {
    TEXT(R,"integer x");
    bad_call(REGS_ARGS(P), R);
  } else if (size == 0) {
    R = Empty;
  } else {
    // FIXME: alloc in parent environment
    LIST_ALLOC(R,s);
    p = &LIST_REF(R,0);
    while(s-- > 0) *p++ = init;
  }
RETURN(R)
RETURNS(0)
BUILTIN1("integer end",integer_end,C_ANY,o)
RETURNS(FIXNUM(1))
BUILTIN1("integer char",integer_char,C_ANY,o)
RETURNS(ADD_TAG((uint64_t)o&~TAG_MASK,T_FIXTEXT))
BUILTIN1("integer hash",integer_hash,C_ANY,o)
RETURNS(o)
BUILTIN_VARARGS("integer _",integer)
  fprintf(stderr, "FIXME: integer _\n");
  abort();
RETURNS_VOID


#define CAR(x) ((void**)getVal(x))[0]
#define CDR(x) ((void**)getVal(x))[1]
#define CONS(a, b) \
  ALLOC_DATA(R, T_CONS, 2); \
  DATA_REF(R,0) = a; \
  DATA_REF(R,1) = b;
BUILTIN1("cons head",cons_head,C_ANY,o)
RETURNS(CAR(o))
BUILTIN1("cons tail",cons_tail,C_ANY,o)
RETURNS(CDR(o))
BUILTIN2("cons is",cons_is,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a == b))
BUILTIN2("cons isnt",cons_isnt,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a != b))
BUILTIN1("cons end",cons_end,C_ANY,o)
RETURNS(FIXNUM(0))
BUILTIN2("cons add",cons_add,C_ANY,o,C_ANY,head)
  R = CONS(head, o);
RETURN(R)
RETURNS(0)

BUILTIN1("tag_of",tag_of,C_ANY,o)
  R = tag_of(o);
  RETURN(R);
RETURNS(0)

BUILTIN0("halt",halt)
  printf("halted.\n");
  exit(0);
RETURNS_VOID

BUILTIN1("log",log,C_ANY,a)
  fprintf(stderr, "log: %s\n", print_object(a));
RETURN(a)
RETURNS(a)

BUILTIN1("set_error_handler",set_error_handler,C_ANY,h)
  fatal("FIXME: implement set_error_handler\n");
RETURNS(Void)

BUILTIN1("load_file",load_file,C_ANY,path)
  fatal("FIXME: implement load_file\n");
RETURNS(Void)

BUILTIN1("utf8_to_text",utf8_to_text,C_ANY,bytes)
  fatal("FIXME: implement utf8_to_text\n");
RETURNS(Void)

static int is_unicode(char *s) {
  while (*s) {
    if (*(uint8_t*)s & 0x80) return 1;
    s++;
  }
  return 0;
}

static void *text_immediate_encoding(char *s) {
  uint64_t r = 0;
  uint64_t c;
  int i = 3;
  while (*s) {
    if (i >= 64) return 0;
    c = (uint8_t)*s++;
    if (c & 0x80) return 0;
    r |= c << i;
    i += 7;
  }
  return ADD_TAG(r,T_FIXTEXT);
}

static int text_immediate_decode(char *dst, void *r) {
  uint8_t *p = (uint8_t*)dst;
  uint64_t x = (uint64_t)r;
  uint64_t c;
  int i = 3;
  while (i < 64) {
    c = (x>>i) & 0x7f;
    i += 7;
    if (!c) break;
    *p++ = c;
  }
  *p = 0;
  return p-(uint8_t*)dst;
}

static void *alloc_text(api_t *api, char *s) {
  int l, a;
  void *r;
  char buf[1024];

  if (is_unicode(s)) fatal("FIXME: implement unicode\n");

  r = text_immediate_encoding(s);
  if (r) return r;

  l = strlen(s);
  a = (l+4+TAG_MASK)>>TAG_BITS;
  ALLOC_DATA(r, T_TEXT, a);
  CLOSURE_REF4(r,0) = (uint32_t)FIXNUM(l);
  memcpy(&DATA_REF1(r,4), s, l);
  return r;
}


static char *read_whole_file_as_string(char *input_file_name) {
  char *file_contents;
  long input_file_size;
  FILE *input_file = fopen(input_file_name, "rb");
  if (!input_file) return 0;
  fseek(input_file, 0, SEEK_END);
  input_file_size = ftell(input_file);
  rewind(input_file);
  file_contents = malloc(input_file_size + 1);
  file_contents[input_file_size] = 0;
  fread(file_contents, sizeof(char), input_file_size, input_file);
  fclose(input_file);
  return file_contents;
}

static char *text_to_cstring(void *o) {
  int i;
  int l = UNFIXNUM(*(uint32_t*)o);
  char *p = (char*)o + 4;
  char *out = print_buffer;
  for (i = 0; i < l; i++) *out++ = *p++;
  *out = 0;
  return print_buffer;
}

BUILTIN1("read_file_as_text",read_file_as_text,C_TEXT,filename_text)
  char *filename = text_to_cstring(filename_text);
  char *contents = read_whole_file_as_string(filename);
  if (contents) {
    TEXT(R, contents);
    free(contents);
  } else {
    R = Void;
  }
RETURNS(R)

BUILTIN2("_apply",_apply,C_ANY,f,C_ANY,args)
  // NOTE: no typecheck, because this function should be hidden from user
  //       intended use is the fast re-apply in handlers
  CALL_NO_POP(R,f);
RETURNS(R)

BUILTIN1("_no_method",_no_method,C_TEXT,name)
  printf("method not found: %s\n", print_object(name));
  STORE(E, 0, P);
  bad_call(REGS_ARGS(P), name);
  abort();
RETURNS(Void)

BUILTIN_VARARGS("undefined",undefined)
  fprintf(stderr, "`%s` has no method ", print_object(tag_of(getArg(0))));
  fprintf(stderr, "`%s`\n", print_object(api->method));
  bad_call(REGS_ARGS(P), api->method);
  abort();
RETURNS(0)

static struct {
  char *name;
  void *fun;
} builtins[] = {
  {"tag_of", b_tag_of},
  {"halt", b_halt},
  {"log", b_log},
  {"_apply", b__apply},
  {"_no_method", b__no_method},
  {"read_file_as_text", b_read_file_as_text},

  //{"save_string_as_file", b_save_text_as_file},
  {0, 0}
};

static char *print_object_r(api_t *api, char *out, void *o) {
  int i;
  int tag = GET_TAG(o);

  if (o == Empty) {
    out += sprintf(out, "()");
  } else if (o == Void) {
    out += sprintf(out, "Void");
  } else if (tag == T_CLOSURE) {
    //FIXME: check metainfo to see if this object has associated print routine
    pfun handler = OBJECT_CODE(o);
    out += sprintf(out, "#(closure %p %p)", handler, o);
  } else if (tag == T_FIXNUM) {
    // FIXME: this relies on the fact that shift preserves sign
    out += sprintf(out, "%ld", (intptr_t)o>>TAG_BITS);
  } else if (tag == T_LIST) {
    int size = (int)UNFIXNUM(LIST_SIZE(o));
    out += sprintf(out, "(");
    for (i = 0; i < size; i++) {
      if (i) out += sprintf(out, " ");
      out = print_object_r(api, out, LIST_REF(o,i));
    }
    out += sprintf(out, ")");
  } else if (tag == T_VIEW) {
    uint32_t start = VIEW_START(o);
    int size = (int)UNFIXNUM(VIEW_SIZE(o));
    out += sprintf(out, "(");
    for (i = 0; i < size; i++) {
      if (i) out += sprintf(out, " ");
      out = print_object_r(api, out, VIEW_REF(o,start,i));
    }
    out += sprintf(out, ")");
  } else if (tag == T_FIXTEXT) {
    out += text_immediate_decode(out, o);
  } else if (tag == T_DATA) {
    uintptr_t dtag = DATA_TAG(o);
    if (dtag == T_TEXT) {
      int size = (int)TEXT_SIZE(o);
      char *p = TEXT_DATA(o);
      while (size-- > 0) *out++ = *p++;
      *out = 0;
    } else if (dtag == T_CONS) {
      out += sprintf(out, "(");
      for (;;) {
        out = print_object_r(api, out, CAR(o));
        o = CDR(o);
        if (o == Empty) break;
        out += sprintf(out, " ");
      }
      out += sprintf(out, ")");
    } else {
      out += sprintf(out, "#(data %ld %p)", dtag, o);
    }
  } else {
    out += sprintf(out, "#(ufo %d %p)", tag, o);
  }
  return out;
}

//FIXME: if callee wouldnt have messed Top, we could have used it instead of passing E
static void *handle_args(REGS, void *E, intptr_t expected, intptr_t size, void *tag, void *meta) {
  intptr_t got = NARGS(E);

  if (got == FIXNUM(-1)) { //request for tag
    RETURN_NO_GC(tag);
  } else if (got == FIXNUM(-2)) {
    RETURN_NO_GC(size)
  } else if (got == FIXNUM(-3)) {
    RETURN_NO_GC(meta);
  }

  if (meta != Empty) {
  }
  if (UNFIXNUM(expected) < 0) {
    printf("bad number of arguments: got %ld, expected at least %ld\n",
       UNFIXNUM(got)-1, -UNFIXNUM(expected)-1);
  } else {
    printf("bad number of arguments: got %ld, expected %ld\n", UNFIXNUM(got)-1, UNFIXNUM(expected)-1);
  }
  fatal("during call to `%s`\n", print_object(tag));
}

#define NEEDS_GC(o) (gc_base <= (void*)(o) && (void*)(o) < gc_end)

static void *gc(api_t *api, void *gc_base, void *gc_end, void *o);

static void *gc_arglist(api_t *api, void *gc_base, void *gc_end, void *o) {
  void *p, *q;
  uintptr_t i;
  uintptr_t size;
  uintptr_t level;

  if (!NEEDS_GC(o)) return o;

  level = OBJECT_LEVEL(o);
  if (level > HEAP_SIZE) {
    // already moved
    return (void*)level;
  }

  size = NARGS(o);
  ARGLIST(p, size);
  ARG_STORE(o, -2, p);
  for (i = 0; i < size; i++) {
    ARG_LOAD(q,o,i);
    q = gc(api, gc_base, gc_end, q);
    ARG_STORE(p, i, q);
  }
  return p;
}

static void *gc(api_t *api, void *gc_base, void *gc_end, void *o) {
  void *p, *q, *e;
  int i, j, size, tag = GET_TAG(o);
  void *E, *P, *A, *C, *R; // dummies
  char buf[1024];
  uintptr_t level;

  //sprintf(buf, "%s", print_object(o));
  //fprintf(stderr, "%p: %s\n", o, print_object(o));

  if (IMMEDIATE(o)) {
    p = o;
    goto end;
  }

  if (!NEEDS_GC(o)) {
    // FIXME: validate this external reference (safe-check we haven't damaged anything)
    //fprintf(stderr, "external: %p\n", o);
    p = o;
    goto end;
  }
  level = OBJECT_LEVEL(o);
  if (level > HEAP_SIZE) {
    // already moved
    p = (void*)level;
    goto end;
  }

  if (GET_TAG(o) == T_CLOSURE) {
    void *fixed_size, *dummy;
    void *savedTop = Top;
    ALLOC_CLOSURE(dummy, FIXNUM(-2), 1); // signal that we want closure size
    CALL_NO_POP(fixed_size,o);
    size = UNFIXNUM(fixed_size);
    Top = savedTop;
    ALLOC_CLOSURE(p, OBJECT_CODE(o), size);
    STORE(o, -2, p);
    for (i = 0; i < size; i++) {
      STORE(p, i, gc_arglist(api, gc_base, gc_end, CLOSURE_REF(o,i)));
    }
  } else if (GET_TAG(o) == T_LIST) {
    size = (int)UNFIXNUM(LIST_SIZE(o));
    LIST_ALLOC(p, size);
    LIST_REF(o,-2) = p;
    for (i = 0; i < size; i++) {
      LIST_REF(p, i) = gc(api, gc_base, gc_end, LIST_REF(o,i));
    }
  } else if (GET_TAG(o) == T_VIEW) {
    uint32_t start = VIEW_START(o);
    uint32_t size = VIEW_SIZE(o);
    VIEW(p, 0, start, size);
    VIEW_GET(o,-2) = p;
    q = ADD_TAG(&VIEW_REF(o,0,0), T_LIST);
    q = gc(api, gc_base, gc_end, q);
    VIEW_GET(p,-1) = &LIST_REF(q, 0);
  } else if (GET_TAG(o) == T_DATA) {
    uintptr_t dtag = DATA_TAG(o);
    if (dtag > MAX_TYPES) {
      p = (void*)dtag;
    } else if (dtag == T_TEXT) {
      TEXT(p, TEXT_DATA(o));
      DATA_REF(o,-2) = p;
    } else if (dtag == T_CONS) {
      CONS(0, 0);
      p = R;
      CAR(p) = gc(api, gc_base, gc_end, CAR(o));
      CDR(p) = gc(api, gc_base, gc_end, CDR(o));
    } else {
      size = DATA_SIZE(o);
      ALLOC_DATA(p, OBJECT_CODE(o), size);
      DATA_REF(o,-2) = p;
      for (i = 0; i < size; i++) {
        DATA_REF(p,i) = gc(api, gc_base, gc_end, DATA_REF(o,i));
      }
    }
  } else {
    printf("cant gc #(ufo %d %p)\n", (int)GET_TAG(o), o);
    abort();
  }

end:
  //fprintf(stderr, "%s: %p -> %p (%ld)\n", ""/*buf*/, o, p, (intptr_t)(p-o));
  return p;
}

#define ON_CURRENT_LEVEL(x) (Top <= (void*)(x) && (void*)(x) < Base)
static void *gc_entry(api_t *api, void *gc_base, void *gc_end, void *o) {
  int i;
  api_t *other = api->other;
  void *xs = LIFTS_LIST(gc_end);
  if (xs) {
    void *ys = LIFTS_LIST(Base);
    while (xs) {
      void **x = (void**)LIFTS_HEAD(xs);
      *x = gc(api, gc_base, gc_end, *x);
      if (ON_CURRENT_LEVEL(x)) {
        // object got lifted to the level of it's holder
        //fprintf(stderr, "lifted!\n");
      } else { // needs future lifting
        LIFTS_CONS(ys, x, ys);
      }
      xs = LIFTS_TAIL(xs);
    }
    LIFTS_LIST(Base) = ys;
  }

  return gc(api, gc_base, gc_end, o);
}

static void fatal_error(api_t *api, char *msg) {
  fprintf(stderr, "%s\n", msg);
  abort();
}



static void *find_export(struct api_t *api, void *name, void *exports) {
  intptr_t i;
  int nexports = UNFIXNUM(LIST_SIZE(exports));

  for (i = 0; i < nexports; i++) {
    void *pair = LIST_REF(exports,i);
    if (GET_TAG(pair) != T_LIST || LIST_SIZE(pair) != (void*)FIXNUM(2)) {
      fatal("bad export: %s", print_object(pair));
    }
    void *export_name = LIST_REF(pair,0);
    if (GET_TAG(export_name) != T_FIXTEXT && !IS_TEXT(export_name)) {
      fatal("bad export: %s", print_object(pair));
    }
    if (texts_equal(name, export_name)) {
      return LIST_REF(pair,1);
    }
  }

  fatal("Couldn't resolve `%s`\n", print_object(name));
}

static void *exec_module(struct api_t *api, void *path) {
  void *lib;
  pfun entry, setup;
  void *R, *P=0, *E=0;

  lib = dlopen(path, RTLD_LAZY);
  if (!lib) fatal("dlopen couldnt load %s\n", path);

  entry = (pfun)dlsym(lib, "entry");
  if (!entry) fatal("dlsym couldnt find symbol `entry` in %s\n", path);

  setup = (pfun)dlsym(lib, "setup");
  if (!setup) fatal("dlsym couldnt find symbol `setup` in %s\n", path);

  //Base = Top;
  HEAP_FLIP();
  ARGLIST(E,0);
  R = setup(REGS_ARGS(P)); // init module's statics
  HEAP_FLIP();

  //Base = Top;
  HEAP_FLIP();
  ARGLIST(E,0);
  R = entry(REGS_ARGS(P)); 
  HEAP_FLIP();

  return R;
}


#define MAX_LIBS 1024

typedef struct {
  char *name;
  void *exports;
} lib_t;

static int libs_used;
static lib_t libs[MAX_LIBS];

static void *load_lib(struct api_t *api, char *name) {
  int i;
  char path[1024];

  for (i = 0; i < libs_used; i++) {
    if (strcmp(libs[i].name, name)) continue;
    return libs[i].exports;
  }

  if (libs_used == MAX_LIBS) {
    fprintf(stderr, "module table overflow\n");
    abort();
  }

  sprintf(path, "%s/%s", lib_path, name);
  libs[libs_used].name = name;
  libs[libs_used].exports = exec_module(api, path);

  return libs[libs_used++].exports;
}

static api_t *init_api(void *ptr) {
  int i;
  api_t *api = (api_t*)ptr;

  api->bad_type = bad_type;
  api->handle_args = handle_args;
  api->print_object_f = print_object_f;
  api->gc = gc_entry;
  api->alloc_text = alloc_text;
  api->fatal = fatal_error;
  api->resolve_method = resolve_method;
  api->resolve_type = resolve_type;
  api->set_type_size_and_name = set_type_size_and_name;
  api->set_method = set_method;
  api->find_export = find_export;
  api->load_lib = load_lib;

  return api;
}

#define METHOD_FN(name, m_int, m_list, m_fixtext, m_text, m_view, m_cons, m_void) \
  multi = api->resolve_method(api, name); \
  if (m_int) {BUILTIN_CLOSURE(multi[T_INTEGER], m_int);}\
  if (m_list) {BUILTIN_CLOSURE(multi[T_LIST], m_list);} \
  if (m_fixtext) {BUILTIN_CLOSURE(multi[T_FIXTEXT], m_fixtext);} \
  if (m_text) {BUILTIN_CLOSURE(multi[T_TEXT], m_text);} \
  if (m_view) {BUILTIN_CLOSURE(multi[T_VIEW], m_view);} \
  if (m_cons) {BUILTIN_CLOSURE(multi[T_CONS], m_cons);} \
  if (m_void) {BUILTIN_CLOSURE(multi[T_VOID], m_void);}

#define METHOD_VAL(name, m_int, m_list, m_fixtext, m_text, m_view, m_cons, m_void) \
  multi = api->resolve_method(api, name); \
  multi[T_INTEGER] = m_int;\
  multi[T_LIST] = m_list; \
  multi[T_FIXTEXT] = m_fixtext; \
  multi[T_TEXT] = m_text; \
  multi[T_VIEW] = m_view; \
  multi[T_CONS] = m_cons; \
  multi[T_VOID] = m_void;

int main(int argc, char **argv) {
  int i, j;
  char *module;
  void *lib;
  pfun entry, setup;
  api_t *api;
  void *R;
  void **multi;
  void *n_int, *n_list, *n_text, *n_void; // typenames
  void *core;

  void *E = 0; // current environment
  void *P = 0; // parent environment

  if (argc != 3) {
    printf("usage: %s <lib_path> <start_module>\n", argv[0]);
    abort();
  }

  lib_path = argv[1];
  module = argv[2];

  api = init_api(apis);
  api->other = init_api(apis+1);
  api->other->other = api;

  api->base = api->top = api->heap+HEAP_SIZE-BASE_HEAD_SIZE;
  api->other->base = api->other->top = api->other->heap+HEAP_SIZE-BASE_HEAD_SIZE;

  api->level = 0;
  api->other->level = 1;

  BUILTIN_CLOSURE(undefined, b_undefined);
  ALLOC_DATA(Void, T_VOID, 0);
  LIST_ALLOC(Empty, 0);

  api->other->void_ = api->void_;
  api->other->empty_ = api->empty_;

  for (i = 0; builtins[i].name; i++) {
    void *t;
    TEXT(builtins[i].name, builtins[i].name);
    BUILTIN_CLOSURE(t, builtins[i].fun);
    builtins[i].fun = t;
  }

  LIST_ALLOC(core, i);
  for (i = 0; builtins[i].name; i++) {
    void *pair;
    LIST_ALLOC(pair, 2);
    LIST_REF(pair,0) = builtins[i].name;
    LIST_REF(pair,1) = builtins[i].fun;
    LIST_REF(core,i) = pair;
  }

  libs[libs_used].name = "core";
  libs[libs_used].exports = core;
  ++libs_used;

  for (i = 0; i < MAX_METHODS; i++) {
    //FIXME: flip api->other here to balance allocation
    ALLOC_BASIC(methods[i].types, 0, MAX_TYPES);
  }

  TEXT(n_int, "int");
  TEXT(n_list, "list");
  TEXT(n_text, "text");
  TEXT(n_void, "void");

  api->resolve_type(api, "int");
  api->resolve_type(api, "closure");
  api->resolve_type(api, "list");
  api->resolve_type(api, "float");
  api->resolve_type(api, "view");
  api->resolve_type(api, "ptr");
  api->resolve_type(api, "fixtext");
  api->resolve_type(api, "_data_");
  api->resolve_type(api, "text");
  api->resolve_type(api, "cons");
  api->resolve_type(api, "void");

  METHOD_VAL("_size", 0, 0, 0, 0, 0, 0, 0);
  METHOD_VAL("_name", n_int, n_list, n_text, n_text, n_list, n_list, n_void);
  METHOD_FN("_gc", 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("_print", 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("neg", b_integer_neg, 0, 0, 0, 0, 0, 0);
  METHOD_FN("+", b_integer_add, 0, 0, 0, 0, 0, 0);
  METHOD_FN("-", b_integer_sub, 0, 0, 0, 0, 0, 0);
  METHOD_FN("*", b_integer_mul, 0, 0, 0, 0, 0, 0);
  METHOD_FN("/", b_integer_div, 0, 0, 0, 0, 0, 0);
  METHOD_FN("%", b_integer_rem, 0, 0, 0, 0, 0, 0);
  METHOD_FN("is", b_integer_is, b_list_is, b_fixtext_is, b_text_is, b_view_is, b_cons_is, b_void_is);
  METHOD_FN("isnt", b_integer_isnt, b_list_isnt, b_fixtext_isnt, b_text_isnt, b_view_isnt, b_cons_isnt, b_void_isnt);
  METHOD_FN("<", b_integer_lt, 0, 0, 0, 0, 0, 0);
  METHOD_FN(">", b_integer_gt, 0, 0, 0, 0, 0, 0);
  METHOD_FN("<<", b_integer_lte, 0, 0, 0, 0, 0, 0);
  METHOD_FN(">>", b_integer_gte, 0, 0, 0, 0, 0, 0);
  METHOD_FN("mask", b_integer_mask, 0, 0, 0, 0, 0, 0);
  METHOD_FN("ior", b_integer_ior, 0, 0, 0, 0, 0, 0);
  METHOD_FN("xor", b_integer_xor, 0, 0, 0, 0, 0, 0);
  METHOD_FN("shl", b_integer_shl, 0, 0, 0, 0, 0, 0);
  METHOD_FN("shr", b_integer_shr, 0, 0, 0, 0, 0, 0);
  METHOD_FN("x", b_integer_char, 0, 0, 0, 0, 0, 0);
  METHOD_FN("head", 0, b_list_head, 0, 0, b_view_head, b_cons_head, 0);
  METHOD_FN("tail", 0, b_list_tail, 0, 0, b_view_tail, b_cons_tail, 0);
  METHOD_FN("add", 0, b_list_add, 0, 0, b_view_add, b_cons_add, 0);
  METHOD_FN("end", 0, b_list_end, 0, 0, b_view_end, b_cons_end, b_void_end);
  METHOD_FN("size", 0, b_list_size, b_fixtext_size, b_text_size, b_view_size, 0, 0);
  METHOD_FN("{}", 0, b_list_get, b_fixtext_get, b_text_get, b_view_get, 0, b_void_get);
  METHOD_FN("{!}", 0, b_list_set, 0, 0, b_view_set, 0, 0);
  METHOD_FN("hash", b_integer_hash, 0, b_fixtext_hash, b_text_hash, 0, 0, 0);
  METHOD_FN("code", 0, 0, b_fixtext_code, 0, 0, 0, 0);
  METHOD_FN("char", b_integer_char, 0, 0, 0, 0, 0, 0);

  R = exec_module(api, module);

  printf("%s\n", print_object(R));

  return 0;
}
