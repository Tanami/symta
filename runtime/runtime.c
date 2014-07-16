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

#define DATA_SIZE(o) ((uintptr_t)methods[0][DATA_TAG(o)])

#define C_ANY(o,arg_index,meta)

#define C_FN(o,arg_index,meta) \
  if (GET_TAG(o) != T_CLOSURE) \
    api->bad_type(REGS_ARGS(P), "fn", arg_index, meta)

#define C_FIXNUM(o,arg_index,meta) \
  if (GET_TAG(o) != T_FIXNUM) \
    api->bad_type(REGS_ARGS(P), "int", arg_index, meta)

#define C_TEXT(o,arg_index,meta) \
  if (!IS_TEXT(o)) \
    api->bad_type(REGS_ARGS(P), "text", arg_index, meta)

#define BUILTIN_CLOSURE(dst,code) { ALLOC_CLOSURE(dst, code, 0); }

#define MAX_TYPES (1024)
#define MAX_METHODS (8*1024)
#define MAX_LIBS 1024

typedef struct {
  int items[MAX_TYPES];
  int used;
} typing_t;
static typing_t subtypings[MAX_TYPES];
static typing_t supertypings[MAX_TYPES];


#define MAX_SINGLE_CHARS (1<<8)
static void *single_chars[MAX_SINGLE_CHARS];

typedef struct {
  char *name;
  void *exports;
} lib_t;

static int libs_used;
static lib_t libs[MAX_LIBS];

static char *lib_path;

static api_t apis[2]; // one for each heap

#define M_SIZE 0
#define M_NAME 1
#define M_SINK 2

static int methods_used;
static void **methods[MAX_METHODS];
static int types_used;
static char *typenames[MAX_TYPES];

static void *undefined;
static void *sink;


// FIXME: use heap instead
static char print_buffer[1024*1024*2];
int print_depth = 0;
#define MAX_PRINT_DEPTH 32

static void print_stack_trace(api_t *api) {
  intptr_t s = Level-1;
  intptr_t parity = s&1;
  fprintf(stderr, "Stack Trace:\n");
  while (s-- > 1) {
    intptr_t l = s + 1;
    api_t *a = ((l&1)^parity) ? api : api->other;
    void *init = a->marks[l>>1];
    fprintf(stderr, "  %s\n", print_object(init));
  }
}

static void fatal(char *fmt, ...) {
   va_list ap;
   va_start(ap,fmt);
   vfprintf(stderr, fmt, ap);
   va_end(ap);
   abort();
}

static void **resolve_method(api_t *api, char *name) {
  int i, j;
  for (i = 0; i < methods_used; i++) {
    void **method = methods[i];
    if (!strcmp(method[T_NAME], name)) {
      return methods[i];
    }
  }
  if (methods_used == MAX_METHODS) {
    fprintf(stderr, "methods table overflow\n");
    abort();
  }
  ++methods_used;

  for (j = 0; j < types_used; j++) methods[i][j] = undefined;

  methods[i][T_NAME] = strdup(name);
  TEXT(methods[i][T_NAME_TEXT] ,name);
  return methods[i];
}

static void add_subtype(api_t *api, int type, int subtype);

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

  for (j = 0; j < methods_used; j++) methods[j][i] = undefined;
  methods[M_SINK][i] = sink;

  add_subtype(api, T_OBJECT, i);

  return i;
}

static void set_method_r(api_t *api, void *method, void *type, void *handler, int depth) {
  int i;
  uintptr_t id = (uintptr_t)(type);
  void *m = *((void**)method+id);
  int inherited = 0;

  if (depth) {
    if (m == undefined) {
      inherited = 1;
    } else {
      typing_t *psup = supertypings+id;
      for (i = 0; i < psup->used; i++) {
        uintptr_t sup_id = (uintptr_t)psup->items[i];
        void *sup_m = *((void**)method+sup_id);
        if (sup_m == m) {
          inherited = 1;
          break;
        }
      }
    }
  }

  if (!depth || inherited) {
    typing_t *psub = subtypings+id;
    for (i = 0; i < psub->used; i++) {
      void *subtype = (void*)(uintptr_t)psub->items[i];
      set_method_r(api, method, subtype, handler, depth+1);
    }
    LIFT(method,(uintptr_t)(type),handler);
  }
}

static void add_subtype(api_t *api, int type, int subtype) {
  int j;
  if (type == subtype) return;
  subtypings[type].items[subtypings[type].used++] = subtype;
  supertypings[subtype].items[supertypings[subtype].used++] = type;
  for (j = 0; j < methods_used; j++) {
    void **method = methods[j];
    void *handler = method[type];
    if (handler == undefined) continue;
    set_method_r(api, method, (void*)(uintptr_t)subtype, handler, 1);
  }
}

static void set_method(api_t *api, void *method, void *type, void *handler) {
  set_method_r(api, method, type, handler, 0);
}

static void set_type_size_and_name(struct api_t *api, intptr_t tag, intptr_t size, void *name) {
  methods[M_SIZE][tag] = (void*)size;
  methods[M_NAME][tag] = name;
}

static void *tag_of(void *o) {
  uintptr_t tag = GET_TAG(o);
  if (tag == T_DATA) {
    tag = DATA_TAG(o);
  }
  return methods[M_NAME][tag];
}

static void *fixtext_encode(char *p) {
  uint64_t r = 0;
  uint64_t c;
  int i = 3;
  char *s = p;
  while (*s) {
    if (i+7 >= 64) return 0;
    c = (uint8_t)*s++;
    if (c & 0x80) return 0;
    r |= c << i;
    i += 7;
  }
  return ADD_TAG(r,T_FIXTEXT);
}

static int fixtext_decode(char *dst, void *r) {
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

static int is_unicode(char *s) {
  while (*s) {
    if (*(uint8_t*)s & 0x80) return 1;
    s++;
  }
  return 0;
}

static void *alloc_bigtext(api_t *api, char *s, int l) {
  int a;
  void *r;
  a = (l+4+TAG_MASK)>>TAG_BITS;
  ALLOC_DATA(r, T_TEXT, a);
  DATA_REF4(r,0) = (uint32_t)l;
  memcpy(&DATA_REF1(r,4), s, l);
  return r;
}

static void *alloc_text(api_t *api, char *s) {
  int l, a;
  void *r;

  if (is_unicode(s)) fatal("FIXME: implement unicode\n");

  r = fixtext_encode(s);
  if (!r) r = alloc_bigtext(api, s, strlen(s));

  return r;
}

static char *decode_text(char *out, void *o) {
  int tag = GET_TAG(o);
  if (tag == T_FIXTEXT) {
    out += fixtext_decode(out, o);
    *out = 0;
  } else if (tag == T_DATA) {
    uintptr_t dtag = DATA_TAG(o);
    if (dtag == T_TEXT) {
      int size = (int)BIGTEXT_SIZE(o);
      char *p = BIGTEXT_DATA(o);
      while (size-- > 0) *out++ = *p++;
      *out = 0;
    } else {
      fprintf(stderr, "decode_text: invalid tag (%ld)\n", dtag);
      abort();
    }
  } else {
    fprintf(stderr, "decode_text: invalid tag (%d)\n", tag);
    abort();
  }
  return out;
}

static int texts_equal(void *a, void *b) {
  intptr_t al, bl;
  if (GET_TAG(a) == T_FIXTEXT || GET_TAG(b) == T_FIXTEXT) return a == b;
  al = BIGTEXT_SIZE(a);
  bl = BIGTEXT_SIZE(b);
  return al == bl && !memcmp(BIGTEXT_DATA(a), BIGTEXT_DATA(b), UNFIXNUM(al));
}

static int fixtext_size(void *o) {
  uint64_t x = (uint64_t)o;
  uint64_t m = 0x7F << 3;
  int l = 0;
  while (x & m) {
    m <<= 7;
    l++;
  }
  return l;
}

static int text_size(void *o) {
  int tag = GET_TAG(o);
  if (tag == T_FIXTEXT) {
    return fixtext_size(o);
  } else if (tag == T_DATA) {
    uintptr_t dtag = DATA_TAG(o);
    if (dtag == T_TEXT) {
      return BIGTEXT_SIZE(o);
    } else {
      fprintf(stderr, "decode_text: invalid tag (%ld)\n", dtag);
      abort();
    }
  }
  fprintf(stderr, "decode_text: invalid tag (%d)\n", tag);
  abort();
  return 0;
}

static char *text_to_cstring(void *o) {
  decode_text(print_buffer, o);
  return print_buffer;
}

static uintptr_t runtime_reserved0;
static uintptr_t runtime_reserved1;
static uintptr_t get_heap_used(int i) {
  return (void*)(apis[i].heap+HEAP_SIZE) - apis[i].top;
}

static uintptr_t show_runtime_info(api_t *api) {
  uintptr_t heap0_used = get_heap_used(0);
  uintptr_t heap1_used = get_heap_used(1);
  uintptr_t total_reserved = runtime_reserved0+runtime_reserved1;
  fprintf(stderr, "level: %ld\n", api->level);
  fprintf(stderr, "usage: %ld = %ld+%ld\n"
         , heap0_used+heap1_used-total_reserved
         , heap0_used-runtime_reserved0
         , heap1_used-runtime_reserved1);
  fprintf(stderr, "total: %ld\n", (uintptr_t)(HEAP_SIZE)*2*8-total_reserved);
  fprintf(stderr, "runtime: %ld\n", total_reserved);
  fprintf(stderr, "types used: %d/%d\n", types_used, MAX_TYPES);
  fprintf(stderr, "methods used: %d/%d\n", methods_used, MAX_METHODS);
}

static void *exec_module(struct api_t *api, char *path) {
  void *lib;
  pfun entry, setup;
  void *R, *P=0, *E=0;

  lib = dlopen(path, RTLD_LAZY);
  if (!lib) fatal("dlopen couldnt load %s\n", path);

  entry = (pfun)dlsym(lib, "entry");
  if (!entry) fatal("dlsym couldnt find symbol `entry` in %s\n", path);

  setup = (pfun)dlsym(lib, "setup");
  if (!setup) fatal("dlsym couldnt find symbol `setup` in %s\n", path);

  ARGLIST(E,0);
  setup(REGS_ARGS(P)); // init module's statics

  PUSH_BASE();
  ARGLIST(E,0);
  R = entry(REGS_ARGS(P)); 
  POP_BASE();

  return R;
}

//FIXME: resolve circular dependencies
//       instead of exports list we may return lazy list
static void *load_lib(struct api_t *api, char *name) {
  int i;
  char path[1024];
  void *exports;

  for (i = 0; i < libs_used; i++) {
    if (strcmp(libs[i].name, name)) continue;
    return libs[i].exports;
  }

  if (libs_used == MAX_LIBS) {
    fprintf(stderr, "module table overflow\n");
    abort();
  }

  name = strdup(name);
  sprintf(path, "%s/%s", lib_path, name);
  exports = exec_module(api, path);

  libs[libs_used].name = name;
  libs[libs_used].exports = exports;
  ++libs_used;
  
  //fprintf(stderr, "%s = %p\n", name, exports);

  return exports;
}

static void *find_export(struct api_t *api, void *name, void *exports) {
  intptr_t i;
  int nexports;

  nexports = UNFIXNUM(LIST_SIZE(exports));

  for (i = 0; i < nexports; i++) {
    void *pair = LIST_REF(exports,i);
    if (GET_TAG(pair) != T_LIST || LIST_SIZE(pair) != (void*)FIXNUM(2)) {
      fatal("bad export: %s", print_object(pair));
    }
    void *export_name = LIST_REF(pair,0);
    if (!IS_TEXT(export_name)) {
      fatal("bad export: %s", print_object(pair));
    }
    if (texts_equal(name, export_name)) {
      return LIST_REF(pair,1);
    }
  }

  fatal("Couldn't resolve `%s`\n", print_object(name));
}

static void bad_type(REGS, char *expected, int arg_index, char *name) {
  PROLOGUE;
  int i, nargs = (int)UNFIXNUM(NARGS(E));
  fprintf(stderr, "arg %d isnt %s, in: %s", arg_index, expected, name);
  for (i = 0; i < nargs; i++) fprintf(stderr, " %s", print_object(getArg(i)));
  fprintf(stderr, "\n");
  print_stack_trace(api);
  abort();
}

static void bad_call(REGS, void *method) {
  PROLOGUE;
  int i, nargs = (int)UNFIXNUM(NARGS(E));
  fprintf(stderr, "bad call: %s", print_object(getArg(0)));
  fprintf(stderr, " %s", print_object(method));
  for (i = 1; i < nargs; i++) fprintf(stderr, " %s", print_object(getArg(i)));
  fprintf(stderr, "\n");
  print_stack_trace(api);
  abort();
}

static char *print_object_r(api_t *api, char *out, void *o);
char* print_object_f(api_t *api, void *object) {
  print_depth = 0;
  print_object_r(api, print_buffer, object);
  return print_buffer;
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

static int write_whole_file(char *file_name, void *content, int size) {
  FILE *file = fopen(file_name, "wb");
  if (!file) return 0;
  fwrite(content, sizeof(char), size, file);
  fclose(file);
  return 1;
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

BUILTIN2("void ><",void_eq,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a == b))
BUILTIN2("void <>",void_ne,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a != b))
BUILTIN1("void hash",void_hash,C_ANY,a)
RETURNS(FIXNUM(0x12345678))


BUILTIN2("fn ><",fn_eq,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a == b))
BUILTIN2("fn <>",fn_ne,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a != b))


BUILTIN2("text ><",text_eq,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(IS_BIGTEXT(b) ? texts_equal(a,b) : 0))
BUILTIN2("text <>",text_ne,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(IS_BIGTEXT(b) ? !texts_equal(a,b) : 1))
BUILTIN1("text size",text_size,C_ANY,o)
RETURNS(FIXNUM(BIGTEXT_SIZE(o)))
BUILTIN2("text .",text_get,C_ANY,o,C_FIXNUM,index)
  char t[2];
  if ((uintptr_t)CLOSURE_REF4(o,0) <= (uintptr_t)index) {
    fprintf(stderr, "index out of bounds\n");
    TEXT(P, ".");
    bad_call(REGS_ARGS(P),P);
  }
RETURNS(single_chars[DATA_REF1(o,4+UNFIXNUM(index))])
BUILTIN1("text hash",text_hash,C_ANY,o)
RETURNS(FIXNUM(hash(BIGTEXT_DATA(o), BIGTEXT_SIZE(o))))
BUILTIN2("text eq",fixtext_eq,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(GET_TAG(b) == T_FIXTEXT ? texts_equal(a,b) : 0))
BUILTIN2("text ne",fixtext_ne,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(GET_TAG(b) == T_FIXTEXT ? !texts_equal(a,b) : 1))
BUILTIN1("text size",fixtext_size,C_ANY,o)
RETURNS(FIXNUM(fixtext_size(o)))
BUILTIN2("text .",fixtext_get,C_ANY,o,C_FIXNUM,index)
  char t[20];
  uint64_t c;
  int i = UNFIXNUM(index);
  if (i >= 8) {
bounds_error:
    fprintf(stderr, "index out of bounds\n");
    TEXT(P, ".");
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

#define CONS(dst, a, b) \
  ALLOC_BASIC(dst, a, 1); \
  dst = ADD_TAG(dst, T_CONS); \
  CONS_REF(dst,0) = b;
#define CAR(x) CONS_REF(x,-1)
#define CDR(x) CONS_REF(x,0)
BUILTIN1("cons head",cons_head,C_ANY,o)
RETURNS(CAR(o))
BUILTIN1("cons tail",cons_tail,C_ANY,o)
RETURNS(CDR(o))
BUILTIN1("cons end",cons_end,C_ANY,o)
RETURNS(FIXNUM(0))
BUILTIN2("cons pre",cons_pre,C_ANY,o,C_ANY,head)
  CONS(R, head, o);
RETURN(R)
RETURNS(0)

BUILTIN1("view size",view_size,C_ANY,o)
RETURNS((uintptr_t)VIEW_SIZE(o))
BUILTIN2("view .",view_get,C_ANY,o,C_FIXNUM,index)
  uint32_t start = VIEW_START(o);
  uint32_t size = VIEW_SIZE(o);
  if (size <= (uint32_t)(uintptr_t)index) {
    fprintf(stderr, "index out of bounds\n");
    TEXT(R, ".");
    bad_call(REGS_ARGS(P),R);
  }
RETURNS(VIEW_REF(o, start, UNFIXNUM(index)))
BUILTIN3("view !",view_set,C_ANY,o,C_FIXNUM,index,C_ANY,value)
  uint32_t start = VIEW_START(o);
  uint32_t size = VIEW_SIZE(o);
  void *p;
  if (size <= (uint32_t)(uintptr_t)index) {
    fprintf(stderr, "view !: index out of bounds\n");
    TEXT(P, "!");
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
BUILTIN2("view pre",view_pre,C_ANY,o,C_ANY,head)
  CONS(R, head, o);
RETURN(R)
RETURNS(0)

BUILTIN1("list size",list_size,C_ANY,o)
RETURNS(LIST_SIZE(o))
BUILTIN2("list .",list_get,C_ANY,o,C_FIXNUM,index)
  if ((uintptr_t)LIST_SIZE(o) <= (uintptr_t)index) {
    fprintf(stderr, "index out of bounds\n");
    TEXT(P, ".");
    bad_call(REGS_ARGS(P),P);
  }
RETURNS(LIST_REF(o, UNFIXNUM(index)))
BUILTIN3("list !",list_set,C_ANY,o,C_FIXNUM,index,C_ANY,value)
  void **p;
  intptr_t i;
  if ((uintptr_t)LIST_SIZE(o) <= (uintptr_t)index) {
    fprintf(stderr, "list !: index out of bounds\n");
    TEXT(P, "!");
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
  intptr_t size = UNFIXNUM(LIST_SIZE(o));
  if (size < 1) {
    fprintf(stderr, "list head: list is empty\n");
    TEXT(P, "head");
    bad_call(REGS_ARGS(P),P);
  }
RETURNS(LIST_REF(o,0))
BUILTIN1("list tail",list_tail,C_ANY,o)
  intptr_t size = UNFIXNUM(LIST_SIZE(o));
  if (size > 1) {
    VIEW(R, &LIST_REF(o,0), 1, FIXNUM(size-1));
  } else if (size != 0) {
    R = Empty;
  } else {
    fprintf(stderr, "list tail: list is empty\n");
    TEXT(P, "tail");
    bad_call(REGS_ARGS(P),P);
  }
RETURN(R)
RETURNS(0)
BUILTIN2("list pre",list_pre,C_ANY,o,C_ANY,head)
  CONS(R, head, o);
RETURN(R)
RETURNS(0)
BUILTIN1("list unchars",list_unchars,C_ANY,o)
  int i;
  void *x, *t;
  uint8_t *p, *q;
  intptr_t s = UNFIXNUM(LIST_SIZE(o));
  int l = 1;
  for (i = 0; i < s; i++) {
    x = LIST_REF(o,i);
    if (!IS_TEXT(x)) {
      fprintf(stderr, "list unchars: not a text (%s)\n", print_object(x));
      bad_call(REGS_ARGS(P),P);
    }
    if (GET_TAG(x) == T_FIXTEXT) {
      l += fixtext_size(x);
    } else {
      l += BIGTEXT_SIZE(x);
    }
  }
  l = (l+TAG_MASK-1) & ~TAG_MASK;
  Top = (uint8_t*)Top - l;
  p = q = (uint8_t*)Top;
  for (i = 0; i < s; i++) {
    x = LIST_REF(o,i);
    if (GET_TAG(x) == T_FIXTEXT) {
      p += fixtext_decode(p,x);
    } else {
      l = BIGTEXT_SIZE(x);
      memcpy(p,BIGTEXT_DATA(x),l);
      p += l;
    }
  }
  *p = 0;
  TEXT(R,q);
RETURN(R)
RETURNS(0)
BUILTIN2("list apply",list_apply,C_ANY,as,C_FN,f)
  int i;
  intptr_t nargs = UNFIXNUM(LIST_SIZE(as));
  void *e;
  ARGLIST(e,nargs);  
  for (i = 0; i < nargs; i++) {
    ARG_STORE(e,i,LIST_REF(as,i));
  }
  CALL_TAGGED_NO_POP(R,f)
RETURNS(R)

BUILTIN1("int neg",integer_neg,C_ANY,o)
RETURNS(-(intptr_t)o)
BUILTIN2("int +",integer_add,C_ANY,a,C_FIXNUM,b)
  R = (void*)((intptr_t)a + (intptr_t)b);
RETURN(R)
RETURNS(0)
BUILTIN2("int -",integer_sub,C_ANY,a,C_FIXNUM,b)
RETURNS((intptr_t)a - (intptr_t)b)
BUILTIN2("int *",integer_mul,C_ANY,a,C_FIXNUM,b)
RETURNS(UNFIXNUM(a) * (intptr_t)b)
BUILTIN2("int /",integer_div,C_ANY,a,C_FIXNUM,b)
 if (!b) {
    fprintf(stderr, "division by zero\n");
    TEXT(R, "/");
    bad_call(REGS_ARGS(P),R);
  }
RETURNS(FIXNUM((intptr_t)a / (intptr_t)b))
BUILTIN2("int %",integer_rem,C_ANY,a,C_FIXNUM,b)
 if (!b) {
    fprintf(stderr, "division by zero\n");
    TEXT(R, "/");
    bad_call(REGS_ARGS(P),R);
  }
RETURNS((intptr_t)a % (intptr_t)b)
BUILTIN2("int ><",integer_eq,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a == b))
BUILTIN2("int <>",integer_ne,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a != b))
BUILTIN2("int <",integer_lt,C_ANY,a,C_FIXNUM,b)
RETURNS(FIXNUM((intptr_t)a < (intptr_t)b))
BUILTIN2("int >",integer_gt,C_ANY,a,C_FIXNUM,b)
RETURNS(FIXNUM((intptr_t)a > (intptr_t)b))
BUILTIN2("int <<",integer_lte,C_ANY,a,C_FIXNUM,b)
RETURNS(FIXNUM((intptr_t)a <= (intptr_t)b))
BUILTIN2("int >>",integer_gte,C_ANY,a,C_FIXNUM,b)
RETURNS(FIXNUM((intptr_t)a <= (intptr_t)b))
BUILTIN2("int mask",integer_mask,C_ANY,a,C_FIXNUM,b)
RETURNS((uintptr_t)a & (uintptr_t)b)
BUILTIN2("int ior",integer_ior,C_ANY,a,C_FIXNUM,b)
RETURNS((uintptr_t)a | (uintptr_t)b)
BUILTIN2("int xor",integer_xor,C_ANY,a,C_FIXNUM,b)
RETURNS((uintptr_t)a ^ (uintptr_t)b)
BUILTIN2("int shl",integer_shl,C_ANY,a,C_FIXNUM,b)
RETURNS((intptr_t)a<<UNFIXNUM(b))
BUILTIN2("int shr",integer_shr,C_ANY,a,C_FIXNUM,b)
RETURNS(((intptr_t)a>>UNFIXNUM(b))&~(TAG_MASK>>1))
BUILTIN2("int dup",integer_dup,C_ANY,size,C_ANY,init)
  void **p;
  intptr_t s = UNFIXNUM(size);
  if (s < 0) {
    fprintf(stderr, "cant copy nagative number of times: %ld\n", s);
    TEXT(R,"integer dup");
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

BUILTIN1("as_text",as_text,C_ANY,o)
  if (!IS_TEXT(o)) {
    TEXT(R, print_object(o));
  } else {
    R = o;
  }
RETURN(R)
RETURNS(0)

BUILTIN1("tag_of",tag_of,C_ANY,o)
  R = tag_of(o);
RETURN(R);
RETURNS(0)

BUILTIN1("address",address,C_ANY,o)
RETURNS(DEL_TAG(o))

BUILTIN0("halt",halt)
  printf("halted.\n");
  exit(0);
RETURNS_VOID

BUILTIN1("log",log,C_ANY,a)
  fprintf(stderr, "log: %s\n", print_object(a));
RETURN(a)
RETURNS(a)

BUILTIN0("rtstat",rtstat)
  show_runtime_info(api);
RETURNS(0)

BUILTIN0("stack_trace",stack_trace)
  void **p;
  intptr_t s = Level-1;
  intptr_t parity = s&1;
  if (s == 0) {
    R = Empty;
  } else {
    LIST_ALLOC(R,s-1);
    p = &LIST_REF(R,0);
    while (s-- > 1) {
      intptr_t l = s + 1;
      api_t *a = ((l&1)^parity) ? api : api->other;
      void *init = a->marks[l>>1];
      *p++ = init;
    }
  }
RETURN(R)
RETURNS(0)

BUILTIN1("load_library",load_library,C_TEXT,path_text)
  char path[1024];
  decode_text(path, path_text);
  fprintf(stderr, "%s\n", path);
  R = exec_module(api, path);
RETURN(R)
RETURNS(0)

BUILTIN1("set_error_handler",set_error_handler,C_ANY,h)
  fatal("FIXME: implement set_error_handler\n");
RETURNS(Void)

BUILTIN1("load_file",load_file,C_ANY,path)
  fatal("FIXME: implement load_file\n");
RETURNS(Void)

BUILTIN1("utf8_to_text",utf8_to_text,C_ANY,bytes)
  fatal("FIXME: implement utf8_to_text\n");
RETURNS(Void)

BUILTIN1("load_text",load_text,C_TEXT,filename_text)
  char *filename = text_to_cstring(filename_text);
  char *contents = read_whole_file_as_string(filename);
  if (contents) {
    TEXT(R, contents);
    free(contents);
  } else {
    R = Void;
  }
RETURN(R)
RETURNS(0)

BUILTIN2("save_text",save_text,C_TEXT,filename_text,C_TEXT,text)
  char buf[32];
  int size = text_size(text);
  char *filename;
  char *xs;
  if (GET_TAG(text) == T_FIXTEXT) {
    decode_text(buf, text);
    xs = buf;
  } else {
    xs = (char*)BIGTEXT_DATA(text);
  }
  filename = text_to_cstring(filename_text);
  write_whole_file(filename, xs, size);
RETURNS(0)

BUILTIN2("_",sink,C_ANY,as,C_ANY,name)
  void *o = LIST_REF(getArg(0),0);
  fprintf(stderr, "%s has no method ", print_object(tag_of(o)));
  fprintf(stderr, "%s\n", print_object(name));
  print_stack_trace(api);
  abort();
RETURNS(0)

BUILTIN_VARARGS("undefined",undefined)
  void *o = getArg(0);
  void **m = methods[M_SINK];
  uintptr_t tag = (uintptr_t)GET_TAG(o);
  void *name = ((void**)api->method)[T_NAME_TEXT];
  void *as = ADD_TAG(E, T_LIST);
  void *e;
  ARGLIST(e,2);
  ARG_STORE(e,0,as);
  ARG_STORE(e,1,name);
  CALL_METHOD_WITH_TAG(R,o,m,tag);
  return (void*)R;
RETURNS(0)

static struct {
  char *name;
  void *fun;
} builtins[] = {
  {"tag_of", b_tag_of},
  {"address", b_address},
  {"halt", b_halt},
  {"log", b_log},
  {"rtstat", b_rtstat},
  {"stack_trace", b_stack_trace},
  {"load_text", b_load_text},
  {"save_text", b_save_text},
  {"load_library", b_load_library},

  //{"save_string_as_file", b_save_text_as_file},
  {0, 0}
};


static char *print_object_r(api_t *api, char *out, void *o) {
  int i;
  int tag = GET_TAG(o);
  int open_par = 1;

  //fprintf(stderr, "%p = %d\n", o, tag);
  //if (print_depth > 4) abort();

  print_depth++;

  if (print_depth > MAX_PRINT_DEPTH) {
    fprintf(stderr, "MAX_PRINT_DEPTH reached: likely a recursive object\n");
    abort();
  }
print_tail:
  if (o == Void) {
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
    if (open_par) out += sprintf(out, "(");
    for (i = 0; i < size; i++) {
      if (i || !open_par) out += sprintf(out, " ");
      out = print_object_r(api, out, LIST_REF(o,i));
    }
    out += sprintf(out, ")");
  } else if (tag == T_VIEW) {
    uint32_t start = VIEW_START(o);
    int size = (int)UNFIXNUM(VIEW_SIZE(o));
    if (open_par) out += sprintf(out, "(");
    for (i = 0; i < size; i++) {
      if (i || !open_par) out += sprintf(out, " ");
      out = print_object_r(api, out, VIEW_REF(o,start,i));
    }
    out += sprintf(out, ")");
  } else if (tag == T_CONS) {
    open_par = 0;
    out += sprintf(out, "(");
    for (;;) {
      out = print_object_r(api, out, CAR(o));
      o = CDR(o);
      tag = GET_TAG(o);
      if (tag != T_CONS) goto print_tail;
      out += sprintf(out, " ");
    }
  } else if (tag == T_FIXTEXT) {
    *out++ = '`';
    out = decode_text(out, o);
    *out++ = '`';
  } else if (tag == T_DATA) {
    uintptr_t dtag = DATA_TAG(o);
    if (dtag == T_TEXT) {
      *out++ = '`';
      out = decode_text(out, o);
      *out++ = '`';
    } else {
      out += sprintf(out, "#(data %ld %p)", dtag, o);
    }
  } else {
    out += sprintf(out, "#(ufo %d %p)", tag, o);
  }
  *out = 0;

  print_depth--;

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
    fprintf(stderr, "bad number of arguments: got %ld, expected at least %ld\n",
       UNFIXNUM(got)-1, -UNFIXNUM(expected)-1);
  } else {
    fprintf(stderr, "bad number of arguments: got %ld, expected %ld\n", UNFIXNUM(got), UNFIXNUM(expected));
  }
  print_stack_trace(api);
  fatal("during call to `%s`\n", print_object(tag));
}

static void *gc(api_t *api, void *o);

#define GCLevel (api->level+1)

static void *gc_arglist(api_t *api, void *o) {
  void *p, *q;
  uintptr_t i;
  uintptr_t size;
  uintptr_t level;

  level = OBJECT_LEVEL(o);

  if (level != GCLevel) {
    if (level > HEAP_SIZE) {
      // already moved
      return (void*)level;
    }
    return o;
  }

  size = NARGS(o);
  ARGLIST(p, size);
  ARG_STORE(o, -2, p);
  for (i = 0; i < size; i++) {
    ARG_LOAD(q,o,i);
    q = gc(api, q);
    ARG_STORE(p, i, q);
  }
  return p;
}

static void *gc(api_t *api, void *o) {
  void *p, *q, *e;
  int i, j, size, tag;
  void *E, *P, *A, *C, *R; // dummies
  char buf[1024];
  uintptr_t level;

  //sprintf(buf, "%s", print_object(o));
  //fprintf(stderr, "%p: %s\n", o, print_object(o));

  if (IMMEDIATE(o)) {
    p = o;
    goto end;
  }

  level = OBJECT_LEVEL(o);
  
  if (level != GCLevel) {
    if (level > HEAP_SIZE) {
      // already moved
      p = (void*)level;
      goto end;
    }
    // FIXME: validate this external reference (safe-check we haven't damaged anything)
    //fprintf(stderr, "external: %p\n", o);
    p = o;
    goto end;
  }

  tag = GET_TAG(o);
  if (tag == T_CLOSURE) {
    void *fixed_size, *dummy;
    void *savedTop = Top;
    ALLOC_CLOSURE(dummy, FIXNUM(-2), 1); // signal that we want closure size
    CALL_NO_POP(fixed_size,o);
    size = UNFIXNUM(fixed_size);
    Top = savedTop;
    ALLOC_CLOSURE(p, OBJECT_CODE(o), size);
    STORE(o, -2, p);
    for (i = 0; i < size; i++) {
      STORE(p, i, gc_arglist(api, CLOSURE_REF(o,i)));
    }
  } else if (tag == T_LIST) {
    size = (int)UNFIXNUM(LIST_SIZE(o));
    LIST_ALLOC(p, size);
    LIST_REF(o,-2) = p;
    for (i = 0; i < size; i++) {
      LIST_REF(p, i) = gc(api, LIST_REF(o,i));
    }
  } else if (tag == T_VIEW) {
    uint32_t start = VIEW_START(o);
    uint32_t size = VIEW_SIZE(o);
    VIEW(p, 0, start, size);
    VIEW_GET(o,-2) = p;
    q = ADD_TAG(&VIEW_REF(o,0,0), T_LIST);
    q = gc(api, q);
    VIEW_GET(p,-1) = &LIST_REF(q, 0);
  } else if (tag == T_CONS) {
    CONS(p, 0, 0);
    CONS_REF(o,-2) = p;
    CAR(p) = gc(api, CAR(o));
    CDR(p) = gc(api, CDR(o));
  } else if (tag == T_DATA) {
    uintptr_t dtag = DATA_TAG(o);
    if (dtag > MAX_TYPES) {
      p = (void*)dtag;
    } else if (dtag == T_TEXT) {
      p = alloc_bigtext(api, BIGTEXT_DATA(o), BIGTEXT_SIZE(o));
      DATA_REF(o,-2) = p;
    } else {
      size = DATA_SIZE(o);
      ALLOC_DATA(p, OBJECT_CODE(o), size);
      DATA_REF(o,-2) = p;
      for (i = 0; i < size; i++) {
        DATA_REF(p,i) = gc(api, DATA_REF(o,i));
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

#define ON_CURRENT_LEVEL(x) (api->top <= (void*)(x) && (void*)(x) < api->base)
static void *gc_entry(api_t *api, void *o) {
  int i;
  void *xs = LIFTS_LIST(api->base);
  api = api->other;
  if (xs) {
    void *ys = LIFTS_LIST(api->base);
    while (xs) {
      void **x = (void**)LIFTS_HEAD(xs);
      *x = gc(api, *x);
      if (ON_CURRENT_LEVEL(x)) {
        // object got lifted to the level of it's holder
        //fprintf(stderr, "lifted!\n");
      } else { // needs future lifting
        LIFTS_CONS(ys, x, ys);
      }
      xs = LIFTS_TAIL(xs);
    }
    LIFTS_LIST(api->base) = ys;
  }
  return gc(api, o);
}

static void fatal_error(api_t *api, char *msg) {
  fprintf(stderr, "%s\n", msg);
  print_stack_trace(api);
  abort();
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

#define METHOD_FN(name, m_int, m_fn, m_list, m_fixtext, m_text, m_view, m_cons, m_void) \
  multi = api->resolve_method(api, name); \
  if (m_int) {BUILTIN_CLOSURE(multi[T_INTEGER], m_int);}\
  if (m_fn) {BUILTIN_CLOSURE(multi[T_CLOSURE], m_fn);}\
  if (m_list) {BUILTIN_CLOSURE(multi[T_LIST], m_list);} \
  if (m_fixtext) {BUILTIN_CLOSURE(multi[T_FIXTEXT], m_fixtext);} \
  if (m_text) {BUILTIN_CLOSURE(multi[T_TEXT], m_text);} \
  if (m_view) {BUILTIN_CLOSURE(multi[T_VIEW], m_view);} \
  if (m_cons) {BUILTIN_CLOSURE(multi[T_CONS], m_cons);} \
  if (m_void) {BUILTIN_CLOSURE(multi[T_VOID], m_void);}

#define METHOD_VAL(name, m_int, m_fn, m_list, m_fixtext, m_text, m_view, m_cons, m_void) \
  multi = api->resolve_method(api, name); \
  multi[T_INTEGER] = m_int;\
  multi[T_CLOSURE] = m_fn; \
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
  void *n_int, *n_fn, *n_list, *n_text, *n_void; // typenames
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

  api->level = 1;
  api->other->level = 0;

  BUILTIN_CLOSURE(undefined, b_undefined);
  BUILTIN_CLOSURE(sink, b_sink);

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
    ALLOC_BASIC(methods[i], 0, MAX_TYPES);
    api = api->other;
  }
  if (api->level != 1) api = api->other;

  for (i = 0; i < 128; i++) {
    char t[2];
    t[0] = (char)i;
    t[1] = 0;
    TEXT(single_chars[i], t);
  }

  for (; i < MAX_SINGLE_CHARS; i++) {
    single_chars[i] = single_chars[0];
  }

  TEXT(n_int, "int");
  TEXT(n_fn, "fn");
  TEXT(n_list, "list");
  TEXT(n_text, "text");
  TEXT(n_void, "void");

  api->resolve_type(api, "int");
  api->resolve_type(api, "_fixtext_");
  api->resolve_type(api, "float");
  api->resolve_type(api, "fn");
  api->resolve_type(api, "_list_");
  api->resolve_type(api, "_view_");
  api->resolve_type(api, "_cons_");
  api->resolve_type(api, "_data_");
  api->resolve_type(api, "_");
  api->resolve_type(api, "_text_");
  api->resolve_type(api, "void");
  api->resolve_type(api, "list");
  api->resolve_type(api, "text");
  api->resolve_type(api, "hard_list");
  api->resolve_type(api, "_name_");
  api->resolve_type(api, "_name_text_");

  METHOD_VAL("_size", 0, 0, 0, 0, 0, 0, 0, 0);
  METHOD_VAL("_name", n_int, n_fn, n_list, n_text, n_text, n_list, n_list, n_void);
  METHOD_FN("_", b_sink, b_sink, b_sink, b_sink, b_sink, b_sink, b_sink, b_sink);
  METHOD_FN("&", 0, 0, 0, 0, 0, 0, 0, 0);

  METHOD_FN("_gc", 0, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("_print", 0, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("neg", b_integer_neg, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("+", b_integer_add, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("-", b_integer_sub, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("*", b_integer_mul, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("/", b_integer_div, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("%", b_integer_rem, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("><", b_integer_eq, b_fn_eq, 0, b_fixtext_eq, b_text_eq, 0, 0, b_void_eq);
  METHOD_FN("<>", b_integer_ne, b_fn_ne, 0, b_fixtext_ne, b_text_ne, 0, 0, b_void_ne);
  METHOD_FN("<", b_integer_lt, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN(">", b_integer_gt, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("<<", b_integer_lte, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN(">>", b_integer_gte, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("mask", b_integer_mask, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("ior", b_integer_ior, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("xor", b_integer_xor, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("shl", b_integer_shl, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("shr", b_integer_shr, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("dup", b_integer_dup, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("head", 0, 0, b_list_head, 0, 0, b_view_head, b_cons_head, 0);
  METHOD_FN("tail", 0, 0, b_list_tail, 0, 0, b_view_tail, b_cons_tail, 0);
  METHOD_FN("pre", 0, 0, b_list_pre, 0, 0, b_view_pre, b_cons_pre, 0);
  METHOD_FN("end", 0, 0, b_list_end, 0, 0, b_view_end, b_cons_end, 0);
  METHOD_FN("size", 0, 0, b_list_size, b_fixtext_size, b_text_size, b_view_size, 0, 0);
  METHOD_FN(".", 0, 0, b_list_get, b_fixtext_get, b_text_get, b_view_get, 0, 0);
  METHOD_FN("!", 0, 0, b_list_set, 0, 0, b_view_set, 0, 0);
  METHOD_FN("hash", b_integer_hash, 0, 0, b_fixtext_hash, b_text_hash, 0, 0, b_void_hash);
  METHOD_FN("code", 0, 0, 0, b_fixtext_code, 0, 0, 0, 0);
  METHOD_FN("char", b_integer_char, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("unchars", 0, 0, b_list_unchars, 0, 0, 0, 0, 0);
  METHOD_FN("as_text", b_as_text, b_as_text, b_as_text, b_as_text, b_as_text, b_as_text, b_as_text, b_as_text);
  METHOD_FN("apply", 0, 0, b_list_apply, 0, 0, 0, 0, 0);

  add_subtype(api, T_GENERIC_TEXT, T_FIXTEXT);
  add_subtype(api, T_GENERIC_TEXT, T_TEXT);

  add_subtype(api, T_HARD_LIST, T_LIST);
  add_subtype(api, T_HARD_LIST, T_VIEW);

  add_subtype(api, T_GENERIC_LIST, T_HARD_LIST);

  add_subtype(api, T_GENERIC_LIST, T_CONS);

  runtime_reserved0 = get_heap_used(0);
  runtime_reserved1 = get_heap_used(1);

  api->m_ampersand = api->other->m_ampersand = resolve_method(api, "&");
  api->m_underscore = api->other->m_underscore = resolve_method(api, "_");

  R = exec_module(api, module);

  fprintf(stderr, "%s\n", print_object(R));

  return 0;
}
