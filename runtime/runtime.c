#include <dlfcn.h>
#include <stdarg.h>
#include <sys/stat.h>

#include <unistd.h>
#include <time.h>
#include <float.h>
#include <math.h>
#include <dirent.h>

#ifdef __MACH__
// OS X does not have clock_gettime, define it through clock_get_time
#include <mach/clock.h>
#include <mach/mach.h>
#define CLOCK_REALTIME 0
int clock_gettime(int clk_id, struct timespec *ts) {
  clock_serv_t cclock;
  mach_timespec_t mts;
  host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock);
  clock_get_time(cclock, &mts);
  mach_port_deallocate(mach_task_self(), cclock);
  ts->tv_sec = mts.tv_sec;
  ts->tv_nsec = mts.tv_nsec;
}
#else
#include <sys/time.h>
#endif

#include "runtime.h"

#ifdef WINDOWS
#include "w/compat.h"
#endif

#define VIEW_START(o) REF4(o,0)
#define VIEW_SIZE(o) REF4(o,1)
#define VIEW(dst,base,start,size) \
  ALLOC_BASIC(dst, base, 1); \
  dst = ADD_TAG(dst, T_VIEW); \
  VIEW_START(dst) = (uint32_t)(start); \
  VIEW_SIZE(dst) = (uint32_t)(size);
#define VIEW_REF(o,start,i) *((void**)O_CODE(o) + start + (i))

static char *main_lib;
static void *main_args;

static char *version = "0.0.3";

typedef struct {
  int items[MAX_TYPES];
  int used;
} typing_t;
static typing_t subtypings[MAX_TYPES];
static typing_t supertypings[MAX_TYPES];

#define MAX_SINGLE_CHARS (1<<8)
static void *single_chars[MAX_SINGLE_CHARS];

static int libs_used;
static char *lib_names[MAX_LIBS];
static void *lib_exports;

#define MAX_LIB_FOLDERS 256
static int lib_folders_used;

static char *lib_folders[MAX_LIB_FOLDERS];

static api_t api_g; // one for each heap


static int methods_used;
static void **methods[MAX_METHODS];
static int types_used;
static char *typenames[MAX_TYPES];
#define DATA_SIZE(o) ((uintptr_t)methods[0][O_TAGH(o)])

static void *undefined;
static void *sink;

static int max_lifted;

static char *print_object_buf_end;
static int print_object_error;
#define PRINT_OBJECT_MARGIN 2048
static int print_depth = 0;
#define MAX_PRINT_DEPTH 32
// FIXME: use heap instead
#define PRINT_BUFFER_SIZE (1024*1024*2)
static char print_buffer[PRINT_BUFFER_SIZE];

static void print_stack_trace(api_t *api) {
  intptr_t s = Level-1;
  intptr_t e = s - 50;
  fprintf(stderr, "Stack Trace:\n");
  if (e < 0) e = 0;
  while (s-- > e) {
    intptr_t l = s + 2;
    void *init = api->frame[l].mark;
    fprintf(stderr, "  %s\n", print_object(init));
  }
  if (s > 0) {
    fprintf(stderr, "  ...stack is too big...\n");
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

static void add_subtype(api_t *api, intptr_t type, intptr_t subtype);
static void *collect_data(void *o);

#define SET_COLLECTOR(type,handler) api->collectors[type] = handler;

static int initializing = 1;

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

  if (!api->collectors[i]) {
    SET_COLLECTOR(i, collect_data);
  }

  if (initializing) { //builtin types all inherit `_`
    add_subtype(api, T_OBJECT, i);
  }

  return i;
}

static void set_method_r(api_t *api, void *method, void *type, void *handler, int depth) {
  int i;
  uintptr_t id = (uintptr_t)(type);
  void *m = *((void**)method+id);
  int inherited = 0;

  //if (depth) {
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
  //}

  if (!depth || inherited) {
    typing_t *psub = subtypings+id;

    if (!depth && !inherited && m != undefined && m != sink) {
       char *tname = typenames[id];
       fprintf(stderr, "set_method: redefinition of %s.%s\n",
               tname, (char*)*((void**)method+T_NAME));
       return; // avoid redefining
    }

    for (i = 0; i < psub->used; i++) {
      void *subtype = (void*)(uintptr_t)psub->items[i];
      set_method_r(api, method, subtype, handler, depth+1);
    }
    LIFT(method,(uintptr_t)(type),handler);
  }
}

static void add_subtype(api_t *api, intptr_t type, intptr_t subtype) {
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
  return methods[M_NAME][O_TYPE(o)];
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
  return ADD_TAGL(r,T_FIXTEXT);
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
  a = (l+4+ALIGN_MASK)>>ALIGN_BITS; // strlen + size (aligned)
  ALLOC_DATA(r, T_TEXT, a);
  REF4(r,0) = (uint32_t)l;
  memcpy(&REF1(r,4), s, l);
  return r;
}

static void *bytes_to_text(api_t *api, uint8_t *bytes, int size) {
  char cs[10];
  if (size <= 8) {
    memcpy(cs, bytes, size);
    cs[size] = 0;
    return fixtext_encode(cs);
  } else {
    return alloc_bigtext(api, bytes, size);
  }
}

#define BYTES_SIZE(o) REF4(o,0)
#define BYTES_DATA(o) (&REF1(o,4))
static void *alloc_bytes(api_t *api, int l) {
  int a;
  void *r;
  a = (l+4+ALIGN_MASK)>>ALIGN_BITS;
  ALLOC_DATA(r, T_BYTES, a);
  BYTES_SIZE(r) = (uint32_t)l;
  return r;
}

static void *text_to_bytes(api_t *api, void *o) {
  char tmp[12];
  char *p;
  int size;
  void *r;
  if (IS_FIXTEXT(o)) {
    size = fixtext_decode(tmp, o);
  } else if (IS_BIGTEXT(o)) {
    size = (int)BIGTEXT_SIZE(o);
    p = BIGTEXT_DATA(o);
  }
  r = alloc_bytes(api, size);
  memcpy(BYTES_DATA(r), p, size);
  return r;
}

static void *alloc_text(char *s) {
  int l, a;
  void *r;
  api_t *api = &api_g;

  if (is_unicode(s)) fatal("FIXME: implement unicode\n");

  r = fixtext_encode(s);
  if (!r) r = alloc_bigtext(api, s, strlen(s));

  return r;
}

static char *decode_text(char *out, void *o) {
  if (IS_FIXTEXT(o)) {
    out += fixtext_decode(out, o);
    *out = 0;
  } else if (IS_BIGTEXT(o)) {
      int size = (int)BIGTEXT_SIZE(o);
      char *p = BIGTEXT_DATA(o);
      while (size-- > 0) *out++ = *p++;
      *out = 0;
  } else {
    fprintf(stderr, "decode_text: invalid type (%d)\n", (int)O_TYPE(o));
    abort();
  }
  return out;
}

static int texts_equal(void *a, void *b) {
  intptr_t al, bl;
  if (IS_FIXTEXT(a) || IS_FIXTEXT(b)) return a == b;
  al = BIGTEXT_SIZE(a);
  bl = BIGTEXT_SIZE(b);
  return al == bl && !memcmp(BIGTEXT_DATA(a), BIGTEXT_DATA(b), al);
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
  if (IS_FIXTEXT(o)) return fixtext_size(o);
  if (!IS_BIGTEXT(o)) {
    fprintf(stderr, "text_size: invalid type (%d)\n", (int)O_TYPE(o));
    abort();
  }
  return BIGTEXT_SIZE(o);
}

static char *text_chars(struct api_t *api, void *text) {
  int a;
  char buf[32];
  int size = text_size(text);
  char *s, *d;
  void *r;
  if (IS_FIXTEXT(text)) {
    decode_text(buf, text);
    s = buf;
  } else {
    s = (char*)BIGTEXT_DATA(text);
  }
  a = (size+1+ALIGN_MASK)>>ALIGN_BITS;
  ALLOC_BASIC(r, T_TEXT, a);
  d = (char*)r;
  memcpy(d, s, size);
  d[size] = 0;
  return d;
}
static char *text_to_cstring(void *o) {
  decode_text(print_buffer, o);
  return print_buffer;
}

static uintptr_t runtime_reserved0;
static uintptr_t runtime_reserved1;
static uintptr_t get_heap_used(int i) {
  return (void*)(api_g.heap[i]+HEAP_SIZE) - api_g.top[i];
}

static uintptr_t show_runtime_info(api_t *api) {
  uintptr_t heap0_used = get_heap_used(0);
  uintptr_t heap1_used = get_heap_used(1);
  uintptr_t total_reserved = runtime_reserved0+runtime_reserved1;
  fprintf(stderr, "-------------\n");
  fprintf(stderr, "level: %ld\n", api->level-1);
  fprintf(stderr, "usage: %ld = %ld+%ld\n"
         , heap0_used+heap1_used-total_reserved
         , heap0_used-runtime_reserved0
         , heap1_used-runtime_reserved1);
  fprintf(stderr, "total: %ld\n", (uintptr_t)(HEAP_SIZE)*2*8-total_reserved);
  fprintf(stderr, "runtime: %ld\n", total_reserved);
  fprintf(stderr, "types used: %d/%d\n", types_used, MAX_TYPES);
  fprintf(stderr, "methods used: %d/%d\n", methods_used, MAX_METHODS);
  fprintf(stderr, "\n");
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

  //fprintf(stderr, "running %s\n", path);

  PUSH_BASE();
  ARGLIST(E,0);
  R = entry(REGS_ARGS(P)); 

  //fprintf(stderr, "done %s\n", path);

  return R;
}


static int file_exists(char *filename) {
  return access(filename, F_OK) != -1;
}


//FIXME: resolve circular dependencies
//       instead of exports list we may return lazy list
static void *load_lib(struct api_t *api, char *name) {
  int i;
  char tmp[1024];
  void *exports;

  if (name[0] != '/' && name[1] != ':' && strcmp(name,"rt_")) {
    for (i = 0; i < lib_folders_used; i++) {
      sprintf(tmp, "%s/%s", lib_folders[i], name);
      if (file_exists(tmp)) break;
    }
    if (i == lib_folders_used) {
      fprintf(stderr, "load_lib: couldnt locate library `%s` in:\n", name);
      for (i = 0; i < lib_folders_used; i++) {
        fprintf(stderr, "  %s\n", lib_folders[i]);
      }
      abort();
    }
    name = tmp;
  }

  for (i = 0; i < libs_used; i++) {
    if (strcmp(lib_names[i], name)) continue;
    return REF(lib_exports,i);
  }

  //fprintf(stderr, "load_lib begin: %s\n", name);

  name = strdup(name);
  exports = exec_module(api, name);

  //fprintf(stderr, "load_lib end: %s\n", name);

  if (libs_used == MAX_LIBS) {
    fprintf(stderr, "module table overflow\n");
    abort();
  }

  lib_names[libs_used] = name;
  LIFT(&REF(lib_exports,0),libs_used,exports);
  ++libs_used;

  return exports;
}

static void add_lib_folder(char *folder) {
  char *d = strdup(folder);
  int l = strlen(folder)-1;
  while (l >= 0 && (d[l] == '/' || d[l] == '\\')) d[l--] = 0;
  lib_folders[lib_folders_used++] = d;
}

static void *find_export(struct api_t *api, void *name, void *exports) {
  intptr_t i;
  int nexports;

  nexports = UNFIXNUM(LIST_SIZE(exports));

  if (O_TAG(exports) != TAG(T_LIST)) {
    fatal("exports ain't a list: %s\n", print_object(exports));
  }

  for (i = 0; i < nexports; i++) {
    void *pair = REF(exports,i);
    if (O_TAG(pair) != TAG(T_LIST) || LIST_SIZE(pair) != FIXNUM(2)) {
      fatal("export contains invalid pair: %s\n", print_object(pair));
    }
    void *export_name = REF(pair,0);
    if (!IS_TEXT(export_name)) {
      fatal("export contains bad name: %s\n", print_object(pair));
    }
    if (texts_equal(name, export_name)) {
      return REF(pair,1);
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
  print_object_buf_end = print_buffer+PRINT_BUFFER_SIZE;\
  print_object_error = 0;
  print_object_r(api, print_buffer, object);
  return print_buffer;
}

static uint8_t *read_whole_file(char *input_file_name, intptr_t *file_size) {
  uint8_t *file_contents;
  FILE *input_file = fopen(input_file_name, "rb");
  if (!input_file) return 0;
  fseek(input_file, 0, SEEK_END);
  *file_size = ftell(input_file);
  rewind(input_file);
  file_contents = malloc(*file_size + 1);
  file_contents[*file_size] = 0;
  fread(file_contents, sizeof(uint8_t), *file_size, input_file);
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

BUILTIN2("void.`><`",void_eq,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a == b))
BUILTIN2("void.`<>`",void_ne,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a != b))
BUILTIN1("void.hash",void_hash,C_ANY,a)
RETURNS(FIXNUM(0x12345678))


BUILTIN2("fn.`><`",fn_eq,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a == b))
BUILTIN2("fn.`<>`",fn_ne,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a != b))
BUILTIN1("fn.nargs",fn_nargs,C_ANY,o)
  void *dummy, *nargs;
  ALLOC_CLOSURE(dummy, FN_GET_NARGS, 1);
  CALL(nargs,o);
RETURNS(nargs)

BUILTIN1("bytes.size",bytes_size,C_ANY,o)
RETURNS(FIXNUM(BYTES_SIZE(o)))
BUILTIN2("bytes.`.`",bytes_get,C_ANY,o,C_INT,index)
  uintptr_t idx = (uintptr_t)UNFIXNUM(index);
  char t[2];
  if ((uintptr_t)BYTES_SIZE(o) <= idx) {
    fprintf(stderr, "index out of bounds\n");
    TEXT(P, ".");
    bad_call(REGS_ARGS(P),P);
  }
RETURNS(FIXNUM(BYTES_DATA(o)[idx]))
BUILTIN3("bytes.`!`",bytes_set,C_ANY,o,C_INT,index,C_INT,value)
  intptr_t i = UNFIXNUM(index);
  if (BYTES_SIZE(o) <= (uintptr_t)i) {
    fprintf(stderr, "list !: index out of bounds\n");
    TEXT(P, "!");
    bad_call(REGS_ARGS(P),P);
  }
  BYTES_DATA(o)[i] = (uint8_t)UNFIXNUM(value);
  R = 0;
RETURNS(R)
BUILTIN2("bytes.clear",bytes_clear,C_ANY,o,C_INT,value)
  intptr_t i;
  intptr_t size = BYTES_SIZE(o);
  uint8_t v = (uint8_t)UNFIXNUM(value);
  uint8_t *p = BYTES_DATA(o);
  for (i = 0; i < size; i++) p[i] = v;
  R = 0;
RETURNS(R)
BUILTIN1("bytes.utf8",bytes_utf8,C_ANY,o)
RETURNS(bytes_to_text(api, BYTES_DATA(o), BYTES_SIZE(o)))


BUILTIN2("text.`><`",text_eq,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(IS_BIGTEXT(b) ? texts_equal(a,b) : 0))
BUILTIN2("text.`<>`",text_ne,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(IS_BIGTEXT(b) ? !texts_equal(a,b) : 1))
BUILTIN1("text.size",text_size,C_ANY,o)
RETURNS(FIXNUM(BIGTEXT_SIZE(o)))
BUILTIN2("text.`.`",text_get,C_ANY,o,C_INT,index)
  uintptr_t idx = (uintptr_t)UNFIXNUM(index);
  char t[2];
  if ((uintptr_t)REF4(o,0) <= idx) {
    fprintf(stderr, "index out of bounds\n");
    TEXT(P, ".");
    bad_call(REGS_ARGS(P),P);
  }
RETURNS(single_chars[REF1(o,4+idx)])
BUILTIN1("text.hash",text_hash,C_ANY,o)
RETURNS(FIXNUM(hash(BIGTEXT_DATA(o), BIGTEXT_SIZE(o))))
BUILTIN1("text.utf8",text_utf8,C_ANY,o)
RETURNS(text_to_bytes(api,o))


BUILTIN2("text.`><`",fixtext_eq,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(IS_FIXTEXT(b) ? texts_equal(a,b) : 0))
BUILTIN2("text.`<>`",fixtext_ne,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(IS_FIXTEXT(b) ? !texts_equal(a,b) : 1))
BUILTIN1("text.size",fixtext_size,C_ANY,o)
RETURNS(FIXNUM(fixtext_size(o)))
BUILTIN2("text.`.`",fixtext_get,C_ANY,o,C_INT,index)
  char t[20];
  uint64_t c;
  int i = UNFIXNUM(index);
  if (i >= 8) {
bounds_error:
    fprintf(stderr, "index out of bounds\n");
    TEXT(P, ".");
    bad_call(REGS_ARGS(P),P);
  }
  c = ((uint64_t)o>>(i*7))&(0x7F<<ALIGN_BITS);
  if (!c) goto bounds_error;
RETURNS(ADD_TAGL(c,T_FIXTEXT))
BUILTIN1("text.end",fixtext_end,C_ANY,o)
RETURNS(FIXNUM(1))
BUILTIN1("text.hash",fixtext_hash,C_ANY,o)
RETURNS(FIXNUM(((uint64_t)o&(((uint64_t)1<<32)-1))^((uint64_t)o>>32)))
BUILTIN1("text.code",fixtext_code,C_ANY,o)
RETURNS(FIXNUM((uint64_t)o>>ALIGN_BITS))
BUILTIN1("text.utf8",fixtext_utf8,C_ANY,o)
RETURNS(text_to_bytes(api,o))


#define CONS(dst, a, b) \
  ALLOC_BASIC(dst, a, 1); \
  dst = ADD_TAG(dst, T_CONS); \
  REF(dst,0) = b;
#define CAR(x) O_CODE(x)
#define CDR(x) REF(x,0)
BUILTIN1("cons.head",cons_head,C_ANY,o)
RETURNS(CAR(o))
BUILTIN1("cons.tail",cons_tail,C_ANY,o)
RETURNS(CDR(o))
BUILTIN1("cons.end",cons_end,C_ANY,o)
RETURNS(FIXNUM(0))
BUILTIN2("cons.pre",cons_pre,C_ANY,o,C_ANY,head)
  CONS(R, head, o);
RETURNS(R)

BUILTIN1("view.size",view_size,C_ANY,o)
RETURNS((uintptr_t)VIEW_SIZE(o))
BUILTIN2("view.`.`",view_get,C_ANY,o,C_INT,index)
  uint32_t start = VIEW_START(o);
  uint32_t size = VIEW_SIZE(o);
  if (size <= (uint32_t)(uintptr_t)index) {
    fprintf(stderr, "index out of bounds\n");
    TEXT(R, ".");
    bad_call(REGS_ARGS(P),R);
  }
RETURNS(VIEW_REF(o, start, UNFIXNUM(index)))
BUILTIN3("view.`!`",view_set,C_ANY,o,C_INT,index,C_ANY,value)
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
RETURNS(No)
BUILTIN1("view.end",view_end,C_ANY,o)
RETURNS(FIXNUM(0))
BUILTIN1("view.head",view_head,C_ANY,o)
RETURNS(VIEW_REF(o, VIEW_START(o), 0))
BUILTIN1("view.tail",view_tail,C_ANY,o)
  uint32_t size = UNFIXNUM(VIEW_SIZE(o));
  if (size == 1) R = Empty;
  else {
    uint32_t start = VIEW_START(o);
    A = o;
    VIEW(R, &VIEW_REF(A,0,0), start+1, FIXNUM(size-1));
  }
RETURNS(R)
BUILTIN2("view.take",view_take,C_ANY,o,C_INT,count)
  intptr_t c = UNFIXNUM(count);
  uint32_t size = UNFIXNUM(VIEW_SIZE(o));
  if (c == 0) {
    R = Empty;
  } else if ((uintptr_t)c < (uintptr_t)size) {
    uint32_t start = VIEW_START(o);
    A = o;
    VIEW(R, &VIEW_REF(A,0,0), start, FIXNUM(c));
  } else if (c == size) {
    R = o;
  } else {
    fprintf(stderr, "list.take: count %d is invalid for list size = %d\n", (int)c, (int)size);
    TEXT(P, "take");
    bad_call(REGS_ARGS(P),P);
  }
RETURNS(R)
BUILTIN2("view.drop",view_drop,C_ANY,o,C_INT,count)
  intptr_t c = UNFIXNUM(count);
  uint32_t size = UNFIXNUM(VIEW_SIZE(o));
  if (c == 0) {
    R = o;
  } else if ((uintptr_t)c < (uintptr_t)size) {
    uint32_t start = VIEW_START(o);
    A = o;
    VIEW(R, &VIEW_REF(A,0,0), start+c, FIXNUM(size-c));
  } else if (c == size) {
    R = Empty;
  } else {
    fprintf(stderr, "list.drop: count %d is invalid for list size = %d\n", (int)c, (int)size);
    TEXT(P, "take");
    bad_call(REGS_ARGS(P),P);
  }
RETURNS(R)
BUILTIN2("view.pre",view_pre,C_ANY,o,C_ANY,head)
  CONS(R, head, o);
RETURNS(R)


BUILTIN1("list.size",list_size,C_ANY,o)
RETURNS(LIST_SIZE(o))
BUILTIN2("list.`.`",list_get,C_ANY,o,C_INT,index)
  if (LIST_SIZE(o) <= (uintptr_t)index) {
    fprintf(stderr, "index out of bounds\n");
    TEXT(P, ".");
    bad_call(REGS_ARGS(P),P);
  }
RETURNS(REF(o, UNFIXNUM(index)))
BUILTIN3("list.`!`",list_set,C_ANY,o,C_INT,index,C_ANY,value)
  void **p;
  intptr_t i;
  if (LIST_SIZE(o) <= (uintptr_t)index) {
    fprintf(stderr, "list !: index out of bounds\n");
    TEXT(P, "!");
    bad_call(REGS_ARGS(P),P);
  }
  p = (void*)O_PTR(o);
  LIFT(p,UNFIXNUM(index),value);
  R = 0;
RETURNS(R)
BUILTIN2("list.clear",list_clear,C_ANY,o,C_ANY,value)
  intptr_t i;
  intptr_t size = LIST_SIZE(o);
  void **p = (void*)O_PTR(o);
  for (i = 0; i < size; i++) {
    LIFT(p,UNFIXNUM(i),value);
  }
  R = 0;
RETURNS(R)
BUILTIN1("list.end",list_end,C_ANY,o)
RETURNS(FIXNUM(LIST_SIZE(o) == 0))
BUILTIN1("list.head",list_head,C_ANY,o)
  intptr_t size = UNFIXNUM(LIST_SIZE(o));
  if (size < 1) {
    fprintf(stderr, "list head: list is empty\n");
    TEXT(P, "head");
    bad_call(REGS_ARGS(P),P);
  }
RETURNS(REF(o,0))
BUILTIN1("list.tail",list_tail,C_ANY,o)
  intptr_t size = UNFIXNUM(LIST_SIZE(o));
  if (size > 1) {
    VIEW(R, &REF(o,0), 1, FIXNUM(size-1));
  } else if (size != 0) {
    R = Empty;
  } else {
    fprintf(stderr, "list tail: list is empty\n");
    TEXT(P, "tail");
    bad_call(REGS_ARGS(P),P);
  }
RETURNS(R)
BUILTIN2("list.take",list_take,C_ANY,o,C_INT,count)
  intptr_t c = UNFIXNUM(count);
  intptr_t size = UNFIXNUM(LIST_SIZE(o));
  if (c == 0) {
    R = Empty;
  } else if ((uintptr_t)c < (uintptr_t)size) {
    VIEW(R, &REF(o,0), 0, FIXNUM(c));
  } else if (c == size) {
    R = o;
  } else {
    fprintf(stderr, "list.take: count %d is invalid for list size = %d\n", (int)c, (int)size);
    TEXT(P, "take");
    bad_call(REGS_ARGS(P),P);
  }
RETURNS(R)
BUILTIN2("list.drop",list_drop,C_ANY,o,C_INT,count)
  intptr_t c = UNFIXNUM(count);
  intptr_t size = UNFIXNUM(LIST_SIZE(o));
  if (c == 0) {
    R = o;
  } else if ((uintptr_t)c < (uintptr_t)size) {
    VIEW(R, &REF(o,0), c, FIXNUM(size-c));
  } else if (c == size) {
    R = Empty;
  } else {
    fprintf(stderr, "list.drop: count %d is invalid for list size = %d\n", (int)c, (int)size);
    TEXT(P, "drop");
    bad_call(REGS_ARGS(P),P);
  }
RETURNS(R)
BUILTIN2("list.pre",list_pre,C_ANY,o,C_ANY,head)
  CONS(R, head, o);
RETURNS(R)

BUILTIN_VARARGS("list.text",list_text)
  int i;
  void *x, *t;
  uint8_t *p, *q;
  void *words = getArg(0);
  intptr_t words_size;
  int l; // length of result text
  void *sep; // separator
  int sep_size;

  if (NARGS(E) == FIXNUM(1)) {
    sep = 0;
    sep_size = 0;
  } else if (NARGS(E) == FIXNUM(2)) {
    sep = getArg(1);
    if (!IS_TEXT(sep)) {
      fprintf(stderr, "list.text: separator is not text (%s)\n", print_object(sep));
    }
    sep_size = text_size(sep);
  } else {
    fprintf(stderr, "list.text: bad number of args\n");
  }

  words_size = UNFIXNUM(LIST_SIZE(words));

  l = 1;
  if (sep && words_size) l += sep_size*(words_size-1);
  for (i = 0; i < words_size; i++) {
    x = REF(words,i);
    if (!IS_TEXT(x)) {
      fprintf(stderr, "list.text: not a text (%s)\n", print_object(x));
      bad_call(REGS_ARGS(P),P);
    }
    l += text_size(x);
  }
  l = (l+ALIGN_MASK) & ~(uintptr_t)ALIGN_MASK;
  Top = (uint8_t*)Top - l;
  p = q = (uint8_t*)Top;
  for (i = 0; i < words_size; ) {
    x = REF(words,i);
    p = decode_text(p, x);
    ++i;
    if (sep && i < words_size) {
      p = decode_text(p, sep);
    }
  }
  *p = 0;
  TEXT(R,q);
RETURNS(R)
BUILTIN2("list.apply",list_apply,C_ANY,as,C_FN,f)
  int i;
  intptr_t nargs = UNFIXNUM(LIST_SIZE(as));
  void *e;
  ARGLIST(e,nargs);  
  for (i = 0; i < nargs; i++) {
    ARG_STORE(e,i,REF(as,i));
  }
  CALL_TAGGED(R,f)
RETURNS_NO_GC(R)
BUILTIN2("list.apply_method",list_apply_method,C_ANY,as,C_ANY,m)
  int i;
  intptr_t nargs = UNFIXNUM(LIST_SIZE(as));
  void *o;
  void *e;
  uintptr_t tag;
  m = (void*)O_PTR(m);
  if (!nargs) {
    fprintf(stderr, "apply_method: empty list\n");
    bad_call(REGS_ARGS(P),P);
  }
  o = REF(as,i);
  ARGLIST(e,nargs);
  for (i = 0; i < nargs; i++) {
    ARG_STORE(e,i,REF(as,i));
  }
  CALL_METHOD(R,o,m);
RETURNS_NO_GC(R)


BUILTIN1("float.neg",float_neg,C_ANY,a)
  double fa;
  UNFLOAT(fa,a);
  LOAD_FLOAT(R,-fa);
RETURNS(R)
BUILTIN2("float.`+`",float_add,C_ANY,a,C_FLOAT,b)
  double fa, fb;
  UNFLOAT(fa,a);
  UNFLOAT(fb,b);
  LOAD_FLOAT(R,fa+fb);
RETURNS(R)
BUILTIN2("float.`-`",float_sub,C_ANY,a,C_FLOAT,b)
  double fa, fb;
  UNFLOAT(fa,a);
  UNFLOAT(fb,b);
  LOAD_FLOAT(R,fa-fb);
RETURNS(R)
BUILTIN2("float.`*`",float_mul,C_ANY,a,C_FLOAT,b)
  double fa, fb;
  UNFLOAT(fa,a);
  UNFLOAT(fb,b);
  LOAD_FLOAT(R,fa*fb);
RETURNS(R)
BUILTIN2("float.`/`",float_div,C_ANY,a,C_FLOAT,b)
  double fa, fb;
  UNFLOAT(fa,a);
  UNFLOAT(fb,b);
  if (fb == 0.0) fb = FLT_MIN;
  LOAD_FLOAT(R,fa/fb);
RETURNS(R)
BUILTIN2("float.`**`",float_pow,C_ANY,a,C_FLOAT,b)
  double fa, fb;
  UNFLOAT(fa,a);
  UNFLOAT(fb,b);
  LOAD_FLOAT(R,pow(fa,fb));
RETURNS(R)
BUILTIN2("float.`><`",float_eq,C_ANY,a,C_ANY,b)
  double fa, fb;
  UNFLOAT(fa,a);
  UNFLOAT(fb,b);
RETURNS(FIXNUM(fa == fb))
BUILTIN2("float.`<>`",float_ne,C_ANY,a,C_ANY,b)
  double fa, fb;
  UNFLOAT(fa,a);
  UNFLOAT(fb,b);
RETURNS(FIXNUM(fa != fb))
BUILTIN2("float.`<`",float_lt,C_ANY,a,C_FLOAT,b)
  double fa, fb;
  UNFLOAT(fa,a);
  UNFLOAT(fb,b);
RETURNS(FIXNUM(fa < fb))
BUILTIN2("float.`>`",float_gt,C_ANY,a,C_FLOAT,b)
  double fa, fb;
  UNFLOAT(fa,a);
  UNFLOAT(fb,b);
RETURNS(FIXNUM(fa > fb))
BUILTIN2("float.`<<`",float_lte,C_ANY,a,C_FLOAT,b)
  double fa, fb;
  UNFLOAT(fa,a);
  UNFLOAT(fb,b);
RETURNS(FIXNUM(fa <= fb))
BUILTIN2("float.`>>`",float_gte,C_ANY,a,C_FLOAT,b)
  double fa, fb;
  UNFLOAT(fa,a);
  UNFLOAT(fb,b);
RETURNS(FIXNUM(fa >= fb))
BUILTIN1("float.as_text",float_as_text,C_ANY,a)
  TEXT(R, print_object(a));
RETURNS(R)
BUILTIN1("float.float",float_float,C_ANY,o)
RETURNS(o)
BUILTIN1("float.int",float_int,C_ANY,o)
  double fo;
  UNFLOAT(fo,o);
RETURNS(FIXNUM((intptr_t)fo))
BUILTIN1("float.sqrt",float_sqrt,C_ANY,o)
  double fo;
  UNFLOAT(fo,o);
  LOAD_FLOAT(R, sqrt(fo));
RETURNS(R)
BUILTIN1("float.log",float_log,C_ANY,o)
  double fo;
  UNFLOAT(fo,o);
  LOAD_FLOAT(R, log(fo));
RETURNS(R)
BUILTIN1("float.sin",float_sin,C_ANY,o)
  double fo;
  UNFLOAT(fo,o);
  LOAD_FLOAT(R, sin(fo));
RETURNS(R)
BUILTIN1("float.asin",float_asin,C_ANY,o)
  double fo;
  UNFLOAT(fo,o);
  LOAD_FLOAT(R, asin(fo));
RETURNS(R)
BUILTIN1("float.cos",float_cos,C_ANY,o)
  double fo;
  UNFLOAT(fo,o);
  LOAD_FLOAT(R, cos(fo));
RETURNS(R)
BUILTIN1("float.acos",float_acos,C_ANY,o)
  double fo;
  UNFLOAT(fo,o);
  LOAD_FLOAT(R, acos(fo));
RETURNS(R)
BUILTIN1("float.tan",float_tan,C_ANY,o)
  double fo;
  UNFLOAT(fo,o);
  LOAD_FLOAT(R, tan(fo));
RETURNS(R)
BUILTIN1("float.atan",float_atan,C_ANY,o)
  double fo;
  UNFLOAT(fo,o);
  LOAD_FLOAT(R, atan(fo));
RETURNS(R)
BUILTIN1("float.floor",float_floor,C_ANY,o)
  double fo;
  UNFLOAT(fo,o);
  LOAD_FLOAT(R, floor(fo));
RETURNS(R)
BUILTIN1("float.ceil",float_ceil,C_ANY,o)
  double fo;
  UNFLOAT(fo,o);
  LOAD_FLOAT(R, ceil(fo));
RETURNS(R)
BUILTIN1("float.round",float_round,C_ANY,o)
  double fo;
  UNFLOAT(fo,o);
  LOAD_FLOAT(R, round(fo));
RETURNS(R)


BUILTIN1("int.neg",int_neg,C_ANY,o)
RETURNS(-(intptr_t)o)
BUILTIN2("int.`+`",int_add,C_ANY,a,C_INT,b)
RETURNS((intptr_t)a + (intptr_t)b)
BUILTIN2("int.`-`",int_sub,C_ANY,a,C_INT,b)
RETURNS((intptr_t)a - (intptr_t)b)
BUILTIN2("int.`*`",int_mul,C_ANY,a,C_INT,b)
RETURNS(UNFIXNUM(a) * (intptr_t)b)
BUILTIN2("int.`/`",int_div,C_ANY,a,C_INT,b)
 if (!b) {
    fprintf(stderr, "division by zero\n");
    TEXT(R, "/");
    bad_call(REGS_ARGS(P),R);
  }
RETURNS(FIXNUM((intptr_t)a / (intptr_t)b))
BUILTIN2("int.`%`",int_rem,C_ANY,a,C_INT,b)
 if (!b) {
    fprintf(stderr, "division by zero\n");
    TEXT(R, "/");
    bad_call(REGS_ARGS(P),R);
  }
RETURNS((intptr_t)a % (intptr_t)b)
BUILTIN2("int.`**`",int_pow,C_ANY,a,C_INT,b)
RETURNS(FIXNUM((intptr_t)pow((double)UNFIXNUM(a), (double)UNFIXNUM(b))))
BUILTIN2("int.`><`",int_eq,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a == b))
BUILTIN2("int.`<>`",int_ne,C_ANY,a,C_ANY,b)
RETURNS(FIXNUM(a != b))
BUILTIN2("int.`<`",int_lt,C_ANY,a,C_INT,b)
RETURNS(FIXNUM((intptr_t)a < (intptr_t)b))
BUILTIN2("int.`>`",int_gt,C_ANY,a,C_INT,b)
RETURNS(FIXNUM((intptr_t)a > (intptr_t)b))
BUILTIN2("int.`<<`",int_lte,C_ANY,a,C_INT,b)
RETURNS(FIXNUM((intptr_t)a <= (intptr_t)b))
BUILTIN2("int.`>>`",int_gte,C_ANY,a,C_INT,b)
RETURNS(FIXNUM((intptr_t)a >= (intptr_t)b))
BUILTIN2("int.`^^`",int_mask,C_ANY,a,C_INT,b)
RETURNS((uintptr_t)a & (uintptr_t)b)
BUILTIN2("int.`++`",int_ior,C_ANY,a,C_INT,b)
RETURNS((uintptr_t)a | (uintptr_t)b)
BUILTIN2("int.`--`",int_xor,C_ANY,a,C_INT,b)
RETURNS((uintptr_t)a ^ (uintptr_t)b)
BUILTIN2("int.`</`",int_shl,C_ANY,a,C_INT,b)
RETURNS((intptr_t)a<<UNFIXNUM(b))
BUILTIN2("int.`/>`",int_shr,C_ANY,a,C_INT,b)
RETURNS(((intptr_t)a>>UNFIXNUM(b))&~TAGL_MASK)
BUILTIN1("int.end",int_end,C_ANY,o)
RETURNS(FIXNUM(1))
BUILTIN1("int.char",int_char,C_ANY,o)
RETURNS(((uint64_t)o&~TAGL_MASK)|T_FIXTEXT)
BUILTIN1("int.hash",int_hash,C_ANY,o)
RETURNS(o)
BUILTIN1("int.float",int_float,C_ANY,o)
  LOAD_FLOAT(R, UNFIXNUM(o));
RETURNS(R)
BUILTIN1("int.int",int_int,C_ANY,o)
RETURNS(o)
BUILTIN1("int.sqrt",int_sqrt,C_ANY,o)
RETURNS(FIXNUM((intptr_t)sqrt((intptr_t)UNFIXNUM(o))))
BUILTIN1("int.log",int_log,C_ANY,o)
RETURNS(FIXNUM((intptr_t)log((double)(intptr_t)UNFIXNUM(o))))
BUILTIN1("int.bytes",int_bytes,C_ANY,count)
RETURNS(alloc_bytes(api,UNFIXNUM(count)))

BUILTIN1("typename",typename,C_ANY,o)
RETURNS(tag_of(o));

BUILTIN1("address",address,C_ANY,o)
RETURNS((uintptr_t)(o)&~ALIGN_MASK)

#define NUM_SPECIAL_METHODS 6

BUILTIN1("methods_",methods_,C_ANY,o)
  int i;
  void **m;
  int t = (int)O_TYPE(o);
  R = Empty; 
  for (i = NUM_SPECIAL_METHODS; i < methods_used; i++) {
     m = methods[i];
     if (m[t] != undefined) {
       void *name, *c, *pair;
       LIST_ALLOC(pair, 2);
       TEXT(name, (char*)m[T_NAME]);
       REF(pair,0) = name;
       REF(pair,1) = m[t];
       CONS(c, pair, R);
       R = c;
     }
  }
RETURNS(R)

BUILTIN0("halt",halt)
  exit(0);
RETURNS(0)

BUILTIN1("log",log,C_ANY,a)
  fprintf(stderr, "log: %s\n", print_object(a));
RETURNS(a)

BUILTIN1("say_",say_,C_TEXT,o)
  if (IS_FIXTEXT(o)) {
    char *out = print_buffer;
    out += fixtext_decode(out, o);
    *out = 0;
    fprintf(stderr, "%s", print_buffer);
  } else {
    fwrite(BIGTEXT_DATA(o), 1, (size_t)BIGTEXT_SIZE(o), stderr);
  }
RETURNS(No)

BUILTIN0("rtstat",rtstat)
  show_runtime_info(api);
RETURNS(0)

BUILTIN0("stack_trace",stack_trace)
  void **p;
  intptr_t s = Level-1;
  if (s == 0) {
    R = Empty;
  } else {
    LIST_ALLOC(R,s-1);
    p = &REF(R,0);
    while (s-- > 1) {
      intptr_t l = s + 1;
      void *init = api->frame[l].mark;
      *p++ = init;
    }
  }
RETURNS(R)

BUILTIN1("inspect",inspect,C_ANY,o)
  if (O_TAGL(o) != T_DATA) {
    fprintf(stderr, "%p: type=%d, level=immediate\n", o, (int)O_TYPE(o));
  } else {
    fprintf(stderr, "%p: type=%d, level=%ld\n", o, (int)O_TYPE(o), O_LEVEL(o));
  }
RETURNS(0)

BUILTIN1("load_library",load_library,C_TEXT,path_text)
  char path[1024];
  decode_text(path, path_text);
  R = load_lib(api, path);
RETURNS(R)

BUILTIN1("register_library_folder",register_library_folder,C_TEXT,path_text)
  char path[1024];
  decode_text(path, path_text);
  add_lib_folder(path);
RETURNS(No)

BUILTIN1("set_error_handler",set_error_handler,C_ANY,h)
  fatal("FIXME: implement set_error_handler\n");
RETURNS(No)

BUILTIN1("load_file",load_file,C_ANY,path)
  fatal("FIXME: implement load_file\n");
RETURNS(No)

BUILTIN1("utf8",utf8,C_ANY,bytes)
  fatal("FIXME: implement utf8\n");
RETURNS(No)


BUILTIN1("get_file_",get_file_,C_ANY,filename_text)
  intptr_t file_size;
  char *filename = text_to_cstring(filename_text);
  uint8_t *contents = read_whole_file(filename, &file_size);
  if (contents) {
    R = alloc_bytes(api, file_size);
    memcpy(BYTES_DATA(R), contents, file_size);
    free(contents);
  } else {
    R = No;
  }
RETURNS(R)

BUILTIN2("set_file_",set_file_,C_ANY,filename_text,C_BYTES,bytes)
  char *filename = text_to_cstring(filename_text);
  write_whole_file(filename, BYTES_DATA(bytes), BYTES_SIZE(bytes));
RETURNS(0)

BUILTIN1("get_text_file_",get_text_file_,C_TEXT,filename_text)
  intptr_t file_size;
  char *filename = text_to_cstring(filename_text);
  char *contents = (char*)read_whole_file(filename, &file_size);
  if (contents) {
    TEXT(R, contents);
    free(contents);
  } else {
    R = No;
  }
RETURNS(R)

BUILTIN2("set_text_file_",set_text_file_,C_TEXT,filename_text,C_TEXT,text)
  char buf[32];
  int size = text_size(text);
  char *filename;
  char *xs;
  if (IS_FIXTEXT(text)) {
    decode_text(buf, text);
    xs = buf;
  } else {
    xs = (char*)BIGTEXT_DATA(text);
  }
  filename = text_to_cstring(filename_text);
  write_whole_file(filename, xs, size);
RETURNS(0)


BUILTIN1("file_time_",file_time_,C_TEXT,filename_text)
  char *filename = text_to_cstring(filename_text);
  struct stat attrib;
  if (!stat(filename, &attrib)) {
    R = (void*)FIXNUM(attrib.st_mtime);
  } else {
    R = 0;
  }
RETURNS(R)

BUILTIN1("file_exists_",file_exists_,C_TEXT,filename_text)
RETURNS(FIXNUM(file_exists(text_to_cstring(filename_text))))

static int folderP(char *Name) {
  DIR *Dir;
  struct dirent *Ent;
  if(!(Dir = opendir(Name))) return 0;
  closedir(Dir);
  return 1;
}

static int fileP(char *Name) {
  FILE *F;
  if (folderP(Name)) return 0;
  F = fopen(Name, "r");
  if (!F) return 0;
  fclose (F);
  return 1;
}

BUILTIN1("folder",text_folder,C_ANY,filename_text)
RETURNS(FIXNUM(folderP(text_to_cstring(filename_text))))

BUILTIN1("file",text_file,C_ANY,filename_text)
RETURNS(FIXNUM(fileP(text_to_cstring(filename_text))))

BUILTIN_VARARGS("text.items",text_items)
  char tmp[1024];
  DIR *dir;
  struct dirent *ent;
  void *filename_text = getArg(0);
  int list_hidden = (NARGS(E) == FIXNUM(2));
  char *path = text_to_cstring(filename_text);
  void **r;
  int i;
  int len = 10;
  int pos = 0;
  if ((dir = opendir(path)) != NULL) {
    r = (void**)malloc(len*sizeof(void*));
    while ((ent = readdir(dir)) != NULL) {
      if (list_hidden) {
        if (!(strcmp(ent->d_name, ".") && strcmp(ent->d_name, ".."))) continue;
      } else {
        if (ent->d_name[0] == '.') continue;
      }
      if (pos + 1 > len) {
        void **t = r;
        len = len*2;
        r = (void**)malloc(len*sizeof(void*));
        memcpy(r, t, pos*sizeof(void*));
        free(t);
      }
      sprintf(tmp, "%s/%s", path, ent->d_name);
      if (folderP(tmp)) {
        sprintf(tmp, "%s/", ent->d_name);
        TEXT(r[pos], tmp);
      } else {
        TEXT(r[pos], ent->d_name);
      }
      ++pos;
    }
    closedir(dir);

    LIST_ALLOC(R, pos);
    for (i = 0; i < pos; i++) {
      REF(R,i) = r[i];
    }
    free(r);
  } else {
    R = Empty;
  }
RETURNS(R)

BUILTIN0("get_work_folder",get_work_folder)
  char cwd[1024];
  if (getcwd(cwd, 1024)) {
    char *p;
    for (p = cwd; *p; p++) if (*p == '\\') *p = '/';
    TEXT(R, cwd);
  } else {
    R = No;
  }
RETURNS(R)

BUILTIN0("get_symta_version",get_symta_version)
  TEXT(R, version);
RETURNS(R)

BUILTIN1("get_rt_flag_",get_rt_flag_,C_TEXT,name_text)
  char *name = text_to_cstring(name_text);
  void *one = (void*)FIXNUM(1);
  R = 0;

#ifndef WINDOWS
  if (!strcmp(name, "unix")) R = one;
#endif

#ifdef WINDOWS
  if (!strcmp(name, "windows")) R = one;
#endif

  if (sizeof(long) == 4 && !strcmp(name, "bits32")) R = one;

  if (sizeof(long) == 8 && !strcmp(name, "bits64")) R = one;

#ifdef _WIN64
  if (!strcmp(name, "win64")) R = one;
#endif

#ifdef __APPLE__
  if (!strcmp(name, "apple")) R = one;
#endif

RETURNS(R)

#ifdef WIN32
#define m_mkdir(X) mkdir(X)
#else
#define m_mkdir(X) mkdir(X, S_IRWXU|S_IRWXG|S_IROTH|S_IXOTH)
#endif

static void makePath(char *Path) {
  char T[1024], *P;
  strcpy(T, Path);
  P = T;
  while((P = strchr(P+1, '/'))) {
    *P = 0;
    // create a directory named with read/write/search permissions for
    // owner and group, and with read/search permissions for others.
    if (!file_exists(T)) m_mkdir(T);
    *P = '/';
  }
  if (!file_exists(T)) m_mkdir(T);
  //free(shell("mkdir -p '%s'", Path));
}

BUILTIN1("mkpath_",mkpath_,C_TEXT,filename_text)
  makePath(text_to_cstring(filename_text));
RETURNS(filename_text)

static char *exec_command(char *cmd) {
  char *r;
  int rdsz = 1024;
  int len = rdsz;
  int pos = 0;
  int s;
  FILE *in = popen(cmd, "r");

  if (!in) return 0;

  r = (char*)malloc(len);

  for(;;) {
    if (pos + rdsz > len) {
      char *t = r;
      len = len*2;
      r = (char*)malloc(len);
      memcpy(r, t, pos);
      free(t);
    }
    s = fread(r+pos,1,rdsz,in);
    pos += s;
    if (s != rdsz) break;
  }
  
  pclose(in);

  r[pos] = 0;
  if (pos && r[pos-1] == '\n') r[pos-1] = 0;

  return r;
}

BUILTIN1("unix",unix,C_TEXT,command_text)
  char *command = text_to_cstring(command_text);
  char *contents = exec_command(command);
  if (contents) {
    TEXT(R, contents);
    free(contents);
  } else {
    R = No;
  }
RETURNS(R)

BUILTIN0("time",time)
RETURNS(FIXNUM((intptr_t)time(0)))

//BUILTIN0("clock",clock)
//  LOAD_FLOAT(R, (double)clock()/(double)CLOCKS_PER_SEC);
//RETURNS(R)
BUILTIN0("clock",clock)
  struct timespec time;
  clock_gettime(CLOCK_REALTIME,&time);
  double dSeconds = time.tv_sec;
  double dNanoSeconds = (double)time.tv_nsec/1000000000L;
  LOAD_FLOAT(R, dSeconds+dNanoSeconds);
RETURNS(R)

BUILTIN0("main_args", main_args)
RETURNS(main_args)

BUILTIN0("main_lib", main_lib)
  TEXT(R, main_lib);
RETURNS(R)

BUILTIN1("parse_float", parse_float,C_TEXT,text)
  char buf[32];
  char *xs;
  if (IS_FIXTEXT(text)) {
    decode_text(buf, text);
    xs = buf;
  } else {
    xs = (char*)BIGTEXT_DATA(text);
  }
  LOAD_FLOAT(R, atof(xs));
RETURNS(R);


static char *get_line() {
  char *line = malloc(100), * linep = line;
  size_t lenmax = 100, len = lenmax;
  int c;

  if(line == NULL) return NULL;

  for(;;) {
    c = fgetc(stdin);
    if(c == EOF) break;

    if(--len == 0) {
      len = lenmax;
      char *linen = realloc(linep, lenmax *= 2);
      
      if(linen == NULL) {
        free(linep);
        return NULL;
      }
      line = linen + (line - linep);
      linep = linen;
    }
    
    if((*line++ = c) == '\n') break;
  }
  line[-1] = '\0';
  return linep;
}

BUILTIN0("get_line", get_line)
  char *line = get_line();
  TEXT(R,line);
  free(line);
RETURNS(R)

typedef struct {
  char *name;
  void *handle;
} ffi_lib_t;

#define FFI_MAX_LIBS 1024
static ffi_lib_t ffi_libs[FFI_MAX_LIBS];
static int ffi_libs_used;

BUILTIN2("ffi_load",ffi_load,C_TEXT,lib_name_text,C_TEXT,sym_name_text)
  int i;
  void *lib = 0;
  char name[1024];
  decode_text(name, lib_name_text);
  for (i = 0; i < ffi_libs_used; i++) {
    if (!strcmp(name, ffi_libs[i].name)) {
      lib = ffi_libs[i].handle;
      break;
    }
  }
  if (!lib) {
    lib = dlopen(name, RTLD_LAZY);
    if (!lib) fatal("ffi_load: dlopen couldnt load %s\n", name);
    ffi_libs[ffi_libs_used].name = strdup(name);
    ffi_libs[ffi_libs_used].handle = lib;
    ++ffi_libs_used;
  }

  decode_text(name, sym_name_text);
  R = dlsym(lib, name);
  if (!R) fatal("dlsym couldnt find symbol `%s` in %s\n", name, ffi_libs[ffi_libs_used-1]);
RETURNS(R)


/*
// that is how a method can be reapplied to other type:
data meta object_ info_
meta._ Method Args =
| Args.0 <= Args.0.object_
| Args.apply_method{Method}
*/
BUILTIN_VARARGS("_",sink)
  void *name;
  void *o = getArg(0);
  METHOD_NAME(name, api->method);
  fprintf(stderr, "%s has no method ", print_object(tag_of(o)));
  fprintf(stderr, "%s\n", print_object(name));
  print_stack_trace(api);
  abort();
RETURNS(0)

// handles methods that werent defined or inherited by type
BUILTIN_VARARGS("undefined",undefined)
  void *o = getArg(0);
  void **m = methods[M_SINK];
  CALL_METHOD_NO_SAVE(R,o,m);
  return (void*)R; //no need to FLIP_HEAP or GC
RETURNS(0)

static struct {
  char *name;
  void *fun;
} builtins[] = {
  {"address", b_address},
  {"inspect", b_inspect},
  {"halt", b_halt},
  {"typename", b_typename},
  {"methods_", b_methods_},
  {"log", b_log},
  {"say_", b_say_},
  {"rtstat", b_rtstat},
  {"stack_trace", b_stack_trace},
  {"get_file_", b_get_file_},
  {"set_file_", b_set_file_},
  {"get_text_file_", b_get_text_file_},
  {"set_text_file_", b_set_text_file_},
  {"file_exists_", b_file_exists_},
  {"file_time_", b_file_time_},
  {"get_work_folder", b_get_work_folder},
  {"get_symta_version", b_get_symta_version},
  {"get_rt_flag_", b_get_rt_flag_},
  {"mkpath_", b_mkpath_},
  {"load_library", b_load_library},
  {"register_library_folder", b_register_library_folder},
  {"unix", b_unix},
  {"time", b_time},
  {"clock", b_clock},
  {"main_args", b_main_args},
  {"main_lib", b_main_lib},
  {"get_line", b_get_line},
  {"parse_float", b_parse_float},
  {"ffi_load", b_ffi_load},

  {0, 0}
};

static char *print_object_r(api_t *api, char *out, void *o) {
  int i;
  int type = (int)O_TYPE(o);
  int open_par = 1;

  //fprintf(stderr, "%p = %d\n", o, type);
  //if (print_depth > 4) abort();

  if (print_depth >= MAX_PRINT_DEPTH) {
    if (!print_object_error) {
      fprintf(stderr, "MAX_PRINT_DEPTH reached: likely a recursive object\n");
      print_object_error = 1;
    }
    *out = 0;
    sprintf(print_buffer, "<MAX_PRINT_DEPTH reached>");
    return out;
  }

  if (print_object_buf_end-out<PRINT_OBJECT_MARGIN) {
    if (!print_object_error) {
      fprintf(stderr, "print_object buffer overflow: likely very large object\n");
      print_object_error = 1;
    }
    *out = 0;
    sprintf(print_buffer, "<print object buffer overflow>");
    return out;
  }

  print_depth++;

print_tail:
  if (o == No) {
    out += sprintf(out, "No");
  } else if (type == T_CLOSURE) {
    //FIXME: check metainfo to see if this object has associated print routine
    pfun handler = O_CODE(o);
    out += sprintf(out, "#(closure %p %p)", handler, o);
  } else if (type == T_INT) {
    // FIXME: this relies on the fact that shift preserves sign
    out += sprintf(out, "%ld", (intptr_t)o>>ALIGN_BITS);
  } else if (type == T_LIST) {
    int size = (int)UNFIXNUM(LIST_SIZE(o));
    if (open_par) out += sprintf(out, "(");
    for (i = 0; i < size; i++) {
      if (i || !open_par) out += sprintf(out, " ");
      out = print_object_r(api, out, REF(o,i));
    }
    out += sprintf(out, ")");
  } else if (type == T_VIEW) {
    uint32_t start = VIEW_START(o);
    int size = (int)UNFIXNUM(VIEW_SIZE(o));
    if (open_par) out += sprintf(out, "(");
    for (i = 0; i < size; i++) {
      if (i || !open_par) out += sprintf(out, " ");
      out = print_object_r(api, out, VIEW_REF(o,start,i));
    }
    out += sprintf(out, ")");
  } else if (type == T_CONS) {
    open_par = 0;
    out += sprintf(out, "(");
    for (;;) {
      out = print_object_r(api, out, CAR(o));
      o = CDR(o);
      type = (int)O_TYPE(o);
      if (type != T_CONS) goto print_tail;
      out += sprintf(out, " ");
    }
  } else if (type == T_FIXTEXT) {
    *out++ = '`';
    out = decode_text(out, o);
    *out++ = '`';
  } else if (type == T_BYTES) {
    int size = (int)BYTES_SIZE(o);
    if (open_par) out += sprintf(out, "(");
    for (i = 0; i < size; i++) {
      if (i || !open_par) out += sprintf(out, " ");
      out += sprintf(out, "%d", BYTES_DATA(o)[i]);
    }
    out += sprintf(out, ")");
  } else if (type == T_FLOAT) {
    double f;
    UNFLOAT(f,o);
    out += sprintf(out, "%.10f", f);
    while (out[-1] == '0' && out[-2] != '.') *--out = 0;
  } else if (type == T_TEXT) {
    *out++ = '`';
    out = decode_text(out, o);
    *out++ = '`';
  } else {
    if (type < types_used) {
      out += sprintf(out, "#(");
      out = decode_text(out, methods[M_NAME][type]);
      out += sprintf(out, " %p)", (void*)O_PTR(o));
    } else {
      out += sprintf(out, "#(bad_type{%d} %p)", type, o);
    }
  }
  *out = 0;

  print_depth--;

  return out;
}

//FIXME: if callee wouldnt have messed Top, we could have used it instead of passing E
static void *handle_args(REGS, void *E, intptr_t expected, intptr_t size, void *tag, void *meta) {
  intptr_t got = NARGS(E);

  if (got == FN_GET_NAME) {
    RETURN_NO_GC(tag);
  } else if (got == FN_GET_SIZE) {
    RETURN_NO_GC(size)
  } else if (got == FN_GET_META) {
    RETURN_NO_GC(meta);
  } else if (got == FN_GET_NARGS) {
    RETURN_NO_GC(expected);
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

#define GCLevel (api->level+1)
#define GC_REC(dst,value) GC_PARAM(dst,value,GCLevel,;,;)
#define MARK_MOVED(o,p) REF(o,-1) = p

static void *gc_arglist(void *o) {
  void *p, *q;
  uintptr_t i;
  uintptr_t size;
  api_t *api = &api_g;

  size = UNFIXNUM(NARGS(o));
  ARGLIST(p, size);
  MARK_MOVED(o,p);
  for (i = 0; i < size; i++) {
    ARG_LOAD(q,o,i);
    GC_REC(q, q);
    ARG_STORE(p, i, q);
  }
  return p;
}

static void *collect_immediate(void *o) {
  return o;
}

static void *collect_closure(void *o) {
  int i, size;
  uintptr_t level;
  void *p, *q;
  void *fixed_size, *dummy;
  api_t *api = &api_g;
  void *savedTop = Top;
  ALLOC_CLOSURE(dummy, FN_GET_SIZE, 1);
  CALL(fixed_size,o);
  size = UNFIXNUM(fixed_size);
  Top = savedTop;
  ALLOC_CLOSURE(p, O_CODE(o), size);
  MARK_MOVED(o,p);
  for (i = 0; i < size; i++) {
    q = REF(o,i);
    level = O_LEVEL(q);
    if (level == GCLevel) {
      q = gc_arglist(q);
    } else {
      if (level > HEAP_SIZE) {
        // already moved
        q = (void*)level;
      }
    }
    STORE(p, i, q);
  }
  return p;
}

static void *collect_list(void *o) {
  int i, size;
  void *p;
  api_t *api = &api_g;
  size = (int)UNFIXNUM(LIST_SIZE(o));
  LIST_ALLOC(p, size);
  MARK_MOVED(o,p);
  for (i = 0; i < size; i++) {
    GC_REC(REF(p,i), REF(o,i));
  }
  return p;
}

static void *collect_view(void *o) {
  void *p, *q;
  api_t *api = &api_g;
  uint32_t start = VIEW_START(o);
  uint32_t size = VIEW_SIZE(o);
  VIEW(p, 0, start, size);
  MARK_MOVED(o,p);
  q = ADD_TAG(&VIEW_REF(o,0,0), T_LIST);
  GC_REC(q, q);
  O_CODE(p) = &REF(q, 0);
  return p;
}

static void *collect_cons(void *o) {
  void *p;
  api_t *api = &api_g;
  CONS(p, 0, 0);
  MARK_MOVED(o,p);
  GC_REC(CAR(p), CAR(o))
  GC_REC(CDR(p), CDR(o))
  return p;
}

static void *collect_text(void *o) {
  void *p;
  api_t *api = &api_g;
  p = alloc_bigtext(api, BIGTEXT_DATA(o), BIGTEXT_SIZE(o));
  MARK_MOVED(o,p);
  return p;
}

static void *collect_bytes(void *o) {
  void *p;
  api_t *api = &api_g;
  int size = (int)BYTES_SIZE(o);
  p = alloc_bytes(api, size);
  memcpy(BYTES_DATA(p), BYTES_DATA(o), size);
  MARK_MOVED(o,p);
  return p;
}

static void *collect_data(void *o) {
  int i, size;
  void *p;
  api_t *api = &api_g;
  size = DATA_SIZE(o);
  ALLOC_DATA(p, O_TAGH(o), size);
  MARK_MOVED(o,p);
  for (i = 0; i < size; i++) {
    GC_REC(REF(p,i), REF(o,i));
  }
  return p;
}

#define ON_CURRENT_LEVEL(x) (Top <= (void*)x && (void*)x < Base)
static void gc_lifts() {
  int i, lifted_count;
  void **lifted;
  void *xs, *ys;
  api_t *api = &api_g;

  xs = Lifts;
  Lifts = 0;

  lifted = (void**)Top;
  --Level;

  lifted_count = 0;
  ys = Lifts;
  while (xs) {
    void **p = (void**)LIFTS_HEAD(xs);
    if ((uintptr_t)*p&LIFT_FLAG) {
      void *q;
      GC_REC(q, *p);
      *--lifted = q;
      *--lifted = p;
      *p = 0;
      lifted_count++;
    }
    xs = LIFTS_TAIL(xs);
  }
  for (i = 0; i < lifted_count; i++) {
    void **p = (void**)*lifted++;
    *p = (void*)*lifted++;
    if (ON_CURRENT_LEVEL(p)) {
      // object got lifted to the level of it's holder
      //fprintf(stderr, "lifted!\n");
    } else { // needs future lifting
      *p = (void*)((uintptr_t)*p | LIFT_FLAG);
      LIFTS_CONS(ys, p, ys);
    }
  }
  if (lifted_count > max_lifted) {
    max_lifted = lifted_count;
    //fprintf(stderr,"max_lifted=%d\n", max_lifted);
  }
  Lifts = ys;
  ++Level;
}

static void fatal_error(api_t *api, void *msg) {
  fprintf(stderr, "fatal_error: %s\n", print_object(msg));
  print_stack_trace(api);
  abort();
}

static void fatal_error_chars(api_t *api, char *msg) {
  fprintf(stderr, "fatal_error: %s\n", msg);
  print_stack_trace(api);
  abort();
}

#define METHOD_FN(name, m_int, m_float, m_fn, m_list, m_fixtext, m_text, m_view, m_cons, m_void) \
  multi = api->resolve_method(api, name); \
  if (m_int) {BUILTIN_CLOSURE(multi[T_INT], m_int);}\
  if (m_float) {BUILTIN_CLOSURE(multi[T_FLOAT], m_float);}\
  if (m_fixtext) {BUILTIN_CLOSURE(multi[T_FIXTEXT], m_fixtext);} \
  if (m_fn) {BUILTIN_CLOSURE(multi[T_CLOSURE], m_fn);}\
  if (m_list) {BUILTIN_CLOSURE(multi[T_LIST], m_list);} \
  if (m_text) {BUILTIN_CLOSURE(multi[T_TEXT], m_text);} \
  if (m_view) {BUILTIN_CLOSURE(multi[T_VIEW], m_view);} \
  if (m_cons) {BUILTIN_CLOSURE(multi[T_CONS], m_cons);} \
  if (m_void) {BUILTIN_CLOSURE(multi[T_VOID], m_void);}

#define METHOD_FN1(name, type, fn) \
  multi = api->resolve_method(api, name); \
  BUILTIN_CLOSURE(multi[type], fn); \
  if (type == T_TEXT) {BUILTIN_CLOSURE(multi[T_FIXTEXT], fn);}

#define METHOD_VAL(name, m_int, m_float, m_fn, m_list, m_fixtext, m_text, m_view, m_cons, m_void, m_bytes) \
  multi = api->resolve_method(api, name); \
  multi[T_INT] = m_int;\
  multi[T_FLOAT] = m_float; \
  multi[T_FIXTEXT] = m_fixtext; \
  multi[T_CLOSURE] = m_fn; \
  multi[T_LIST] = m_list; \
  multi[T_TEXT] = m_text; \
  multi[T_VIEW] = m_view; \
  multi[T_CONS] = m_cons; \
  multi[T_VOID] = m_void; \
  multi[T_BYTES] = m_bytes;

static void init_types(api_t *api) {
  void *n_int, *n_float, *n_fn, *n_list, *n_text, *n_void; // typenames
  void **multi;

  SET_COLLECTOR(T_INT, collect_immediate);
  SET_COLLECTOR(T_FLOAT, collect_immediate);
  SET_COLLECTOR(T_FIXTEXT, collect_immediate);
  SET_COLLECTOR(T_CLOSURE, collect_closure);
  SET_COLLECTOR(T_LIST, collect_list);
  SET_COLLECTOR(T_VIEW, collect_view);
  SET_COLLECTOR(T_CONS, collect_cons);
  SET_COLLECTOR(T_TEXT,collect_text);
  SET_COLLECTOR(T_BYTES,collect_bytes);

  TEXT(n_int, "int");
  TEXT(n_float, "float");
  TEXT(n_fn, "fn");
  TEXT(n_list, "list");
  TEXT(n_text, "text");
  TEXT(n_void, "void");

  api->resolve_type(api, "int");
  api->resolve_type(api, "float");
  api->resolve_type(api, "_fixtext_");
  api->resolve_type(api, "_data_");
  api->resolve_type(api, "fn");
  api->resolve_type(api, "_list_");
  api->resolve_type(api, "_view_");
  api->resolve_type(api, "_cons_");
  api->resolve_type(api, "_");
  api->resolve_type(api, "_text_");
  api->resolve_type(api, "void");
  api->resolve_type(api, "list");
  api->resolve_type(api, "text");
  api->resolve_type(api, "hard_list");
  api->resolve_type(api, "_name_");
  api->resolve_type(api, "_name_text_");
  api->resolve_type(api, "_bytes_");

  METHOD_VAL("_size", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  METHOD_VAL("_name", n_int, n_float, n_fn, n_list, n_text, n_text, n_list, n_list, n_void, n_list);
  METHOD_FN("_", b_sink, b_sink, b_sink, b_sink, b_sink, b_sink, b_sink, b_sink, b_sink);
  METHOD_FN("&", 0, 0, 0, 0, 0, 0, 0, 0, 0);

  METHOD_FN("_gc", 0, 0, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("_print", 0, 0, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("neg", b_int_neg, b_float_neg, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("+", b_int_add, b_float_add, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("-", b_int_sub, b_float_sub, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("*", b_int_mul, b_float_mul, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("/", b_int_div, b_float_div, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("%", b_int_rem, 0, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("**", b_int_pow, b_float_pow, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("><", b_int_eq, b_float_eq, b_fn_eq, 0, b_fixtext_eq, b_text_eq, 0, 0, b_void_eq);
  METHOD_FN("<>", b_int_ne, b_float_ne, b_fn_ne, 0, b_fixtext_ne, b_text_ne, 0, 0, b_void_ne);
  METHOD_FN("<", b_int_lt, b_float_lt, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN(">", b_int_gt, b_float_gt, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("<<", b_int_lte, b_float_lte, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN(">>", b_int_gte, b_float_gte, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("^^", b_int_mask, 0, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("++", b_int_ior, 0, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("--", b_int_xor, 0, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("</", b_int_shl, 0, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("/>", b_int_shr, 0, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("head", 0, 0, 0, b_list_head, 0, 0, b_view_head, b_cons_head, 0);
  METHOD_FN("tail", 0, 0, 0, b_list_tail, 0, 0, b_view_tail, b_cons_tail, 0);
  METHOD_FN("take", 0, 0, 0, b_list_take, 0, 0, b_view_take, 0, 0);
  METHOD_FN("drop", 0, 0, 0, b_list_drop, 0, 0, b_view_drop, 0, 0);
  METHOD_FN("pre", 0, 0, 0, b_list_pre, 0, 0, b_view_pre, b_cons_pre, 0);
  METHOD_FN("end", 0, 0, 0, b_list_end, 0, 0, b_view_end, b_cons_end, 0);
  METHOD_FN("size", 0, 0, 0, b_list_size, b_fixtext_size, b_text_size, b_view_size, 0, 0);
  METHOD_FN(".", 0, 0, 0, b_list_get, b_fixtext_get, b_text_get, b_view_get, 0, 0);
  METHOD_FN("!", 0, 0, 0, b_list_set, 0, 0, b_view_set, 0, 0);
  METHOD_FN("clear", 0, 0, 0, b_list_clear, 0, 0, 0, 0, 0);
  METHOD_FN("hash", b_int_hash, 0, 0, 0, b_fixtext_hash, b_text_hash, 0, 0, b_void_hash);
  METHOD_FN("code", 0, 0, 0, 0, b_fixtext_code, 0, 0, 0, 0);
  METHOD_FN("char", b_int_char, 0, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("text", 0, 0, 0, b_list_text, 0, 0, 0, 0, 0);
  METHOD_FN("apply", 0, 0, 0, b_list_apply, 0, 0, 0, 0, 0);
  METHOD_FN("apply_method", 0, 0, 0, b_list_apply_method, 0, 0, 0, 0, 0);
  METHOD_FN("nargs", 0, 0, b_fn_nargs, 0, 0, 0, 0, 0, 0);
  METHOD_FN("as_text", 0, b_float_as_text, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("float", b_int_float, b_float_float, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("int", b_int_int, b_float_int, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("sqrt", b_int_sqrt, b_float_sqrt, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("log", b_int_log, b_float_log, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("sin", 0, b_float_sin, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("asin", 0, b_float_asin, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("cos", 0, b_float_cos, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("acos", 0, b_float_acos, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("tan", 0, b_float_tan, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("atan", 0, b_float_atan, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("floor", 0, b_float_floor, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("ceil", 0, b_float_ceil, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("round", 0, b_float_round, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("bytes", b_int_bytes, 0, 0, 0, 0, 0, 0, 0, 0);
  METHOD_FN("utf8", 0, 0, 0, 0, b_fixtext_utf8, b_text_utf8, 0, 0, 0);

  METHOD_FN1("file", T_TEXT, b_text_file);
  METHOD_FN1("folder", T_TEXT, b_text_folder);
  METHOD_FN1("items", T_TEXT, b_text_items);

  METHOD_FN1("_", T_BYTES, b_sink);
  METHOD_FN1("size", T_BYTES, b_bytes_size);
  METHOD_FN1(".", T_BYTES, b_bytes_get);
  METHOD_FN1("!", T_BYTES, b_bytes_set);
  METHOD_FN1("clear", T_BYTES, b_bytes_clear);
  METHOD_FN1("utf8", T_BYTES, b_bytes_utf8);

  add_subtype(api, T_GENERIC_TEXT, T_FIXTEXT);
  add_subtype(api, T_GENERIC_TEXT, T_TEXT);

  add_subtype(api, T_HARD_LIST, T_LIST);
  add_subtype(api, T_HARD_LIST, T_VIEW);
  add_subtype(api, T_HARD_LIST, T_BYTES);

  add_subtype(api, T_GENERIC_LIST, T_HARD_LIST);
  add_subtype(api, T_GENERIC_LIST, T_CONS);

  api->m_ampersand = resolve_method(api, "&");
  api->m_underscore = resolve_method(api, "_");
}

static void init_builtins(api_t *api) {
  int i;
  void *rt;

  for (i = 0; builtins[i].name; i++) {
    void *t;
    TEXT(builtins[i].name, builtins[i].name);
    BUILTIN_CLOSURE(t, builtins[i].fun);
    builtins[i].fun = t;
  }

  LIST_ALLOC(rt, i);
  for (i = 0; builtins[i].name; i++) {
    void *pair;
    LIST_ALLOC(pair, 2);
    REF(pair,0) = builtins[i].name;
    REF(pair,1) = builtins[i].fun;
    REF(rt,i) = pair;
  }

  LIST_ALLOC(lib_exports, MAX_LIBS);

  lib_names[libs_used] = "rt_";
  REF(lib_exports,libs_used) = rt;
  ++libs_used;

  for (i = 0; i < 128; i++) {
    char t[2];
    t[0] = (char)i;
    t[1] = 0;
    TEXT(single_chars[i], t);
  }

  for (; i < MAX_SINGLE_CHARS; i++) {
    single_chars[i] = single_chars[0];
  }

  for (i = 0; i < MAX_METHODS; i++) {
    ALLOC_BASIC(methods[i], FIXNUM(MAX_TYPES), MAX_TYPES);
    Level = i&1;
  }
  Level = 1;

  init_types(api);
}

static void init_args(api_t *api, int argc, char **argv) {
  int i;
  char tmp[1024];
  char *lib_path;
  int main_argc = argc-1;
  char **main_argv = argv+1;

  if (argc > 1 && argv[1][0] == ':') {
    lib_path = argv[1]+1;
    --main_argc;
    ++main_argv;
  } else if (strchr(argv[0], '/') || strchr(argv[0], '\\')) {
    char *q = strdup(argv[0]);
    char *p = strchr(q, 0);
    while (*p != '/' && *p != '\\') --p;
    *++p = 0;
    sprintf(tmp, "%slib", q);
    lib_path = strdup(tmp);
    free(q);
  } else {
    lib_path = "./lib";
  }

  realpath(lib_path, tmp);
  lib_path = tmp;

  lib_path = strdup(lib_path);
  i = strlen(lib_path);

  //printf("lib_path: %s\n", lib_path);

  while (i > 0 && lib_path[i-1] == '/' || lib_path[i-1] == '\\') {
   lib_path[--i] = 0;
  }

  add_lib_folder(lib_path);
  main_lib = strdup(lib_path);

  LIST_ALLOC(main_args, main_argc);
  for (i = 0; i < main_argc; i++) {
    void *a;
    TEXT(a, main_argv[i]);
    LIFT(O_PTR(main_args),i,a);
  }
}

static api_t *init_api() {
  api_t *api = &api_g;

  api->bad_type = bad_type;
  api->handle_args = handle_args;
  api->print_object_f = print_object_f;
  api->gc_lifts = gc_lifts;
  api->alloc_text = alloc_text;
  api->fatal = fatal_error;
  api->fatal_chars = fatal_error_chars;
  api->resolve_method = resolve_method;
  api->resolve_type = resolve_type;
  api->add_subtype = add_subtype;
  api->set_type_size_and_name = set_type_size_and_name;
  api->set_method = set_method;
  api->find_export = find_export;
  api->load_lib = load_lib;
  api->text_chars = text_chars;

#define BASE_HEAD_SIZE 1
  api->frame[0].base = api->heap[0] + HEAP_SIZE;
  api->top[0] = (void**)api->frame[0].base - BASE_HEAD_SIZE;
  api->frame[1].base = api->heap[1] + HEAP_SIZE;
  api->top[1] = (void**)api->frame[1].base - BASE_HEAD_SIZE;

  api->level = 0;

  BUILTIN_CLOSURE(undefined, b_undefined);
  BUILTIN_CLOSURE(sink, b_sink);

  ALLOC_DATA(No, T_VOID, 0);
  LIST_ALLOC(Empty, 0);

  return api;
}

int main(int argc, char **argv) {
  int i;
  char tmp[1024];
  api_t *api;
  void *R;

  initializing = 1;
  api = init_api();
  init_args(api, argc, argv);
  init_builtins(api);
  initializing = 0;

  runtime_reserved0 = get_heap_used(0);
  runtime_reserved1 = get_heap_used(1);

  sprintf(tmp, "%s/%s", main_lib, "main");
  R = exec_module(api, tmp);
  //fprintf(stderr, "%s\n", print_object(R));

  return 0;
}
