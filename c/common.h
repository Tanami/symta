#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef void (*pfun)();

#define TAG_BITS 2
#define TAG_MASK ((1<<TAG_BITS)-1)

#define getArg(N) ((void**)(E))[N]
#define getTag(X) ((uintptr_t)(X)&TAG_MASK)
#define getVal(X) (void*)((uintptr_t)(X)&~TAG_MASK)


#define T_INTEGER  0
#define T_STRING   1
#define T_PAIR     2
#define T_CLOSURE  3

// make strings and pairs statically allocable
#define INTEGER(dst,x) dst = (void*)(((uintptr_t)(x)<<TAG_BITS) | T_INTEGER)
#define STRING(dst,x)  dst = (void*)((uintptr_t)strdup(x) | T_STRING)
#define PAIR(dst,a,b) \
  ALLOC(T, 2); \
  STORE(T, 0, a); \
  STORE(T, 1, b); \
  dst = (void*)((uintptr_t)T | T_PAIR);
#define ALLOC(dst, size) dst = malloc(size*sizeof(void*))
#define CALL(f) \
  P = (void*)((uintptr_t)f-T_CLOSURE); \
  (((pfun*)P)[0])();
#define STORE(dst,off,src) ((void**)(dst))[(int)(off)] = (src)
#define LOAD(dst,src,off) dst = ((void**)(src))[(int)(off)]
#define COPY(dst,p,src,q) ((void**)(dst))[(int)(p)] = ((void**)(src))[(int)(q)]
#define MOVE(dst,src) dst = (void*)(src)
#define IOR(dst,a,b) dst = (void*)((uintptr_t)(a)|(uintptr_t)(b))
#define TAGCHECK(src,expected) if (getTag(src) != expected) bad_tag(getTag(src), expected);

static void bad_tag(int tag, int expected) {
  printf("bad tag=%d, expected tag=%d\n", tag, expected);
  abort();
}


static void entry();
static void run_f();
static void fin_f();
static void host_f();

static void
  *E, // current environment
  *P, // parent environment
  *A, // args scratchpad
  *C, // code pointer
  *R, // return value
  *T, // temporary, used by CLOSURE, PAIR and other macros
  *N, // number of arguments to the current function (size of E)
  *v_void,
  *v_yes,
  *v_no,
  *fin, // the closure, which would recieve evaluation result
  *run, // the closure, which would recieve the resulting program
  *host;

#define CLOSURE(dst,f) \
  ALLOC(T, 1); \
  STORE(T, 0, f); \
  dst = (void*)((uintptr_t)T | T_CLOSURE);


int main(int argc, char **argv) {
  CLOSURE(run, run_f);
  CLOSURE(fin, fin_f);
  CLOSURE(host, host_f);
  entry();
}

#define GETARG(N) ((void**)(E))[N]

//E[0] = environment, E[1] = continuation, E[2] = function_name

// run continuation recieves entry point into user specified program
// it should run it with supplyed host resolver, which should resolve all unknown symbols
static void run_f() {
  printf("got into run!\n");
  LOAD(C, E, 0);
  ALLOC(E, 2);
  STORE(E, 0, fin); // continuation
  STORE(E, 1, host); // resolver
  CALL(C);
}

static void fin_f() {
  printf("got into fin!\n");
  abort();
}

static void host_f() {
  //LOAD(R, E, 1);
  //LOAD(C, R, 0);
  printf("host \"%s\"\n", (char*)getVal(getArg(1)));
  printf("host is unimplemented!\n");
  abort();
}
