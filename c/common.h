#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef void (*pfun)();

#define TAG_BITS 2
#define TAG_MASK ((1<<TAG_BITS)-1)
#define TAG_OF(src) ((uintptr_t)(src) & TAG_MASK)

#define T_INTEGER  0
#define T_STRING   1
#define T_PAIR     2
#define T_CLOSURE  3

#define INTEGER(dst,x) dst = (void*)(((uintptr_t)(x)<<TAG_BITS) | T_INTEGER)
#define STRING(dst,x)  dst = (void*)((uintptr_t)strdup(x) | T_STRING)
#define PAIR(dst,a,b) \
  ALLOC(dst, 2); \
  STORE(dst, 0, a); \
  STORE(dst, 1, b); \
  dst = (void*)((uintptr_t)dst | T_PAIR);

#define CLOSURE(dst,code,env) \
  ALLOC(dst, 2); \
  STORE(dst, 0, code); \
  STORE(dst, 1, env); \
  dst = (void*)((uintptr_t)dst | T_CLOSURE);


static void bad_tag(int tag, int expected) {
  printf("bad tag=%d, expected tag=%d\n", tag, expected);
  abort();
}


#define ALLOC(dst, size) dst = malloc(size*sizeof(void*))
#define CALL(f) \
  f = (void*)((uintptr_t)f-T_CLOSURE); \
  LOAD(P,f,1); \
  (((pfun*)f)[0])();
#define STORE(dst,off,src) ((void**)(dst))[(int)(off)] = (src)
#define LOAD(dst,src,off) dst = ((void**)(src))[(int)(off)]
#define COPY(dst,p,src,q) ((void**)(dst))[(int)(p)] = ((void**)(src))[(int)(q)]
#define MOVE(dst,src) dst = (void*)(src)
#define TAGCHECK(src,expected) if (TAG_OF(src) != expected) bad_tag(TAG_OF(src), expected);

static void entry();
static void run_f();

static void
  *E, // environment/args
  *P, // parent environment
  *A, // args scratchpad
  *C, // code pointer
  *R, // return value
  *run;

int main(int argc, char **argv) {
  CLOSURE(run, run_f, 0);
  entry();
}

//E[0] = environment, E[1] = continuation, E[2] = function_name
static void host() {
  //LOAD(R, E, 1);
  //LOAD(C, R, 0);
  printf("host is unimplemented\n");
  abort();
}

static void run_f() {
  printf("got into run!\n");
  abort();
  LOAD(R, E, 1);
  LOAD(C, R, 0);
  ALLOC(A, 2);
  STORE(A, 0, host);
  STORE(A, 1, 0);
  MOVE(E, A);
  CALL(C);
}
