cflags="-O1 -Wno-return-type -Wno-pointer-sign -I ./runtime"
bs="bootstrap/c/"
gcc ${cflags} runtime/runtime.c -o symta
mkdir lib
gcc ${cflags} -fpic -shared "${bs}core_.c" -o lib/core_
gcc ${cflags} -fpic -shared "${bs}reader.c" -o lib/reader
gcc ${cflags} -fpic -shared "${bs}compiler.c" -o lib/compiler
gcc ${cflags} -fpic -shared "${bs}macro.c" -o lib/macro
gcc ${cflags} -fpic -shared "${bs}eval.c" -o lib/eval
gcc ${cflags} -fpic -shared "${bs}main.c" -o lib/main

