@echo off
set cflags=-O1 -Wno-return-type -Wno-pointer-sign -D WINDOWS -I ./runtime
set bs=bootstrap/c/
gcc %cflags% -I ./runtime/w runtime/runtime.c runtime/w/dlfcn.c runtime/w/compat.c -o symta.exe
mkdir lib
gcc %cflags% -fpic -shared %bs%core_.c -o lib/core_.
gcc %cflags% -fpic -shared %bs%reader.c -o lib/reader.
gcc %cflags% -fpic -shared %bs%compiler.c -o lib/compiler.
gcc %cflags% -fpic -shared %bs%macro.c -o lib/macro.
gcc %cflags% -fpic -shared %bs%eval.c -o lib/eval.
gcc %cflags% -fpic -shared %bs%main.c -o lib/main.
