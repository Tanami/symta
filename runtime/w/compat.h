#ifndef SYMTA_COMPAT_H_
#define SYMTA_COMPAT_H_

char *realpath(const char *path, char *resolved_path);
int clock_gettime(int X, struct timeval *tv);

#endif
