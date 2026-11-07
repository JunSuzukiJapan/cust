#define FLAG 1
#ifdef FLAG
int a = 1;
#else
int a = 2;
#endif
#ifndef MISSING
int b = 3;
#endif
int c = 4;
