#define A 1
#if A
int outer = 1;
#if 0
int never0 = 1;
#else
int inner_else = 1;
#endif
#else
int never1 = 1;
#if 1
int never2 = 1;
#else
int never3 = 1;
#endif
#endif
#if 0
#define SKIPPED_DEF 1
#if (((unbalanced-looking but skipped
#endif
int never4 = 1;
#endif
int tail = 1;
#ifndef SKIPPED_DEF
int not_defined = 1;
#endif
