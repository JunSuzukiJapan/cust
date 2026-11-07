#define EMPTY
#ifdef EMPTY
int empty_defined = 1;
#endif
int e = EMPTY;
#define SPACED (1 + 2)
int spaced = SPACED;
#define R1A 1
#define R1A 1
int r1 = R1A;
#define R2A 1
#define R2A 2
int r2 = R2A;
#define VARIADIC(...) 0
int variadic_defined = 1;
#define PRE 1
#define PREFIX 2
int pre = PRE + PREFIX;
