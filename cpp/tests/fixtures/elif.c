#define LEVEL 2
#if LEVEL == 1
int l1 = 1;
#elif LEVEL == 2
int l2 = 1;
#elif LEVEL == 3
int l3 = 1;
#else
int other = 1;
#endif
#if 1
int taken = 1;
#elif !!!
int never = 1;
#endif
#if 0
int never2 = 1;
#elif 0
int never3 = 1;
#endif
int done = 1;
