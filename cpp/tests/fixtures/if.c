#define VER 3
#define ZERO 0
#if VER >= 3
int v3 = 1;
#endif
#if ZERO
int never0 = 1;
#elif VER == 3
int elif_hit = 1;
#else
int never1 = 1;
#endif
#if (VER + 1) * 2 == 8 && VER
int arith = 1;
#endif
#if !(VER < 3) && (VER != 2 || VER > 10)
int logic = 1;
#endif
#if VER > 5 ? 0 : 1
int ternary = 1;
#endif
#if 10 / 3 == 3 && -7 / 2 == -3 && -7 % 2 == -1
int divmod = 1;
#endif
#if 0xFF == 255 && 010 == 8 && 0b101 == 5
int bases = 1;
#endif
#if 'a' == 97 && '\n' == 10 && 'ab' == 0x6162
int chars = 1;
#endif
#if -1 < 0U
int never2 = 1;
#else
int unsigned_cmp = 1;
#endif
#if UNDEFINED_NAME_HERE
int never3 = 1;
#endif
#if defined(VER) && !defined(NOPE)
int defined_op = 1;
#endif
#if defined VER
int defined_bare = 1;
#endif
