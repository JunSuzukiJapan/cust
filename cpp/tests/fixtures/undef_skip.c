#define KEEP 1
#if 0
#undef KEEP
#endif
int x = KEEP;
#ifdef KEEP
int kept = 1;
#endif
#undef KEEP
#ifndef KEEP
int gone = 1;
#endif
#undef NEVER_DEFINED
int done = 1;
