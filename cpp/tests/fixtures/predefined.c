int std = __STDC__;
long ver = __STDC_VERSION__;
int hosted = __STDC_HOSTED__;
int l1 = __LINE__;
const char *f1 = __FILE__;
int c1 = __COUNTER__;
int c2 = __COUNTER__;
int lvl = __INCLUDE_LEVEL__;
const char *d = __DATE__;
const char *ts = __TIMESTAMP__;
#if __STDC__ == 1
int is_stdc = 1;
#endif
#ifdef __LINE__
int has_line = 1;
#endif
#if defined(__COUNTER__)
int has_counter = 1;
#endif
