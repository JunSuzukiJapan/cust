int a = 1;
#line 100 "other.c"
int b = __LINE__;
const char *fb = __FILE__;
#line 200
int c = __LINE__;
const char *fc = __FILE__;
# 300 "bare.c"
int d = __LINE__;
#define N 42
#define F "m.c"
#line N F
int e = __LINE__;
const char *fe = __FILE__;
#line 0
int f = __LINE__;
