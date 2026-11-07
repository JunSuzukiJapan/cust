#define S(x) #x
const char *a = S(hello world);
const char *b = S(a  +   b);
const char *c = S("q");
const char *d = S();
#define T(x) # x
const char *e = T(spaced);
