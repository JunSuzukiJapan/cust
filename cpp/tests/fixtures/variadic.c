#define V(...) f(__VA_ARGS__)
V();
V(1);
V(1, 2);
V(p
q);
#define LOG(fmt, ...) printf(fmt, ##__VA_ARGS__)
LOG("x");
LOG("x", 1);
LOG("x", 1, 2);
LOG("x",);
#define L1(fmt, ...) printf(fmt,##__VA_ARGS__)
#define L2(fmt, ...) printf(fmt,## __VA_ARGS__)
#define L3(fmt, ...) printf(fmt , ##__VA_ARGS__)
L1("x", 1);
L2("x", 1);
L3("x", 1);
#define N(args...) args
int n = N(7, 8);
