#define A F
#define F(x) x + 1
int rescan = A(2);
#define ID(x) x
int prescan = ID(ID(3));
#define SELF SELF + 1
int selfref = SELF;
#define M1 M2
#define M2 M1
int mutual = M1;
#define FX(x) x
int nested_call = FX(FX(4));
