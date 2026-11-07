#define CAT(a, b) a ## b
int ab = CAT(a, b);
int a_only = CAT(a, );
int b_only = CAT(, b);
int neither = CAT(,);
#define NUM(n) n ## 12
int num = NUM(X);
#define VER(maj, min) maj ## . ## min
double ver = VER(1, 2);
