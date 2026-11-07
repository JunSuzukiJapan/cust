#pragma foo bar
int x = 1;
#pragma STDC FENV_ACCESS ON
int y = 2;
int a; _Pragma("pack(push, 1)") int b;
_Pragma("pack(pop)") int c;
int d; _Pragma("comment(lib)")
_Pragma("e1") _Pragma("e2") int g;
#define DO_PRAGMA _Pragma("instrument")
DO_PRAGMA
int after_macro_pragma = 3;
#pragma mark section one
#pragma region R
#pragma endregion
#pragma GCC diagnostic ignored "-Wdeprecated"
#pragma clang diagnostic push
int after_pragmas = 5;
_Pragma("once")
#pragma once
int z = 4;
