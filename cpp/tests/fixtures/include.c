#define OUTER 1
#include "inc_a.h"
int after_a = OUTER + FROM_A;
#include <inc_b.h>
int after_b = FROM_B;
#define HDR "inc_guard.h"
#include HDR
#include HDR
int guarded = GUARDED;
#include "inc_once.h"
#include "inc_once.h"
int onced = ONCED;
#include "inc_tail.h"
