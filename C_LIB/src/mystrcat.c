#include "mystrcat.h"

char *mystrcat(char *dest, const char *src)
{
    char *ret = dest;
    while (*dest){dest++;};
    while (*dest++ = *src++);
    return ret;
}