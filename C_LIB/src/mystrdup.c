#include <alloc.h>
#include "mystrdup.h"

char *mystrdup(char *src)
{
    char *ret;
    char *p;
    int len = 0;

    while (src[len])
        len++;
    ret = malloc(len + 1);
    p = ret;
    while (*src)
        *p++ = *src++;
    *p = '\0';
    return ret;
}
