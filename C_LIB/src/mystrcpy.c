#include <stdlib.h>
#include "mystrcpy.h"

char* mystrcpy(char* dest, const char* src)
{
    char *ptr;
    // return if no memory is allocated to the dest
    if (!dest)
        return NULL;

    // make ptr pointing to the beginning of dest string
    ptr = dest;

    // copy the C-string pointed by src into the array
    // pointed by dest
    while (*src != '\0')
    {
        *dest = *src;
        dest++;
        src++;
    }

    // include the terminating null character
    *dest = '\0';

    // dest is returned by standard strcpy()
    return ptr;
}
