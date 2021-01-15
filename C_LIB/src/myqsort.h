#ifndef _MYQSORT_H
#define _MYQSORT_H
typedef int  comparF (const void *, const void *);
void myqsort(void *baseP, size_t nElem, size_t sz, comparF *compar);
#endif
