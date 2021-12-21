#include <stdlib.h>
#include "myqsort.h"

static  comparF    *Fmycmp;
static  unsigned    qsSize;

static  void  Exchange (void  *leftP, void *rightP)
{
        unsigned  i;
        char  c;
        char *lp = (char *)leftP;
        char *rp = (char *)rightP;

        for (i = 0; i < qsSize; i++ )
        {
                c = *rp;
                *rp++ = *lp;
                *lp++ = c;
        }
}

static void  myqSortHelper (char *pivotP, size_t nElem)
{
    char     *leftP, *rightP, *pivotEnd, *pivotTemp, *leftTemp;
    unsigned  lNum;
    int       retval;


tailRecursion:
    if (nElem <= 2)
        {
        if (nElem == 2)
            {
            if (Fmycmp (pivotP, rightP = qsSize + pivotP) > 0)
                Exchange (pivotP, rightP);
            }
        return;
        }

    rightP = (nElem - 1) * qsSize + pivotP;
    leftP  = (nElem >> 1) * qsSize + pivotP;

    if (Fmycmp (leftP, rightP) > 0)
        Exchange (leftP, rightP);
    if (Fmycmp (leftP, pivotP) > 0)
        Exchange (leftP, pivotP);
    else if (Fmycmp (pivotP, rightP) > 0)
        Exchange (pivotP, rightP);

    if (nElem == 3)
        {
        Exchange (pivotP, leftP);
        return;
        }

    leftP = pivotEnd = pivotP + qsSize;

    do
        {
        while ((retval = Fmycmp(leftP, pivotP)) <= 0)
            {
            if (retval == 0)
                {
                Exchange(leftP, pivotEnd);
                pivotEnd += qsSize;
                }
            if (leftP < rightP)
                leftP += qsSize;
            else
                goto qBreak;
            }

        while (leftP < rightP)
            {
            if ((retval = Fmycmp(pivotP, rightP)) < 0)
                rightP -= qsSize;
            else
                {
                Exchange (leftP, rightP);
                if (retval != 0)
                    {
                    leftP += qsSize;
                    rightP -= qsSize;
                    }
                break;
                }
            }
        }   while (leftP < rightP);

qBreak:

    if (Fmycmp(leftP, pivotP) <= 0)
        leftP = leftP + qsSize;

    leftTemp = leftP - qsSize;

    pivotTemp = pivotP;

    while ((pivotTemp < pivotEnd) && (leftTemp >= pivotEnd))
        {
        Exchange(pivotTemp, leftTemp);
        pivotTemp += qsSize;
        leftTemp -= qsSize;
        }

    lNum = (leftP - pivotEnd) / qsSize;
    nElem = ((nElem * qsSize + pivotP) - leftP)/qsSize;

    if (nElem < lNum)
        {
        myqSortHelper (leftP, nElem);
        nElem = lNum;
        }
    else
        {
        myqSortHelper (pivotP, lNum);
        pivotP = leftP;
        }

    goto tailRecursion;
}

void myqsort(void *baseP, size_t nElem, size_t sz, comparF *compar)
{
    if ((qsSize = sz) == 0)
        return;

    Fmycmp = compar;

    myqSortHelper (baseP, nElem);
}
