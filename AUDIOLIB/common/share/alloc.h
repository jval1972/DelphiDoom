/* alloc - Convenience routines for safely allocating memory
 * Copyright (C) 2007-2009  Josh Coalson
 * Copyright (C) 2011-2016  Xiph.Org Foundation
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * - Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of the Xiph.org Foundation nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef FLAC__SHARE__ALLOC_H
#define FLAC__SHARE__ALLOC_H

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

/* WATCHOUT: for c++ you may have to #define __STDC_LIMIT_MACROS 1 real early
 * before #including this file,  otherwise SIZE_MAX might not be defined
 */

#include <limits.h> /* for SIZE_MAX */
#if HAVE_STDINT_H
#include <stdint.h> /* for SIZE_MAX in case limits.h didn't get it */
#endif
#include <stdlib.h> /* for size_t, malloc(), etc */
#include "../../common/share/compat.h"

# define SIZE_MAX 65535

void *safe_malloc_(size_t size);

void *safe_calloc_(size_t nmemb, size_t size);

void *safe_malloc_add_2op_(size_t size1, size_t size2);

void *safe_malloc_add_3op_(size_t size1, size_t size2, size_t size3);

void *safe_malloc_add_4op_(size_t size1, size_t size2, size_t size3, size_t size4);

void *safe_malloc_mul_2op_(size_t size1, size_t size2);

void *safe_malloc_mul_3op_(size_t size1, size_t size2, size_t size3);

/* size1*size2 + size3 */
void *safe_malloc_mul2add_(size_t size1, size_t size2, size_t size3);

/* size1 * (size2 + size3) */
void *safe_malloc_muladd2_(size_t size1, size_t size2, size_t size3);

void *safe_realloc_(void *ptr, size_t size);

void *safe_realloc_add_2op_(void *ptr, size_t size1, size_t size2);

void *safe_realloc_add_3op_(void *ptr, size_t size1, size_t size2, size_t size3);

void *safe_realloc_add_4op_(void *ptr, size_t size1, size_t size2, size_t size3, size_t size4);

/* size1 * (size2 + size3) */
void *safe_realloc_muladd2_(void *ptr, size_t size1, size_t size2, size_t size3);

void *safe_realloc_mul_2op_(void *ptr, size_t size1, size_t size2);

#endif
