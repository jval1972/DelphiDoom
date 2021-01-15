/*
** Copyright (C) 1999-2016 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation; either version 2.1 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#ifndef SFENDIAN_INCLUDED
#define SFENDIAN_INCLUDED

#include "sfconfig.h"

#include <stdint.h>

#define	ENDSWAP_16(x)		((((x) >> 8) & 0xFF) + (((x) & 0xFF) << 8))
#define	ENDSWAP_32(x)		((((x) >> 24) & 0xFF) + (((x) >> 8) & 0xFF00) + (((x) & 0xFF00) << 8) + (((x) & 0xFF) << 24))

uint64_t ENDSWAP_64 (uint64_t x);

/*
** Many file types (ie WAV, AIFF) use sets of four consecutive bytes as a
** marker indicating different sections of the file.
** The following MAKE_MARKER macro allows th creation of integer constants
** for these markers.
*/

#define CPU_IS_LITTLE_ENDIAN 1
#define CPU_IS_BIG_ENDIAN 0

/*
** Macros to handle reading of data of a specific endian-ness into host endian
** shorts and ints. The single input is an unsigned char* pointer to the start
** of the object. There are two versions of each macro as we need to deal with
** both big and little endian CPUs.
*/

#if (CPU_IS_LITTLE_ENDIAN == 1)
	#define LE2H_16(x)			(x)
	#define LE2H_32(x)			(x)

	#define BE2H_16(x)			ENDSWAP_16 (x)
	#define BE2H_32(x)			ENDSWAP_32 (x)
	#define BE2H_64(x)			ENDSWAP_64 (x)

	#define H2BE_16(x)			ENDSWAP_16 (x)
	#define H2BE_32(x)			ENDSWAP_32 (x)

	#define H2LE_16(x)			(x)
	#define H2LE_32(x)			(x)

#elif (CPU_IS_BIG_ENDIAN == 1)
	#define LE2H_16(x)			ENDSWAP_16 (x)
	#define LE2H_32(x)			ENDSWAP_32 (x)

	#define BE2H_16(x)			(x)
	#define BE2H_32(x)			(x)
	#define	BE2H_64(x)			(x)

	#define H2BE_16(x)			(x)
	#define H2BE_32(x)			(x)

	#define H2LE_16(x)			ENDSWAP_16 (x)
	#define H2LE_32(x)			ENDSWAP_32 (x)

#else
	#error "Target CPU endian-ness unknown. May need to hand edit src/sfconfig.h"
#endif

#define LET2H_16_PTR(x)			((x) [1] + ((x) [2] << 8))
#define LET2H_32_PTR(x)			(((x) [0] << 8) + ((x) [1] << 16) + ((x) [2] << 24))

#define BET2H_16_PTR(x)			(((x) [0] << 8) + (x) [1])
#define BET2H_32_PTR(x)			(((x) [0] << 24) + ((x) [1] << 16) + ((x) [2] << 8))

void psf_put_be64 (uint8_t *ptr, int offset, int64_t value);

void psf_put_be32 (uint8_t *ptr, int offset, int32_t value);

void psf_put_be16 (uint8_t *ptr, int offset, int16_t value);

int64_t psf_get_be64 (uint8_t *ptr, int offset);

int64_t psf_get_le64 (uint8_t *ptr, int offset);

int32_t psf_get_be32 (uint8_t *ptr, int offset);

int32_t psf_get_le32 (uint8_t *ptr, int offset);

int32_t psf_get_be24 (uint8_t *ptr, int offset);

int32_t psf_get_le24 (uint8_t *ptr, int offset);

int16_t psf_get_be16 (uint8_t *ptr, int offset);

/*-----------------------------------------------------------------------------------------------
** Generic functions for performing endian swapping on integer arrays.
*/

void endswap_short_array (short *ptr, int len);

void endswap_short_copy (short *dest, const short *src, int len);

void endswap_int_array (int *ptr, int len);

void endswap_int_copy (int *dest, const int *src, int len);

/*========================================================================================
*/

void endswap_int64_t_array (int64_t *ptr, int len);

void endswap_int64_t_copy (int64_t *dest, const int64_t *src, int len);

/* A couple of wrapper functions. */

void endswap_float_array (float *ptr, int len);

void endswap_double_array (double *ptr, int len);

void endswap_float_copy (float *dest, const float *src, int len);

void endswap_double_copy (double *dest, const double *src, int len);

#endif /* SFENDIAN_INCLUDED */

