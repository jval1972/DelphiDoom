#include "sfendian.h"

uint64_t ENDSWAP_64 (uint64_t x)
{	union
	{	uint32_t parts [2] ;
		uint64_t whole ;
	} u ;
	uint32_t temp ;

	u.whole = x ;
	temp = u.parts [0] ;
	u.parts [0] = ENDSWAP_32 (u.parts [1]) ;
	u.parts [1] = ENDSWAP_32 (temp) ;
	return u.whole ;
}

void psf_put_be64 (uint8_t *ptr, int offset, int64_t value)
{
	ptr [offset] = value >> 56 ;
	ptr [offset + 1] = value >> 48 ;
	ptr [offset + 2] = value >> 40 ;
	ptr [offset + 3] = value >> 32 ;
	ptr [offset + 4] = value >> 24 ;
	ptr [offset + 5] = value >> 16 ;
	ptr [offset + 6] = value >> 8 ;
	ptr [offset + 7] = value ;
} /* psf_put_be64 */

void psf_put_be32 (uint8_t *ptr, int offset, int32_t value)
{
	ptr [offset] = value >> 24 ;
	ptr [offset + 1] = value >> 16 ;
	ptr [offset + 2] = value >> 8 ;
	ptr [offset + 3] = value ;
} /* psf_put_be32 */

void psf_put_be16 (uint8_t *ptr, int offset, int16_t value)
{
	ptr [offset] = value >> 8 ;
	ptr [offset + 1] = value ;
} /* psf_put_be16 */

int64_t psf_get_be64 (uint8_t *ptr, int offset)
{	int64_t value ;

	value = ((uint32_t) ptr [offset]) << 24 ;
	value += ptr [offset + 1] << 16 ;
	value += ptr [offset + 2] << 8 ;
	value += ptr [offset + 3] ;

	value = ((uint64_t) value) << 32 ;

	value += ((uint32_t) ptr [offset + 4]) << 24 ;
	value += ptr [offset + 5] << 16 ;
	value += ptr [offset + 6] << 8 ;
	value += ptr [offset + 7] ;
	return value ;
} /* psf_get_be64 */

int64_t psf_get_le64 (uint8_t *ptr, int offset)
{	int64_t value ;

	value = ((uint32_t) ptr [offset + 7]) << 24 ;
	value += ptr [offset + 6] << 16 ;
	value += ptr [offset + 5] << 8 ;
	value += ptr [offset + 4] ;

	value = ((uint64_t) value) << 32 ;

	value += ((uint32_t) ptr [offset + 3]) << 24 ;
	value += ptr [offset + 2] << 16 ;
	value += ptr [offset + 1] << 8 ;
	value += ptr [offset] ;
	return value ;
} /* psf_get_le64 */

int32_t psf_get_be32 (uint8_t *ptr, int offset)
{	int32_t value ;

	value = ((uint32_t) ptr [offset]) << 24 ;
	value += ptr [offset + 1] << 16 ;
	value += ptr [offset + 2] << 8 ;
	value += ptr [offset + 3] ;
	return value ;
} /* psf_get_be32 */

int32_t psf_get_le32 (uint8_t *ptr, int offset)
{	int32_t value ;

	value = ((uint32_t) ptr [offset + 3]) << 24 ;
	value += ptr [offset + 2] << 16 ;
	value += ptr [offset + 1] << 8 ;
	value += ptr [offset] ;
	return value ;
} /* psf_get_le32 */

int32_t psf_get_be24 (uint8_t *ptr, int offset)
{	int32_t value ;

	value = ((uint32_t) ptr [offset]) << 24 ;
	value += ptr [offset + 1] << 16 ;
	value += ptr [offset + 2] << 8 ;
	return value ;
} /* psf_get_be24 */

int32_t psf_get_le24 (uint8_t *ptr, int offset)
{	int32_t value ;

	value = ((uint32_t) ptr [offset + 2]) << 24 ;
	value += ptr [offset + 1] << 16 ;
	value += ptr [offset] << 8 ;
	return value ;
} /* psf_get_le24 */

int16_t psf_get_be16 (uint8_t *ptr, int offset)
{	return (ptr [offset] << 8) + ptr [offset + 1] ;
} /* psf_get_be16 */

void endswap_short_array (short *ptr, int len)
{	short	temp ;

	while (--len >= 0)
	{	temp = ptr [len] ;
		ptr [len] = ENDSWAP_16 (temp) ;
		} ;
} /* endswap_short_array */

void endswap_short_copy (short *dest, const short *src, int len)
{
	while (--len >= 0)
	{	dest [len] = ENDSWAP_16 (src [len]) ;
		} ;
} /* endswap_short_copy */

void endswap_int_array (int *ptr, int len)
{	int temp ;

	while (--len >= 0)
	{	temp = ptr [len] ;
		ptr [len] = ENDSWAP_32 (temp) ;
		} ;
} /* endswap_int_array */

void endswap_int_copy (int *dest, const int *src, int len)
{
	while (--len >= 0)
	{	dest [len] = ENDSWAP_32 (src [len]) ;
		} ;
} /* endswap_int_copy */

void endswap_int64_t_array (int64_t *ptr, int len)
{	int64_t value ;

	while (--len >= 0)
	{	value = ptr [len] ;
		ptr [len] = ENDSWAP_64 (value) ;
		} ;
} /* endswap_int64_t_array */

void endswap_int64_t_copy (int64_t *dest, const int64_t *src, int len)
{	int64_t value ;

	while (--len >= 0)
	{	value = src [len] ;
		dest [len] = ENDSWAP_64 (value) ;
		} ;
} /* endswap_int64_t_copy */

void endswap_float_array (float *ptr, int len)
{	endswap_int_array ((int *) ptr, len) ;
} /* endswap_float_array */

void endswap_double_array (double *ptr, int len)
{	endswap_int64_t_array ((int64_t *) ptr, len) ;
} /* endswap_double_array */

void endswap_float_copy (float *dest, const float *src, int len)
{	endswap_int_copy ((int *) dest, (const int *) src, len) ;
} /* endswap_float_copy */

void endswap_double_copy (double *dest, const double *src, int len)
{	endswap_int64_t_copy ((int64_t *) dest, (const int64_t *) src, len) ;
} /* endswap_double_copy */
