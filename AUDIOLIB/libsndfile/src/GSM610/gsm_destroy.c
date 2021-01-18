/*
 * Copyright 1992 by Jutta Degener and Carsten Bormann, Technische
 * Universitaet Berlin.  See the accompanying file "COPYRIGHT" for
 * details.  THERE IS ABSOLUTELY NO WARRANTY FOR THIS SOFTWARE.
 */

#include "gsm.h"
#include "config.h"

#include <stdlib.h>

void gsm_destroy (gsm S)
{
	if (S)
		free ((char *) S) ;
}

