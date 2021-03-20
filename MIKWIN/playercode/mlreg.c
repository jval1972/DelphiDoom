/*	MikMod sound library
	(c) 1998, 1999 Miodrag Vallat and others - see file AUTHORS for
	complete list.

	This library is free software; you can redistribute it and/or modify
	it under the terms of the GNU Library General Public License as
	published by the Free Software Foundation; either version 2 of
	the License, or (at your option) any later version.
 
	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Library General Public License for more details.
 
	You should have received a copy of the GNU Library General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
	02111-1307, USA.
*/

/*==============================================================================

  $Id: mlreg.c,v 1.16 1999/02/08 07:24:32 miod Exp $

  Routine for registering all loaders in libmikmod for the current platform.

==============================================================================*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <mikmod_internals.h>

void MikMod_RegisterAllLoaders(void)
{
	MikMod_RegisterLoader(&load_669);
	MikMod_RegisterLoader(&load_amf);
	MikMod_RegisterLoader(&load_dsm);
	MikMod_RegisterLoader(&load_far);
	MikMod_RegisterLoader(&load_it);
	MikMod_RegisterLoader(&load_imf);
	MikMod_RegisterLoader(&load_mod);
	MikMod_RegisterLoader(&load_med);
	MikMod_RegisterLoader(&load_mtm);
	MikMod_RegisterLoader(&load_s3m);
	MikMod_RegisterLoader(&load_stm);
	MikMod_RegisterLoader(&load_stx);
	MikMod_RegisterLoader(&load_ult);
	MikMod_RegisterLoader(&load_uni);
	MikMod_RegisterLoader(&load_xm);

	MikMod_RegisterLoader(&load_m15);
}
