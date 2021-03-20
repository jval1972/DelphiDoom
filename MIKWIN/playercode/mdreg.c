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

  $Id: mdreg.c,v 1.19 1999/02/08 07:24:38 miod Exp $

  Routine for registering all drivers in MikMod for the current platform.

  Modified by Jörg Mensmann 15.02.1999 (added "#ifndef WIN32"s)

==============================================================================*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <mikmod_internals.h>

void MikMod_RegisterAllDrivers(void)
{
	/* Register network drivers */
#ifdef DRV_AF
	MikMod_RegisterDriver(&drv_AF);
#endif
#ifdef DRV_ESD
	MikMod_RegisterDriver(&drv_esd);
#endif

	/* Register hardware drivers - hardware mixing */
#ifdef DRV_ULTRA
	MikMod_RegisterDriver(&drv_ultra);
#endif

	/* Register hardware drivers - software mixing */
#ifdef DRV_AIX
	MikMod_RegisterDriver(&drv_aix);
#endif
#ifdef DRV_ALSA
	MikMod_RegisterDriver(&drv_alsa);
#endif
#ifdef DRV_HP
	MikMod_RegisterDriver(&drv_hp);
#endif
#ifdef DRV_OSS
	MikMod_RegisterDriver(&drv_oss);
#endif
#ifdef DRV_SGI
	MikMod_RegisterDriver(&drv_sgi);
#endif
#ifdef DRV_SUN
	MikMod_RegisterDriver(&drv_sun);
#endif
#ifdef DRV_DART
	MikMod_RegisterDriver(&drv_dart);
#endif
#ifdef DRV_OS2
	MikMod_RegisterDriver(&drv_os2s);
	MikMod_RegisterDriver(&drv_os2l);
#endif
#ifdef WIN32
	MikMod_RegisterDriver(&drv_ds_raw);
#endif

#ifndef WIN32
	/* Register disk writers */
	MikMod_RegisterDriver(&drv_raw);
	MikMod_RegisterDriver(&drv_wav);

	/* Register other drivers */
	MikMod_RegisterDriver(&drv_stdout);
#endif
	MikMod_RegisterDriver(&drv_nos);
}
