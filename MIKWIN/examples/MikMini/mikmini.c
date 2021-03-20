/*  MikMod example program
	(c) 1999 Jörg Mensmann

	This program is free software; you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation; either version 2 of the License, or
	(at your option) any later version.
 
	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
 
	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

	Last edited 15.02.99
*/

#include <conio.h>
#include <stdio.h>
#include <windows.h>

#include "..\..\include\mikmod.h"
#include "..\..\include\mikwin.h"

int main(int argc,char *argv[])
{
    MODULE *module;

	if (argc < 2) {
       fprintf(stderr,"Usage: mikmin.exe <filename>");
		return 1;
	}

	/* register all the drivers */
	MikMod_RegisterAllDrivers();

	/* register all the module loaders */
	MikMod_RegisterAllLoaders();

	/* initialize the library */
	set_md_mixfreq(44100);
	set_md_mode(DMODE_16BITS | DMODE_STEREO | DMODE_SOFT_MUSIC | DMODE_INTERP);
    if (MikMod_Init()) {
        fprintf(stderr,"Could not initialize sound, reason: %s\n",
                MikMod_strerror(get_MikMod_errno()));
        return 1;
    }

    /* load module */
    module = Player_Load(argv[1], 32, 0);
    if (module) {
        /* start module */
        Player_Start_threaded(module);
		printf("Playing... (Press ESC to quit)\n");
		set_md_volume(128);      /* Global sound volume (0-128) */
        while (1) {
			/* the file is played in the background */           
			char c;
			c = kbhit() ? getch() : 0;
			if (c==0x1b){
				break;
			}			
        }

        Player_Stop_threaded();
        Player_Free(module);
    } else
        fprintf(stderr,"Could not load module, reason: %s\n",
                MikMod_strerror(get_MikMod_errno()));

	/* give up */
	MikMod_Exit();

	return 0;
}