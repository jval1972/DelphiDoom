//------------------------------------------------------------------------------
//
//  DelphiDoom is a source port of the game Doom and it is
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2022 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$IFNDEF FPC}
{$DEFINE DELPHI}
{$ENDIF}


{$IFDEF DELPHI}
{$B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$ALIGN 8}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00200000}
{$IMAGEBASE $01000000}
{$APPTYPE GUI}
{$ENDIF}

{$IFDEF FPC}
{$A+,B-,C+,D+,H+,I+,J+,M-,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00200000}
{$IMAGEBASE $00100000}
{$APPTYPE GUI}
{$ENDIF}

{$IFDEF OPTIMIZE_FOR_SIZE}
{$DEFINE NO_INLINE_LOOPS}
{$ENDIF}

{$IFDEF OPTIMIZE_FOR_SPEED}
{$UNDEF NO_INLINE_LOOPS}
{$ENDIF}

{$IFDEF DOOM}
{$DEFINE DOOM_OR_HERETIC}
{$DEFINE DOOM_OR_HEXEN}
{$DEFINE DOOM_OR_STRIFE}
{$DEFINE HERETIC_OR_DOOM}
{$DEFINE HEXEN_OR_DOOM}
{$DEFINE STRIFE_OR_DOOM}
{$ENDIF}

{$IFDEF HERETIC}
{$DEFINE HERETIC_OR_DOOM}
{$DEFINE HERETIC_OR_HEXEN}
{$DEFINE HERETIC_OR_STRIFE}
{$DEFINE DOOM_OR_HERETIC}
{$DEFINE HEXEN_OR_HERETIC}
{$DEFINE STRIFE_OR_HERETIC}
{$ENDIF}

{$IFDEF HEXEN}
{$DEFINE HEXEN_OR_DOOM}
{$DEFINE HEXEN_OR_HERETIC}
{$DEFINE HEXEN_OR_STRIFE}
{$DEFINE DOOM_OR_HEXEN}
{$DEFINE HERETIC_OR_HEXEN}
{$DEFINE STRIFE_OR_HEXEN}
{$ENDIF}

{$IFDEF STRIFE}
{$DEFINE STRIFE_OR_DOOM}
{$DEFINE STRIFE_OR_HERETIC}
{$DEFINE STRIFE_OR_HEXEN}
{$DEFINE DOOM_OR_STRIFE}
{$DEFINE HERETIC_OR_STRIFE}
{$DEFINE HEXEN_OR_STRIFE}
{$ENDIF}

{$IFDEF DEBUG}
{$O-}
{$ENDIF}

