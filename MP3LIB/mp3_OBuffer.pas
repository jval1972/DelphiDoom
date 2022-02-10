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
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

(*
 *  File:     $RCSfile: OBuffer.pas,v $
 *  Revision: $Revision: 1.1.1.1 $
 *  Version : $Id: OBuffer.pas,v 1.1.1.1 2002/04/21 12:57:22 fobmagog Exp $
 *  Author:   $Author: fobmagog $
 *  Homepage: http://delphimpeg.sourceforge.net/
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *)
{$DEFINE SEEK_STOP}
unit mp3_OBuffer;

interface

const
  OBUFFERSIZE  = 2 * 1152;  // max. 2 * 1152 samples per frame
  MAX_CHANNELS = 2;         // max. number of channels

type
  // Abstract base class for audio output classes:
  TOBuffer = class
  public
    // this function takes a 16 Bit PCM sample
    procedure Append(Channel: Cardinal; Value: SmallInt); virtual; abstract;

    // this function should write the samples to the filedescriptor
    // or directly to the audio hardware
    procedure WriteBuffer; virtual; abstract;

{$IFDEF SEEK_STOP}
    // Clears all data in the buffer (for seeking)
    procedure ClearBuffer; virtual; abstract;

    // Notify the buffer that the user has stopped the stream
    procedure SetStopFlag; virtual; abstract;
{$ENDIF}
  end;

//==============================================================================
//
// TOBuffer_Wave_Failure
//
//==============================================================================
procedure TOBuffer_Wave_Failure;

implementation

uses
  i_system;

//==============================================================================
//
// TOBuffer_Wave_Failure
//
//==============================================================================
procedure TOBuffer_Wave_Failure;
begin
  I_Error('TOBuffer_Wave(): Output device failure');
end;

end.
