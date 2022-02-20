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
 *  File:     $RCSfile: CRC.pas,v $
 *  Revision: $Revision: 1.1.1.1 $
 *  Version : $Id: CRC.pas,v 1.1.1.1 2002/04/21 12:57:16 fobmagog Exp $
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
unit mp3_CRC;

interface

const
  POLYNOMIAL: Word = $8005;

type
  TCRC16 = class
  private
    FCRC: Word;

  public
    constructor Create;
    procedure AddBits(BitString: Cardinal; Length: Cardinal);
    function Checksum: Word;
  end;

implementation

{ TCRC16 }

//==============================================================================
// TCRC16.AddBits
//
// feed a bitstring to the crc calculation (0 < length <= 32)
//
//==============================================================================
procedure TCRC16.AddBits(BitString, Length: Cardinal);
var BitMask: Cardinal;
begin
  BitMask := 1 shl (Length - 1);
  repeat
    if (FCRC and $8000 = 0) xor (BitString and BitMask = 0) then
    begin
      FCRC := FCRC shl 1;
      FCRC := FCRC xor POLYNOMIAL;
    end
    else
      FCRC := FCRC shl 1;

    BitMask := BitMask shr 1;
  until (BitMask = 0);
end;

//==============================================================================
// TCRC16.Checksum
//
// return the calculated checksum and erase it for next calls to add_bits()
//
//==============================================================================
function TCRC16.Checksum: Word;
begin
  result := FCRC;
  FCRC := $FFFF;
end;

//==============================================================================
//
// TCRC16.Create
//
//==============================================================================
constructor TCRC16.Create;
begin
  FCRC := $FFFF;
end;

end.
