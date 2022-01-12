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
 *  File:     $RCSfile: L3Type.pas,v $
 *  Revision: $Revision: 1.1.1.1 $
 *  Version : $Id: L3Type.pas,v 1.1.1.1 2002/04/21 12:57:21 fobmagog Exp $
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
unit mp3_L3Type;

interface

type
  PGrInfo = ^TGrInfo;
  TGrInfo = record
    part2_3_length: Cardinal;
    big_values: Cardinal;
    global_gain: Cardinal;
    scalefac_compress: Cardinal;
    window_switching_flag: Cardinal;
    block_type: Cardinal;
    mixed_block_flag: Cardinal;
    table_select: array[0..2] of Cardinal;
    subblock_gain: array[0..2] of Cardinal;
    region0_count: Cardinal;
    region1_count: Cardinal;
    preflag: Cardinal;
    scalefac_scale: Cardinal;
    count1table_select: Cardinal;
  end;

  PIIISideInfo = ^TIIISideInfo;
  TIIISideInfo = record
    main_data_begin: Integer;
    private_bits: Cardinal;
    ch: array[0..1] of record
      scfsi: array[0..3] of Cardinal;
      gr: array[0..1] of TGrInfo;
    end;
  end;

  PIIIScaleFac = ^TIIIScaleFac;
  TIIIScaleFac = array[0..1] of record
    l: array[0..22] of Integer;
    s: array[0..2, 0..12] of integer;  // [window][cb]
  end;

implementation

end.
