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
 *  File:     $RCSfile: ScaleFac.pas,v $
 *  Revision: $Revision: 1.1.1.1 $
 *  Version : $Id: ScaleFac.pas,v 1.1.1.1 2002/04/21 12:57:22 fobmagog Exp $
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
unit mp3_ScaleFac;

interface

const
  // Scalefactors for layer I and II, Annex 3-B.1 in ISO/IEC DIS 11172:
  ScaleFactors: array[0..63] of Single = (
    2.00000000000000, 1.58740105196820, 1.25992104989487, 1.00000000000000,
    0.79370052598410, 0.62996052494744, 0.50000000000000, 0.39685026299205,
    0.31498026247372, 0.25000000000000, 0.19842513149602, 0.15749013123686,
    0.12500000000000, 0.09921256574801, 0.07874506561843, 0.06250000000000,
    0.04960628287401, 0.03937253280921, 0.03125000000000, 0.02480314143700,
    0.01968626640461, 0.01562500000000, 0.01240157071850, 0.00984313320230,
    0.00781250000000, 0.00620078535925, 0.00492156660115, 0.00390625000000,
    0.00310039267963, 0.00246078330058, 0.00195312500000, 0.00155019633981,
    0.00123039165029, 0.00097656250000, 0.00077509816991, 0.00061519582514,
    0.00048828125000, 0.00038754908495, 0.00030759791257, 0.00024414062500,
    0.00019377454248, 0.00015379895629, 0.00012207031250, 0.00009688727124,
    0.00007689947814, 0.00006103515625, 0.00004844363562, 0.00003844973907,
    0.00003051757813, 0.00002422181781, 0.00001922486954, 0.00001525878906,
    0.00001211090890, 0.00000961243477, 0.00000762939453, 0.00000605545445,
    0.00000480621738, 0.00000381469727, 0.00000302772723, 0.00000240310869,
    0.00000190734863, 0.00000151386361, 0.00000120155435, 0.00000000000000 (* illegal scalefactor *));

implementation

end.
