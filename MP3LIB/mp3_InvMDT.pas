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
 *  File:     $RCSfile: InvMDT.pas,v $
 *  Revision: $Revision: 1.1.1.1 $
 *  Version : $Id: InvMDT.pas,v 1.1.1.1 2002/04/21 12:57:17 fobmagog Exp $
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
unit mp3_InvMDT;

interface
uses
  mp3_Shared;

//==============================================================================
//
// InvMDCT
//
//==============================================================================
procedure InvMDCT(input: PSingleArray; output: PSingleArray; block_type: Integer);

implementation

const
  PI12     =  0.261799387;
  PI36     =  0.087266462;
  COSPI3   =  0.500000000;
  COSPI6   =  0.866025403;
  DCTODD1  =  0.984807753;
  DCTODD2  = -0.342020143;
  DCTODD3  = -0.642787609;
  DCTEVEN1 =  0.939692620;
  DCTEVEN2 = -0.173648177;
  DCTEVEN3 = -0.766044443;

(*
This uses Byeong Gi Lee's Fast Cosine Transform algorithm to
decompose the 36 point and 12 point IDCT's into 9 point and 3
point IDCT's, respectively. Then the 9 point IDCT is computed
by a modified version of Mikko Tommila's IDCT algorithm, based on
the WFTA. See his comments before the first 9 point IDCT. The 3
point IDCT is already efficient to implement. -- Jeff Tsay. *)

const
  Win: array[0..3, 0..35] of Single = (
    (-1.6141214951E-02, -5.3603178919E-02, -1.0070713296E-01, -1.6280817573E-01,
     -4.9999999679E-01, -3.8388735032E-01, -6.2061144372E-01, -1.1659756083E+00,
     -3.8720752656E+00, -4.2256286556E+00, -1.5195289984E+00, -9.7416483388E-01,
     -7.3744074053E-01, -1.2071067773E+00, -5.1636156596E-01, -4.5426052317E-01,
     -4.0715656898E-01, -3.6969460527E-01, -3.3876269197E-01, -3.1242222492E-01,
     -2.8939587111E-01, -2.6880081906E-01, -5.0000000266E-01, -2.3251417468E-01,
     -2.1596714708E-01, -2.0004979098E-01, -1.8449493497E-01, -1.6905846094E-01,
     -1.5350360518E-01, -1.3758624925E-01, -1.2103922149E-01, -2.0710679058E-01,
     -8.4752577594E-02, -6.4157525656E-02, -4.1131172614E-02, -1.4790705759E-02),

    (-1.6141214951E-02, -5.3603178919E-02, -1.0070713296E-01, -1.6280817573E-01,
     -4.9999999679E-01, -3.8388735032E-01, -6.2061144372E-01, -1.1659756083E+00,
     -3.8720752656E+00, -4.2256286556E+00, -1.5195289984E+00, -9.7416483388E-01,
     -7.3744074053E-01, -1.2071067773E+00, -5.1636156596E-01, -4.5426052317E-01,
     -4.0715656898E-01, -3.6969460527E-01, -3.3908542600E-01, -3.1511810350E-01,
     -2.9642226150E-01, -2.8184548650E-01, -5.4119610000E-01, -2.6213228100E-01,
     -2.5387916537E-01, -2.3296291359E-01, -1.9852728987E-01, -1.5233534808E-01,
     -9.6496400054E-02, -3.3423828516E-02,  0.0000000000E+00,  0.0000000000E+00,
      0.0000000000E+00,  0.0000000000E+00,  0.0000000000E+00,  0.0000000000E+00),

    (-4.8300800645E-02, -1.5715656932E-01, -2.8325045177E-01, -4.2953747763E-01,
     -1.2071067795E+00, -8.2426483178E-01, -1.1451749106E+00, -1.7695290101E+00,
     -4.5470225061E+00, -3.4890531002E+00, -7.3296292804E-01, -1.5076514758E-01,
      0.0000000000E+00,  0.0000000000E+00,  0.0000000000E+00,  0.0000000000E+00,
      0.0000000000E+00,  0.0000000000E+00,  0.0000000000E+00,  0.0000000000E+00,
      0.0000000000E+00,  0.0000000000E+00,  0.0000000000E+00,  0.0000000000E+00,
      0.0000000000E+00,  0.0000000000E+00,  0.0000000000E+00,  0.0000000000E+00,
      0.0000000000E+00,  0.0000000000E+00,  0.0000000000E+00,  0.0000000000E+00,
      0.0000000000E+00,  0.0000000000E+00,  0.0000000000E+00,  0.0000000000E+00),

    ( 0.0000000000E+00,  0.0000000000E+00,  0.0000000000E+00,  0.0000000000E+00,
      0.0000000000E+00,  0.0000000000E+00, -1.5076513660E-01, -7.3296291107E-01,
     -3.4890530566E+00, -4.5470224727E+00, -1.7695290031E+00, -1.1451749092E+00,
     -8.3137738100E-01, -1.3065629650E+00, -5.4142014250E-01, -4.6528974900E-01,
     -4.1066990750E-01, -3.7004680800E-01, -3.3876269197E-01, -3.1242222492E-01,
     -2.8939587111E-01, -2.6880081906E-01, -5.0000000266E-01, -2.3251417468E-01,
     -2.1596714708E-01, -2.0004979098E-01, -1.8449493497E-01, -1.6905846094E-01,
     -1.5350360518E-01, -1.3758624925E-01, -1.2103922149E-01, -2.0710679058E-01,
     -8.4752577594E-02, -6.4157525656E-02, -4.1131172614E-02, -1.4790705759E-02));

//==============================================================================
//
// InvMDCT
//
//==============================================================================
procedure InvMDCT(input: PSingleArray; output: PSingleArray; block_type: Integer);
var tmp: array[0..17] of Single;
    win_bt: PSingleArray;
    i, p: integer;
    six_i: Integer;
    pp1, pp2, sum, save: Single;
    tmp0, tmp1, tmp2, tmp3, tmp4, tmp0_, tmp1_, tmp2_, tmp3_: Single;
    tmp0o, tmp1o, tmp2o, tmp3o, tmp4o, tmp0_o, tmp1_o, tmp2_o, tmp3_o: Single;
    i0, i0p12, i6_, e, o: Single;
begin
  if (block_type = 2) then
  begin
    p := 0;
    while (p < 36) do
    begin
      output[p]   := 0.0;
      output[p+1] := 0.0;
      output[p+2] := 0.0;
      output[p+3] := 0.0;
      output[p+4] := 0.0;
      output[p+5] := 0.0;
      output[p+6] := 0.0;
      output[p+7] := 0.0;
      output[p+8] := 0.0;
      inc(p, 9);
    end;

    six_i := 0;
    for i := 0 to 2 do
    begin
      // 12 point IMDCT
      // Begin 12 point IDCT
      // Input aliasing for 12 pt IDCT
      input[15+i] := input[15+i] + input[12+i];
      input[12+i] := input[12+i] + input[9+i];
      input[9+i]  := input[9+i]  + input[6+i];
      input[6+i]  := input[6+i]  + input[3+i];
      input[3+i]  := input[3+i]  + input[0+i];

      // Input aliasing on odd indices (for 6 point IDCT)
      input[15+i] := input[15+i] + input[9+i];
      input[9+i]  := input[9+i]  + input[3+i];

      // 3 point IDCT on even indices
      pp2 := input[12+i] * 0.500000000;
      pp1 := input[ 6+i] * 0.866025403;
      sum := input[0+i] + pp2;
      tmp[1] := input[0+i] - input[12+i];
      tmp[0] := sum + pp1;
      tmp[2] := sum - pp1;
      // End 3 point IDCT on even indices

      // 3 point IDCT on odd indices (for 6 point IDCT)
      pp2 := input[15+i] * 0.500000000;
      pp1 := input[ 9+i] * 0.866025403;
      sum := input[ 3+i] + pp2;
      tmp[4] := input[3+i] - input[15+i];
      tmp[5] := sum + pp1;
      tmp[3] := sum - pp1;
      // End 3 point IDCT on odd indices

      // Twiddle factors on odd indices (for 6 point IDCT)
      tmp[3] := tmp[3] * 1.931851653;
      tmp[4] := tmp[4] * 0.707106781;
      tmp[5] := tmp[5] * 0.517638090;

      // Output butterflies on 2 3 point IDCT's (for 6 point IDCT)
      save := tmp[0];
      tmp[0] := tmp[0] + tmp[5];
      tmp[5] := save - tmp[5];
      save := tmp[1];
      tmp[1] := tmp[1] + tmp[4];
      tmp[4] := save - tmp[4];
      save := tmp[2];
      tmp[2] := tmp[2] + tmp[3];
      tmp[3] := save - tmp[3];
      // End 6 point IDCT

      // Twiddle factors on indices (for 12 point IDCT)
      tmp[0] := tmp[0] * 0.504314480;
      tmp[1] := tmp[1] * 0.541196100;
      tmp[2] := tmp[2] * 0.630236207;
      tmp[3] := tmp[3] * 0.821339815;
      tmp[4] := tmp[4] * 1.306562965;
      tmp[5] := tmp[5] * 3.830648788;
      // End 12 point IDCT

      // Shift to 12 point modified IDCT, multiply by window type 2
      tmp[8]  := -tmp[0] * 0.793353340;
      tmp[9]  := -tmp[0] * 0.608761429;
      tmp[7]  := -tmp[1] * 0.923879532;
      tmp[10] := -tmp[1] * 0.382683432;
      tmp[6]  := -tmp[2] * 0.991444861;
      tmp[11] := -tmp[2] * 0.130526192;

      tmp[0]  :=  tmp[3];
      tmp[1]  :=  tmp[4] * 0.382683432;
      tmp[2]  :=  tmp[5] * 0.608761429;

      tmp[3]  := -tmp[5] * 0.793353340;
      tmp[4]  := -tmp[4] * 0.923879532;
      tmp[5]  := -tmp[0] * 0.991444861;

      tmp[0]  :=  tmp[0] * 0.130526192;

      output[six_i + 6]  := output[six_i + 6]  + tmp[0];
      output[six_i + 7]  := output[six_i + 7]  + tmp[1];
      output[six_i + 8]  := output[six_i + 8]  + tmp[2];
      output[six_i + 9]  := output[six_i + 9]  + tmp[3];
      output[six_i + 10] := output[six_i + 10] + tmp[4];
      output[six_i + 11] := output[six_i + 11] + tmp[5];
      output[six_i + 12] := output[six_i + 12] + tmp[6];
      output[six_i + 13] := output[six_i + 13] + tmp[7];
      output[six_i + 14] := output[six_i + 14] + tmp[8];
      output[six_i + 15] := output[six_i + 15] + tmp[9];
      output[six_i + 16] := output[six_i + 16] + tmp[10];
      output[six_i + 17] := output[six_i + 17] + tmp[11];

      inc(six_i, 6);
    end;
  end
  else
  begin
    // 36 point IDCT
    // input aliasing for 36 point IDCT
    input[17] := input[17] + input[16];
    input[16] := input[16] + input[15];
    input[15] := input[15] + input[14];
    input[14] := input[14] + input[13];
    input[13] := input[13] + input[12];
    input[12] := input[12] + input[11];
    input[11] := input[11] + input[10];
    input[10] := input[10] + input[9];
    input[9]  := input[9]  + input[8];
    input[8]  := input[8]  + input[7];
    input[7]  := input[7]  + input[6];
    input[6]  := input[6]  + input[5];
    input[5]  := input[5]  + input[4];
    input[4]  := input[4]  + input[3];
    input[3]  := input[3]  + input[2];
    input[2]  := input[2]  + input[1];
    input[1]  := input[1]  + input[0];
    // 18 point IDCT for odd indices

    // input aliasing for 18 point IDCT
    input[17] := input[17] + input[15];
    input[15] := input[15] + input[13];
    input[13] := input[13] + input[11];
    input[11] := input[11] + input[9];
    input[9]  := input[9]  + input[7];
    input[7]  := input[7]  + input[5];
    input[5]  := input[5]  + input[3];
    input[3]  := input[3]  + input[1];

// Fast 9 Point Inverse Discrete Cosine Transform
//
// By  Francois-Raymond Boyer
//         mailto:boyerf@iro.umontreal.ca
//         http://www.iro.umontreal.ca/~boyerf
//
// The code has been optimized for Intel processors
//  (takes a lot of time to convert float to and from iternal FPU representation)
//
// It is a simple "factorization" of the IDCT matrix.

    // 9 point IDCT on even indices
    // 5 points on odd indices (not realy an IDCT)
    i0 := input[0] + input[0];
    i0p12 := i0 + input[12];

    tmp0 := i0p12 + input[4] * 1.8793852415718  + input[8] * 1.532088886238 + input[16] * 0.34729635533386;
    tmp1 := i0    + input[4]                    - input[8] - input[12] - input[12] - input[16];
    tmp2 := i0p12 - input[4] * 0.34729635533386 - input[8] * 1.8793852415718  + input[16] * 1.532088886238;
    tmp3 := i0p12 - input[4] * 1.532088886238   + input[8] * 0.34729635533386 - input[16] * 1.8793852415718;
    tmp4 := input[0] - input[4]                 + input[8] - input[12]          + input[16];

    // 4 points on even indices
    i6_ := input[6] * 1.732050808;    // Sqrt[3]

    tmp0_ := input[2] * 1.9696155060244  + i6_ + input[10] * 1.2855752193731  + input[14] * 0.68404028665134;
    tmp1_ := (input[2]                        - input[10]                   - input[14]) * 1.732050808;
    tmp2_ := input[2] * 1.2855752193731  - i6_ - input[10] * 0.68404028665134 + input[14] * 1.9696155060244;
    tmp3_ := input[2] * 0.68404028665134 - i6_ + input[10] * 1.9696155060244  - input[14] * 1.2855752193731;

    // 9 point IDCT on odd indices
    // 5 points on odd indices (not realy an IDCT)
    i0 := input[0+1] + input[0+1];
    i0p12 := i0 + input[12+1];

    tmp0o := i0p12   + input[4+1] * 1.8793852415718  + input[8+1]*1.532088886238 + input[16+1] * 0.34729635533386;
    tmp1o := i0      + input[4+1]                   - input[8+1] - input[12+1] - input[12+1] - input[16+1];
    tmp2o := i0p12   - input[4+1] * 0.34729635533386 - input[8+1] * 1.8793852415718 + input[16+1] * 1.532088886238;
    tmp3o := i0p12   - input[4+1] * 1.532088886238   + input[8+1] * 0.34729635533386 - input[16+1] * 1.8793852415718;
    tmp4o := (input[0+1] - input[4+1] + input[8+1] - input[12+1] + input[16+1]) * 0.707106781;  // Twiddled

    // 4 points on even indices
    i6_ := input[6+1] * 1.732050808;  // Sqrt[3]

    tmp0_o := input[2+1] * 1.9696155060244 + i6_ + input[10+1] * 1.2855752193731  + input[14+1] * 0.68404028665134;
    tmp1_o := (input[2+1]                        - input[10+1]                   - input[14+1]) * 1.732050808;
    tmp2_o := input[2+1] * 1.2855752193731 - i6_ - input[10+1] * 0.68404028665134 + input[14+1] * 1.9696155060244;
    tmp3_o := input[2+1] * 0.68404028665134 - i6_ + input[10+1] * 1.9696155060244  - input[14+1] * 1.2855752193731;

    // Twiddle factors on odd indices
    // and
    // Butterflies on 9 point IDCT's
    // and
    // twiddle factors for 36 point IDCT

    e := tmp0 + tmp0_;
    o := (tmp0o + tmp0_o) * 0.501909918;
    tmp[0] := e + o;
    tmp[17] := e - o;
    e := tmp1 + tmp1_;
    o := (tmp1o + tmp1_o) * 0.517638090;
    tmp[1] := e + o;
    tmp[16] := e - o;
    e := tmp2 + tmp2_;
    o := (tmp2o + tmp2_o) * 0.551688959;
    tmp[2] := e + o;
    tmp[15] := e - o;
    e := tmp3 + tmp3_;
    o := (tmp3o + tmp3_o) * 0.610387294;
    tmp[3] := e + o;
    tmp[14] := e - o;
    tmp[4] := tmp4 + tmp4o;
    tmp[13] := tmp4 - tmp4o;
    e := tmp3 - tmp3_;
    o := (tmp3o - tmp3_o) * 0.871723397;
    tmp[5] := e + o;
    tmp[12] := e - o;
    e := tmp2 - tmp2_;
    o := (tmp2o - tmp2_o) * 1.183100792;
    tmp[6] := e + o;
    tmp[11] := e - o;
    e := tmp1 - tmp1_;
    o := (tmp1o - tmp1_o) * 1.931851653;
    tmp[7] := e + o;
    tmp[10] := e - o;
    e := tmp0 - tmp0_;
    o := (tmp0o - tmp0_o) * 5.736856623;
    tmp[8] := e + o;
    tmp[9] := e - o;

    // end 36 point IDCT */

    // shift to modified IDCT
    win_bt := @win[block_type];

    output[0]  := -tmp[9]  * win_bt[0];
    output[1]  := -tmp[10] * win_bt[1];
    output[2]  := -tmp[11] * win_bt[2];
    output[3]  := -tmp[12] * win_bt[3];
    output[4]  := -tmp[13] * win_bt[4];
    output[5]  := -tmp[14] * win_bt[5];
    output[6]  := -tmp[15] * win_bt[6];
    output[7]  := -tmp[16] * win_bt[7];
    output[8]  := -tmp[17] * win_bt[8];

    output[9]  :=  tmp[17] * win_bt[9];
    output[10] :=  tmp[16] * win_bt[10];
    output[11] :=  tmp[15] * win_bt[11];
    output[12] :=  tmp[14] * win_bt[12];
    output[13] :=  tmp[13] * win_bt[13];
    output[14] :=  tmp[12] * win_bt[14];
    output[15] :=  tmp[11] * win_bt[15];
    output[16] :=  tmp[10] * win_bt[16];
    output[17] :=  tmp[9]  * win_bt[17];
    output[18] :=  tmp[8]  * win_bt[18];
    output[19] :=  tmp[7]  * win_bt[19];
    output[20] :=  tmp[6]  * win_bt[20];
    output[21] :=  tmp[5]  * win_bt[21];
    output[22] :=  tmp[4]  * win_bt[22];
    output[23] :=  tmp[3]  * win_bt[23];
    output[24] :=  tmp[2]  * win_bt[24];
    output[25] :=  tmp[1]  * win_bt[25];
    output[26] :=  tmp[0]  * win_bt[26];

    output[27] :=  tmp[0]  * win_bt[27];
    output[28] :=  tmp[1]  * win_bt[28];
    output[29] :=  tmp[2]  * win_bt[29];
    output[30] :=  tmp[3]  * win_bt[30];
    output[31] :=  tmp[4]  * win_bt[31];
    output[32] :=  tmp[5]  * win_bt[32];
    output[33] :=  tmp[6]  * win_bt[33];
    output[34] :=  tmp[7]  * win_bt[34];
    output[35] :=  tmp[8]  * win_bt[35];
  end;
end;

end.

