//------------------------------------------------------------------------------
//
//  DelphiDoom is a source port of the game Doom and it is
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2019 by Jim Valavanis
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
 *  File:     $RCSfile: Layer3.pas,v $
 *  Revision: $Revision: 1.1.1.1 $
 *  Version : $Id: Layer3.pas,v 1.1.1.1 2002/04/21 12:57:21 fobmagog Exp $
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
unit mp3_Layer3;

interface

uses
  mp3_Shared, mp3_BitReserve, mp3_BitStream, mp3_Header, mp3_OBuffer, mp3_SynthFilter, mp3_L3Type;

const
  SSLIMIT = 18;
  SBLIMIT = 32;

  // Size of the table of whole numbers raised to 4/3 power.
  // This may be adjusted for performance without any problems.
  POW_TABLE_LIMIT = 512;

type
  PSArray = ^TSArray;
  TSArray = array[0..SBLIMIT - 1, 0..SSLIMIT - 1] of Single;

  TLayerIII_Decoder = class
  private
    FRO: array[0..1] of TSArray;
    FLR: array[0..1] of TSArray;
    FIS_1D: array[0..(SBLIMIT * SSLIMIT) - 1] of Integer;
    FOut_1D: array[0..(SBLIMIT * SSLIMIT) - 1] of Single;
    FPrevBlock: array[0..1, 0..(SBLIMIT * SSLIMIT) - 1] of Single;
    FK: array[0..1, 0..(SBLIMIT * SSLIMIT) - 1] of Single;
    FNonZero: array[0..1] of Integer;

    FStream: TBitStream;
    FHeader: THeader;
    FFilter1, FFilter2: TSynthesisFilter;
    FBuffer: TOBuffer;
    FWhichChannels: TChannels;
    FBR: TBitReserve;
    FSI: PIIISideInfo;
    FScaleFac: TIIIScaleFac;

    FMaxGr: Cardinal;
    FFrameStart: Integer;
    FPart2Start: Cardinal;
    FChannels: Cardinal;
    FFirstChannel: Cardinal;
    FLastChannel: Cardinal;
    FSFreq: Cardinal;

    function GetSideInfo: Boolean;
    procedure GetScaleFactors(ch: Cardinal; gr: Cardinal);
    procedure GetLSFScaleData(ch: Cardinal; gr: Cardinal);
    procedure GetLSFScaleFactors(ch: Cardinal; gr: Cardinal);
    procedure HuffmanDecode(ch: Cardinal; gr: Cardinal);
    procedure IStereoKValues(IsPos: Cardinal; IOType: Cardinal; i: Cardinal);
    procedure DequantizeSample(var xr: TSArray; ch: Cardinal; gr: Cardinal);
    procedure Reorder(xr: PSArray; ch: Cardinal; gr: Cardinal);
    procedure Stereo(gr: Cardinal);
    procedure Antialias(ch: Cardinal; gr: Cardinal);
    procedure Hybrid(ch: Cardinal; gr: Cardinal);
    procedure DoDownmix;
    procedure CreateBitReserve;

  public
    constructor Create(Stream: TBitStream; Header: THeader; FilterA, FilterB: TSynthesisFilter;
                       Buffer: TOBuffer; Which_Ch: TChannels);
    destructor Destroy; override;

    // Notify decoder that a seek is being made
    procedure SeekNotify;

    // Decode one frame, filling the buffer with the output samples
    procedure Decode;
  end;

implementation

uses
  d_delphi,
  i_system,
  mp3_Huffman,
  Math,
  mp3_InvMDT,
  mp3_L3Tables;

{ TLayerIII_Decoder }

//==============================================================================
//
// TLayerIII_Decoder.Antialias
//
//==============================================================================
procedure TLayerIII_Decoder.Antialias(ch, gr: Cardinal);
var ss, sb18, sb18lim: Cardinal;
    gr_info: PGRInfo;
    bu, bd: Single;
    src_idx1, src_idx2: Integer;
begin
  gr_info := @FSI.ch[ch].gr[gr];

  // 31 alias-reduction operations between each pair of sub-bands
  // with 8 butterflies between each pair
  if (gr_info.window_switching_flag <> 0) and (gr_info.block_type = 2) and (gr_info.mixed_block_flag = 0) then
    exit;

  if (gr_info.window_switching_flag <> 0) and (gr_info.mixed_block_flag <> 0) and (gr_info.block_type = 2) then
    sb18lim := 18
  else
    sb18lim := 558;

  sb18 := 0;
  while sb18 < sb18lim do
  begin
    for ss := 0 to 7 do
    begin
      src_idx1 := sb18 + 17 - ss;
      src_idx2 := sb18 + 18 + ss;
      bu := FOut_1D[src_idx1];
      bd := FOut_1D[src_idx2];
      FOut_1D[src_idx1] := (bu * cs[ss]) - (bd * ca[ss]);
      FOut_1D[src_idx2] := (bd * cs[ss]) + (bu * ca[ss]);
    end;

    inc(sb18, 18);
  end;
end;

//==============================================================================
//
// TLayerIII_Decoder.Create
//
//==============================================================================
constructor TLayerIII_Decoder.Create(Stream: TBitStream; Header: THeader;
  FilterA, FilterB: TSynthesisFilter; Buffer: TOBuffer;
  Which_Ch: TChannels);
begin
  FStream := Stream;
  FHeader := Header;
  FFilter1 := FilterA;
  FFilter2 := FilterB;
  FBuffer := Buffer;
  FWhichChannels := Which_Ch;

  FFrameStart := 0;
  if (FHeader.Mode = SingleChannel) then
    FChannels := 1
  else
    FChannels := 2;

  if (FHeader.Version = MPEG1) then
    FMaxGr := 2
  else
    FMaxGr := 1;

  FSFreq := Cardinal(FHeader.SampleFrequency);//FHeader.Frequency;
  if FHeader.Version = MPEG1 then
    FSFreq := FSFreq + 3;

  if FChannels = 2 then
  begin
    case FWhichChannels of
      Left,
      Downmix:
        begin
          FFirstChannel := 0;
          FLastChannel := 0;
        end;

      Right:
        begin
          FFirstChannel := 1;
          FLastChannel := 1;
        end;

      Both:
        begin
          FFirstChannel := 0;
          FLastChannel := 1;
        end;

      else
        begin
          FFirstChannel := 0;
          FLastChannel := 1;
        end;
    end;
  end
  else
  begin
    FFirstChannel := 0;
    FLastChannel := 0;
  end;

  FillChar(FPrevBlock, Sizeof(FPrevBlock), 0);
  FNonZero[0] := 576;
  FNonZero[1] := 576;

  CreateBitReserve;
  GetMem(FSI, Sizeof(TIIISideInfo));
end;

//==============================================================================
//
// TLayerIII_Decoder.CreateBitReserve
//
//==============================================================================
procedure TLayerIII_Decoder.CreateBitReserve;
var
  cnt: integer;
  done: boolean;
begin
  cnt := 0;
  done := True;
  repeat
    try
      FBR := TBitReserve.Create;
    except
      I_Sleep(100);
      inc(cnt);
      done := false;
    end;
  until done or (cnt > 32);
  if not done then
    I_Error('TLayerIII_Decoder.CreateBitReserve(): Can not create FBR');
end;

//==============================================================================
//
// TLayerIII_Decoder.Decode
//
//==============================================================================
procedure TLayerIII_Decoder.Decode;
var
  nSlots: Cardinal;
  flush_main, ch, ss, sb, sb18: Cardinal;
  main_data_end: Integer;
  bytes_to_discard: Integer;
  i, gr: Cardinal;
begin
  nSlots := FHeader.Slots;
  GetSideInfo;

  for i := 0 to nSlots - 1 do
    FBR.hputbuf(FStream.GetBits(8));

  main_data_end := FBR.hsstell shr 3;  // of previous frame

  flush_main := FBR.hsstell and 7;
  if flush_main <> 0 then
  begin
    FBR.hgetbits(8 - flush_main);
    inc(main_data_end);
  end;

  bytes_to_discard := FFrameStart - main_data_end - FSI.main_data_begin;
  inc(FFrameStart, nSlots);

  if bytes_to_discard < 0 then
    exit;

  if main_data_end > 4096 then
  begin
    dec(FFrameStart, 4096);
    FBR.rewindNbytes(4096);
  end;

  while bytes_to_discard > 0 do
  begin
    FBR.hgetbits(8);
    dec(bytes_to_discard);
  end;

  for gr := 0 to FMaxGr - 1 do
  begin
    for ch := 0 to FChannels - 1 do
    begin
      FPart2Start := FBR.hsstell;

      if FHeader.version = MPEG1 then
        GetScaleFactors(ch, gr)
      else  // MPEG-2 LSF
        GetLSFScaleFactors(ch, gr);

      HuffmanDecode(ch, gr);
      DequantizeSample(FRO[ch], ch, gr);
    end;

    Stereo(gr);

    if (FWhichChannels = Downmix) and (FChannels > 1) then
      DoDownmix;

    for ch := FFirstChannel to FLastChannel do
    begin
      Reorder(@FLR[ch], ch, gr);
      Antialias(ch, gr);
      Hybrid(ch, gr);

      sb18 := 18;
      while sb18 < 576 do
      begin  // Frequency inversion
        ss := 1;
        while ss < SSLIMIT do
        begin
          FOut_1D[sb18 + ss] := -FOut_1D[sb18 + ss];
          inc(ss, 2);
        end;

        inc(sb18, 36);
      end;

      if (ch = 0) or (FWhichChannels = Right) then
      begin
        for ss := 0 to SSLIMIT - 1 do
        begin  // Polyphase synthesis
          sb := 0;
          sb18 := 0;
          while sb18 < 576 do
          begin
            FFilter1.InputSample(FOut_1D[sb18 + ss], sb);
            inc(sb18, 18);
            inc(sb);
          end;

          FFilter1.CalculatePCMSamples(FBuffer);
        end;
      end
      else
      begin
        for ss := 0 to SSLIMIT - 1 do
        begin  // Polyphase synthesis
          sb := 0;
          sb18 := 0;
          while sb18 < 576 do
          begin
            FFilter2.InputSample(FOut_1D[sb18 + ss], sb);
            inc(sb18, 18);
            inc(sb);
          end;

          FFilter2.CalculatePCMSamples(FBuffer);
        end;
      end;
    end;
  end;

  FBuffer.WriteBuffer;
end;

//==============================================================================
//
// TLayerIII_Decoder.DequantizeSample
//
//==============================================================================
procedure TLayerIII_Decoder.DequantizeSample(var xr: TSArray; ch,
  gr: Cardinal);
var
  gr_info: PGRInfo;
  cb: Integer;
  j, next_cb_boundary, cb_begin, cb_width: Integer;
  index, t_index: integer;
  g_gain: Single;
  xr1d: PSingleArray;
  abv, idx: Cardinal;
begin
  gr_info := @FSI.ch[ch].gr[gr];
  cb := 0;
  index := 0;
  cb_begin := 0;
  cb_width := 0;
  xr1d := @xr[0, 0];

  // choose correct scalefactor band per block type, initalize boundary
  if (gr_info.window_switching_flag <> 0) and (gr_info.block_type = 2) then
  begin
    if gr_info.mixed_block_flag <> 0 then
      next_cb_boundary := sfBandIndex[FSFreq].l[1]  // LONG blocks: 0,1,3
    else
    begin
      cb_width := sfBandIndex[FSFreq].s[1];
      next_cb_boundary := (cb_width shl 2) - cb_width;
      cb_begin := 0;
    end;
  end
  else
    next_cb_boundary := sfBandIndex[FSFreq].l[1];  // LONG blocks: 0,1,3

  // Compute overall (global) scaling.
  g_gain := Power(2.0, (0.25 * (gr_info.global_gain - 210.0)));

  for j := 0 to FNonZero[ch] - 1 do
  begin
    if FIS_1D[j] = 0 then
      xr1d[j] := 0.0
    else
    begin
      abv := FIS_1D[j];
      if FIS_1D[j] > 0 then
        xr1d[j] := g_gain * t_43[abv]
      else
        xr1d[j] := -g_gain * t_43[-abv];
    end;
  end;

  // apply formula per block type
  for j := 0 to FNonZero[ch] - 1 do
  begin
    if index = next_cb_boundary then
    begin  // Adjust critical band boundary
      if (gr_info.window_switching_flag <> 0) and (gr_info.block_type = 2) then
      begin
        if gr_info.mixed_block_flag <> 0 then
        begin
          if index = sfBandIndex[FSFreq].l[8] then
          begin
            next_cb_boundary := sfBandIndex[FSFreq].s[4];
            next_cb_boundary := (next_cb_boundary shl 2) - next_cb_boundary;
            cb := 3;
            cb_width := sfBandIndex[FSFreq].s[4] - sfBandIndex[FSFreq].s[3];
            cb_begin := sfBandIndex[FSFreq].s[3];
            cb_begin := (cb_begin shl 2) - cb_begin;
          end
          else if index < sfBandIndex[FSFreq].l[8] then
          begin
            inc(cb);
            next_cb_boundary := sfBandIndex[FSFreq].l[cb + 1];
          end
          else
          begin
            inc(cb);
            next_cb_boundary := sfBandIndex[FSFreq].s[cb + 1];
            next_cb_boundary := (next_cb_boundary shl 2) - next_cb_boundary;
            cb_begin := sfBandIndex[FSFreq].s[cb];
            cb_width := sfBandIndex[FSFreq].s[cb + 1] - cb_begin;
            cb_begin := (cb_begin shl 2) - cb_begin;
          end;
        end
        else
        begin
          inc(cb);
          next_cb_boundary := sfBandIndex[FSFreq].s[cb + 1];
          next_cb_boundary := (next_cb_boundary shl 2) - next_cb_boundary;
          cb_begin := sfBandIndex[FSFreq].s[cb];
          cb_width := sfBandIndex[FSFreq].s[cb+1] - cb_begin;
          cb_begin := (cb_begin shl 2) - cb_begin;
        end;
      end
      else
      begin  // long blocks
        inc(cb);
        next_cb_boundary := sfBandIndex[FSFreq].l[cb+1];
      end;
    end;

    // Do long/short dependent scaling operations
    if (gr_info.window_switching_flag <> 0) and (((gr_info.block_type = 2) and (gr_info.mixed_block_flag = 0)) or
       ((gr_info.block_type = 2) and (gr_info.mixed_block_flag <> 0) and (j >= 36))) then
    begin
      t_index := (index - cb_begin) div cb_width;
(*            xr[sb][ss] *= pow(2.0, ((-2.0 * gr_info->subblock_gain[t_index])
                                    -(0.5 * (1.0 + gr_info->scalefac_scale)
                                      * scalefac[ch].s[t_index][cb]))); *)
      idx := FScaleFac[ch].s[t_index][cb] shl gr_info.scalefac_scale;
      idx := idx + (gr_info.subblock_gain[t_index] shl 2);
      xr1d[j] := xr1d[j] * two_to_negative_half_pow[idx];
    end
    else
    begin  // LONG block types 0,1,3 & 1st 2 subbands of switched blocks
(*        xr[sb][ss] *= pow(2.0, -0.5 * (1.0+gr_info->scalefac_scale)
                                * (scalefac[ch].l[cb]
                                + gr_info->preflag * pretab[cb])); *)
      idx := FScaleFac[ch].l[cb];
      if (gr_info.preflag <> 0) then
        idx := idx + pretab[cb];

      idx := idx shl gr_info.scalefac_scale;
      xr1d[j] := xr1d[j] * two_to_negative_half_pow[idx];
    end;
    inc(index);
  end;

  for j := FNonZero[ch] to 576 - 1 do
    xr1d[j] := 0.0;
end;

//==============================================================================
//
// TLayerIII_Decoder.Destroy
//
//==============================================================================
destructor TLayerIII_Decoder.Destroy;
begin
  FreeAndNil(FBR);
  FreeMem(FSI);

  inherited Destroy;
end;

//==============================================================================
//
// TLayerIII_Decoder.DoDownmix
//
//==============================================================================
procedure TLayerIII_Decoder.DoDownmix;
var
  ss, sb: Cardinal;
begin
  for sb := 0 to SBLIMIT - 1 do
  begin
    ss := 0;
    while ss < SSLIMIT do
    begin
      FLR[0][sb][ss]   := (FLR[0][sb][ss]   + FLR[1][sb][ss])   * 0.5;
      FLR[0][sb][ss+1] := (FLR[0][sb][ss+1] + FLR[1][sb][ss+1]) * 0.5;
      FLR[0][sb][ss+2] := (FLR[0][sb][ss+2] + FLR[1][sb][ss+2]) * 0.5;
      inc(ss, 3);
    end;
  end;
end;

const
  NrOfSFBBlock: array[0..5, 0..2, 0..3] of Cardinal = (
    (( 6, 5, 5, 5), ( 9, 9, 9, 9), ( 6, 9, 9, 9)),
    (( 6, 5, 7, 3), ( 9, 9,12, 6), ( 6, 9,12, 6)),
    ((11,10, 0, 0), (18,18, 0, 0), (15,18, 0, 0)),
    (( 7, 7, 7, 0), (12,12,12, 0), ( 6,15,12, 0)),
    (( 6, 6, 6, 3), (12, 9, 9, 6), ( 6,12, 9, 6)),
    (( 8, 8, 5, 0), (15,12, 9, 0), ( 6,18, 9, 0)));

var
  ScaleFacBuffer: array[0..53] of Cardinal;

//==============================================================================
//
// TLayerIII_Decoder.GetLSFScaleData
//
//==============================================================================
procedure TLayerIII_Decoder.GetLSFScaleData(ch, gr: Cardinal);
var
  new_slen: array[0..3] of Cardinal;
  scalefac_comp, int_scalefac_comp: Cardinal;
  mode_ext: Cardinal;
  m: Integer;
  blocktypenumber, blocknumber: Integer;
  gr_info: PGRInfo;
  x, i, j: Cardinal;
begin
  mode_ext := FHeader.ModeExtension;
  gr_info := @FSI.ch[ch].gr[gr];
  scalefac_comp := gr_info.scalefac_compress;
  blocknumber := 0;

  if gr_info.block_type = 2 then
  begin
    if (gr_info.mixed_block_flag = 0) then
      blocktypenumber := 1
    else if (gr_info.mixed_block_flag = 1) then
      blocktypenumber := 2
    else
      blocktypenumber := 0;
  end
  else
    blocktypenumber := 0;

  if not (((mode_ext = 1) or (mode_ext = 3)) and (ch = 1)) then
  begin
    if scalefac_comp < 400 then
    begin
      new_slen[0] := (scalefac_comp shr 4) div 5;
      new_slen[1] := (scalefac_comp shr 4) mod 5;
      new_slen[2] := (scalefac_comp and $F) shr 2;
      new_slen[3] := (scalefac_comp and 3);
      FSI.ch[ch].gr[gr].preflag := 0;
      blocknumber := 0;
    end
    else if scalefac_comp < 500 then
    begin
      new_slen[0] := ((scalefac_comp - 400) shr 2) div 5;
      new_slen[1] := ((scalefac_comp - 400) shr 2) mod 5;
      new_slen[2] := (scalefac_comp - 400) and 3;
      new_slen[3] := 0;
      FSI.ch[ch].gr[gr].preflag := 0;
      blocknumber := 1;
    end
    else if scalefac_comp < 512 then
    begin
      new_slen[0] := (scalefac_comp - 500) div 3;
      new_slen[1] := (scalefac_comp - 500) mod 3;
      new_slen[2] := 0;
      new_slen[3] := 0;
      FSI.ch[ch].gr[gr].preflag := 1;
      blocknumber := 2;
    end;
  end;

  if ((mode_ext = 1) or (mode_ext = 3)) and (ch = 1) then
  begin
    int_scalefac_comp := scalefac_comp shr 1;

    if int_scalefac_comp < 180 then
    begin
      new_slen[0] := int_scalefac_comp div 36;
      new_slen[1] := (int_scalefac_comp mod 36 ) div 6;
      new_slen[2] := (int_scalefac_comp mod 36) mod 6;
      new_slen[3] := 0;
      FSI.ch[ch].gr[gr].preflag := 0;
      blocknumber := 3;
    end
    else if int_scalefac_comp < 244 then
    begin
      new_slen[0] := ((int_scalefac_comp - 180) and $3F) shr 4;
      new_slen[1] := ((int_scalefac_comp - 180) and $F) shr 2;
      new_slen[2] := (int_scalefac_comp - 180) and 3;
      new_slen[3] := 0;
      FSI.ch[ch].gr[gr].preflag := 0;
      blocknumber := 4;
    end
    else if int_scalefac_comp < 255 then
    begin
      new_slen[0] := (int_scalefac_comp - 244) div 3;
      new_slen[1] := (int_scalefac_comp - 244) mod 3;
      new_slen[2] := 0;
      new_slen[3] := 0;
      FSI.ch[ch].gr[gr].preflag := 0;
      blocknumber := 5;
    end;
  end;

  for x := 0 to 44 do  // why 45, not 54?
    ScaleFacBuffer[x] := 0;

  m := 0;
  for i := 0 to 3 do
    for j := 0 to NrOfSFBBlock[blocknumber, blocktypenumber, i] do
    begin
      if new_slen[i] = 0 then
        ScaleFacBuffer[m] := 0
      else
        ScaleFacBuffer[m] := FBR.hgetbits(new_slen[i]);

      inc(m);
    end;
end;

//==============================================================================
//
// TLayerIII_Decoder.GetLSFScaleFactors
//
//==============================================================================
procedure TLayerIII_Decoder.GetLSFScaleFactors(ch, gr: Cardinal);
var
  m: Cardinal;
  sfb, window: Cardinal;
  gr_info: PGRInfo;
begin
  m := 0;
  gr_info := @FSI.ch[ch].gr[gr];
  GetLSFScaleData(ch, gr);

  if (gr_info.window_switching_flag <> 0) and (gr_info.block_type = 2) then
  begin
    if gr_info.mixed_block_flag <> 0 then
    begin  // MIXED
      for sfb := 0 to 7 do
      begin
        FScaleFac[ch].l[sfb] := ScaleFacBuffer[m];
        inc(m);
      end;

      for sfb := 3 to 11 do
        for window := 0 to 2 do
        begin
          FScaleFac[ch].s[window][sfb] := ScaleFacBuffer[m];
          inc(m);
        end;

      for window := 0 to 2 do
        FScaleFac[ch].s[window, 12] := 0;
    end
    else
    begin  // SHORT
      for sfb := 0 to 11 do
        for window := 0 to 2 do
        begin
          FScaleFac[ch].s[window, sfb] := ScaleFacBuffer[m];
          inc(m);
        end;

      for window := 0 to 2 do
        FScaleFac[ch].s[window, 12] := 0;
    end;
  end
  else
  begin  // LONG types 0,1,3
    for sfb := 0 to 20 do
    begin
      FScaleFac[ch].l[sfb] := ScaleFacBuffer[m];
      inc(m);
    end;

    FScaleFac[ch].l[21] := 0; // Jeff
    FScaleFac[ch].l[22] := 0;
  end;
end;

//==============================================================================
//
// TLayerIII_Decoder.GetScaleFactors
//
//==============================================================================
procedure TLayerIII_Decoder.GetScaleFactors(ch, gr: Cardinal);
var
  sfb, window: Integer;
  gr_info: PGRInfo;
  scale_comp, length0, length1: Integer;
begin
  gr_info := @FSI.ch[ch].gr[gr];
  scale_comp := gr_info.scalefac_compress;
  length0 := slen[0, scale_comp];
  length1 := slen[1, scale_comp];

  if (gr_info.window_switching_flag <> 0) and (gr_info.block_type = 2) then
  begin
    if gr_info.mixed_block_flag <> 0 then
    begin  // MIXED
      for sfb := 0 to 7 do
        FScaleFac[ch].l[sfb] := FBR.hgetbits(slen[0, gr_info.scalefac_compress]);
      for sfb := 3 to 5 do
        for window := 0 to 2 do
          FScaleFac[ch].s[window, sfb] := FBR.hgetbits(slen[0, gr_info.scalefac_compress]);
      for sfb := 6 to 11 do
        for window := 0 to 2 do
          FScaleFac[ch].s[window, sfb] := FBR.hgetbits(slen[1, gr_info.scalefac_compress]);
      sfb := 12;
      for window := 0 to 2 do
        FScaleFac[ch].s[window, sfb] := 0;
    end
    else
    begin  // SHORT
      FScaleFac[ch].s[0, 0]  := FBR.hgetbits(length0);
      FScaleFac[ch].s[1, 0]  := FBR.hgetbits(length0);
      FScaleFac[ch].s[2, 0]  := FBR.hgetbits(length0);
      FScaleFac[ch].s[0, 1]  := FBR.hgetbits(length0);
      FScaleFac[ch].s[1, 1]  := FBR.hgetbits(length0);
      FScaleFac[ch].s[2, 1]  := FBR.hgetbits(length0);
      FScaleFac[ch].s[0, 2]  := FBR.hgetbits(length0);
      FScaleFac[ch].s[1, 2]  := FBR.hgetbits(length0);
      FScaleFac[ch].s[2, 2]  := FBR.hgetbits(length0);
      FScaleFac[ch].s[0, 3]  := FBR.hgetbits(length0);
      FScaleFac[ch].s[1, 3]  := FBR.hgetbits(length0);
      FScaleFac[ch].s[2, 3]  := FBR.hgetbits(length0);
      FScaleFac[ch].s[0, 4]  := FBR.hgetbits(length0);
      FScaleFac[ch].s[1, 4]  := FBR.hgetbits(length0);
      FScaleFac[ch].s[2, 4]  := FBR.hgetbits(length0);
      FScaleFac[ch].s[0, 5]  := FBR.hgetbits(length0);
      FScaleFac[ch].s[1, 5]  := FBR.hgetbits(length0);
      FScaleFac[ch].s[2, 5]  := FBR.hgetbits(length0);
      FScaleFac[ch].s[0, 6]  := FBR.hgetbits(length1);
      FScaleFac[ch].s[1, 6]  := FBR.hgetbits(length1);
      FScaleFac[ch].s[2, 6]  := FBR.hgetbits(length1);
      FScaleFac[ch].s[0, 7]  := FBR.hgetbits(length1);
      FScaleFac[ch].s[1, 7]  := FBR.hgetbits(length1);
      FScaleFac[ch].s[2, 7]  := FBR.hgetbits(length1);
      FScaleFac[ch].s[0, 8]  := FBR.hgetbits(length1);
      FScaleFac[ch].s[1, 8]  := FBR.hgetbits(length1);
      FScaleFac[ch].s[2, 8]  := FBR.hgetbits(length1);
      FScaleFac[ch].s[0, 9]  := FBR.hgetbits(length1);
      FScaleFac[ch].s[1, 9]  := FBR.hgetbits(length1);
      FScaleFac[ch].s[2, 9]  := FBR.hgetbits(length1);
      FScaleFac[ch].s[0, 10] := FBR.hgetbits(length1);
      FScaleFac[ch].s[1, 10] := FBR.hgetbits(length1);
      FScaleFac[ch].s[2, 10] := FBR.hgetbits(length1);
      FScaleFac[ch].s[0, 11] := FBR.hgetbits(length1);
      FScaleFac[ch].s[1, 11] := FBR.hgetbits(length1);
      FScaleFac[ch].s[2, 11] := FBR.hgetbits(length1);
      FScaleFac[ch].s[0, 12] := 0;
      FScaleFac[ch].s[1, 12] := 0;
      FScaleFac[ch].s[2, 12] := 0;
    end;
  end
  else
  begin  // LONG types 0,1,3
    if (FSI.ch[ch].scfsi[0] = 0) or (gr = 0) then
    begin
      FScaleFac[ch].l[0]  := FBR.hgetbits(length0);
      FScaleFac[ch].l[1]  := FBR.hgetbits(length0);
      FScaleFac[ch].l[2]  := FBR.hgetbits(length0);
      FScaleFac[ch].l[3]  := FBR.hgetbits(length0);
      FScaleFac[ch].l[4]  := FBR.hgetbits(length0);
      FScaleFac[ch].l[5]  := FBR.hgetbits(length0);
    end;

    if (FSI.ch[ch].scfsi[1] = 0) or (gr = 0) then
    begin
      FScaleFac[ch].l[6]  := FBR.hgetbits(length0);
      FScaleFac[ch].l[7]  := FBR.hgetbits(length0);
      FScaleFac[ch].l[8]  := FBR.hgetbits(length0);
      FScaleFac[ch].l[9]  := FBR.hgetbits(length0);
      FScaleFac[ch].l[10] := FBR.hgetbits(length0);
    end;

    if (FSI.ch[ch].scfsi[2] = 0) or (gr = 0) then
    begin
      FScaleFac[ch].l[11] := FBR.hgetbits(length1);
      FScaleFac[ch].l[12] := FBR.hgetbits(length1);
      FScaleFac[ch].l[13] := FBR.hgetbits(length1);
      FScaleFac[ch].l[14] := FBR.hgetbits(length1);
      FScaleFac[ch].l[15] := FBR.hgetbits(length1);
    end;

    if (FSI.ch[ch].scfsi[3] = 0) or (gr = 0) then
    begin
      FScaleFac[ch].l[16] := FBR.hgetbits(length1);
      FScaleFac[ch].l[17] := FBR.hgetbits(length1);
      FScaleFac[ch].l[18] := FBR.hgetbits(length1);
      FScaleFac[ch].l[19] := FBR.hgetbits(length1);
      FScaleFac[ch].l[20] := FBR.hgetbits(length1);
    end;

    FScaleFac[ch].l[21] := 0;
    FScaleFac[ch].l[22] := 0;
  end;
end;

//==============================================================================
// TLayerIII_Decoder.GetSideInfo
//
// Reads the side info from the stream, assuming the entire
// frame has been read already.
// Mono   : 136 bits (= 17 bytes)
// Stereo : 256 bits (= 32 bytes)
//
//==============================================================================
function TLayerIII_Decoder.GetSideInfo: Boolean;
var
  ch, gr: Cardinal;
begin
  if FHeader.Version = MPEG1 then
  begin
    FSI.main_data_begin := FStream.GetBits(9);
    if FChannels = 1 then
      FSI.private_bits := FStream.GetBits(5)
    else
      FSI.private_bits := FStream.GetBits(3);

    for ch := 0 to FChannels - 1 do
    begin
      FSI.ch[ch].scfsi[0] := FStream.GetBits(1);
      FSI.ch[ch].scfsi[1] := FStream.GetBits(1);
      FSI.ch[ch].scfsi[2] := FStream.GetBits(1);
      FSI.ch[ch].scfsi[3] := FStream.GetBits(1);
    end;

    for gr := 0 to 1 do
    begin
      for ch := 0 to FChannels - 1 do
      begin
        FSI.ch[ch].gr[gr].part2_3_length := FStream.GetBits(12);
        FSI.ch[ch].gr[gr].big_values := FStream.GetBits(9);
        FSI.ch[ch].gr[gr].global_gain := FStream.GetBits(8);
        FSI.ch[ch].gr[gr].scalefac_compress := FStream.GetBits(4);
        FSI.ch[ch].gr[gr].window_switching_flag := FStream.GetBits(1);
        if FSI.ch[ch].gr[gr].window_switching_flag <> 0 then
        begin
          FSI.ch[ch].gr[gr].block_type := FStream.GetBits(2);
          FSI.ch[ch].gr[gr].mixed_block_flag := FStream.GetBits(1);

          FSI.ch[ch].gr[gr].table_select[0] := FStream.GetBits(5);
          FSI.ch[ch].gr[gr].table_select[1] := FStream.GetBits(5);

          FSI.ch[ch].gr[gr].subblock_gain[0] := FStream.GetBits(3);
          FSI.ch[ch].gr[gr].subblock_gain[1] := FStream.GetBits(3);
          FSI.ch[ch].gr[gr].subblock_gain[2] := FStream.GetBits(3);

          // Set region_count parameters since they are implicit in this case.
          if FSI.ch[ch].gr[gr].block_type = 0 then
          begin
            // Side info bad: block_type == 0 in split block
            result := False;
            exit;
          end
          else if (FSI.ch[ch].gr[gr].block_type = 2) and (FSI.ch[ch].gr[gr].mixed_block_flag = 0) then
            FSI.ch[ch].gr[gr].region0_count := 8
          else
            FSI.ch[ch].gr[gr].region0_count := 7;

          FSI.ch[ch].gr[gr].region1_count := 20 - FSI.ch[ch].gr[gr].region0_count;
        end
        else
        begin
          FSI.ch[ch].gr[gr].table_select[0] := FStream.GetBits(5);
          FSI.ch[ch].gr[gr].table_select[1] := FStream.GetBits(5);
          FSI.ch[ch].gr[gr].table_select[2] := FStream.GetBits(5);
          FSI.ch[ch].gr[gr].region0_count := FStream.GetBits(4);
          FSI.ch[ch].gr[gr].region1_count := FStream.GetBits(3);
          FSI.ch[ch].gr[gr].block_type := 0;
        end;
        FSI.ch[ch].gr[gr].preflag := FStream.GetBits(1);
        FSI.ch[ch].gr[gr].scalefac_scale := FStream.GetBits(1);
        FSI.ch[ch].gr[gr].count1table_select := FStream.GetBits(1);
      end;
    end;
  end
  else
  begin  // MPEG-2 LSF
    FSI.main_data_begin := FStream.GetBits(8);
    if FChannels = 1 then
      FSI.private_bits := FStream.GetBits(1)
    else
      FSI.private_bits := FStream.GetBits(2);

    for ch := 0 to FChannels - 1 do
    begin
      FSI.ch[ch].gr[0].part2_3_length := FStream.GetBits(12);
      FSI.ch[ch].gr[0].big_values := FStream.GetBits(9);
      FSI.ch[ch].gr[0].global_gain := FStream.GetBits(8);
      FSI.ch[ch].gr[0].scalefac_compress := FStream.GetBits(9);
      FSI.ch[ch].gr[0].window_switching_flag := FStream.GetBits(1);

      if FSI.ch[ch].gr[0].window_switching_flag <> 0 then
      begin
        FSI.ch[ch].gr[0].block_type := FStream.GetBits(2);
        FSI.ch[ch].gr[0].mixed_block_flag := FStream.GetBits(1);
        FSI.ch[ch].gr[0].table_select[0] := FStream.GetBits(5);
        FSI.ch[ch].gr[0].table_select[1] := FStream.GetBits(5);

        FSI.ch[ch].gr[0].subblock_gain[0] := FStream.GetBits(3);
        FSI.ch[ch].gr[0].subblock_gain[1] := FStream.GetBits(3);
        FSI.ch[ch].gr[0].subblock_gain[2] := FStream.GetBits(3);

        // Set region_count parameters since they are implicit in this case.
        if (FSI.ch[ch].gr[0].block_type = 0) then
        begin
          // Side info bad: block_type = 0 in split block
          result := False;
          exit;
        end
        else if (FSI.ch[ch].gr[0].block_type = 2) and (FSI.ch[ch].gr[0].mixed_block_flag = 0) then
          FSI.ch[ch].gr[0].region0_count := 8
        else
        begin
          FSI.ch[ch].gr[0].region0_count := 7;
          FSI.ch[ch].gr[0].region1_count := 20 - FSI.ch[ch].gr[0].region0_count;
        end;
      end
      else
      begin
        FSI.ch[ch].gr[0].table_select[0] := FStream.GetBits(5);
        FSI.ch[ch].gr[0].table_select[1] := FStream.GetBits(5);
        FSI.ch[ch].gr[0].table_select[2] := FStream.GetBits(5);
        FSI.ch[ch].gr[0].region0_count := FStream.GetBits(4);
        FSI.ch[ch].gr[0].region1_count := FStream.GetBits(3);
        FSI.ch[ch].gr[0].block_type := 0;
      end;

      FSI.ch[ch].gr[0].scalefac_scale := FStream.GetBits(1);
      FSI.ch[ch].gr[0].count1table_select := FStream.GetBits(1);
    end;
  end;

  result := true;
end;

//==============================================================================
//
// TLayerIII_Decoder.HuffmanDecode
//
//==============================================================================
procedure TLayerIII_Decoder.HuffmanDecode(ch, gr: Cardinal);
var
  i: Cardinal;
  x, y, v, w: Integer;
  part2_3_end: Integer;
  num_bits: Integer;
  region1Start: Cardinal;
  region2Start: Cardinal;
  index: Integer;
  h: PHuffCodeTab;
begin
  part2_3_end := FPart2Start + FSI.ch[ch].gr[gr].part2_3_length;

  // Find region boundary for short block case
  if (FSI.ch[ch].gr[gr].window_switching_flag <> 0) and (FSI.ch[ch].gr[gr].block_type = 2) then
  begin
    // Region2.
    region1Start := 36;   // sfb[9/3]*3=36
    region2Start := 576;  // No Region2 for short block case
  end
  else
  begin  // Find region boundary for long block case
    region1Start := sfBandIndex[FSFreq].l[FSI.ch[ch].gr[gr].region0_count + 1];
    region2Start := sfBandIndex[FSFreq].l[FSI.ch[ch].gr[gr].region0_count + FSI.ch[ch].gr[gr].region1_count + 2];  // MI
  end;

  index := 0;
  // Read bigvalues area
  i := 0;
  while i < (FSI.ch[ch].gr[gr].big_values shl 1) do
  begin
    if i < region1Start then
      h := @ht[FSI.ch[ch].gr[gr].table_select[0]]
    else if i < region2Start then
      h := @ht[FSI.ch[ch].gr[gr].table_select[1]]
    else
      h := @ht[FSI.ch[ch].gr[gr].table_select[2]];

    HuffmanDecoder(h, x, y, v, w, FBR);

    FIS_1D[index] := x;
    FIS_1D[index + 1] := y;

    inc(index, 2);
    inc(i, 2);
  end;

  // Read count1 area
  h := @ht[FSI.ch[ch].gr[gr].count1table_select + 32];
  num_bits := FBR.hsstell;

  while (num_bits < part2_3_end) and (index < 576) do
  begin
    HuffmanDecoder(h, x, y, v, w, FBR);

    FIS_1D[index] := v;
    FIS_1D[index + 1] := w;
    FIS_1D[index + 2] := x;
    FIS_1D[index + 3] := y;

    inc(index, 4);
    num_bits := FBR.hsstell;
  end;

  if num_bits > part2_3_end then
  begin
    FBR.rewindNbits(num_bits - part2_3_end);
    dec(index, 4);
  end;

  num_bits := FBR.hsstell;

  // Dismiss stuffing bits
  if num_bits < part2_3_end then
    FBR.hgetbits(part2_3_end - num_bits);

  // Zero out rest
  if index < 576 then
    FNonZero[ch] := index
  else
    FNonZero[ch] := 576;

  // may not be necessary
  while index < 576 do
  begin
    FIS_1D[index] := 0;
    inc(index);
  end;
end;

//==============================================================================
//
// TLayerIII_Decoder.Hybrid
//
//==============================================================================
procedure TLayerIII_Decoder.Hybrid(ch, gr: Cardinal);
var
  rawout: array[0..35] of Single;
  bt: Cardinal;
  gr_info: PGRInfo;
  tsOut: PSingleArray;
  prvblk: PSingleArray;
  sb18: Cardinal;
begin
  gr_info := @FSI.ch[ch].gr[gr];

  sb18 := 0;
  while sb18 < 576 do
  begin
    if (gr_info.window_switching_flag <> 0) and (gr_info.mixed_block_flag <> 0) and (sb18 < 36) then
      bt := 0
    else
      bt := gr_info.block_type;

    tsOut := @FOut_1D[sb18];
    InvMDCT(tsOut, @rawout, bt);

    // overlap addition
    prvblk := @FPrevblock[ch, sb18];

    tsOut[0]   := rawout[0]  + prvblk[0];
    prvblk[0]  := rawout[18];
    tsOut[1]   := rawout[1]  + prvblk[1];
    prvblk[1]  := rawout[19];
    tsOut[2]   := rawout[2]  + prvblk[2];
    prvblk[2]  := rawout[20];
    tsOut[3]   := rawout[3]  + prvblk[3];
    prvblk[3]  := rawout[21];
    tsOut[4]   := rawout[4]  + prvblk[4];
    prvblk[4]  := rawout[22];
    tsOut[5]   := rawout[5]  + prvblk[5];
    prvblk[5]  := rawout[23];
    tsOut[6]   := rawout[6]  + prvblk[6];
    prvblk[6]  := rawout[24];
    tsOut[7]   := rawout[7]  + prvblk[7];
    prvblk[7]  := rawout[25];
    tsOut[8]   := rawout[8]  + prvblk[8];
    prvblk[8]  := rawout[26];
    tsOut[9]   := rawout[9]  + prvblk[9];
    prvblk[9]  := rawout[27];
    tsOut[10]  := rawout[10] + prvblk[10];
    prvblk[10] := rawout[28];
    tsOut[11]  := rawout[11] + prvblk[11];
    prvblk[11] := rawout[29];
    tsOut[12]  := rawout[12] + prvblk[12];
    prvblk[12] := rawout[30];
    tsOut[13]  := rawout[13] + prvblk[13];
    prvblk[13] := rawout[31];
    tsOut[14]  := rawout[14] + prvblk[14];
    prvblk[14] := rawout[32];
    tsOut[15]  := rawout[15] + prvblk[15];
    prvblk[15] := rawout[33];
    tsOut[16]  := rawout[16] + prvblk[16];
    prvblk[16] := rawout[34];
    tsOut[17]  := rawout[17] + prvblk[17];
    prvblk[17] := rawout[35];

    inc(sb18, 18);
  end;
end;

//==============================================================================
//
// TLayerIII_Decoder.IStereoKValues
//
//==============================================================================
procedure TLayerIII_Decoder.IStereoKValues(IsPos, IOType, i: Cardinal);
begin
  if IsPos = 0 then
  begin
    FK[0, i] := 1.0;
    FK[1, i] := 1.0;
  end
  else if IsPos and 1 <> 0 then
  begin
    FK[0, i] := io[IOType, (IsPos + 1) shr 1];
    FK[1, i] := 1.0;
  end
  else
  begin
    FK[0, i] := 1.0;
    FK[1, i] := io[IOType, IsPos shr 1];
  end;
end;

//==============================================================================
//
// TLayerIII_Decoder.Reorder
//
//==============================================================================
procedure TLayerIII_Decoder.Reorder(xr: PSArray; ch, gr: Cardinal);
var
  gr_info: PGRInfo;
  freq, freq3: Cardinal;
  sfb, sfb_start, sfb_start3, sfb_lines: Cardinal;
  src_line, des_line: Integer;
  xr1d: PSingleArray;
  index: Cardinal;
begin
  xr1d := @xr[0, 0];
  gr_info := @FSI.ch[ch].gr[gr];
  if (gr_info.window_switching_flag <> 0) and (gr_info.block_type = 2) then
  begin
    for index := 0 to 576 - 1 do
      FOut_1D[index] := 0.0;

    if gr_info.mixed_block_flag <> 0 then
    begin
      // NO REORDER FOR LOW 2 SUBBANDS
      for index := 0 to 36 - 1 do
        FOut_1D[index] := xr1d[index];

      // REORDERING FOR REST SWITCHED SHORT
      sfb_start := sfBandIndex[FSFreq].s[3];
      sfb_lines := Cardinal(sfBandIndex[FSFreq].s[4]) - sfb_start;
      for sfb := 3 to 12 do
      begin
        sfb_start3 := (sfb_start shl 2) - sfb_start;
        freq3 := 0;
        for freq := 0 to sfb_lines - 1 do
        begin
          src_line := sfb_start3 + freq;
          des_line := sfb_start3 + freq3;
          FOut_1D[des_line] := xr1d[src_line];
          inc(src_line, sfb_lines);
          inc(des_line);
          FOut_1D[des_line] := xr1d[src_line];
          inc(src_line, sfb_lines);
          inc(des_line);
          FOut_1D[des_line] := xr1d[src_line];
          inc(freq3, 3);
        end;
        sfb_start := sfBandIndex[FSFreq].s[sfb];
        sfb_lines := Cardinal(sfBandIndex[FSFreq].s[sfb+1]) - sfb_start;
      end;
    end
    else
    begin  // pure short
      for index := 0 to 576 - 1 do
        FOut_1D[index] := xr1d[reorder_table[FSFreq][index]];
    end;
  end
  else
  begin  // long blocks
    for index := 0 to 576 - 1 do
      FOut_1D[index] := xr1d[index];
  end;
end;

//==============================================================================
//
// TLayerIII_Decoder.SeekNotify
//
//==============================================================================
procedure TLayerIII_Decoder.SeekNotify;
begin
  FFrameStart := 0;

  FillChar(FPrevBlock, Sizeof(FPrevBlock), 0);

  FreeAndNil(FBR);
  CreateBitReserve;
end;

//==============================================================================
//
// TLayerIII_Decoder.Stereo
//
//==============================================================================
procedure TLayerIII_Decoder.Stereo(gr: Cardinal);
var
  sb, ss: Integer;
  is_pos: array[0..575] of Cardinal;
  is_ratio: array[0..575] of Single;
  gr_info: PGRInfo;
  mode_ext: Cardinal;
  i, j, lines, temp, temp2: Integer;
  ms_stereo, i_stereo, lsf: Boolean;
  io_type: Cardinal;
  max_sfb, sfbcnt, sfb: Integer;
begin
  if FChannels = 1 then
  begin  // mono , bypass xr[0][][] to lr[0][][]
    for sb := 0 to SBLIMIT - 1 do
    begin
      ss := 0;
      while ss < SSLIMIT do
      begin
        FLR[0][sb][ss]   := FRO[0][sb][ss];
        FLR[0][sb][ss+1] := FRO[0][sb][ss+1];
        FLR[0][sb][ss+2] := FRO[0][sb][ss+2];
        inc(ss, 3);
      end;
    end;
  end
  else
  begin
    gr_info := @FSI.ch[0].gr[gr];
    mode_ext := FHeader.ModeExtension;
    ms_stereo := (FHeader.Mode = JointStereo) and (Mode_Ext and $2 <> 0);
    i_stereo := (FHeader.Mode = JointStereo) and (Mode_Ext and $1 <> 0);
    lsf := (FHeader.Version = MPEG2_LSF);
    io_type := (gr_info.scalefac_compress and 1);

    // initialization
    for i := 0 to 576 - 1 do
      is_pos[i] := 7;

    if i_stereo then
    begin
      if (gr_info.window_switching_flag <> 0) and (gr_info.block_type = 2) then
      begin
        if gr_info.mixed_block_flag <> 0 then
        begin
          max_sfb := 0;

          for j := 0 to 2 do
          begin
            sfbcnt := 2;
            sfb := 12;
            while sfb >= 3 do
            begin
              i := sfBandIndex[FSFreq].s[sfb];
              lines := sfBandIndex[FSFreq].s[sfb+1] - i;
              i := (i shl 2) - i + (j+1) * lines - 1;

              while lines > 0 do
              begin
                if FRO[1][ss_div[i]][ss_mod[i]] <> 0.0 then
                begin
                  sfbcnt := sfb;
                  sfb := -10;
                  lines := -10;
                end;

                dec(lines);
                dec(i);
              end;

              dec(sfb);
            end;
            sfb := sfbcnt + 1;

            if sfb > max_sfb then
              max_sfb := sfb;

            while sfb < 12 do
            begin
              temp := sfBandIndex[FSFreq].s[sfb];
              sb := sfBandIndex[FSFreq].s[sfb+1] - temp;
              i := (temp shl 2) - temp + j * sb;

              while sb > 0 do
              begin
                is_pos[i] := FScaleFac[1].s[j][sfb];
                if is_pos[i] <> 7 then
                  if (lsf) then
                    IStereoKValues(is_pos[i], io_type, i)
                  else
                    is_ratio[i] := TAN12[is_pos[i]];

                inc(i);
                dec(sb);
              end;
              inc(sfb);
            end;

            sfb := sfBandIndex[FSFreq].s[10];
            sb := sfBandIndex[FSFreq].s[11] - sfb;
            sfb := (sfb shl 2) - sfb + j * sb;
            temp := sfBandIndex[FSFreq].s[11];
            sb := sfBandIndex[FSFreq].s[12] - temp;
            i := (temp shl 2) - temp + j * sb;

            while sb > 0 do
            begin
              is_pos[i] := is_pos[sfb];

              if (lsf) then
              begin
                FK[0][i] := FK[0][sfb];
                FK[1][i] := FK[1][sfb];
              end
              else
                is_ratio[i] := is_ratio[sfb];

              inc(i);
              dec(sb);
            end;
          end;

          if max_sfb <= 3 then
          begin
            i := 2;
            ss := 17;
            sb := -1;
            while i >= 0 do
            begin
              if FRO[1][i][ss] <> 0.0 then
              begin
                sb := (i shl 4) + (i shl 1) + ss;
                i := -1;
              end
              else
              begin
                dec(ss);
                if ss < 0 then
                begin
                  dec(i);
                  ss := 17;
                end;
              end;
            end;

            i := 0;
            while sfBandIndex[FSFreq].l[i] <= sb do
              inc(i);

            sfb := i;
            i := sfBandIndex[FSFreq].l[i];
            while sfb < 8 do
            begin
              sb := sfBandIndex[FSFreq].l[sfb+1] - sfBandIndex[FSFreq].l[sfb];
              while sb > 0 do
              begin
                is_pos[i] := FScaleFac[1].l[sfb];
                if is_pos[i] <> 7 then
                  if lsf then
                    IStereoKValues(is_pos[i], io_type, i)
                  else
                    is_ratio[i] := TAN12[is_pos[i]];

                inc(i);
                inc(sb);
              end;
              inc(sfb);
            end;
          end;
        end
        else
        begin  // if (gr_info->mixed_block_flag)
          for j := 0 to 2 do
          begin
            sfbcnt := -1;
            sfb := 12;
            while sfb >= 0 do
            begin
              temp := sfBandIndex[FSFreq].s[sfb];
              lines := sfBandIndex[FSFreq].s[sfb+1] - temp;
              i := (temp shl 2) - temp + (j+1) * lines - 1;

              while lines > 0 do
              begin
                if FRO[1][ss_div[i]][ss_mod[i]] <> 0.0 then
                begin
                  sfbcnt := sfb;
                  sfb := -10;
                  lines := -10;
                end;

                dec(lines);
                dec(i);
              end;
              dec(sfb);
            end;

            sfb := sfbcnt + 1;
            while sfb < 12 do
            begin
              temp := sfBandIndex[FSFreq].s[sfb];
              sb := sfBandIndex[FSFreq].s[sfb+1] - temp;
              i := (temp shl 2) - temp + j * sb;
              while sb > 0 do
              begin
                // dec(sb);
                is_pos[i] := FScaleFac[1].s[j][sfb];
                if is_pos[i] <> 7 then
                  if lsf then
                    IStereoKValues(is_pos[i], io_type, i)
                  else
                    is_ratio[i] := TAN12[is_pos[i]];

                inc(i);
                dec(sb);
              end;

              inc(sfb);
            end;

            temp := sfBandIndex[FSFreq].s[10];
            temp2 := sfBandIndex[FSFreq].s[11];
            sb   := temp2 - temp;
            sfb  := (temp shl 2) - temp + j * sb;
            sb   := sfBandIndex[FSFreq].s[12] - temp2;
            i    := (temp2 shl 2) - temp2 + j * sb;

            while sb > 0 do
            begin
              is_pos[i] := is_pos[sfb];

              if lsf then
              begin
                FK[0][i] := FK[0][sfb];
                FK[1][i] := FK[1][sfb];
              end
              else
                is_ratio[i] := is_ratio[sfb];

              inc(i);
              dec(sb);
            end;
          end;
        end;
      end
      else
      begin  // if (gr_info->window_switching_flag ...
        i := 31;
        ss := 17;
        sb := 0;
        while i >= 0 do
        begin
          if FRO[1][i][ss] <> 0.0 then
          begin
            sb := (i shl 4) + (i shl 1) + ss;
            i := -1;
          end
          else
          begin
            dec(ss);
            if ss < 0 then
            begin
              dec(i);
              ss := 17;
            end;
          end;
        end;

        i := 0;
        while (sfBandIndex[FSFreq].l[i] <= sb) do
          inc(i);

        sfb := i;
        i := sfBandIndex[FSFreq].l[i];
        while sfb < 21 do
        begin
          sb := sfBandIndex[FSFreq].l[sfb+1] - sfBandIndex[FSFreq].l[sfb];
          while sb > 0 do
          begin
            is_pos[i] := FScaleFac[1].l[sfb];
            if is_pos[i] <> 7 then
              if (lsf) then
                IStereoKValues(is_pos[i], io_type, i)
              else
                is_ratio[i] := TAN12[is_pos[i]];

            inc(i);
            dec(sb);
          end;
          inc(sfb);
        end;

        sfb := sfBandIndex[FSFreq].l[20];
        sb := 576 - sfBandIndex[FSFreq].l[21];
        while (sb > 0) and (i < 576) do
        begin
          is_pos[i] := is_pos[sfb]; // error here : i >=576
          if lsf then
          begin
            FK[0][i] := FK[0][sfb];
            FK[1][i] := FK[1][sfb];
          end
          else
            is_ratio[i] := is_ratio[sfb];

          inc(i);
          dec(sb);
        end;
      end;
    end;

    i := 0;
    for sb := 0 to SBLIMIT - 1 do
      for ss := 0 to SSLIMIT - 1 do
      begin
        if is_pos[i] = 7 then
        begin
          if ms_stereo then
          begin
            FLR[0][sb][ss] := (FRO[0][sb][ss] + FRO[1][sb][ss]) * 0.707106781;
            FLR[1][sb][ss] := (FRO[0][sb][ss] - FRO[1][sb][ss]) * 0.707106781;
          end
          else
          begin
            FLR[0][sb][ss] := FRO[0][sb][ss];
            FLR[1][sb][ss] := FRO[1][sb][ss];
          end;
        end
        else if (i_stereo) then
        begin
          if lsf then
          begin
            FLR[0][sb][ss] := FRO[0][sb][ss] * FK[0][i];
            FLR[1][sb][ss] := FRO[0][sb][ss] * FK[1][i];
          end
          else
          begin
            FLR[1][sb][ss] := FRO[0][sb][ss] / (1 + is_ratio[i]);
            FLR[0][sb][ss] := FLR[1][sb][ss] * is_ratio[i];
          end;
        end;

        inc(i);
      end;
  end;
end;

end.

