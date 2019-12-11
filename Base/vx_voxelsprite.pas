//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
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
// DESCRIPTION:
//  Use Voxels as sprites without patch inside WAD
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit vx_voxelsprite;

interface

procedure VX_VoxelToSprite;

implementation

uses
  d_delphi,
  doomdef,
  i_tmp,
  m_argv,
  m_sha1,
  {$IFDEF OPENGL}
  gl_voxels,
  {$ELSE}
  r_voxels,
  {$ENDIF}
  sc_engine,
  w_folders,
  w_pak,
  w_wad,
  w_wadwriter;

var
  vx_names: TDStringList = nil;

function VX_SpriteExistsInWAD(const filename: string): boolean;
var
  check, lumpname: string;
  i: integer;
  in_loop: boolean;
begin
  check := firstword(filename, '.'); // Input is uppercase
  SetLength(check, 5); // SPRITE & FRAME
  in_loop := false;
  for i := 0 to W_NumLumps - 1 do
  begin
    lumpname := char8tostring(W_GetNameForNum(i));
    if (lumpname = 'SS_START') or (lumpname = 'S_START') then
      in_loop := true
    else if (lumpname = 'SS_END') or (lumpname = 'S_END') then
      in_loop := false
    else if in_loop then
    begin
      if Pos(check, lumpname) = 1 then
      begin
        result := true;
        exit;
      end;
    end;
  end;
  result := false;
end;

procedure VX_AddFileName(const filename: string);
var
  check: string;
  name: string;
  ext: string;
begin
  check := strupper(filename);
  ext := fext(check);
  if (ext = '.DDVOX') or (ext = '.DDMESH') or (ext = '.KVX') or (ext = '.VOX') or
     ((ext = '') and (Pos(FOLDER_VOXELS + '\', fixpathname(check)) > 0)) then
  begin
    check := fname(check);
    name := firstword(check, '.');
    if (Length(name) = 5) or ((Length(name) = 6) and (name[6] = '0')) then
      if vx_names.IndexOf(name) < 0 then
        if not VX_SpriteExistsInWAD(name) then
          vx_names.Add(name + ext)
  end;
end;

const
//  LUMP0: array[0..465] of Byte = (
{ F:\DelphiDoom_Release\DelphiDoom_Src\HVIAD.png (11/12/2019 12:07:54 ðì)
  StartOffset: 00000000, EndOffset: 000001B2, ÌÞêïò: 000001B3 }

  LUMP0: array[0..434] of Byte = (
  $89, $50, $4E, $47, $0D, $0A, $1A, $0A, $00, $00, $00, $0D, $49, $48, $44,
  $52, $00, $00, $00, $80, $00, $00, $00, $80, $08, $02, $00, $00, $00, $4C,
  $5C, $F6, $9C, $00, $00, $00, $08, $67, $72, $41, $62, $00, $00, $00, $40,
  $00, $00, $00, $80, $37, $F7, $B6, $0F, $00, $00, $00, $09, $70, $48, $59,
  $73, $00, $00, $0B, $13, $00, $00, $0B, $13, $01, $00, $9A, $9C, $18, $00,
  $00, $01, $51, $49, $44, $41, $54, $78, $9C, $ED, $D9, $B1, $09, $C0, $30,
  $10, $04, $41, $0B, $5C, $98, $FA, $6F, $4A, $2E, $C1, $3C, $08, $36, $99,
  $89, $DF, $D1, $82, $02, $DF, $3A, $67, $3F, $84, $46, $01, $A6, $B5, $DC,
  $FF, $DE, $BF, $A3, $0F, $B8, $4E, $80, $98, $00, $31, $01, $62, $02, $C4,
  $04, $88, $09, $10, $13, $20, $26, $40, $4C, $80, $98, $00, $31, $01, $62,
  $02, $C4, $96, $DF, $D1, $31, $BF, $A3, $DB, $7B, $4F, $50, $4C, $80, $98,
  $00, $31, $01, $62, $02, $C4, $04, $88, $09, $10, $13, $20, $26, $40, $4C,
  $80, $98, $00, $31, $01, $62, $02, $C4, $04, $88, $D9, $03, $6A, $F6, $80,
  $F6, $DE, $13, $14, $13, $20, $26, $40, $4C, $80, $98, $00, $31, $01, $62,
  $02, $C4, $04, $88, $09, $10, $13, $20, $26, $40, $4C, $80, $98, $00, $31,
  $01, $62, $F6, $80, $9A, $3D, $A0, $BD, $F7, $04, $C5, $04, $88, $09, $10,
  $13, $20, $26, $40, $4C, $80, $98, $00, $31, $01, $62, $02, $C4, $04, $88,
  $09, $10, $13, $20, $26, $40, $4C, $80, $98, $3D, $A0, $66, $0F, $68, $EF,
  $3D, $41, $31, $01, $62, $02, $C4, $04, $88, $09, $10, $13, $20, $26, $40,
  $4C, $80, $98, $00, $31, $01, $62, $02, $C4, $04, $88, $09, $10, $13, $20,
  $66, $0F, $A8, $D9, $03, $DA, $7B, $4F, $50, $4C, $80, $98, $00, $31, $01,
  $62, $02, $C4, $04, $88, $09, $10, $13, $20, $26, $40, $4C, $80, $98, $00,
  $31, $01, $62, $02, $C4, $04, $88, $D9, $03, $6A, $F6, $80, $F6, $DE, $13,
  $14, $13, $20, $26, $40, $4C, $80, $98, $00, $31, $01, $62, $02, $C4, $04,
  $88, $09, $10, $13, $20, $26, $40, $4C, $80, $98, $00, $31, $01, $62, $F6,
  $80, $9A, $3D, $A0, $BD, $F7, $04, $C5, $04, $88, $09, $10, $13, $20, $26,
  $40, $4C, $80, $98, $00, $31, $01, $62, $02, $C4, $04, $88, $09, $10, $13,
  $20, $26, $40, $4C, $80, $98, $3D, $20, $F6, $01, $68, $2F, $B7, $64, $73,
  $C2, $B3, $80, $00, $00, $00, $00, $49, $45, $4E, $44, $AE, $42, $60, $82
);
{    $89, $50, $4E, $47, $0D, $0A, $1A, $0A, $00, $00, $00, $0D, $49, $48, $44,
    $52, $00, $00, $00, $80, $00, $00, $00, $80, $08, $02, $00, $00, $00, $4C,
    $5C, $F6, $9C, $00, $00, $00, $15, $74, $45, $58, $74, $43, $72, $65, $61,
    $74, $69, $6F, $6E, $20, $54, $69, $6D, $65, $00, $07, $E3, $0C, $0A, $12,
    $33, $01, $BF, $86, $0F, $21, $00, $00, $00, $07, $74, $49, $4D, $45, $07,
    $E3, $0C, $0A, $12, $36, $30, $46, $2E, $18, $2E, $00, $00, $00, $09, $70,
    $48, $59, $73, $00, $00, $0B, $12, $00, $00, $0B, $12, $01, $D2, $DD, $7E,
    $FC, $00, $00, $01, $50, $49, $44, $41, $54, $78, $DA, $ED, $D9, $B1, $09,
    $C0, $30, $10, $04, $41, $CB, $95, $A9, $FF, $A6, $E4, $12, $CC, $83, $60,
    $93, $99, $F8, $1D, $2D, $28, $F0, $AD, $73, $F6, $43, $68, $14, $60, $5A,
    $CB, $FD, $EF, $FD, $3B, $FA, $80, $EB, $04, $88, $09, $10, $13, $20, $26,
    $40, $4C, $80, $98, $00, $31, $01, $62, $02, $C4, $04, $88, $09, $10, $13,
    $20, $26, $40, $6C, $F9, $1D, $1D, $F3, $3B, $BA, $BD, $F7, $04, $C5, $04,
    $88, $09, $10, $13, $20, $26, $40, $4C, $80, $98, $00, $31, $01, $62, $02,
    $C4, $04, $88, $09, $10, $13, $20, $26, $40, $4C, $80, $98, $3D, $A0, $66,
    $0F, $68, $EF, $3D, $41, $31, $01, $62, $02, $C4, $04, $88, $09, $10, $13,
    $20, $26, $40, $4C, $80, $98, $00, $31, $01, $62, $02, $C4, $04, $88, $09,
    $10, $13, $20, $66, $0F, $A8, $D9, $03, $DA, $7B, $4F, $50, $4C, $80, $98,
    $00, $31, $01, $62, $02, $C4, $04, $88, $09, $10, $13, $20, $26, $40, $4C,
    $80, $98, $00, $31, $01, $62, $02, $C4, $04, $88, $D9, $03, $6A, $F6, $80,
    $F6, $DE, $13, $14, $13, $20, $26, $40, $4C, $80, $98, $00, $31, $01, $62,
    $02, $C4, $04, $88, $09, $10, $13, $20, $26, $40, $4C, $80, $98, $00, $31,
    $01, $62, $F6, $80, $9A, $3D, $A0, $BD, $F7, $04, $C5, $04, $88, $09, $10,
    $13, $20, $26, $40, $4C, $80, $98, $00, $31, $01, $62, $02, $C4, $04, $88,
    $09, $10, $13, $20, $26, $40, $4C, $80, $98, $3D, $A0, $66, $0F, $68, $EF,
    $3D, $41, $31, $01, $62, $02, $C4, $04, $88, $09, $10, $13, $20, $26, $40,
    $4C, $80, $98, $00, $31, $01, $62, $02, $C4, $04, $88, $09, $10, $13, $20,
    $66, $0F, $A8, $D9, $03, $DA, $7B, $4F, $50, $4C, $80, $98, $00, $31, $01,
    $62, $02, $C4, $04, $88, $09, $10, $13, $20, $26, $40, $4C, $80, $98, $00,
    $31, $01, $62, $02, $C4, $04, $88, $D9, $03, $62, $1F, $00, $E0, $B6, $86,
    $3F, $BA, $63, $F3, $00, $00, $00, $00, $49, $45, $4E, $44, $AE, $42, $60,
    $82
  );}

function readablestring(const s: string): string;
var
  i: integer;
  h: string;
begin
  result := '';
  h := '0123456789ABCDEF';
  for i := 1 to Length(s) do
  begin
    if Pos(toupper(s[i]), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789') > 0 then
      result := result + toupper(s[i])
    else
      result := result + h[Ord(s[i]) div 16] + h[Ord(s[i]) mod 16];
  end;
end;

procedure VX_VoxelToSprite;
var
  wad: TWADWriter;
  wadfilename: string;
  i: integer;
  mem: TDMemoryStream;
begin               
  vx_names := TDStringList.Create;
  PAK_FileNameIterator(@VX_AddFileName);

  if vx_names.Count > 0 then
  begin
    wad := TWADWriter.Create;

    wad.AddSeparator('SS_START');

    for i := 0 to vx_names.Count - 1 do
      wad.AddData(firstword(vx_names.Strings[i], '.') + '0', @LUMP0, SizeOf(LUMP0));

    wad.AddSeparator('SS_END');

    mem := TDMemoryStream.Create;

    wad.SaveToStream(mem);

    wadfilename := M_SaveFileName('DATA\');
    MkDir(wadfilename);
    wadfilename := wadfilename + 'TMP\';
    MkDir(wadfilename);
    wadfilename := wadfilename + 'voxelsprite_' + readablestring(SHA1_CalcSHA1Buf(mem.memory^, mem.Size)) + '.wad';
    mem.Free;

    wad.SaveToFile(wadfilename);

    wad.Free;

    W_RuntimeLoad(wadfilename, F_ORIGIN_WAD);

    I_DeclareTempFile(wadfilename);
  end;

  vx_names.Free;
end;

end.
