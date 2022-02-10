//------------------------------------------------------------------------------
//
//  DelphiDoom is a source port of the game Doom and it is
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
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
//
// DESCRIPTION:
//  Translation tables
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_translations;

interface

uses
  p_mobj_h;

type
  translationtable_t = array[0..255] of byte;
  Ptranslationtable_t = ^translationtable_t;
  translationtable_tArray = array[0..$FF] of translationtable_t;
  Ptranslationtable_tArray = ^translationtable_tArray;

//==============================================================================
//
// R_InitTranslations
//
//==============================================================================
procedure R_InitTranslations;

//==============================================================================
//
// R_ShutDownTranslations
//
//==============================================================================
procedure R_ShutDownTranslations;

//==============================================================================
//
// R_GetTranslationTable
//
//==============================================================================
function R_GetTranslationTable(const name: string): Ptranslationtable_t;

//==============================================================================
//
// R_BloodTranslation
//
//==============================================================================
function R_BloodTranslation(const bt: integer): Pointer;

//==============================================================================
//
// R_GetBloodTranslationIdForName
//
//==============================================================================
function R_GetBloodTranslationIdForName(const name: string): integer;

//==============================================================================
//
// R_GetBloodName
//
//==============================================================================
function R_GetBloodName(const id: integer): string;

//==============================================================================
//
// R_InitMobjTranslation
//
//==============================================================================
procedure R_InitMobjTranslation(const mo: Pmobj_t);

//==============================================================================
//
// R_SetMobjBloodTranslation
//
//==============================================================================
procedure R_SetMobjBloodTranslation(const mo: Pmobj_t; const blood: integer);

implementation

uses
  Classes,
  d_delphi,
  i_system,
  r_draw,
  w_pak,
  w_wad,
  z_zone;

var
  translations: TStringList;

//==============================================================================
//
// R_InitTranslations
//
//==============================================================================
procedure R_InitTranslations;
begin
  translations := TStringList.Create;
  translations.AddObject('BRICK', TObject(colorregions[Ord(CR_BRICK)]));
  translations.AddObject('TAN', TObject(colorregions[Ord(CR_TAN)]));
  translations.AddObject('GRAY', TObject(colorregions[Ord(CR_GRAY)]));
  translations.AddObject('GREEN', TObject(colorregions[Ord(CR_GREEN)]));
  translations.AddObject('BROWN', TObject(colorregions[Ord(CR_BROWN)]));
  translations.AddObject('GOLD', TObject(colorregions[Ord(CR_GOLD)]));
  translations.AddObject('RED', TObject(colorregions[Ord(CR_RED)]));
  translations.AddObject('BLUE', TObject(colorregions[Ord(CR_BLUE)]));
  translations.AddObject('ORANGE', TObject(colorregions[Ord(CR_ORANGE)]));
  translations.AddObject('YELLOW', TObject(colorregions[Ord(CR_YELLOW)]));
  translations.AddObject('BLUE2', TObject(colorregions[Ord(CR_BLUE2)]));
  translations.AddObject('BLACK', TObject(colorregions[Ord(CR_BLACK)]));
  translations.AddObject('PURPL', TObject(colorregions[Ord(CR_PURPL)]));
  translations.AddObject('PURPLE', TObject(colorregions[Ord(CR_PURPL)]));
  translations.AddObject('WHITE', TObject(colorregions[Ord(CR_WHITE)]));
  translations.AddObject('TRANS0', TObject(colorregions[Ord(CR_TRANS0)]));
  translations.AddObject('TRANS1', TObject(colorregions[Ord(CR_TRANS1)]));
  translations.AddObject('TRANS2', TObject(colorregions[Ord(CR_TRANS2)]));
  translations.AddObject('TRANS3', TObject(colorregions[Ord(CR_TRANS3)]));
  translations.AddObject('TRANS4', TObject(colorregions[Ord(CR_TRANS4)]));
  translations.AddObject('TRANS5', TObject(colorregions[Ord(CR_TRANS5)]));
  translations.AddObject('TRANS6', TObject(colorregions[Ord(CR_TRANS6)]));
  translations.AddObject('TRANS7', TObject(colorregions[Ord(CR_TRANS7)]));
  translations.AddObject('TRANS8', TObject(colorregions[Ord(CR_TRANS8)]));
  translations.AddObject('TRANS9', TObject(colorregions[Ord(CR_TRANS9)]));
  translations.Sorted := True;
end;

//==============================================================================
//
// R_ShutDownTranslations
//
//==============================================================================
procedure R_ShutDownTranslations;
begin
  translations.Free;
end;

//==============================================================================
//
// R_GetTranslationTable
//
//==============================================================================
function R_GetTranslationTable(const name: string): Ptranslationtable_t;
type
  tryfunc_t = function (const aname: string): Boolean;
var
  check: string;
  idx: integer;
  newtrans: Ptranslationtable_t;

  function _dotry_pk3(const pk3name: string): Boolean;
  var
    strm: TDStream;
  begin
    strm := TPakStream.Create(pk3name, pm_short);
    strm.OnBeginBusy := I_BeginDiskBusy;
    Result := (strm.IOResult = 0) and (strm.Size = SizeOf(translationtable_t));
    if Result then
    begin
      newtrans := Z_Malloc(SizeOf(translationtable_t), PU_STATIC, nil);
      strm.Read(newtrans^, SizeOf(translationtable_t));
    end;
    strm.Free;
  end;

  function _dotry_wad(const wadname: string): Boolean;
  var
    lumpname: string;
    lump: integer;
  begin
    lumpname := char8tostring(stringtochar8(wadname));
    lump := W_CheckNumForName(lumpname);
    if lump >= 0 then
      if W_LumpLength(lump) = SizeOf(translationtable_t) then
      begin
        newtrans := Z_Malloc(SizeOf(translationtable_t), PU_STATIC, nil);
        W_ReadLump(lump, newtrans);
        Result := True;
        Exit;
      end;
    Result := False;
  end;

  function _try_load(const aname: string; afunc: tryfunc_t): Boolean;
  var
    stmp: string;
  begin
    Result := afunc(aname);
    if Result then
      Exit;
    if Pos('CR_', strupper(aname)) = 1 then
    begin
      stmp := aname;
      Delete(stmp, 3, 1);
      Result := afunc(stmp);
      Exit;
    end;
    if Pos('CR', strupper(aname)) = 1 then
    begin
      stmp := aname;
      Insert('_', stmp, 3);
      Result := afunc(stmp);
      Exit;
    end;
    stmp := 'CR' + aname;
    Result := afunc(stmp);
    if Result then
      exit;
    stmp := 'CR_' + aname;
    Result := afunc(stmp);
    if Result then
      exit;
  end;

begin
  check := strupper(name);
  idx := translations.IndexOf(check);
  if idx < 0 then
  begin
    if Pos('CR_', check) = 1 then
    begin
      Delete(check, 1, 3);
      idx := translations.IndexOf(check);
    end;
    if idx < 0 then
    begin
      if Pos('CR', check) = 1 then
      begin
        Delete(check, 1, 2);
        idx := translations.IndexOf(check);
      end;
    end;
  end;

  if idx >= 0 then
  begin
    Result := Ptranslationtable_t(translations.Objects[idx]);
    Exit;
  end;

  newtrans := nil;
  if not _try_load(name, @_dotry_pk3) then
    _try_load(name, @_dotry_wad);

  Result := newtrans;
  if Result <> nil then
  begin
    translations.Sorted := False;
    translations.AddObject(check, TObject(Result));
    translations.Sorted := True;
  end;
end;

//==============================================================================
//
// R_BloodTranslation
//
//==============================================================================
function R_BloodTranslation(const bt: integer): Pointer;
begin
  case bt of
    1: Result := colorregions[Ord(CR_GRAY)];
    2: Result := colorregions[Ord(CR_GREEN)];
    3: Result := colorregions[Ord(CR_BLUE2)];
    4: Result := colorregions[Ord(CR_YELLOW)];
    5: Result := colorregions[Ord(CR_BLACK)];
    6: Result := colorregions[Ord(CR_PURPL)];
    7: Result := colorregions[Ord(CR_WHITE)];
    8: Result := colorregions[Ord(CR_ORANGE)];
  else
    Result := nil;
  end;
end;

const
  NUMBLOODNAMES = 9;
  BLOODNAMES: array[0..NUMBLOODNAMES - 1] of string = (
    'RED',
    'GRAY',
    'GREEN',
    'BLUE2',
    'YELLOW',
    'BLACK',
    'PURPL',
    'WHITE',
    'ORANGE'
  );

//==============================================================================
//
// R_GetBloodTranslationIdForName
//
//==============================================================================
function R_GetBloodTranslationIdForName(const name: string): integer;
var
  num: integer;
  uname: string;
  i: integer;
begin
  if StrIsLongWord(name) then
  begin
    num := atoi(name);
    if IsIntegerInRange(num, 0, NUMBLOODNAMES - 1) then
      Result := num
    else
      Result := 0;
    Exit;
  end;
  uname := strupper(name);
  if uname = 'PURPLE' then
    uname := 'PURPL';
  for i := 0 to NUMBLOODNAMES - 1 do
    if (uname = BLOODNAMES[i]) or (uname = 'CR' + BLOODNAMES[i]) or (uname = 'CR_' + BLOODNAMES[i]) then
    begin
      Result := i;
      Exit;
    end;
  Result := 0;
end;

//==============================================================================
//
// R_GetBloodName
//
//==============================================================================
function R_GetBloodName(const id: integer): string;
begin
  if not IsIntegerInRange(id, 1, NUMBLOODNAMES - 1) then
    Result := ''
  else
    Result := BLOODNAMES[id];
end;

//==============================================================================
//
// R_InitMobjTranslation
//
//==============================================================================
procedure R_InitMobjTranslation(const mo: Pmobj_t);
begin
  if mo.translationname = '' then
    mo.translationtable := nil
  else
    mo.translationtable := R_GetTranslationTable(mo.translationname);
end;

//==============================================================================
//
// R_SetMobjBloodTranslation
//
//==============================================================================
procedure R_SetMobjBloodTranslation(const mo: Pmobj_t; const blood: integer);
begin
  mo.translationname := R_GetBloodName(blood);
  mo.translationtable := R_GetTranslationTable(mo.translationname);
end;

end.
