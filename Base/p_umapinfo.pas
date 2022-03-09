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
// DESCRIPTION:
//  UMAPINFO support
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_umapinfo;

interface

const
  BIGSTRINGSIZE = 1024;

type
  ubigstring_t = array[0..BIGSTRINGSIZE - 1] of char;

type
  bossaction_t = record
    typ: integer;
    special: integer;
    tag: integer;
  end;
  Pbossaction_t = ^bossaction_t;
  bossaction_tArray = array[0..$FF] of bossaction_t;
  Pbossaction_tArray = ^bossaction_tArray;

  ulumpname_t = string[8];

  mapentry_t = record
    mapname: ulumpname_t;
    levelname: string[255];
    mlabel: string[255];
    intertext: ubigstring_t;
    intertextsecret: ubigstring_t;
    levelpic: ulumpname_t;
    nextmap: ulumpname_t;
    nextsecret: ulumpname_t;
    music: ulumpname_t;
    musicnum: integer;
    skytexture: ulumpname_t;
    skytexture2: ulumpname_t;
    endpic: ulumpname_t;
    exitpic: ulumpname_t;
    enterpic: ulumpname_t;
    interbackdrop: ulumpname_t;
    intermusic: ulumpname_t;
    intermusicnum: integer;
    partime: integer;
    nointermission: boolean;
    lightning: boolean;
    nojump: boolean;
    nocrouch: boolean;
    numbossactions: integer;
    bossactions: Pbossaction_tArray;
  end;
  Pmapentry_t = ^mapentry_t;
  mapentry_tArray = array[0..$FF] of mapentry_t;
  Pmapentry_tArray = ^mapentry_tArray;

  umapinfo_t = record
    mapcount: integer;
    maps: Pmapentry_tArray;
  end;

var
  u_mapinfo: umapinfo_t;
  default_mapinfo: umapinfo_t;

const
  S_UMAPINFO = 'UMAPINFO';
  S_UMAPDEF = 'UMAPDEF';

//==============================================================================
//
// U_ParseMapInfo
//
//==============================================================================
procedure U_ParseMapInfo(const is_default: boolean; const lumppname: string);

//==============================================================================
//
// U_FreeMapInfo
//
//==============================================================================
procedure U_FreeMapInfo;

//==============================================================================
//
// ubigstringtostring
//
//==============================================================================
function ubigstringtostring(const ub: ubigstring_t): string;

implementation

uses
  d_delphi,
  doomdef,
  g_game,
  info_common,
  m_menu,
  p_uactornames,
  sounds,
  sc_engine,
  sc_utils,
  w_pak,
  w_wad,
  z_zone;

//==============================================================================
//
// ubigstringtostring
//
//==============================================================================
function ubigstringtostring(const ub: ubigstring_t): string;
var
  i: integer;
begin
  result := '';
  i := 0;
  while ub[i] <> #0 do
  begin
    result := result + ub[i];
    inc(i);
    if i = BIGSTRINGSIZE then
      exit;
  end;
end;

//==============================================================================
//
// stringtoubigstring
//
//==============================================================================
function stringtoubigstring(const s: string): ubigstring_t;
var
  i, len: integer;
begin
  i := 1;
  len := Length(s);
  while i < len do
  begin
    result[i - 1] := s[i];
    Inc(i);
    if i = BIGSTRINGSIZE - 1 then
      Break;
  end;
  result[i] := #0;
end;

//==============================================================================
//
// FreeMap
//
//==============================================================================
procedure FreeMap(mape: Pmapentry_t);
begin
  mape.mapname := '';
  mape.levelname := '';
  mape.mlabel := '';
  mape.intertext := '';
  mape.intertextsecret := '';
end;

//==============================================================================
//
// UpdateMapEntry
//
//==============================================================================
procedure UpdateMapEntry(mape: Pmapentry_t; newe: Pmapentry_t);
begin
  if newe.mapname <> '' then
    mape.mapname := newe.mapname;
  if newe.levelname <> '' then
    mape.levelname := newe.levelname;
  if newe.mlabel <> '' then
    mape.mlabel := newe.mlabel;
  if newe.intertext <> '' then
    mape.intertext := newe.intertext;
  if newe.intertextsecret <> '' then
    mape.intertextsecret := newe.intertextsecret;
  if newe.levelpic <> '' then
    mape.levelpic := newe.levelpic;
  if newe.nextmap <> '' then
    mape.nextmap := newe.nextmap;
  if newe.music <> '' then
  begin
    mape.music := newe.music;
    mape.musicnum := S_GetMusicNumForName(mape.music);
  end;
  if newe.skytexture <> '' then
    mape.skytexture := newe.skytexture;
  if newe.skytexture2 <> '' then
    mape.skytexture2 := newe.skytexture2;
  if newe.endpic <> '' then
    mape.endpic := newe.endpic;
  if newe.exitpic <> '' then
    mape.exitpic := newe.exitpic;
  if newe.enterpic <> '' then
    mape.enterpic := newe.enterpic;
  if newe.interbackdrop <> '' then
    mape.interbackdrop := newe.interbackdrop;
  if newe.intermusic <> '' then
  begin
    mape.intermusic := newe.intermusic;
    mape.intermusicnum := S_GetMusicNumForName(mape.intermusic);
  end;
  if newe.partime <> 0 then
    mape.partime := newe.partime;
  if newe.lightning then
    mape.lightning := newe.lightning;
  if newe.nojump then
    mape.nojump := newe.nojump;
  if newe.nocrouch then
    mape.nocrouch := newe.nocrouch;
  if newe.nointermission then
    mape.nointermission := newe.nointermission;
  if newe.numbossactions <> 0 then
  begin
    mape.numbossactions := newe.numbossactions;
    if mape.numbossactions = -1 then
    begin
      if mape.bossactions <> nil then
        Z_Free(mape.bossactions);
      mape.bossactions := nil;
    end
    else
    begin
      mape.bossactions := Z_Realloc(mape.bossactions, SizeOf(bossaction_t) * mape.numbossactions, PU_STATIC, nil);
      memcpy(mape.bossactions, newe.bossactions, Sizeof(bossaction_t) * mape.numbossactions);
    end;
  end;
end;

//==============================================================================
// U_ParseMultiString
//
// -----------------------------------------------
// Parses a set of string and concatenates them
// Returns a pointer to the string (must be freed)
// -----------------------------------------------
//
//==============================================================================
function U_ParseMultiString(sc: TScriptEngine): string;
var
  i: integer;
begin
  sc.MustGetString;
  if not sc.QuotedToken then
    if strupper(sc._String) = 'CLEAR' then
    begin
      result := '-';  // this was explicitly deleted to override the default.
      exit;
    end
    else
    begin
      sc.ScriptError('UMAPINO.U_ParseMultiString(): Either ''clear'' or string constant expected');
      result := '';
      exit;
    end;
  result := sc._String;

  while sc.GetString do
  begin
    if not sc.QuotedToken then
    begin
      sc.UnGet;
      break;
    end;
    result := result + #13#10 + sc._String;
  end;

  // Translate "C" style string
  for i := 2 to Length(result) do
    if result[i] = 'n' then
      if result[i - 1] = '\' then
      begin
        result[i - 1] := #13;
        result[i] := #10;
      end;
end;

//==============================================================================
// ParseLumpName
//
// -----------------------------------------------
// Parses a lump name. The buffer must be at least 9 characters.
// If parsed name is longer than 8 chars, sets NULL pointer.
//
// returns 1 on successfully parsing an element
//         0 on parse error in last read token
// -----------------------------------------------
//
//==============================================================================
function ParseLumpName(sc: TScriptEngine; var buffer: ulumpname_t): boolean;
begin
  if not sc.GetString then
  begin
    result := false;
    exit;
  end;

  if Length(sc._String) > 8 then
  begin
    sc.ScriptError('UMAPINO.ParseLumpName(): String ''%s'' is too long. Maximum size is 8 characters.', [sc._String]);
    result := true; // not a parse error
    exit;
  end;

  buffer := strupper(sc._String);
  result := true;
end;

//==============================================================================
// U_ParseStandardProperty
//
// -----------------------------------------------
// Parses a standard property that is already known
// These do not get stored in the property list
// but in dedicated struct member variables.
//
// returns 1 on successfully parsing an element
//         0 on parse error in last read token
// -----------------------------------------------
//
//==============================================================================
function U_ParseStandardProperty(sc: TScriptEngine; mape: Pmapentry_t): boolean;
var
  pname: string;
  {$IFNDEF STRIFE}
  lumpname: ulumpname_t;
  alttext: string;
  key: char;
  {$ENDIF}
  lname: string;
  aname: string;
  i, special, tag, typ: integer;
begin
  if not sc.GetString then
  begin
    result := false;
    exit;
  end;

  pname := strupper(sc._String);

  if pname = '_ENDBLOCK' then
  begin
    result := false;
    exit;
  end;

  if not sc.GetString then
  begin
    result := false;
    exit;
  end;

  if sc._String <> '=' then
  begin
    sc.ScriptError('UMAPINO.U_ParseStandardProperty(): ''='' expected but ''%s'' found.', [sc._String]);
    result := false;
    exit;
  end;

  result := true;

  if pname = 'LEVELNAME' then
  begin
    if sc.MustGetString then
      mape.levelname := sc._String;
  end
  else if pname = 'LABEL' then
  begin
    if sc.MustGetString then
    begin
      if sc.QuotedToken then
        mape.mlabel := sc._String
      else if strupper(sc._String) = 'CLEAR' then
        mape.mlabel := '-'
      else
        sc.ScriptError('UMAPINFO.U_ParseStandardProperty(): Either ''clear'' or string constant expected after ''label'' keyword');
    end
  end
  {$IFNDEF STRIFE}
  else if pname = 'EPISODE' then
  begin
    if sc.MustGetString then
    begin
      if not sc.QuotedToken then
      begin
        if strupper(sc._String) = 'CLEAR' then
          M_ClearEpisodes
        else
          sc.ScriptError('UMAPINFO.U_ParseStandardProperty(): Either ''clear'' or string constant expected after ''episode'' keyword');
      end
      else
      begin
        sc.UnGet;

        lumpname := '';
        alttext := '';
        key := #0;

        ParseLumpName(sc, lumpname);

        if sc.MustGetString then
        begin
          if sc.QuotedToken then
          begin
            alttext := sc._String;
            if sc.MustGetString then
              if sc.QuotedToken and (sc._String <> '') then
                key := sc._String[1]
              else
                sc.UnGet;
          end
          else
            sc.UnGet;
        end;
        M_AddEpisode(mape.mapname, lumpname, alttext, key);
      end;
    end;
  end
  {$ENDIF}
  else if pname = 'NEXT' then
  begin
    result := ParseLumpName(sc, mape.nextmap);
    if not G_ValidateMapName(mape.nextmap, nil, nil) then
    begin
      sc.ScriptError('UMAPINFO.U_ParseStandardProperty(): Invalid map name ''%s''', [mape.nextmap]);
      result := false;
    end;
  end
  else if pname = 'NEXTSECRET' then
  begin
    result := ParseLumpName(sc, mape.nextsecret);
    if not G_ValidateMapName(mape.nextsecret, nil, nil) then
    begin
      sc.ScriptError('UMAPINFO.U_ParseStandardProperty(): Invalid map name ''%s''', [mape.nextsecret]);
      result := false;
    end;
  end
  else if pname = 'LEVELPIC' then
  begin
    result := ParseLumpName(sc, mape.levelpic);
  end
  else if (pname = 'SKYTEXTURE')  or (pname = 'SKYTEXTURE1') then
  begin
    result := ParseLumpName(sc, mape.skytexture);
  end
  else if pname = 'SKYTEXTURE2' then
  begin
    result := ParseLumpName(sc, mape.skytexture2);
  end
  else if pname = 'MUSIC' then
  begin
    result := ParseLumpName(sc, mape.music);
    if result then
    begin
      if W_CheckNumForName(mape.music) >= 0 then
        mape.musicnum := S_GetMusicNumForName(mape.music)
      else
      begin
        mape.music := '';
        mape.musicnum := 0;
      end;
    end;
  end
  else if pname = 'ENDPIC' then
  begin
    result := ParseLumpName(sc, mape.endpic);
  end
  else if pname = 'ENDCAST' then
  begin
    if sc.MustGetBoolean then
    begin
      if sc._Boolean then
        mape.endpic := '$CAST'
      else
        mape.endpic := '-';
    end;
  end
  else if pname = 'ENDBUNNY' then
  begin
    if sc.MustGetBoolean then
    begin
      if sc._Boolean then
        mape.endpic := '$BUNNY'
      else
        mape.endpic := '-';
    end;
  end
  else if pname = 'ENDGAME' then
  begin
    if sc.MustGetBoolean then
    begin
      if sc._Boolean then
        mape.endpic := '!'
      else
        mape.endpic := '-';
    end;
  end
  else if pname = 'EXITPIC' then
  begin
    result := ParseLumpName(sc, mape.exitpic);
  end
  else if pname = 'ENTERPIC' then
  begin
    result := ParseLumpName(sc, mape.enterpic);
  end
  else if pname = 'LIGHTNING' then
  begin
    if sc.MustGetBoolean then
      mape.lightning := sc._Boolean;
  end
  else if pname = 'NOJUMP' then
  begin
    if sc.MustGetBoolean then
      mape.nojump := sc._Boolean;
  end
  else if pname = 'NOCROUCH' then
  begin
    if sc.MustGetBoolean then
      mape.nocrouch := sc._Boolean;
  end
  else if pname = 'NOINTERMISSION' then
  begin
    if sc.MustGetBoolean then
      mape.nointermission := sc._Boolean;
  end
  else if pname = 'PARTIME' then
  begin
    if sc.MustGetInteger then
      mape.partime := TICRATE * sc._Integer;
  end
  else if pname = 'INTERTEXT' then
  begin
    lname := U_ParseMultiString(sc);
    if lname = '' then
    begin
      result := false;
      exit;
    end;
    mape.intertext := stringtoubigstring(lname);
  end
  else if pname = 'INTERTEXTSECRET' then
  begin
    lname := U_ParseMultiString(sc);
    if lname = '' then
    begin
      result := false;
      exit;
    end;
    mape.intertextsecret := stringtoubigstring(lname);
  end
  else if pname = 'INTERBACKDROP' then
  begin
    result := ParseLumpName(sc, mape.interbackdrop);
  end
  else if pname = 'INTERMUSIC' then
  begin
    result := ParseLumpName(sc, mape.intermusic);
    if result then
    begin
      if W_CheckNumForName(mape.intermusic) >= 0 then
        mape.intermusicnum := S_GetMusicNumForName(mape.intermusic)
      else
      begin
        mape.intermusic := '';
        mape.intermusicnum := 0;
      end;
    end;
  end
  else if pname = 'BOSSACTION' then
  begin
    if sc.MustGetString then
    begin
      if (strupper(sc._String) = 'clear') then
      begin
        // mark level free of boss actions
        mape.bossactions := Z_Realloc(mape.bossactions, 0, PU_STATIC, nil);
        mape.numbossactions := -1;
      end
      else
      begin
        typ := -1;
        aname := strupper(sc._String);
        for i := 0 to NUMACTORNAMES - 1 do
          if aname = strupper(ActorNames[i]) then
          begin
            typ := i;
            break;
          end;
        if typ = -1 then
          typ := Info_GetMobjNumForName(aname);
        if typ < 0 then
        begin
          sc.ScriptError('UMAPINO.U_ParseStandardProperty(): bossaction: unknown thing type ''%s''', [sc._String]);
          result := False;
          exit;
        end;
        sc.MustGetInteger;
        special := sc._Integer;
        sc.MustGetInteger;
        tag := sc._Integer;
        // allow no 0-tag specials here, unless a level exit.
        {$IFDEF DOOM}
        if (tag <> 0) or (special = 11) or (special = 51) or (special = 52) or (special = 124) then
        {$ENDIF}
        {$IFDEF HERETIC}
        if (tag <> 0) or (special = 11) or (special = 51) or (special = 52) or (special = 105) then
        {$ENDIF}
        begin
          if mape.numbossactions = -1 then
            mape.numbossactions := 1
          else
            Inc(mape.numbossactions);
          mape.bossactions := Z_Realloc(mape.bossactions, SizeOf(bossaction_t) * mape.numbossactions, PU_STATIC, nil);
          mape.bossactions[mape.numbossactions - 1].typ := typ;
          mape.bossactions[mape.numbossactions - 1].special := special;
          mape.bossactions[mape.numbossactions - 1].tag := tag;
        end;
      end;
    end;
  end
  // If no known property name was given, skip all comma-separated values after the = sign
  else
    sc.GetStringEOLUnChanged;
end;

//==============================================================================
// ParseMapEntry
//
// -----------------------------------------------
//
// Parses a complete map entry
//
// -----------------------------------------------
//
//==============================================================================
function ParseMapEntry(const sc: TScriptEngine; const entry: Pmapentry_t): boolean;
begin
  entry.mapname := '';

  sc.GetString;
  if sc._String = '_ENDBLOCK' then
    sc.GetString;

  if sc._Finished then
  begin
    result := false;
    exit;
  end;
  sc.UnGet;

  if not sc.MustGetStringName('map') then
  begin
    result := false;
    exit;
  end;

  if sc.MustGetString then
    if not G_ValidateMapName(sc._String, nil, nil) then
    begin
      sc.ScriptError('UMAPINFO.ParseMapEntry(): Invalid map name ''%s''', [sc._String]);
      result := false;
      exit;
    end;

  entry.mapname := strupper(sc._String);
  if sc.MustGetString then
    if sc.MatchString('_BEGINBLOCK') then
      while not sc.MatchString('_ENDBLOCK') and not sc._Finished do
        U_ParseStandardProperty(sc, entry);

  result := true;
end;

// -----------------------------------------------
//
// Parses a complete UMAPINFO lump
//
// -----------------------------------------------
var
  uis_default: boolean;

//==============================================================================
//
// U_DoParseMapInfoLump
//
//==============================================================================
procedure U_DoParseMapInfoLump(const in_text: string);
var
  i: integer;
  sc: TScriptEngine;
  parsed: mapentry_t;
begin
  sc := TScriptEngine.Create(in_text);

  while not sc._Finished do
  begin
    ZeroMemory(@parsed, SizeOf(mapentry_t));
    if not ParseMapEntry(sc, @parsed) then
    begin
      if sc._Finished then
        Break;
      sc.ScriptError('UMAPINFO.U_DoParseMapInfoLump(): Skipping entry: ''%s''', [sc._String]);
      Continue;
    end;

    if uis_default then
    begin
      Inc(default_mapinfo.mapcount);
      default_mapinfo.maps := Z_Realloc(default_mapinfo.maps, SizeOf(mapentry_t) * default_mapinfo.mapcount, PU_LEVEL, nil);
      default_mapinfo.maps[default_mapinfo.mapcount - 1] := parsed;
      Continue;
    end;

    // Does this property already exist? If yes, replace it.
    i := 0;
    while i < u_mapinfo.mapcount do
    begin
      if parsed.mapname = u_mapinfo.maps[i].mapname then
      begin
        FreeMap(@u_mapinfo.maps[i]);
        if default_mapinfo.mapcount > i then
        begin
          ZeroMemory(@u_mapinfo.maps[i], SizeOf(mapentry_t));
          UpdateMapEntry(@u_mapinfo.maps[i], @default_mapinfo.maps[i]);
          UpdateMapEntry(@u_mapinfo.maps[i], @parsed);
          FreeMap(@parsed);
        end
        else
          u_mapinfo.maps[i] := parsed;
        break;
      end;
      Inc(i);
    end;

    // Not found so create a new one.
    if i = u_mapinfo.mapcount then
    begin
      Inc(u_mapinfo.mapcount);
      u_mapinfo.maps := Z_Realloc(u_mapinfo.maps, SizeOf(mapentry_t) * u_mapinfo.mapcount, PU_STATIC, nil);

      if default_mapinfo.mapcount > i then
      begin
        ZeroMemory(@u_mapinfo.maps[i], SizeOf(mapentry_t));
        UpdateMapEntry(@u_mapinfo.maps[i], @default_mapinfo.maps[i]);
        UpdateMapEntry(@u_mapinfo.maps[i], @parsed);
        FreeMap(@parsed);
      end
      else
        u_mapinfo.maps[u_mapinfo.mapcount - 1] := parsed;
    end;
  end;
  sc.Free;
end;

//==============================================================================
//
// U_Preproccessor
//
//==============================================================================
function U_Preproccessor(in_text: string): string;
var
  i: integer;
  len: integer;
  inquotes: boolean;
  incomments: boolean;
  inLcomments: boolean;
begin
  in_text := in_text + #13#10;
  result := '';
  inquotes := false;
  incomments := false;
  inLcomments := false;
  i := 1;
  len := Length(in_text);
  while i < len do
  begin
    if in_text[i] = '"' then
      if not (incomments or inLcomments) then
        inquotes := not inquotes;
    if not inquotes then
    begin
      if in_text[i] = '/' then
      begin
        if in_text[i + 1] = '/' then
          inLcomments := true
        else if in_text[i + 1] = '*' then
          incomments := true;
      end;
      if in_text[i] in [#13, #10] then
        inLcomments := false;
      if in_text[i] = '*' then
        if in_text[i + 1] = '/' then
          incomments := false;
    end;
    if not (incomments or inLcomments) then
    begin
      if inquotes then
        result := result + in_text[i]
      else if in_text[i] = ',' then
      else if in_text[i] = '=' then
        result := result + ' = '
      else if in_text[i] = '{' then
        result := result + #13#10'_BEGINBLOCK'#13#10
      else if in_text[i] = '}' then
        result := result + #13#10'_ENDBLOCK'#13#10
      else
        result := result + in_text[i]
    end;
    inc(i);
  end;
end;

//==============================================================================
//
// U_ParseMapInfoLump
//
//==============================================================================
procedure U_ParseMapInfoLump(const in_text: string);
begin
  U_DoParseMapInfoLump(U_Preproccessor(SC_Preprocess(in_text, false)));
end;

//==============================================================================
//
// U_ParseMapInfo
//
//==============================================================================
procedure U_ParseMapInfo(const is_default: boolean; const lumppname: string);
var
  i: integer;
  u_text: string;
  ulumpname: string;
begin
  uis_default := is_default;

  ulumpname := strupper(lumppname);
  u_text := '';

  for i := 0 to W_NumLumps - 1 do
  begin
    if char8tostring(W_GetNameForNum(i)) = ulumpname then
    begin
      if u_text = '' then
        u_text := W_TextLumpNum(i)
      else
        u_text := u_text + #13#10 + W_TextLumpNum(i);
    end;
  end;

  if u_text <> '' then
    U_ParseMapInfoLump(u_text);

  PAK_StringIterator(lumppname, @U_ParseMapInfoLump);
  PAK_StringIterator(lumppname + '.txt', @U_ParseMapInfoLump);
end;

//==============================================================================
//
// U_FreeMapInfo
//
//==============================================================================
procedure U_FreeMapInfo;
var
  i: integer;
begin
  for i := 0 to u_mapinfo.mapcount - 1 do
    FreeMap(@u_mapinfo.maps[i]);
  if u_mapinfo.maps <> nil then
    Z_Free(u_mapinfo.maps);
  u_mapinfo.maps := nil;
  u_mapinfo.mapcount := 0;
end;

end.
