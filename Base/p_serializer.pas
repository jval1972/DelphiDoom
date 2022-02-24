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
//  Base binary serializer
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_serializer;

interface

uses
  d_delphi;

type
  serializetype_t = (
    st_generic,
    st_byte,
    st_shortint,
    st_word,
    st_smallint,
    st_longword,
    st_integer,
    st_int64,
    st_float,
    st_state,
    st_mobj,
    st_player,
    st_string
  );

const
  SERIALIZESIZES: array[serializetype_t] of integer = (
    0,
    1,
    1,
    2,
    2,
    4,
    4,
    8,
    4,
    4,
    4,
    4,
    255
  );

type
  serializeitem_t = record
    id: word;
    offs: integer;
    typ: serializetype_t;
    size: integer;
    idefault: int64;
    fdefault: float;
    sdefault: string[255];
    loadproc: PPointerParmProcedure;
  end;
  Pserializeitem_t = ^serializeitem_t;
  serializeitem_tArray = array[0..$FF] of serializeitem_t;
  Pserializeitem_tArray = ^serializeitem_tArray;

type
  TSerializer = class(TObject)
  private
    fitems: Pserializeitem_tArray;
    fnumitems: integer;
  protected
    function LoadItem(const strm: TDStream; const struct: Pointer; const it: Pserializeitem_t): boolean; virtual;
    function SaveItem(const strm: TDStream; const struct: Pointer; const it: Pserializeitem_t): boolean; virtual;
    function SetDefault(const struct: Pointer; const it: Pserializeitem_t): boolean; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure AddItem(const offs: Pointer; const size: integer; const lproc: PPointerParmProcedure = nil);
    procedure AddStringItem(const offs: Pointer; const def: string; const lproc: PPointerParmProcedure = nil);
    procedure AddFloatItem(const offs: Pointer; const def: float; const lproc: PPointerParmProcedure = nil);
    procedure AddTypedItem(const offs: Pointer; const typ: serializetype_t; const def: int64; const lproc: PPointerParmProcedure = nil);
    function SaveToMem(const mem, struct: Pointer; const maxsize: integer): integer;
    function SaveToStream(const strm: TDStream; const struct: Pointer; const maxsize: integer): integer;
    function LoadFromMem(const mem, struct: Pointer; const maxsize: integer): integer;
    function LoadFromStream(const strm: TDStream; const struct: Pointer; const maxsize: integer): integer;
    procedure ResolvePointers(const struct: Pointer);
  end;

implementation

uses
  d_player,
  info_h,
  info,
  p_mobj_h,
  p_mobj;

//==============================================================================
//
// TSerializer.Create
//
//==============================================================================
constructor TSerializer.Create;
begin
  fitems := nil;
  fnumitems := 0;
  inherited Create;
end;

//==============================================================================
//
// TSerializer.Destroy
//
//==============================================================================
destructor TSerializer.Destroy;
begin
  Clear;
  inherited;
end;

//==============================================================================
//
// TSerializer.Clear
//
//==============================================================================
procedure TSerializer.Clear;
begin
  if fnumitems > 0 then
  begin
    memfree(Pointer(fitems), fnumitems * SizeOf(serializeitem_t));
    fnumitems := 0;
  end;
end;

//==============================================================================
//
// TSerializer.AddItem
//
//==============================================================================
procedure TSerializer.AddItem(const offs: Pointer; const size: integer; const lproc: PPointerParmProcedure = nil);
var
  item: Pserializeitem_t;
begin
  realloc(Pointer(fitems), fnumitems * SizeOf(serializeitem_t), (fnumitems + 1) * SizeOf(serializeitem_t));
  item := @fitems[fnumitems];
  Inc(fnumitems);

  item.id := fnumitems;
  item.size := size;
  item.typ := st_generic;
  item.offs := Integer(offs);
  item.idefault := 0;
  item.fdefault := 0.0;
  item.sdefault := '';
  item.loadproc := lproc;
end;

//==============================================================================
//
// TSerializer.AddStringItem
//
//==============================================================================
procedure TSerializer.AddStringItem(const offs: Pointer; const def: string; const lproc: PPointerParmProcedure = nil);
var
  item: Pserializeitem_t;
begin
  realloc(Pointer(fitems), fnumitems * SizeOf(serializeitem_t), (fnumitems + 1) * SizeOf(serializeitem_t));
  item := @fitems[fnumitems];
  Inc(fnumitems);

  item.id := fnumitems;
  item.size := 255;
  item.typ := st_string;
  item.offs := Integer(offs);
  item.idefault := 0;
  item.fdefault := 0.0;
  item.sdefault := def;
  item.loadproc := lproc;
end;

//==============================================================================
//
// TSerializer.AddFloatItem
//
//==============================================================================
procedure TSerializer.AddFloatItem(const offs: Pointer; const def: float; const lproc: PPointerParmProcedure = nil);
var
  item: Pserializeitem_t;
begin
  realloc(Pointer(fitems), fnumitems * SizeOf(serializeitem_t), (fnumitems + 1) * SizeOf(serializeitem_t));
  item := @fitems[fnumitems];
  Inc(fnumitems);

  item.id := fnumitems;
  item.size := 4;
  item.typ := st_float;
  item.offs := Integer(offs);
  item.idefault := 0;
  item.fdefault := def;
  item.sdefault := '';
  item.loadproc := lproc;
end;

//==============================================================================
//
// TSerializer.AddTypedItem
//
//==============================================================================
procedure TSerializer.AddTypedItem(const offs: Pointer; const typ: serializetype_t; const def: int64; const lproc: PPointerParmProcedure = nil);
var
  item: Pserializeitem_t;
begin
  realloc(Pointer(fitems), fnumitems * SizeOf(serializeitem_t), (fnumitems + 1) * SizeOf(serializeitem_t));
  item := @fitems[fnumitems];
  Inc(fnumitems);

  item.id := fnumitems;
  item.size := SERIALIZESIZES[typ];
  item.typ := typ;
  item.offs := Integer(offs);
  item.idefault := def;
  item.fdefault := 0.0;
  item.sdefault := '';
  item.loadproc := lproc;
end;

//==============================================================================
//
// TSerializer.LoadItem
//
//==============================================================================
function TSerializer.LoadItem(const strm: TDStream; const struct: Pointer; const it: Pserializeitem_t): boolean;
var
  b: PByteArray;
  len: byte;
  i: integer;
begin
  b := struct;
  result := true;
  case it.typ of
    st_generic,
    st_byte,
    st_shortint,
    st_word,
    st_smallint,
    st_longword,
    st_integer,
    st_int64,
    st_float,
    st_state,
    st_mobj,
    st_player:
      strm.Read(b[it.offs], it.size);
    st_string:
      begin
        strm.Read(len, SizeOf(len));
        b[it.offs] := len;
        for i := 1 to len do
          strm.Read(b[it.offs + i], SizeOf(Char));
      end;
  else
    result := false;
  end;
  if result then
    if Assigned(it.loadproc) then
      it.loadproc(struct);
end;

//==============================================================================
//
// TSerializer.SaveItem
//
//==============================================================================
function TSerializer.SaveItem(const strm: TDStream; const struct: Pointer; const it: Pserializeitem_t): boolean;
var
  b: PByteArray;
  len: byte;
  i: integer;
  pp: LongWord;
  st: Pstate_t;
  mo: Pmobj_t;
  pl: Pplayer_t;
begin
  b := struct;
  result := true;
  case it.typ of
    st_generic:
      begin
        if not IsZeroes(@b[it.offs], it.size) then
        begin
          strm.Write(it.id, SizeOf(word));
          strm.Write(b[it.offs], it.size);
        end;
      end;
    st_byte:
      begin
        if PByte(@b[it.offs])^ <> it.idefault then
        begin
          strm.Write(it.id, SizeOf(word));
          strm.Write(b[it.offs], it.size);
        end;
      end;
    st_shortint:
      begin
        if PShortInt(@b[it.offs])^ <> it.idefault then
        begin
          strm.Write(it.id, SizeOf(word));
          strm.Write(b[it.offs], it.size);
        end;
      end;
    st_word:
      begin
        if PWord(@b[it.offs])^ <> it.idefault then
        begin
          strm.Write(it.id, SizeOf(word));
          strm.Write(b[it.offs], it.size);
        end;
      end;
    st_smallint:
      begin
        if PSmallInt(@b[it.offs])^ <> it.idefault then
        begin
          strm.Write(it.id, SizeOf(word));
          strm.Write(b[it.offs], it.size);
        end;
      end;
    st_longword:
      begin
        if PLongWord(@b[it.offs])^ <> it.idefault then
        begin
          strm.Write(it.id, SizeOf(word));
          strm.Write(b[it.offs], it.size);
        end;
      end;
    st_integer:
      begin
        if PInteger(@b[it.offs])^ <> it.idefault then
        begin
          strm.Write(it.id, SizeOf(word));
          strm.Write(b[it.offs], it.size);
        end;
      end;
    st_int64:
      begin
        if PInt64(@b[it.offs])^ <> it.idefault then
        begin
          strm.Write(it.id, SizeOf(word));
          strm.Write(b[it.offs], it.size);
        end;
      end;
    st_float:
      begin
        if Pfloat(@b[it.offs])^ <> it.fdefault then
        begin
          strm.Write(it.id, SizeOf(word));
          strm.Write(b[it.offs], it.size);
        end;
      end;
    st_string:
      begin
        strm.Write(it.id, SizeOf(word));
        len := b[it.offs];
        strm.Write(len, SizeOf(len));
        for i := 1 to len do
          strm.Write(b[it.offs + i], SizeOf(byte));
      end;
    st_state:
      begin
        st := Pstate_t(PInteger(@b[it.offs])^);
        if st <> nil then
        begin
          strm.Write(it.id, SizeOf(word));
          pp := (integer(st) - integer(@states[0])) div SizeOf(state_t);
          strm.Write(pp, SizeOf(Integer));
        end;
      end;
    st_mobj:
      begin
        mo := Pmobj_t(PInteger(@b[it.offs])^);
        if mo <> nil then
          if mo.key <> 0 then
          begin
            strm.Write(it.id, SizeOf(word));
            strm.Write(mo.key, SizeOf(LongWord));
          end;
      end;
    st_player:
      begin
        pl := Pplayer_t(PInteger(@b[it.offs])^);
        if pl <> nil then
        begin
          strm.Write(it.id, SizeOf(word));
          pp := (integer(pl) - integer(@players[0])) div SizeOf(player_t) + 1;
          strm.Write(pp, SizeOf(Integer));
        end;
      end;
  else
    result := false;
  end;
end;

//==============================================================================
//
// TSerializer.SetDefault
//
//==============================================================================
function TSerializer.SetDefault(const struct: Pointer; const it: Pserializeitem_t): boolean;
var
  b: PByteArray;
  i: integer;
begin
  b := struct;
  result := false;
  case it.typ of
    st_generic:
      begin
        ZeroMemory(@b[it.offs], it.size);
      end;
    st_byte:
      begin
        PByte(@b[it.offs])^ := it.idefault;
      end;
    st_shortint:
      begin
        PShortInt(@b[it.offs])^ := it.idefault;
      end;
    st_word:
      begin
        PWord(@b[it.offs])^ := it.idefault;
      end;
    st_smallint:
      begin
        PSmallInt(@b[it.offs])^ := it.idefault;
      end;
    st_longword:
      begin
        PLongWord(@b[it.offs])^ := it.idefault;
      end;
    st_integer:
      begin
        PInteger(@b[it.offs])^ := it.idefault;
      end;
    st_int64:
      begin
        PInt64(@b[it.offs])^ := it.idefault;
      end;
    st_float:
      begin
        Pfloat(@b[it.offs])^ := it.fdefault;
      end;
    st_string:
      begin
        PByte(@b[it.offs])^ := Length(it.sdefault);
        for i := 1 to Length(it.sdefault) do
          PByte(@b[it.offs + i])^ := Ord(it.sdefault[i]);
      end;
    st_state,
    st_mobj,
    st_player:
      begin
        PPointer(@b[it.offs])^ := nil;
      end;
  else
    result := false;
  end;
end;

//==============================================================================
//
// TSerializer.SaveToMem
//
//==============================================================================
function TSerializer.SaveToMem(const mem, struct: Pointer; const maxsize: integer): integer;
var
  strm: TAttachableMemoryStream;
begin
  strm := TAttachableMemoryStream.Create;
  strm.Attach(mem, maxsize);
  result := SaveToStream(strm, struct, maxsize);
  strm.Free;
end;

//==============================================================================
//
// TSerializer.SaveToStream
//
//==============================================================================
function TSerializer.SaveToStream(const strm: TDStream; const struct: Pointer; const maxsize: integer): integer;
var
  id: word;
  i: integer;
  oldpos, newpos: integer;
begin
  oldpos := strm.Position;
  for i := 0 to fnumitems - 1 do
    SaveItem(strm, struct, @fitems[i]);
  id := 0;
  strm.Write(id, SizeOf(word));
  newpos := strm.Position;
  result := newpos - oldpos;
end;

//==============================================================================
//
// TSerializer.LoadFromMem
//
//==============================================================================
function TSerializer.LoadFromMem(const mem, struct: Pointer; const maxsize: integer): integer;
var
  strm: TAttachableMemoryStream;
begin
  strm := TAttachableMemoryStream.Create;
  strm.Attach(mem, maxsize);
  result := LoadFromStream(strm, struct, maxsize);
  strm.Free;
end;

//==============================================================================
//
// TSerializer.LoadFromStream
//
//==============================================================================
function TSerializer.LoadFromStream(const strm: TDStream; const struct: Pointer; const maxsize: integer): integer;
var
  id: word;
  i: integer;
  oldpos, newpos: integer;
begin
  oldpos := strm.Position;
  for i := 0 to fnumitems - 1 do
    SetDefault(struct, @fitems[i]);
  while true do
  begin
    strm.Read(id, SizeOf(word));
    if id = 0 then
      Break;
    LoadItem(strm, struct, @fitems[id - 1]);
  end;
  newpos := strm.Position;
  result := newpos - oldpos;
end;

//==============================================================================
//
// TSerializer.ResolvePointers
//
//==============================================================================
procedure TSerializer.ResolvePointers(const struct: Pointer);
var
  i: integer;
  it: Pserializeitem_t;
  b: PByteArray;
begin
  b := struct;
  for i := 0 to fnumitems - 1 do
  begin
    it := @fitems[i];
    if it.typ = st_mobj then
      PPointer(@b[it.offs])^ := P_FindMobjFromKey(PLongWord(@b[it.offs])^)
    else if it.typ = st_player then
    begin
      if PLongWord(@b[it.offs])^ <> 0 then
        PPointer(@b[it.offs])^ := @players[PLongWord(@b[it.offs])^ - 1]
    end
    else if it.typ = st_state then
      PPointer(@b[it.offs])^ := @states[PLongWord(@b[it.offs])^];
  end;
end;

end.
