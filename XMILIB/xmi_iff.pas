unit xmi_iff;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

type
  TChunk = record
    Offs: cardinal;
    Name, Sub: ansistring;
    DataOffs, Size: cardinal;
  end;
  IFFNodes = array of TChunk;

//==============================================================================
// IFFGetNodes
//
// Tree types:
// IFF   - Electronic Arts IFF/FORM chunk tree structure
// OIFF  - IFF with possible odd chunk sizes
// RIFF  - Microsoft RIFF chunk tree structure
// ORIFF - RIFF with possible odd chunk sizes
//
//==============================================================================
function IFFGetNodes(Tree: ansistring; P: Pointer; Offs, MaxSz: cardinal;
  var Nodes: IFFNodes): integer;

//==============================================================================
//
// IFFSearchNode
//
//==============================================================================
function IFFSearchNode(Nodes: IFFNodes; var Found: IFFNodes; Name: ansistring;
  Sub: ansistring = ''): integer; overload;

//==============================================================================
//
// IFFSearchNode
//
//==============================================================================
function IFFSearchNode(Nodes: IFFNodes; Name: ansistring;
  Sub: ansistring = ''): integer; overload;

implementation

//==============================================================================
//
// CheckName
//
//==============================================================================
function CheckName(Name: ansistring): boolean;
begin
  Result := False;
  if Length(Name) <> 4 then
    Exit;
  if (byte(Name[1]) < $20) or (byte(Name[1]) > $7E) then
    Exit;
  if (byte(Name[2]) < $20) or (byte(Name[2]) > $7E) then
    Exit;
  if (byte(Name[3]) < $20) or (byte(Name[3]) > $7E) then
    Exit;
  if (byte(Name[4]) < $20) or (byte(Name[4]) > $7E) then
    Exit;
  Result := True;
end;

//==============================================================================
//
// IFFGetNodes
//
//==============================================================================
function IFFGetNodes(Tree: ansistring; P: Pointer; Offs, MaxSz: cardinal;
  var Nodes: IFFNodes): integer;
var
  Pos: cardinal;
  LastName: ansistring;
  LastSize: cardinal;
  IsSub: boolean;

  function GetByte(): byte;
  begin
    if Pos < MaxSz then
      Result := PByte(cardinal(P) + Pos)^
    else
      Result := 0;
    Inc(Pos);
  end;

  function GetName(): ansistring;
  var
    Buf: array[0..3] of AnsiChar;
  begin
    Buf[0] := AnsiChar(GetByte());
    Buf[1] := AnsiChar(GetByte());
    Buf[2] := AnsiChar(GetByte());
    Buf[3] := AnsiChar(GetByte());
    Result := Buf;
  end;

  function GetSize(): cardinal;
  var
    Buf: array[0..3] of byte;
  begin
    if (Tree = 'RIFF') or (Tree = 'ORIFF') then
    begin
      Buf[0] := GetByte();
      Buf[1] := GetByte();
      Buf[2] := GetByte();
      Buf[3] := GetByte();
    end
    else
    begin
      Buf[3] := GetByte();
      Buf[2] := GetByte();
      Buf[1] := GetByte();
      Buf[0] := GetByte();
    end;
    Result := cardinal(Buf);
  end;

begin
  if Offs > 0 then
    P := Pointer(cardinal(P) + Offs);
  Result := 0;
  Pos := 0;
  while Pos < MaxSz do
  begin
    LastName := GetName();
    if not CheckName(LastName) then
      Break;
    LastSize := GetSize();
    Inc(Result);
    SetLength(Nodes, Result);
    Nodes[Result - 1].Offs := Offs + Pos - 8;
    Nodes[Result - 1].Name := LastName;
    IsSub := False;
    if LastSize >= 4 then
    begin
      if (Tree = 'RIFF') or (Tree = 'ORIFF') then
        IsSub := (LastName = 'RIFF') or (LastName = 'LIST');
      if (Tree = 'IFF') or (Tree = 'OIFF') then
        IsSub := (LastName = 'FORM') or (LastName = 'FOR1') or
          (LastName = 'FOR2') or (LastName = 'FOR3') or (LastName = 'FOR4') or
          (LastName = 'FOR5') or (LastName = 'FOR6') or (LastName = 'FOR7') or
          (LastName = 'FOR8') or (LastName = 'FOR9') or (LastName = 'CAT ') or
          (LastName = 'CAT1') or (LastName = 'CAT2') or (LastName = 'CAT3') or
          (LastName = 'CAT4') or (LastName = 'CAT5') or (LastName = 'CAT6') or
          (LastName = 'CAT7') or (LastName = 'CAT8') or (LastName = 'CAT9') or
          (LastName = 'LIST') or (LastName = 'LIS1') or (LastName = 'LIS2') or
          (LastName = 'LIS3') or (LastName = 'LIS4') or (LastName = 'LIS5') or
          (LastName = 'LIS6') or (LastName = 'LIS7') or (LastName = 'LIS8') or
          (LastName = 'LIS9') or (LastName = 'PROP');
    end;
    if IsSub then
    begin
      Nodes[Result - 1].Sub := GetName();
      Nodes[Result - 1].DataOffs := Offs + Pos;
      Nodes[Result - 1].Size := LastSize - 4;
      Pos := Pos + LastSize - 4;
      Continue;
    end;
    Nodes[Result - 1].Sub := '';
    Nodes[Result - 1].DataOffs := Offs + Pos;
    Nodes[Result - 1].Size := LastSize;
    if (Tree <> 'OIFF') and (Tree <> 'ORIFF') and Odd(LastSize) then
      Pos := Pos + LastSize + 1
    else
      Pos := Pos + LastSize;
  end;
end;

//==============================================================================
//
// IFFSearchNode
//
//==============================================================================
function IFFSearchNode(Nodes: IFFNodes; var Found: IFFNodes; Name: ansistring;
  Sub: ansistring = ''): integer; overload;
var
  I: integer;
begin
  Result := 0;
  SetLength(Found, 0);
  for I := 0 to Length(Nodes) - 1 do
    if ((Nodes[I].Name = Name) and (Sub = '')) or
      ((Nodes[I].Name = Name) and (Sub <> '') and (Nodes[I].Sub = Sub)) then
    begin
      Inc(Result);
      SetLength(Found, Result);
      Found[Result - 1] := Nodes[I];
    end;
end;

//==============================================================================
//
// IFFSearchNode
//
//==============================================================================
function IFFSearchNode(Nodes: IFFNodes; Name: ansistring;
  Sub: ansistring = ''): integer; overload;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to Length(Nodes) - 1 do
    if ((Nodes[I].Name = Name) and (Sub = '')) or
      ((Nodes[I].Name = Name) and (Sub <> '') and (Nodes[I].Sub = Sub)) then
      Inc(Result);
end;

end.

