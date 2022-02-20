unit ide_project;

interface

uses
  Classes, SysUtils,
  ide_baseframe;

type
  projectitemtype_t = (pi_script, pi_compiledscript, pi_actor, pi_project, pi_none);

//==============================================================================
//
// projectitemtype2string
//
//==============================================================================
function projectitemtype2string(const x: projectitemtype_t): AnsiString;

//==============================================================================
//
// string2projectitemtype
//
//==============================================================================
function string2projectitemtype(const s: AnsiString): projectitemtype_t;

type
  TIDEProject = class;

  TIDEProjectItem = class(TBaseEditorObject)
  private
    fitemtype: projectitemtype_t;
    fparent: TIDEProject;
  protected
    function GetRelativePath: AnsiString; virtual;
    procedure DataToFields(const adata: AnsiString); override;
    procedure FieldsToData(var adata: AnsiString); override;
  public
    constructor Create(const aparent: TIDEProject; const afilename: AnsiString; atype: projectitemtype_t); reintroduce; virtual;
    property RelativePath: AnsiString read GetRelativePath;
    property ItemType: projectitemtype_t read fitemtype;
    property Parent: TIDEProject read fparent;
  end;

  projectquerymodifieditem_t = function (const x: TIDEProjectItem): Boolean of object;
  projectonchangegame_t = procedure (const newgame: string) of object;

  TIDEProject = class(TBaseEditorObject)
  private
    fList: TStringList;
    fGame: string;
  protected
    pendingrename: string;
    function GetCount: integer;
    procedure DoAdd(const fname: AnsiString; const typ: projectitemtype_t); virtual;
    procedure DataToFields(const adata: AnsiString); override;
    procedure FieldsToData(var adata: AnsiString); override;
    function GetItem(const x: Integer): TIDEProjectItem; virtual;
    function GetModified: boolean; override;
    function CanCloseItemQuery(const item: TIDEProjectItem): Boolean;
    procedure SetGame(const g: string); virtual;
    procedure ChangePath(const path: string); virtual;
  public
    projectquerymodifieditem: projectquerymodifieditem_t;
    projectonchangegame: projectonchangegame_t;
    function CanRename(const oldname: string): Boolean;
    function ExecRename(const newname: string): Boolean;
    function Rename(const oldname, newname: string): Boolean;
    constructor Create(afilename: AnsiString = ''); override;
    destructor Destroy; override;
    procedure Add(const fname: AnsiString; const typ: projectitemtype_t); virtual;
    procedure Clear; virtual;
    procedure Delete(const idx: integer); overload;
    procedure Delete(const s: string); overload;
    procedure Delete(const it: TIDEProjectItem); overload;
    function Find(const s: string): TIDEProjectItem;
    function LoadFromFile(const aname: AnsiString): Boolean; override;
    function SaveToFile(const aname: AnsiString): Boolean; override;
    function GeneratePK3(const aname: AnsiString): Boolean;
    property Count: integer read GetCount;
    property Game: string read fGame write SetGame;
    property Items[const x: Integer]: TIDEProjectItem read GetItem;
  end;

//==============================================================================
//
// DefItemName
//
//==============================================================================
function DefItemName(const s: string): string;

implementation

uses
  ide_utils, ide_version, ide_zipfile;

//==============================================================================
//
// projectitemtype2string
//
//==============================================================================
function projectitemtype2string(const x: projectitemtype_t): AnsiString;
begin
  case x of
    pi_script:
      Result := 'script';
    pi_compiledscript:
      Result := 'compiledscript';
    pi_actor:
      Result := 'actor';
    pi_project:
      Result := 'project';
  else
    Result := 'none';
  end;
end;

//==============================================================================
//
// string2projectitemtype
//
//==============================================================================
function string2projectitemtype(const s: AnsiString): projectitemtype_t;
begin
  if s = 'script' then
    Result := pi_script
  else if s = 'compiledscript' then
    Result := pi_compiledscript
  else if s = 'compiledscript' then
    Result := pi_compiledscript
  else if s = 'actor' then
    Result := pi_actor
  else if s = 'project' then
    Result := pi_project
  else
    Result := pi_none;
end;

//==============================================================================
//
// TIDEProjectItem.Create
//
//==============================================================================
constructor TIDEProjectItem.Create(const aparent: TIDEProject; const afilename: AnsiString; atype: projectitemtype_t);
begin
  Inherited Create(afilename);
  fparent := aparent;
  fitemtype := atype;
end;

//==============================================================================
//
// TIDEProjectItem.GetRelativePath
//
//==============================================================================
function TIDEProjectItem.GetRelativePath: AnsiString;
begin
  Result := ExtractRelativepath(fparent.FileName, FileName);
end;

//==============================================================================
//
// TIDEProjectItem.DataToFields
//
//==============================================================================
procedure TIDEProjectItem.DataToFields(const adata: AnsiString);
begin
  Inherited DataToFields(adata);
end;

//==============================================================================
//
// TIDEProjectItem.FieldsToData
//
//==============================================================================
procedure TIDEProjectItem.FieldsToData(var adata: AnsiString);
begin
  Inherited FieldsToData(adata);
end;

//==============================================================================
//
// TIDEProject.Create
//
//==============================================================================
constructor TIDEProject.Create(afilename: AnsiString = '');
begin
  fList := TStringList.Create;
  pendingrename := '';
  projectquerymodifieditem := nil;
  projectonchangegame := nil;
  Game := 'doom';
  Inherited Create(afilename);
end;

//==============================================================================
//
// TIDEProject.Destroy
//
//==============================================================================
destructor TIDEProject.Destroy;
var
  i: integer;
begin
  for i := 0 to fList.Count - 1 do
    fList.Objects[i].Free;
  fList.Free;
  Inherited;
end;

//==============================================================================
//
// TIDEProject.GetCount
//
//==============================================================================
function TIDEProject.GetCount: integer;
begin
  Result := fList.Count;
end;

//==============================================================================
//
// TIDEProject.DoAdd
//
//==============================================================================
procedure TIDEProject.DoAdd(const fname: AnsiString; const typ: projectitemtype_t);
var
  pi: TIDEProjectItem;
begin
  pi := TIDEProjectItem.Create(self, fname, typ);
  fList.AddObject(pi.GetRelativePath, pi);
end;

//==============================================================================
//
// TIDEProject.Add
//
//==============================================================================
procedure TIDEProject.Add(const fname: AnsiString; const typ: projectitemtype_t);
begin
  if Find(fname) = nil then
  begin
    DoAdd(fname, typ);
    GetData;
    Modified := True;
  end;
end;

//==============================================================================
//
// TIDEProject.Clear
//
//==============================================================================
procedure TIDEProject.Clear;
var
  i: integer;
begin
  for i := 0 to fList.Count - 1 do
    fList.Objects[i].Free;
  fList.Clear;
end;

//==============================================================================
//
// TIDEProject.Delete
//
//==============================================================================
procedure TIDEProject.Delete(const idx: integer);
begin
  if idx < 0 then
    Exit;
  if idx >= fList.Count then
    Exit;

  fList.Objects[idx].Free;
  fList.Delete(idx);
end;

//==============================================================================
//
// TIDEProject.Delete
//
//==============================================================================
procedure TIDEProject.Delete(const s: string);
var
  idx: integer;
  item: TIDEProjectItem;
begin
  item := Find(s);
  if item = nil then
    Exit;

  idx := fList.IndexOfObject(item);
  Delete(idx);
end;

//==============================================================================
//
// TIDEProject.Delete
//
//==============================================================================
procedure TIDEProject.Delete(const it: TIDEProjectItem);
var
  idx: integer;
begin
  if it = nil then
    Exit;

  idx := fList.IndexOfObject(it);
  Delete(idx);
end;

//==============================================================================
//
// TIDEProject.Find
//
//==============================================================================
function TIDEProject.Find(const s: string): TIDEProjectItem;
var
  i: integer;
  item: TIDEProjectItem;
  s2: string;
begin
  s2 := ExtractRelativepath(FileName, s);
  for i := 0 to fList.Count - 1 do
  begin
    item := fList.Objects[i] as TIDEProjectItem;
    if item <> nil then
    begin
      if AnsiCompareText(item.RelativePath, s2) = 0 then
      begin
        Result := item;
        Exit;
      end;
    end;
  end;

  for i := 0 to fList.Count - 1 do
  begin
    item := fList.Objects[i] as TIDEProjectItem;
    if item <> nil then
    begin
      if AnsiCompareText(item.FileName, s) = 0 then
      begin
        Result := item;
        Exit;
      end;
    end;
  end;

  s2 := ExpandFileName(s);
  for i := 0 to fList.Count - 1 do
  begin
    item := fList.Objects[i] as TIDEProjectItem;
    if item <> nil then
    begin
      if AnsiCompareText(ExpandFileName(item.FileName), s2) = 0 then
      begin
        Result := item;
        Exit;
      end;
    end;
  end;

  Result := nil;
end;

//==============================================================================
//
// TIDEProject.DataToFields
//
//==============================================================================
procedure TIDEProject.DataToFields(const adata: AnsiString);
var
  s: TStringList;
  i: integer;
  typ, path: string;
begin
  Clear;
  s := TStringList.Create;
  try
    s.Text := adata;
    if s.Count > 0 then
    begin
      splitstring(s.Strings[0], typ, path, '|');
      if AnsiCompareText(typ, 'version') = 0 then
      begin
        if path = IDEVERSION then
          for i := 1 to s.Count - 1 do
          begin
            splitstring(s.Strings[i], typ, path, '|');
            if AnsiCompareText(typ, 'game') = 0 then
              Game := LowerCase(path)
            else
              Add(path, string2projectitemtype(typ));
          end;
      end;
    end;
  finally
    s.Free;
  end;

  Inherited DataToFields(adata);
end;

//==============================================================================
//
// TIDEProject.FieldsToData
//
//==============================================================================
procedure TIDEProject.FieldsToData(var adata: AnsiString);
var
  s: TStringList;
  i: integer;
  item: TIDEProjectItem;
begin
  s := TStringList.Create;
  try
    s.Add('version|' + IDEVERSION);
    s.Add('game|' + fgame);
    for i := 0 to fList.Count - 1 do
    begin
      item := fList.Objects[i] as TIDEProjectItem;
      if item <> nil then
        s.Add(projectitemtype2string(item.ItemType) + '|' + item.GetRelativePath);
    end;
    adata := s.Text;
  finally
    s.Free;
  end;
end;

//==============================================================================
//
// TIDEProject.ChangePath
//
//==============================================================================
procedure TIDEProject.ChangePath(const path: string);
begin
  if path <> '' then
  begin
    try
      ChDir(path);
    except
      LogOutput(LOG_WARNING,
        'Can not access path "%s"'#13#10,
          [path]);
    end;
  end;
end;

//==============================================================================
//
// TIDEProject.LoadFromFile
//
//==============================================================================
function TIDEProject.LoadFromFile(const aname: AnsiString): Boolean;
begin
  ChangePath(ExtractFilePath(aname));
  Result := inherited LoadFromFile(aname);
end;

//==============================================================================
//
// TIDEProject.GetItem
//
//==============================================================================
function TIDEProject.GetItem(const x: Integer): TIDEProjectItem;
begin
  if (x < 0) or (x >= fList.Count) then
  begin
    Result := nil;
    Exit;
  end;

  Result := fList.Objects[x] as TIDEProjectItem;
end;

//==============================================================================
//
// TIDEProject.GetModified
//
//==============================================================================
function TIDEProject.GetModified: boolean;
var
  i: integer;
  item: TIDEProjectItem;
begin
  Result := inherited GetModified;
  if Result then
    Exit;

  for i := 0 to fList.Count - 1 do
  begin
    item := fList.Objects[i] as TIDEProjectItem;
    if item <> nil then
    begin
      Result := item.Modified;
      if Result then
        Exit;
    end;
  end;
end;

//==============================================================================
//
// TIDEProject.SaveToFile
//
//==============================================================================
function TIDEProject.SaveToFile(const aname: AnsiString): Boolean;
var
  i: integer;
  item: TIDEProjectItem;
  ret: Boolean;
begin
  ChangePath(ExtractFilePath(aname));
  Result := inherited SaveToFile(aname);

  for i := 0 to fList.Count - 1 do
  begin
    item := fList.Objects[i] as TIDEProjectItem;
    if item <> nil then
    begin
      ret := item.SaveToFile(item.FileName);
      Result := Result and ret;
    end;
  end;
end;

//==============================================================================
//
// TIDEProject.GeneratePK3
//
//==============================================================================
function TIDEProject.GeneratePK3(const aname: AnsiString): Boolean;
var
  i: integer;
  id: integer;
  actordef: string;
  pk3: TZipFile;
  item: TIDEProjectItem;
begin
  Result := True;
  try
    pk3 := TZipFile.Create;
    try
      actordef := '';
      for i := 0 to fList.Count - 1 do
      begin
        item := fList.Objects[i] as TIDEProjectItem;
        if item.ItemType = pi_actor then
          actordef := actordef + item.data + #13#10
        else
        begin
          if item.ItemType = pi_compiledscript then
            item.LoadFromFile(item.FileName);
          id := pk3.AddFile(item.ShortFileName);
          pk3.Data[id] := item.data;
          pk3.DateTime[id] := Now();
        end;
      end;
      if actordef <> '' then
      begin
        id := pk3.AddFile('ACTORDEF.TXT');
        pk3.Data[id] := actordef;
        pk3.DateTime[id] := Now();
      end;
      pk3.SaveToFile(aname);
    finally
      pk3.Free;
    end;
  except
    Result := False;
  end;
  if not Result then
    LogOutput(LOG_ERROR,
      'Can not generate file "%s"'#13#10, [aname])
  else
    LogOutput(LOG_NORMAL,
      'Created PK3 file "%s"'#13#10, [aname]);
end;

//==============================================================================
//
// TIDEProject.CanRename
//
//==============================================================================
function TIDEProject.CanRename(const oldname: string): Boolean;
begin
  Result := Find(oldname) <> nil;
  if Result then
    pendingrename := oldname
  else
    pendingrename := '';
end;

//==============================================================================
//
// TIDEProject.ExecRename
//
//==============================================================================
function TIDEProject.ExecRename(const newname: string): Boolean;
var
  item: TIDEProjectItem;
  item2: TIDEProjectItem;
  oldname: string;
begin
  if newname = '' then
  begin
    Result := False;
    Exit;
  end;

  oldname := pendingrename;
  pendingrename := '';

  if oldname = '' then
  begin
    Result := False;
    Exit;
  end;

  item  := Find(oldname);
  Result := item <> nil;
  if not Result then
    Exit;

  item2  := Find(newname);
  if item2 <> nil then
  begin
    LogOutput(LOG_ERROR,
      'TIDEProject.ExecRename(): Can not rename "%s" to "%s". "%s" already in project'#13#10,
        [oldname, newname, newname]);
    Result := False;
    Exit;
  end;

  if FileExists(newname) then
  begin
    if CanCloseItemQuery(item) then
    begin
      Result := item.LoadFromFile(newname);
      if Result then
        LogOutput(LOG_INFO,
          'File "%s" replaced with existing file "%s"'#13#10,
            [oldname, newname]);
    end
    else
    begin
      LogOutput(LOG_WARNING,
        'Please save "%s" before replacing it with file "%s"'#13#10,
          [oldname, newname]);
      Result := False;
    end;
  end
  else
  begin
    Result := item.SaveToFile(newname);
    if Result then
      LogOutput(LOG_INFO,
        'File "%s" saved in new file "%s"'#13#10,
          [oldname, newname]);
  end;

  DoAdjustLayout;
end;

//==============================================================================
//
// TIDEProject.Rename
//
//==============================================================================
function TIDEProject.Rename(const oldname, newname: string): Boolean;
begin
  Result := CanRename(oldname);
  if Result then
    Result := ExecRename(newname);
end;

//==============================================================================
//
// TIDEProject.CanCloseItemQuery
//
//==============================================================================
function TIDEProject.CanCloseItemQuery(const item: TIDEProjectItem): Boolean;
begin
  Result := True;
  if item.Modified then
    if Assigned(projectquerymodifieditem) then
      Result := projectquerymodifieditem(item);
end;

//==============================================================================
//
// TIDEProject.SetGame
//
//==============================================================================
procedure TIDEProject.SetGame(const g: string);
var
  gU: string;
begin
  gU := LowerCase(Trim(g));
  if (gU = 'doom') or (gU = 'heretic') or (gU = 'hexen') or (gU = 'strife') or (gU = 'radix') or (gU = 'mars') then
    fGame := gU
  else
    LogOutput(LOG_WARNING, 'Unknown game type "%s"'#13#10, [g]);
  if Assigned(projectonchangegame) then
    projectonchangegame(gU);
end;

//==============================================================================
//
// DefItemName
//
//==============================================================================
function DefItemName(const s: string): string;
begin
  if s = '' then
    Result := 'Untitled'
  else
    Result := s;
end;

end.

