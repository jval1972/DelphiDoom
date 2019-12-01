unit ide_baseframe;

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls,
  ide_undo;

type
  { TFrame_ScriptEditor }

  TBaseEditorObject = class(TObject)
  private
    { private declarations }
    undoManager: TUndoRedoManager;
    fdata: AnsiString;
    ffilename: AnsiString;
    fmodified: boolean;
    fEditor: TFrame;
  protected
    function GetData: AnsiString; virtual;
    procedure SetData(const adata: AnsiString); virtual;
    procedure DoLoadFromStream(s: TStream); virtual;
    procedure DoSaveToStream(s: TStream); virtual;
    procedure DoAdjustLayout; virtual;
    procedure DataToFields(const adata: AnsiString); virtual;
    procedure FieldsToData(var adata: AnsiString); virtual;
    procedure SetEditor(const ed: TFrame); virtual;
    function GetModified: boolean; virtual;
  public
    constructor Create(afilename: AnsiString = ''); virtual;
    destructor Destroy; override;
    function LoadFromFile(const aname: AnsiString): Boolean; virtual;
    function SaveToFile(const aname: AnsiString): Boolean; virtual;
    function CanUndo: boolean;
    function CanRedo: boolean;
    procedure Undo; virtual;
    procedure Redo; virtual;
    procedure SaveUndo; virtual;
    procedure AdjustLayout(const adata: AnsiString); virtual;
    procedure Edit(const aEditor: TFrame); virtual;
    function ShortFileName: string;
  published
    property data: AnsiString read GetData write SetData;
    property FileName: AnsiString read ffilename;
    property Modified: boolean read GetModified write fmodified;
    property Editor: TFrame read fEditor write SetEditor;
    { public declarations }
  end;

implementation

uses
  ide_utils,
  frm_projectmanager, frm_scripteditor;

constructor TBaseEditorObject.Create(afilename: AnsiString = '');
begin
  Inherited Create;
  ffilename := ExpandFileName(afilename);
  fdata := '';
  fEditor := nil;
  fmodified := False;
  undoManager := TUndoRedoManager.Create;
  undoManager.OnLoadFromStream := DoLoadFromStream;
  undoManager.OnSaveToStream := DoSaveToStream;
  if ffilename <> '' then
    LoadFromFile(ffilename);
end;

destructor TBaseEditorObject.Destroy;
begin
  undoManager.Free;
  inherited;
end;

function TBaseEditorObject.GetData: string;
begin
  FieldsToData(fdata);
  Result := fdata;
end;

procedure TBaseEditorObject.SetData(const adata: AnsiString);
begin
  if fdata <> adata then
  begin
    fdata := adata;
    fmodified := True;
    DataToFields(adata);
  end;
end;

function TBaseEditorObject.LoadFromFile(const aname: AnsiString): Boolean;
var
  f: TFileStream;
  oldname: string;
begin
  oldname := ffilename;
  ffilename := ExpandFileName(aname);
  if not FileExists(ffilename) then
  begin
    try
      f := TFileStream.Create(ffilename, fmCreate or fmShareExclusive);
      f.Free;
    except
      LogOutput(LOG_ERROR,
        'Can not create file "%s"'#13#10, [ffilename]);
      ffilename := ExpandFileName(oldname);
      Result := False;
      Exit;
    end;
  end;
  try
    f := TFileStream.Create(ffilename, fmOpenRead or fmShareDenyWrite);
    DoLoadFromStream(f);
    f.Free;
  except
    LogOutput(LOG_ERROR,
      'Can not read file "%s"'#13#10, [ffilename]);
    ffilename := ExpandFileName(oldname);
    Result := False;
    Exit;
  end;
  undoManager.Clear;
  fmodified := False;
  Result := True;
  LogOutput(LOG_NORMAL,
      'Loading "%s"'#13#10, [ffilename]);
end;

function TBaseEditorObject.SaveToFile(const aname: AnsiString): Boolean;
var
  f: TFileStream;
  oldname: string;
begin
  oldname := ffilename;
  ffilename := ExpandFileName(aname);
  try
    BackupFile(aname);
    f := TFileStream.Create(ffilename, fmCreate or fmShareExclusive);
    DoSaveToStream(f);
    f.Free;
  except
    LogOutput(LOG_ERROR,
      'Can not write file "%s"'#13#10, [ffilename]);
    ffilename := ExpandFileName(oldname);
    Result := False;
    Exit;
  end;
  fmodified := False;
  Result := True;
  LogOutput(LOG_NORMAL,
      'Saving "%s"'#13#10, [ffilename]);
end;

procedure TBaseEditorObject.DoLoadFromStream(s: TStream);
var
  i, len: integer;
begin
  len := s.Size;
  SetLength(fdata, len);
  s.Position := 0;
  for i := 1 to len do
    s.Read(fdata[i], 1);
  DataToFields(fdata);
  DoAdjustLayout;
end;

procedure TBaseEditorObject.DoSaveToStream(s: TStream);
var
  i, len: integer;
begin
  FieldsToData(fdata);
  len := Length(fdata);
  for i := 1 to len do
    s.Write(fdata[i], 1);
end;

function TBaseEditorObject.CanUndo: boolean;
begin
  Result := undoManager.CanUndo;
end;

function TBaseEditorObject.CanRedo: boolean;
begin
  Result := undoManager.CanRedo;
end;

procedure TBaseEditorObject.Undo;
begin
  undoManager.Undo;
end;

procedure TBaseEditorObject.Redo;
begin
  undoManager.Redo;
end;

procedure TBaseEditorObject.DoAdjustLayout;
begin
  if fEditor <> nil then
  begin
    if fEditor is TFrame_ProjectManager then
    begin
      (fEditor as TFrame_ProjectManager).obj := self;
      (fEditor as TFrame_ProjectManager).AdjustLayout(GetData)
    end
    else if (fEditor is TFrame_ScriptEditor) then
    begin
      (fEditor as TFrame_ScriptEditor).obj := self;
      (fEditor as TFrame_ScriptEditor).AdjustLayout(GetData);
    end;
  end;
end;

procedure TBaseEditorObject.AdjustLayout(const adata: AnsiString);
begin
  SetData(adata);
  DoAdjustLayout;
end;

procedure TBaseEditorObject.SaveUndo;
begin
  undoManager.SaveUndo;
end;

procedure TBaseEditorObject.Edit(const aEditor: TFrame);
begin
  fEditor := aEditor;
  if fEditor <> nil then
  begin
    if (fEditor is TFrame_ProjectManager) then
    begin
      (fEditor as TFrame_ProjectManager).obj := self;
      (fEditor as TFrame_ProjectManager).AdjustLayout(fdata);
    end
    else if (fEditor is TFrame_ScriptEditor) then
    begin
      (fEditor as TFrame_ScriptEditor).obj := self;
      (fEditor as TFrame_ScriptEditor).AdjustLayout(fdata);
    end;
  end;
end;

procedure TBaseEditorObject.DataToFields(const adata: AnsiString);
begin

end;

procedure TBaseEditorObject.FieldsToData(var adata: AnsiString);
begin

end;

function TBaseEditorObject.ShortFileName: string;
begin
  Result := ExtractFileName(ffilename);
end;

procedure TBaseEditorObject.SetEditor(const ed: TFrame);
begin
  Edit(ed);
end;

function TBaseEditorObject.GetModified: boolean;
begin
  Result := fmodified;
end;

end.

