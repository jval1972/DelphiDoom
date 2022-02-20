unit frm_actordef;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, StdCtrls, Buttons, ExtCtrls,
  SynEdit, SynHighlighterActorDef;

type
  TActorDefFuncExampleSynEdit = class(TSynEdit)
  private
    ffuncname: string;
    function GetSaveFileName(const fn: string): string;
    procedure SetFuncName(const fn: string);
  public
    property funcname: string read ffuncname write SetFuncName;
    procedure DoSave;
  end;

type
  actordeffuncinfo_t = record
    actordeffuncname: string[64];
    actordeffuncdecl: string[255];
  end;
  actordeffuncinfo_p = ^actordeffuncinfo_t;
  actordeffuncinfo_a = array[0..$FFF] of actordeffuncinfo_t;
  actordeffuncinfo_pa = ^actordeffuncinfo_a;

type
  TFrame_Actordef = class(TFrame)
    ToolbarPanel: TPanel;
    EditorPanel: TPanel;
    Splitter1: TSplitter;
    DetailPanel: TPanel;
    Panel1: TPanel;
    ListView1: TListView;
    SearchEdit: TEdit;
    ClearFilterSpeedButton: TSpeedButton;
    DeclPanel: TPanel;
    DeclEdit: TEdit;
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView1Compare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
    procedure SearchEditChange(Sender: TObject);
    procedure ClearFilterSpeedButtonClick(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure DeclPanelResize(Sender: TObject);
  private
    { Private declarations }
    actordeffuncs: actordeffuncinfo_pa;
    numactordeffuncs: integer;
    fgame: string;
    sortcolumn: integer;
    procedure ClearActorDefFuncs;
    procedure ResizeActorDefFuncs(const sz: Integer);
    procedure OnExampleChange(Sender: TObject);
  protected
    SynEdit1: TActorDefFuncExampleSynEdit;
    SynPasSyn1: TSynActordefSyn;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FillListView;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateGameControls(const game: string);
    procedure FocusAndSelectFirstItem;
  end;

implementation

{$R *.dfm}

uses
  ddc_base, ide_utils;

//==============================================================================
//
// TActorDefFuncExampleSynEdit.GetSaveFileName
//
//==============================================================================
function TActorDefFuncExampleSynEdit.GetSaveFileName(const fn: string): string;
var
  base: string;
begin
  base := ExtractFilePath(ParamStr(0));
  base := base + '\actordeffuncs\';
  if not DirectoryExists(base) then
    MkDir(base);
  Result := base + fn + '.txt';
end;

//==============================================================================
//
// TActorDefFuncExampleSynEdit.SetFuncName
//
//==============================================================================
procedure TActorDefFuncExampleSynEdit.SetFuncName(const fn: string);
var
  lst: TStringList;
begin
  if ffuncname <> fn then
  begin
    if not ReadOnly then
    begin
      if ffuncname <> '' then
      begin
        lst := TStringList.Create;
        lst.Text := Text;
        lst.SaveToFile(GetSaveFileName(ffuncname));
        lst.Free;
      end;
    end;
    lst := TStringList.Create;
    if FileExists(GetSaveFileName(fn)) then
      lst.LoadFromFile(GetSaveFileName(fn));
    Text := lst.Text;
    lst.Free;
    ffuncname := fn;
  end;
end;

//==============================================================================
//
// TActorDefFuncExampleSynEdit.DoSave
//
//==============================================================================
procedure TActorDefFuncExampleSynEdit.DoSave;
var
  lst: TStringList;
begin
  if not ReadOnly then
    if ffuncname <> '' then
    begin
      lst := TStringList.Create;
      lst.Text := Text;
      lst.SaveToFile(GetSaveFileName(ffuncname));
      lst.Free;
    end;
end;

//==============================================================================
//
// TFrame_Actordef.Create
//
//==============================================================================
constructor TFrame_Actordef.Create(AOwner: TComponent);
begin
  inherited;

  fgame := '';

  SynEdit1 := TActorDefFuncExampleSynEdit.Create(Self);
  SynEdit1.Parent := DetailPanel;
  SynEdit1.Align := alClient;
  SynEdit1.Highlighter := nil;
  SynEdit1.OnChange := OnExampleChange;
  SynEdit1.Gutter.ShowLineNumbers := True;
  SynEdit1.Gutter.AutoSize := True;
  SynEdit1.MaxScrollWidth := 255;
  SynEdit1.WantTabs := True;
  SynEdit1.funcname := '';

  SynPasSyn1 := TSynActordefSyn.Create(Self);

  SynEdit1.Highlighter := SynPasSyn1;
  SynEdit1.ReadOnly := CheckParm('-devparm') <= 0;
end;

//==============================================================================
//
// TFrame_Actordef.CreateParams
//
//==============================================================================
procedure TFrame_Actordef.CreateParams(var Params: TCreateParams);
begin
  sortcolumn := 0;
  Inherited;
end;

//==============================================================================
//
// TFrame_Actordef.Destroy
//
//==============================================================================
destructor TFrame_Actordef.Destroy;
begin
  ClearActorDefFuncs;
  inherited;
end;

//==============================================================================
//
// TFrame_Actordef.OnExampleChange
//
//==============================================================================
procedure TFrame_Actordef.OnExampleChange(Sender: TObject);
begin
  if SynEdit1.Modified then
    SynEdit1.DoSave;
end;

//==============================================================================
//
// TFrame_Actordef.ClearActorDefFuncs
//
//==============================================================================
procedure TFrame_Actordef.ClearActorDefFuncs;
begin
  FreeMem(actordeffuncs);
  numactordeffuncs := 0;
end;

//==============================================================================
//
// TFrame_Actordef.ResizeActorDefFuncs
//
//==============================================================================
procedure TFrame_Actordef.ResizeActorDefFuncs(const sz: Integer);
begin
  ReallocMem(actordeffuncs, sz * SizeOf(actordeffuncinfo_t));
  numactordeffuncs := sz;
end;

//==============================================================================
//
// TFrame_Actordef.FocusAndSelectFirstItem
//
//==============================================================================
procedure TFrame_Actordef.FocusAndSelectFirstItem;
begin
  if ListView1.Items.Count > 0 then
  begin
    ListView1.Selected := ListView1.Items[0];
    ListView1.ItemFocused := ListView1.Selected;
  end;
end;

//==============================================================================
//
// TFrame_Actordef.UpdateGameControls
//
//==============================================================================
procedure TFrame_Actordef.UpdateGameControls(const game: string);
var
  i, j: integer;
  dllret: TStringList;
  s1, s2: string;
begin
  if fgame = LowerCase(game) then
    Exit;

  fgame := LowerCase(game);

  dllret := dll_getactordeffunctions(fgame);

  if dllret = nil then
    Exit;

  ResizeActorDefFuncs(dllret.Count);

  j := 0;
  for i := 0 to dllret.Count - 1 do
  begin
    splitstring(dllret.Strings[i], s1, s2, '(');
    if Pos('A_', UpperCase(s1)) = 1 then
    begin
      s1[1] := ' ';
      s1[2] := ' ';
    end;
    s1 := Trim(s1);
    if s1 <> '' then
    begin
      actordeffuncs[j].actordeffuncname := s1;
      actordeffuncs[j].actordeffuncdecl := dllret.Strings[i];
      inc(j);
    end;
  end;

  ResizeActorDefFuncs(j);

  dllret.Free;

  FillListView;
end;

//==============================================================================
//
// TFrame_Actordef.FillListView
//
//==============================================================================
procedure TFrame_Actordef.FillListView;

  procedure AddListItem(Info: actordeffuncinfo_p);
  var
    ListItem: TListItem;
  begin
    ListItem := ListView1.Items.Add;
    ListItem.Caption := Info.actordeffuncname;
    ListItem.SubItems.Add(Info.actordeffuncdecl);
    ListItem.Data := Info;
  end;

var
  i: integer;
  srch: string;
begin
  ListView1.Items.BeginUpdate;
  try
    ListView1.Items.Clear;

    srch := LowerCase(Trim(SearchEdit.Text));

    for i := 0 to numactordeffuncs - 1 do
    begin
      if Length(srch) = 0 then
        AddListItem(@actordeffuncs[i])
      else if Pos(srch, LowerCase(actordeffuncs[i].actordeffuncname)) > 0 then
        AddListItem(@actordeffuncs[i]);
    end;

  finally
    ListView1.AlphaSort;
    ListView1.Items.EndUpdate;
    FocusAndSelectFirstItem;
  end;
end;

//==============================================================================
//
// TFrame_Actordef.ListView1Data
//
//==============================================================================
procedure TFrame_Actordef.ListView1Data(Sender: TObject;
  Item: TListItem);
begin
  //
end;

//==============================================================================
//
// TFrame_Actordef.ListView1Compare
//
//==============================================================================
procedure TFrame_Actordef.ListView1Compare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  value1, value2: string;
begin
  if sortcolumn = 0 then
  begin
    value1 := Item1.Caption;
    value2 := Item2.Caption;
  end
  else
  begin
    value1 := Item1.SubItems[sortcolumn - 1];
    value2 := Item2.SubItems[sortcolumn - 1];
  end;
  Compare := AnsiCompareText(value1, value2);
end;

//==============================================================================
//
// TFrame_Actordef.ListView1ColumnClick
//
//==============================================================================
procedure TFrame_Actordef.ListView1ColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  sortcolumn := Column.Index;
  FillListView;
end;

//==============================================================================
//
// TFrame_Actordef.SearchEditChange
//
//==============================================================================
procedure TFrame_Actordef.SearchEditChange(Sender: TObject);
begin
  FillListView;
  ClearFilterSpeedButton.Visible := SearchEdit.Text <> '';
end;

procedure TFrame_Actordef.ClearFilterSpeedButtonClick(
  Sender: TObject);
begin
  SearchEdit.Clear;
end;

//==============================================================================
//
// TFrame_Actordef.ListView1Change
//
//==============================================================================
procedure TFrame_Actordef.ListView1Change(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
  inf: actordeffuncinfo_p;
begin
  if ListView1.Selected <> nil then
  begin
    inf := ListView1.Selected.Data;
    DeclEdit.Text := inf.actordeffuncdecl;
    DeclEdit.Hint := DeclEdit.Text;
    DeclPanel.Hint := DeclEdit.Text;
    SynEdit1.funcname := inf.actordeffuncname;
  end;
end;

//==============================================================================
//
// TFrame_Actordef.DeclPanelResize
//
//==============================================================================
procedure TFrame_Actordef.DeclPanelResize(Sender: TObject);
begin
  DeclEdit.Width := DeclPanel.Width - 16;
end;

end.
