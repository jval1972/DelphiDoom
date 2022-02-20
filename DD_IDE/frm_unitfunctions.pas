unit frm_unitfunctions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, StdCtrls, Buttons, ExtCtrls,
  SynEdit, SynHighlighterDDScript;

type
  TFuncExampleSynEdit = class(TSynEdit)
  private
    ffuncname: string;
    function GetSaveFileName(const fn: string): string;
    procedure SetFuncName(const fn: string);
  public
    property funcname: string read ffuncname write SetFuncName;
    procedure DoSave;
  end;

type
  funcinfo_t = record
    declaration: string[255];
    funcname: string[32];
    functype: string[20];
    unitname: string[64];
  end;
  funcinfo_p = ^funcinfo_t;
  funcinfo_a = array[0..$FFF] of funcinfo_t;
  funcinfo_pa = ^funcinfo_a;

type
  TFrame_UnitFunctions = class(TFrame)
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
    funcs: funcinfo_pa;
    numfuncs: integer;
    fgame: string;
    sortcolumn: integer;
    procedure ClearFuncs;
    procedure ResizeFuncs(const sz: Integer);
    procedure OnExampleChange(Sender: TObject);
  protected
    SynEdit1: TFuncExampleSynEdit;
    SynPasSyn1: TSynDDScriptSyn;
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
// TFuncExampleSynEdit.GetSaveFileName
//
//==============================================================================
function TFuncExampleSynEdit.GetSaveFileName(const fn: string): string;
var
  base: string;
begin
  base := ExtractFilePath(ParamStr(0));
  base := base + '\funcs\';
  if not DirectoryExists(base) then
    MkDir(base);
  Result := base + fn + '.ddscript';
end;

//==============================================================================
//
// TFuncExampleSynEdit.SetFuncName
//
//==============================================================================
procedure TFuncExampleSynEdit.SetFuncName(const fn: string);
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
// TFuncExampleSynEdit.DoSave
//
//==============================================================================
procedure TFuncExampleSynEdit.DoSave;
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
// TFrame_UnitFunctions.Create
//
//==============================================================================
constructor TFrame_UnitFunctions.Create(AOwner: TComponent);
begin
  inherited;

  fgame := '';

  SynEdit1 := TFuncExampleSynEdit.Create(Self);
  SynEdit1.Parent := DetailPanel;
  SynEdit1.Align := alClient;
  SynEdit1.Highlighter := nil;
  SynEdit1.OnChange := OnExampleChange;
  SynEdit1.Gutter.ShowLineNumbers := True;
  SynEdit1.Gutter.AutoSize := True;
  SynEdit1.MaxScrollWidth := 255;
  SynEdit1.WantTabs := True;
  SynEdit1.funcname := '';

  SynPasSyn1 := TSynDDScriptSyn.Create(Self);

  SynEdit1.Highlighter := SynPasSyn1;
  SynEdit1.ReadOnly := CheckParm('-devparm') <= 0;
end;

//==============================================================================
//
// TFrame_UnitFunctions.CreateParams
//
//==============================================================================
procedure TFrame_UnitFunctions.CreateParams(var Params: TCreateParams);
begin
  sortcolumn := 2;
  Inherited;
end;

//==============================================================================
//
// TFrame_UnitFunctions.Destroy
//
//==============================================================================
destructor TFrame_UnitFunctions.Destroy;
begin
  ClearFuncs;
  inherited;
end;

//==============================================================================
//
// TFrame_UnitFunctions.OnExampleChange
//
//==============================================================================
procedure TFrame_UnitFunctions.OnExampleChange(Sender: TObject);
begin
  if SynEdit1.Modified then
    SynEdit1.DoSave;
end;

//==============================================================================
//
// TFrame_UnitFunctions.ClearFuncs
//
//==============================================================================
procedure TFrame_UnitFunctions.ClearFuncs;
begin
  FreeMem(funcs);
  numfuncs := 0;
end;

//==============================================================================
//
// TFrame_UnitFunctions.ResizeFuncs
//
//==============================================================================
procedure TFrame_UnitFunctions.ResizeFuncs(const sz: Integer);
begin
  ReallocMem(funcs, sz * SizeOf(funcinfo_t));
  numfuncs := sz;
end;

//==============================================================================
//
// TFrame_UnitFunctions.FocusAndSelectFirstItem
//
//==============================================================================
procedure TFrame_UnitFunctions.FocusAndSelectFirstItem;
begin
  if ListView1.Items.Count > 0 then
  begin
    ListView1.Selected := ListView1.Items[0];
    ListView1.ItemFocused := ListView1.Selected;
  end;
end;

//==============================================================================
//
// TFrame_UnitFunctions.UpdateGameControls
//
//==============================================================================
procedure TFrame_UnitFunctions.UpdateGameControls(const game: string);
var
  i, j, k: integer;
  units: TStringList;
  lst: TStringList;
  cnt: integer;
  decl: string;
begin
  if fgame = LowerCase(game) then
    Exit;

  fgame := LowerCase(game);

  units := dll_getuntisfuncdeclarations(fgame);

  if units = nil then
    Exit;

  cnt := 0;
  for i := 0 to units.Count - 1 do
  begin
    lst := units.Objects[i] as TStringList;
    cnt := cnt + lst.Count;
  end;

  ResizeFuncs(cnt);

  cnt := 0;
  for i := 0 to units.Count - 1 do
  begin
    lst := units.Objects[i] as TStringList;
    for j := 0 to lst.Count - 1 do
    begin
      funcs[cnt].declaration := lst.Strings[j];
      decl := funcs[cnt].declaration;
      for k := 1 to Length(decl) do
        if decl[k] in ['(', ';', ':'] then
          decl[k] := ' ';
      funcs[cnt].functype := LowerCase(firstword(decl, ' '));
      funcs[cnt].funcname := firstword(secondword(decl, ' '), ' ');
      funcs[cnt].unitname := units.Strings[i];
      Inc(cnt);
    end;
  end;

  for i := 0 to units.Count - 1 do
    units.Objects[i].Free;
  units.Free;

  FillListView;
end;

//==============================================================================
//
// TFrame_UnitFunctions.FillListView
//
//==============================================================================
procedure TFrame_UnitFunctions.FillListView;

  procedure AddListItem(ProcInfo: funcinfo_p);
  var
    ListItem: TListItem;
  begin
    ListItem := ListView1.Items.Add;
    ListItem.Caption := ProcInfo.funcname;
    ListItem.SubItems.Add(ProcInfo.functype);
    ListItem.SubItems.Add(ProcInfo.unitname);
    ListItem.Data := ProcInfo;
  end;

var
  i: integer;
  srch: string;
begin
  ListView1.Items.BeginUpdate;
  try
    ListView1.Items.Clear;

    srch := LowerCase(Trim(SearchEdit.Text));

    for i := 0 to numfuncs - 1 do
    begin
      if Length(srch) = 0 then
        AddListItem(@funcs[i])
      else if Pos(srch, LowerCase(funcs[i].funcname)) > 0 then
        AddListItem(@funcs[i]);
    end;

  finally
    ListView1.AlphaSort;
    ListView1.Items.EndUpdate;
    FocusAndSelectFirstItem;
  end;
end;

//==============================================================================
//
// TFrame_UnitFunctions.ListView1Data
//
//==============================================================================
procedure TFrame_UnitFunctions.ListView1Data(Sender: TObject;
  Item: TListItem);
begin
  //
end;

//==============================================================================
//
// TFrame_UnitFunctions.ListView1Compare
//
//==============================================================================
procedure TFrame_UnitFunctions.ListView1Compare(Sender: TObject; Item1,
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
// TFrame_UnitFunctions.ListView1ColumnClick
//
//==============================================================================
procedure TFrame_UnitFunctions.ListView1ColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  sortcolumn := Column.Index;
  FillListView;
end;

//==============================================================================
//
// TFrame_UnitFunctions.SearchEditChange
//
//==============================================================================
procedure TFrame_UnitFunctions.SearchEditChange(Sender: TObject);
begin
  FillListView;
  ClearFilterSpeedButton.Visible := SearchEdit.Text <> '';
end;

procedure TFrame_UnitFunctions.ClearFilterSpeedButtonClick(
  Sender: TObject);
begin
  SearchEdit.Clear;
end;

//==============================================================================
//
// TFrame_UnitFunctions.ListView1Change
//
//==============================================================================
procedure TFrame_UnitFunctions.ListView1Change(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
  fi: funcinfo_p;
begin
  if ListView1.Selected <> nil then
  begin
    fi := ListView1.Selected.Data;
    DeclEdit.Text := fi.declaration;
    DeclEdit.Hint := fi.declaration;
    DeclPanel.Hint := fi.declaration;
    SynEdit1.funcname := fi.funcname;
  end;
end;

//==============================================================================
//
// TFrame_UnitFunctions.DeclPanelResize
//
//==============================================================================
procedure TFrame_UnitFunctions.DeclPanelResize(Sender: TObject);
begin
  DeclEdit.Width := DeclPanel.Width - 16;
end;

end.
