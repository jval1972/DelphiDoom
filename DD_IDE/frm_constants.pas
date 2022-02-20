unit frm_constants;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, StdCtrls, Buttons, ExtCtrls,
  SynEdit, SynHighlighterDDScript;

type
  TConstExampleSynEdit = class(TSynEdit)
  private
    fconstname: string;
    function GetSaveFileName(const fn: string): string;
    procedure SetFuncName(const fn: string);
  public
    property constname: string read fconstname write SetFuncName;
    procedure DoSave;
  end;

type
  constinfo_t = record
    constname: string[128];
    constvalue: string[255];
  end;
  constinfo_p = ^constinfo_t;
  constinfo_a = array[0..$FFF] of constinfo_t;
  constinfo_pa = ^constinfo_a;

type
  TFrame_Constants = class(TFrame)
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
    consts: constinfo_pa;
    numconsts: integer;
    fgame: string;
    sortcolumn: integer;
    procedure ClearConsts;
    procedure ResizeConsts(const sz: Integer);
    procedure OnExampleChange(Sender: TObject);
  protected
    SynEdit1: TConstExampleSynEdit;
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
// TConstExampleSynEdit.GetSaveFileName
//
//==============================================================================
function TConstExampleSynEdit.GetSaveFileName(const fn: string): string;
var
  base: string;
begin
  base := ExtractFilePath(ParamStr(0));
  base := base + '\consts\';
  if not DirectoryExists(base) then
    MkDir(base);
  Result := base + fn + '.ddscript';
end;

//==============================================================================
//
// TConstExampleSynEdit.SetFuncName
//
//==============================================================================
procedure TConstExampleSynEdit.SetFuncName(const fn: string);
var
  lst: TStringList;
begin
  if fconstname <> fn then
  begin
    if not ReadOnly then
    begin
      if fconstname <> '' then
      begin
        lst := TStringList.Create;
        lst.Text := Text;
        lst.SaveToFile(GetSaveFileName(fconstname));
        lst.Free;
      end;
    end;
    lst := TStringList.Create;
    if FileExists(GetSaveFileName(fn)) then
      lst.LoadFromFile(GetSaveFileName(fn));
    Text := lst.Text;
    lst.Free;
    fconstname := fn;
  end;
end;

//==============================================================================
//
// TConstExampleSynEdit.DoSave
//
//==============================================================================
procedure TConstExampleSynEdit.DoSave;
var
  lst: TStringList;
begin
  if not ReadOnly then
    if fconstname <> '' then
    begin
      lst := TStringList.Create;
      lst.Text := Text;
      lst.SaveToFile(GetSaveFileName(fconstname));
      lst.Free;
    end;
end;

//==============================================================================
//
// TFrame_Constants.Create
//
//==============================================================================
constructor TFrame_Constants.Create(AOwner: TComponent);
begin
  inherited;

  fgame := '';

  SynEdit1 := TConstExampleSynEdit.Create(Self);
  SynEdit1.Parent := DetailPanel;
  SynEdit1.Align := alClient;
  SynEdit1.Highlighter := nil;
  SynEdit1.OnChange := OnExampleChange;
  SynEdit1.Gutter.ShowLineNumbers := True;
  SynEdit1.Gutter.AutoSize := True;
  SynEdit1.MaxScrollWidth := 255;
  SynEdit1.WantTabs := True;
  SynEdit1.constname := '';

  SynPasSyn1 := TSynDDScriptSyn.Create(Self);

  SynEdit1.Highlighter := SynPasSyn1;
  SynEdit1.ReadOnly := CheckParm('-devparm') <= 0;
end;

//==============================================================================
//
// TFrame_Constants.CreateParams
//
//==============================================================================
procedure TFrame_Constants.CreateParams(var Params: TCreateParams);
begin
  sortcolumn := 0;
  Inherited;
end;

//==============================================================================
//
// TFrame_Constants.Destroy
//
//==============================================================================
destructor TFrame_Constants.Destroy;
begin
  ClearConsts;
  inherited;
end;

//==============================================================================
//
// TFrame_Constants.OnExampleChange
//
//==============================================================================
procedure TFrame_Constants.OnExampleChange(Sender: TObject);
begin
  if SynEdit1.Modified then
    SynEdit1.DoSave;
end;

//==============================================================================
//
// TFrame_Constants.ClearConsts
//
//==============================================================================
procedure TFrame_Constants.ClearConsts;
begin
  FreeMem(consts);
  numconsts := 0;
end;

//==============================================================================
//
// TFrame_Constants.ResizeConsts
//
//==============================================================================
procedure TFrame_Constants.ResizeConsts(const sz: Integer);
begin
  ReallocMem(consts, sz * SizeOf(constinfo_t));
  numconsts := sz;
end;

//==============================================================================
//
// TFrame_Constants.FocusAndSelectFirstItem
//
//==============================================================================
procedure TFrame_Constants.FocusAndSelectFirstItem;
begin
  if ListView1.Items.Count > 0 then
  begin
    ListView1.Selected := ListView1.Items[0];
    ListView1.ItemFocused := ListView1.Selected;
  end;
end;

//==============================================================================
//
// TFrame_Constants.UpdateGameControls
//
//==============================================================================
procedure TFrame_Constants.UpdateGameControls(const game: string);
var
  i: integer;
  fconstants: TStringList;
  s1, s2: string;
begin
  if fgame = LowerCase(game) then
    Exit;

  fgame := LowerCase(game);

  fconstants := dll_getconstants(fgame);

  if fconstants = nil then
    Exit;

  ResizeConsts(fconstants.Count);

  for i := 0 to fconstants.Count - 1 do
  begin
    splitstring(fconstants.Strings[i], s1, s2, '=');
    consts[i].constname := s1;
    consts[i].constvalue := s2;
  end;

  fconstants.Free;

  FillListView;
end;

//==============================================================================
//
// TFrame_Constants.FillListView
//
//==============================================================================
procedure TFrame_Constants.FillListView;

  procedure AddListItem(Info: constinfo_p);
  var
    ListItem: TListItem;
  begin
    ListItem := ListView1.Items.Add;
    ListItem.Caption := Info.constname;
    ListItem.SubItems.Add(Info.constvalue);
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

    for i := 0 to numconsts - 1 do
    begin
      if Length(srch) = 0 then
        AddListItem(@consts[i])
      else if Pos(srch, LowerCase(consts[i].constname)) > 0 then
        AddListItem(@consts[i]);
    end;

  finally
    ListView1.AlphaSort;
    ListView1.Items.EndUpdate;
    FocusAndSelectFirstItem;
  end;
end;

//==============================================================================
//
// TFrame_Constants.ListView1Data
//
//==============================================================================
procedure TFrame_Constants.ListView1Data(Sender: TObject;
  Item: TListItem);
begin
  //
end;

//==============================================================================
//
// TFrame_Constants.ListView1Compare
//
//==============================================================================
procedure TFrame_Constants.ListView1Compare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  value1, value2: string;
  f1, f2: extended;
  c1, c2: integer;
begin
  if sortcolumn = 0 then
  begin
    value1 := Item1.Caption;
    value2 := Item2.Caption;
    Compare := AnsiCompareText(value1, value2);
  end
  else
  begin
    value1 := Item1.SubItems[sortcolumn - 1];
    value2 := Item2.SubItems[sortcolumn - 1];
    Val(value1, f1, c1);
    Val(value2, f2, c2);
    if (c1 = 0) and (c2 = 0) then
    begin
      if f1 < f2 then
        Compare := -1
      else if f1 > f2 then
        Compare := 1
      else
        Compare := 0;
      Exit;
    end;
    if (c1 = 0) or (c2 = 0) then
    begin
      if c1 = 0 then
        Compare := 1
      else
        Compare := -1;
      Exit;
    end;
    Compare := AnsiCompareText(value1, value2);
  end;
end;

//==============================================================================
//
// TFrame_Constants.ListView1ColumnClick
//
//==============================================================================
procedure TFrame_Constants.ListView1ColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  sortcolumn := Column.Index;
  FillListView;
end;

//==============================================================================
//
// TFrame_Constants.SearchEditChange
//
//==============================================================================
procedure TFrame_Constants.SearchEditChange(Sender: TObject);
begin
  FillListView;
  ClearFilterSpeedButton.Visible := SearchEdit.Text <> '';
end;

procedure TFrame_Constants.ClearFilterSpeedButtonClick(
  Sender: TObject);
begin
  SearchEdit.Clear;
end;

//==============================================================================
//
// TFrame_Constants.ListView1Change
//
//==============================================================================
procedure TFrame_Constants.ListView1Change(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
  inf: constinfo_p;
begin
  if ListView1.Selected <> nil then
  begin
    inf := ListView1.Selected.Data;
    DeclEdit.Text := inf.constname + ' = ' + inf.constvalue + ';';
    DeclEdit.Hint := DeclEdit.Text;
    DeclPanel.Hint := DeclEdit.Text;
    SynEdit1.constname := inf.constname;
  end;
end;

//==============================================================================
//
// TFrame_Constants.DeclPanelResize
//
//==============================================================================
procedure TFrame_Constants.DeclPanelResize(Sender: TObject);
begin
  DeclEdit.Width := DeclPanel.Width - 16;
end;

end.
