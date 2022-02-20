unit frm_classes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, StdCtrls, Buttons, ExtCtrls,
  SynEdit, SynHighlighterDDScript;

type
  TClassExampleSynEdit = class(TSynEdit)
  private
    fclsname: string;
    function GetSaveFileName(const fn: string): string;
    procedure SetClsName(const fn: string);
  public
    property clsname: string read fclsname write SetClsName;
    procedure DoSave;
  end;

type
  TFrame_Classes = class(TFrame)
    ToolbarPanel: TPanel;
    EditorPanel: TPanel;
    Splitter1: TSplitter;
    DetailPanel: TPanel;
    Panel1: TPanel;
    SearchEdit: TEdit;
    ClearFilterSpeedButton: TSpeedButton;
    DeclPanel: TPanel;
    DeclEdit: TEdit;
    TreeView1: TTreeView;
    procedure SearchEditChange(Sender: TObject);
    procedure ClearFilterSpeedButtonClick(Sender: TObject);
    procedure DeclPanelResize(Sender: TObject);
    procedure TreeView1Editing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
  private
    { Private declarations }
    clss: TStringList;
    fgame: string;
    sortcolumn: integer;
    procedure ClearClss;
    procedure OnExampleChange(Sender: TObject);
  protected
    SynEdit1: TClassExampleSynEdit;
    SynPasSyn1: TSynDDScriptSyn;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FillTreeView;
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
// TClassExampleSynEdit.GetSaveFileName
//
//==============================================================================
function TClassExampleSynEdit.GetSaveFileName(const fn: string): string;
var
  base: string;
begin
  base := ExtractFilePath(ParamStr(0));
  base := base + '\classes\';
  if not DirectoryExists(base) then
    MkDir(base);
  Result := base + fn + '.ddscript';
end;

//==============================================================================
//
// TClassExampleSynEdit.SetClsName
//
//==============================================================================
procedure TClassExampleSynEdit.SetClsName(const fn: string);
var
  lst: TStringList;
begin
  if fclsname <> fn then
  begin
    if not ReadOnly then
    begin
      if fclsname <> '' then
      begin
        lst := TStringList.Create;
        lst.Text := Text;
        lst.SaveToFile(GetSaveFileName(fclsname));
        lst.Free;
      end;
    end;
    lst := TStringList.Create;
    if FileExists(GetSaveFileName(fn)) then
      lst.LoadFromFile(GetSaveFileName(fn));
    Text := lst.Text;
    lst.Free;
    fclsname := fn;
  end;
end;

//==============================================================================
//
// TClassExampleSynEdit.DoSave
//
//==============================================================================
procedure TClassExampleSynEdit.DoSave;
var
  lst: TStringList;
begin
  if not ReadOnly then
    if fclsname <> '' then
    begin
      lst := TStringList.Create;
      lst.Text := Text;
      lst.SaveToFile(GetSaveFileName(fclsname));
      lst.Free;
    end;
end;

//==============================================================================
//
// TFrame_Classes.Create
//
//==============================================================================
constructor TFrame_Classes.Create(AOwner: TComponent);
begin
  inherited;

  fgame := '';

  SynEdit1 := TClassExampleSynEdit.Create(Self);
  SynEdit1.Parent := DetailPanel;
  SynEdit1.Align := alClient;
  SynEdit1.Highlighter := nil;
  SynEdit1.OnChange := OnExampleChange;
  SynEdit1.Gutter.ShowLineNumbers := True;
  SynEdit1.Gutter.AutoSize := True;
  SynEdit1.MaxScrollWidth := 255;
  SynEdit1.WantTabs := True;
  SynEdit1.clsname := '';

  SynPasSyn1 := TSynDDScriptSyn.Create(Self);

  SynEdit1.Highlighter := SynPasSyn1;
  SynEdit1.ReadOnly := CheckParm('-devparm') <= 0;

  clss := nil;
end;

//==============================================================================
//
// TFrame_Classes.CreateParams
//
//==============================================================================
procedure TFrame_Classes.CreateParams(var Params: TCreateParams);
begin
  sortcolumn := 0;
  Inherited;
end;

//==============================================================================
//
// TFrame_Classes.Destroy
//
//==============================================================================
destructor TFrame_Classes.Destroy;
begin
  ClearClss;
  inherited;
end;

//==============================================================================
//
// TFrame_Classes.OnExampleChange
//
//==============================================================================
procedure TFrame_Classes.OnExampleChange(Sender: TObject);
begin
  if SynEdit1.Modified then
    SynEdit1.DoSave;
end;

//==============================================================================
//
// TFrame_Classes.ClearClss
//
//==============================================================================
procedure TFrame_Classes.ClearClss;
var
  i: integer;
begin
  if clss <> nil then
  begin
    for i := 0 to clss.Count - 1 do
      clss.Objects[i].Free;
    clss.Free;
    clss := nil;
  end;
end;

//==============================================================================
//
// TFrame_Classes.FocusAndSelectFirstItem
//
//==============================================================================
procedure TFrame_Classes.FocusAndSelectFirstItem;
var
  i: integer;
begin
  if TreeView1.Items.Count > 0 then
  begin
    for i := 1 to TreeView1.Items.Count - 1 do
      TreeView1.Items[i].Focused := False;
    TreeView1.Selected := TreeView1.Items[0];
    TreeView1.Items[0].Focused := False;
  end;
end;

//==============================================================================
//
// TFrame_Classes.UpdateGameControls
//
//==============================================================================
procedure TFrame_Classes.UpdateGameControls(const game: string);
begin
  if fgame = LowerCase(game) then
    Exit;

  fgame := LowerCase(game);

  ClearClss;
  clss := dll_getclassesdeclarations(fgame);

  if clss = nil then
    Exit;

  FillTreeView;
end;

//==============================================================================
//
// _ClassItemsCompare
//
//==============================================================================
function _ClassItemsCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  s1, s2: string;
  x1, x2: integer;
begin
  s1 := LowerCase(List.Strings[Index1]);
  s2 := LowerCase(List.Strings[Index2]);
  if Pos('inherit', s1) = 1 then
    x1 := 1
  else if Pos('constructor', s1) = 1 then
    x1 := 2
  else if Pos('destructor', s1) = 1 then
    x1 := 3
  else if Pos('function', s1) = 1 then
    x1 := 4
  else if Pos('procedure', s1) = 1 then
    x1 := 5
  else if Pos('property', s1) = 1 then
    x1 := 6
  else
    x1 := 7;
  if Pos('inherit', s2) = 1 then
    x2 := 1
  else if Pos('constructor', s2) = 1 then
    x2 := 2
  else if Pos('destructor', s2) = 1 then
    x2 := 3
  else if Pos('function', s2) = 1 then
    x2 := 4
  else if Pos('procedure', s2) = 1 then
    x2 := 5
  else if Pos('property', s2) = 1 then
    x2 := 6
  else
    x2 := 7;
  Result := x1 - x2;
  if Result = 0 then
  begin
    if s1 < s2 then
      Result := -1
    else if s1 > s2 then
      Result := 1;
  end;
end;

//==============================================================================
//
// TFrame_Classes.FillTreeView
//
//==============================================================================
procedure TFrame_Classes.FillTreeView;

  procedure AddTreeItem(const id: integer);
  var
    Item: TTreeNode;
    i: integer;
    lst: TStringList;
  begin
    if id = 0 then
      Item := TreeView1.Items.AddFirst(nil, clss.Strings[id])
    else
      Item := TreeView1.Items.AddChild(nil, clss.Strings[id]);
    lst := clss.Objects[id] as TStringList;
    if lst <> nil then
    begin
      lst.CustomSort(_ClassItemsCompare);
      for i := 0 to lst.Count - 1 do
        TreeView1.Items.AddChild(Item, lst.Strings[i]);
    end;
  end;

var
  i: integer;
  srch: string;
begin
  if clss = nil then
    Exit;

  TreeView1.OnChange := nil;
  TreeView1.Items.BeginUpdate;
  try
    TreeView1.Items.Clear;

    srch := LowerCase(Trim(SearchEdit.Text));

    for i := 0 to clss.Count - 1 do
    begin
      if Length(srch) = 0 then
        AddTreeItem(i)
      else if Pos(srch, LowerCase(clss.Strings[i])) > 0 then
        AddTreeItem(i);
    end;

  finally
    TreeView1.Items.AlphaSort;
    TreeView1.Items.EndUpdate;
    TreeView1.OnChange := TreeView1Change;
    FocusAndSelectFirstItem;
  end;
end;

//==============================================================================
//
// TFrame_Classes.SearchEditChange
//
//==============================================================================
procedure TFrame_Classes.SearchEditChange(Sender: TObject);
begin
  FillTreeView;
  ClearFilterSpeedButton.Visible := SearchEdit.Text <> '';
end;

procedure TFrame_Classes.ClearFilterSpeedButtonClick(
  Sender: TObject);
begin
  SearchEdit.Clear;
end;

//==============================================================================
//
// TFrame_Classes.DeclPanelResize
//
//==============================================================================
procedure TFrame_Classes.DeclPanelResize(Sender: TObject);
begin
  DeclEdit.Width := DeclPanel.Width - 16;
end;

//==============================================================================
//
// TFrame_Classes.TreeView1Editing
//
//==============================================================================
procedure TFrame_Classes.TreeView1Editing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;

//==============================================================================
//
// TFrame_Classes.TreeView1Change
//
//==============================================================================
procedure TFrame_Classes.TreeView1Change(Sender: TObject; Node: TTreeNode);
var
  it: TTreeNode;
begin
  it := TreeView1.Selected;
  if it <> nil then
  begin
    DeclEdit.Text := it.Text;
    DeclEdit.Hint := DeclEdit.Text;
    DeclPanel.Hint := DeclEdit.Text;
    while it.Parent <> nil do
      it := it.Parent;
    SynEdit1.clsname := it.Text;
  end;
end;

end.
