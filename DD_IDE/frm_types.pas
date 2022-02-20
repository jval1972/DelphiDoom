unit frm_types;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, StdCtrls, Buttons, ExtCtrls,
  SynEdit, SynHighlighterDDScript;

type
  TTypesExampleSynEdit = class(TSynEdit)
  private
    ftypename: string;
    function GetSaveFileName(const fn: string): string;
    procedure SetFuncName(const fn: string);
  public
    property typename: string read ftypename write SetFuncName;
    procedure DoSave;
  end;

type
  typeinfo_t = record
    typename: string[128];
    typebasevalue: string[64];
  end;
  typeinfo_p = ^typeinfo_t;
  typeinfo_a = array[0..$FFF] of typeinfo_t;
  typeinfo_pa = ^typeinfo_a;

type
  TFrame_Types = class(TFrame)
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
    types: typeinfo_pa;
    numtypes: integer;
    fgame: string;
    sortcolumn: integer;
    procedure ClearTypes;
    procedure ResizeTypes(const sz: Integer);
    procedure OnExampleChange(Sender: TObject);
  protected
    SynEdit1: TTypesExampleSynEdit;
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
// TTypesExampleSynEdit.GetSaveFileName
//
//==============================================================================
function TTypesExampleSynEdit.GetSaveFileName(const fn: string): string;
var
  base: string;
begin
  base := ExtractFilePath(ParamStr(0));
  base := base + '\types\';
  if not DirectoryExists(base) then
    MkDir(base);
  Result := base + fn + '.ddscript';
end;

//==============================================================================
//
// TTypesExampleSynEdit.SetFuncName
//
//==============================================================================
procedure TTypesExampleSynEdit.SetFuncName(const fn: string);
var
  lst: TStringList;
begin
  if ftypename <> fn then
  begin
    if not ReadOnly then
    begin
      if ftypename <> '' then
      begin
        lst := TStringList.Create;
        lst.Text := Text;
        lst.SaveToFile(GetSaveFileName(ftypename));
        lst.Free;
      end;
    end;
    lst := TStringList.Create;
    if FileExists(GetSaveFileName(fn)) then
      lst.LoadFromFile(GetSaveFileName(fn));
    Text := lst.Text;
    lst.Free;
    ftypename := fn;
  end;
end;

//==============================================================================
//
// TTypesExampleSynEdit.DoSave
//
//==============================================================================
procedure TTypesExampleSynEdit.DoSave;
var
  lst: TStringList;
begin
  if not ReadOnly then
    if ftypename <> '' then
    begin
      lst := TStringList.Create;
      lst.Text := Text;
      lst.SaveToFile(GetSaveFileName(ftypename));
      lst.Free;
    end;
end;

//==============================================================================
//
// TFrame_Types.Create
//
//==============================================================================
constructor TFrame_Types.Create(AOwner: TComponent);
begin
  inherited;

  fgame := '';

  SynEdit1 := TTypesExampleSynEdit.Create(Self);
  SynEdit1.Parent := DetailPanel;
  SynEdit1.Align := alClient;
  SynEdit1.Highlighter := nil;
  SynEdit1.OnChange := OnExampleChange;
  SynEdit1.Gutter.ShowLineNumbers := True;
  SynEdit1.Gutter.AutoSize := True;
  SynEdit1.MaxScrollWidth := 255;
  SynEdit1.WantTabs := True;
  SynEdit1.typename := '';

  SynPasSyn1 := TSynDDScriptSyn.Create(Self);

  SynEdit1.Highlighter := SynPasSyn1;
  SynEdit1.ReadOnly := CheckParm('-devparm') <= 0;
end;

//==============================================================================
//
// TFrame_Types.CreateParams
//
//==============================================================================
procedure TFrame_Types.CreateParams(var Params: TCreateParams);
begin
  sortcolumn := 0;
  Inherited;
end;

//==============================================================================
//
// TFrame_Types.Destroy
//
//==============================================================================
destructor TFrame_Types.Destroy;
begin
  ClearTypes;
  inherited;
end;

//==============================================================================
//
// TFrame_Types.OnExampleChange
//
//==============================================================================
procedure TFrame_Types.OnExampleChange(Sender: TObject);
begin
  if SynEdit1.Modified then
    SynEdit1.DoSave;
end;

//==============================================================================
//
// TFrame_Types.ClearTypes
//
//==============================================================================
procedure TFrame_Types.ClearTypes;
begin
  FreeMem(types);
  numtypes := 0;
end;

//==============================================================================
//
// TFrame_Types.ResizeTypes
//
//==============================================================================
procedure TFrame_Types.ResizeTypes(const sz: Integer);
begin
  ReallocMem(types, sz * SizeOf(typeinfo_t));
  numtypes := sz;
end;

//==============================================================================
//
// TFrame_Types.FocusAndSelectFirstItem
//
//==============================================================================
procedure TFrame_Types.FocusAndSelectFirstItem;
begin
  if ListView1.Items.Count > 0 then
  begin
    ListView1.Selected := ListView1.Items[0];
    ListView1.ItemFocused := ListView1.Selected;
  end;
end;

//==============================================================================
//
// TFrame_Types.UpdateGameControls
//
//==============================================================================
procedure TFrame_Types.UpdateGameControls(const game: string);
var
  i: integer;
  typlst: TStringList;
  s1, s2: string;
begin
  if fgame = LowerCase(game) then
    Exit;

  fgame := LowerCase(game);

  typlst := dll_gettypes(fgame);

  if typlst = nil then
    Exit;

  ResizeTypes(typlst.Count);

  for i := 0 to typlst.Count - 1 do
  begin
    splitstring(typlst.Strings[i], s1, s2, '=');
    types[i].typename := s1;
    types[i].typebasevalue := s2;
  end;

  typlst.Free;

  FillListView;
end;

//==============================================================================
//
// TFrame_Types.FillListView
//
//==============================================================================
procedure TFrame_Types.FillListView;

  procedure AddListItem(Info: typeinfo_p);
  var
    ListItem: TListItem;
  begin
    ListItem := ListView1.Items.Add;
    ListItem.Caption := Info.typename;
    ListItem.SubItems.Add(Info.typebasevalue);
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

    for i := 0 to numtypes - 1 do
    begin
      if Length(srch) = 0 then
        AddListItem(@types[i])
      else if Pos(srch, LowerCase(types[i].typename)) > 0 then
        AddListItem(@types[i]);
    end;

  finally
    ListView1.AlphaSort;
    ListView1.Items.EndUpdate;
    FocusAndSelectFirstItem;
  end;
end;

//==============================================================================
//
// TFrame_Types.ListView1Data
//
//==============================================================================
procedure TFrame_Types.ListView1Data(Sender: TObject;
  Item: TListItem);
begin
  //
end;

//==============================================================================
//
// TFrame_Types.ListView1Compare
//
//==============================================================================
procedure TFrame_Types.ListView1Compare(Sender: TObject; Item1,
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
// TFrame_Types.ListView1ColumnClick
//
//==============================================================================
procedure TFrame_Types.ListView1ColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  sortcolumn := Column.Index;
  FillListView;
end;

//==============================================================================
//
// TFrame_Types.SearchEditChange
//
//==============================================================================
procedure TFrame_Types.SearchEditChange(Sender: TObject);
begin
  FillListView;
  ClearFilterSpeedButton.Visible := SearchEdit.Text <> '';
end;

procedure TFrame_Types.ClearFilterSpeedButtonClick(
  Sender: TObject);
begin
  SearchEdit.Clear;
end;

//==============================================================================
//
// TFrame_Types.ListView1Change
//
//==============================================================================
procedure TFrame_Types.ListView1Change(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
  inf: typeinfo_p;
begin
  if ListView1.Selected <> nil then
  begin
    inf := ListView1.Selected.Data;
    DeclEdit.Text := inf.typename + ' (' + inf.typebasevalue + ')';
    DeclEdit.Hint := DeclEdit.Text;
    DeclPanel.Hint := DeclEdit.Text;
    SynEdit1.typename := inf.typename;
  end;
end;

//==============================================================================
//
// TFrame_Types.DeclPanelResize
//
//==============================================================================
procedure TFrame_Types.DeclPanelResize(Sender: TObject);
begin
  DeclEdit.Width := DeclPanel.Width - 16;
end;

end.

