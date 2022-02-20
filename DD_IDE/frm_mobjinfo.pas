unit frm_mobjinfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, StdCtrls, Buttons, ExtCtrls;

type
  TFrame_Mobjinfo = class(TFrame)
    ToolbarPanel: TPanel;
    EditorPanel: TPanel;
    Splitter1: TSplitter;
    DetailPanel: TPanel;
    Panel1: TPanel;
    SearchEdit: TEdit;
    ClearFilterSpeedButton: TSpeedButton;
    HintPanel: TPanel;
    HintEdit: TEdit;
    TreeView1: TTreeView;
    ListView1: TListView;
    procedure SearchEditChange(Sender: TObject);
    procedure ClearFilterSpeedButtonClick(Sender: TObject);
    procedure HintPanelResize(Sender: TObject);
    procedure TreeView1Editing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
  private
    { Private declarations }
    minfo: TStringList;
    sinfo: TStringList;
    sprnames: TStringList;
    statenames: TStringList;
    fgame: string;
    sortcolumn: integer;
    procedure ClearMinfo;
    procedure ClearSinfo;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FillTreeView;
    procedure FillListView;
    procedure FocusAndSelectFirstListItem;
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
  ddc_base,
  ide_utils;

//==============================================================================
//
// TFrame_Mobjinfo.Create
//
//==============================================================================
constructor TFrame_Mobjinfo.Create(AOwner: TComponent);
begin
  inherited;

  fgame := '';

  minfo := nil;
  sinfo := nil;
  sprnames := nil;
  statenames := nil;
end;

//==============================================================================
//
// TFrame_Mobjinfo.CreateParams
//
//==============================================================================
procedure TFrame_Mobjinfo.CreateParams(var Params: TCreateParams);
begin
  sortcolumn := 0;
  Inherited;
end;

//==============================================================================
//
// TFrame_Mobjinfo.Destroy
//
//==============================================================================
destructor TFrame_Mobjinfo.Destroy;
begin
  ClearMinfo;
  ClearSinfo;
  sprnames.Free;
  statenames.Free;
  inherited;
end;

//==============================================================================
//
// TFrame_Mobjinfo.ClearMinfo
//
//==============================================================================
procedure TFrame_Mobjinfo.ClearMinfo;
var
  i: integer;
begin
  if minfo <> nil then
  begin
    for i := 0 to minfo.Count - 1 do
      minfo.Objects[i].Free;
    minfo.Free;
    minfo := nil;
  end;
end;

//==============================================================================
//
// TFrame_Mobjinfo.ClearSinfo
//
//==============================================================================
procedure TFrame_Mobjinfo.ClearSinfo;
var
  i: integer;
begin
  if sinfo <> nil then
  begin
    for i := 0 to sinfo.Count - 1 do
      sinfo.Objects[i].Free;
    sinfo.Free;
    sinfo := nil;
  end;
end;

//==============================================================================
//
// TFrame_Mobjinfo.FocusAndSelectFirstItem
//
//==============================================================================
procedure TFrame_Mobjinfo.FocusAndSelectFirstItem;
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
// TFrame_Mobjinfo.FocusAndSelectFirstListItem
//
//==============================================================================
procedure TFrame_Mobjinfo.FocusAndSelectFirstListItem;
begin
  if ListView1.Items.Count > 0 then
  begin
    ListView1.Selected := ListView1.Items[0];
    ListView1.ItemFocused := ListView1.Selected;
  end;
end;

//==============================================================================
//
// TFrame_Mobjinfo.UpdateGameControls
//
//==============================================================================
procedure TFrame_Mobjinfo.UpdateGameControls(const game: string);
var
  lst: TStringList;
begin
  if fgame = LowerCase(game) then
    Exit;

  fgame := LowerCase(game);

  ClearMinfo;
  minfo := dll_getmobjinfodeclarations(fgame);

  ClearSinfo;
  sinfo := dll_getstatesdeclarations(fgame);

  sprnames.Free;
  sprnames := dll_getspritenames(fgame);

  statenames.Free;
  lst := dll_getpcharfunc(game, 'dd_getstatescsv_');
  statenames := getcolumnfromcsv(lst, 'Name');
  lst.Free;

  if minfo <> nil then
    FillTreeView;

  FillListView;
end;

//==============================================================================
//
// _MinfoItemsCompare
//
//==============================================================================
function _MinfoItemsCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  s1, s2: string;
  x1, x2: integer;
begin
  s1 := LowerCase(List.Strings[Index1]);
  s2 := LowerCase(List.Strings[Index2]);

  if Pos('inheritsfrom', s1) = 1 then
    x1 := 10
  else if Pos('id #:', s1) = 1 then
    x1 := 11
  else if Pos('id:', s1) = 1 then
    x1 := 12
  else if Pos('name', s1) = 1 then
    x1 := 20
  else if Pos('frame', s1) >= 1 then
    x1 := 30
  else if Pos('bits:', s1) >= 1 then
    x1 := 31
  else if Pos('bits2:', s1) >= 1 then
    x1 := 32
  else if Pos('flags_ex:', s1) >= 1 then
    x1 := 33
  else if Pos('flags2_ex:', s1) >= 1 then
    x1 := 34
  else if Pos('flags3_ex:', s1) >= 1 then
    x1 := 35
  else if Pos('flags4_ex:', s1) >= 1 then
    x1 := 36
  else if Pos('flags5_ex:', s1) >= 1 then
    x1 := 37
  else if Pos('flags6_ex:', s1) >= 1 then
    x1 := 38
  else if Pos('sound', s1) >= 1 then
    x1 := 40
  else
    x1 := 50;

  if Pos('inheritsfrom', s2) = 1 then
    x2 := 10
  else if Pos('id #:', s2) = 1 then
    x2 := 11
  else if Pos('id:', s2) = 1 then
    x2 := 12
  else if Pos('name', s2) = 1 then
    x2 := 20
  else if Pos('frame', s2) >= 1 then
    x2 := 30
  else if Pos('bits:', s2) >= 1 then
    x2 := 31
  else if Pos('bits2:', s2) >= 1 then
    x2 := 32
  else if Pos('flags_ex:', s2) >= 1 then
    x2 := 33
  else if Pos('flags2_ex:', s2) >= 1 then
    x2 := 34
  else if Pos('flags3_ex:', s2) >= 1 then
    x2 := 35
  else if Pos('flags4_ex:', s2) >= 1 then
    x2 := 36
  else if Pos('flags5_ex:', s2) >= 1 then
    x2 := 37
  else if Pos('flags6_ex:', s2) >= 1 then
    x2 := 38
  else if Pos('sound', s2) >= 1 then
    x2 := 40
  else
    x2 := 50;

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
// TFrame_Mobjinfo.FillTreeView
//
//==============================================================================
procedure TFrame_Mobjinfo.FillTreeView;

  procedure AddTreeItem(const id: integer);
  var
    Item: TTreeNode;
    i: integer;
    lst: TStringList;
    stnum: integer;
    sn, sv: string;
  begin
    if id = 0 then
      Item := TreeView1.Items.AddFirst(nil, minfo.Strings[id])
    else
      Item := TreeView1.Items.AddChild(nil, minfo.Strings[id]);
    lst := minfo.Objects[id] as TStringList;
    if lst <> nil then
    begin
      lst.CustomSort(_MinfoItemsCompare);
      for i := 0 to lst.Count - 1 do
      begin
        splitstring(lst.Strings[i], sn, sv, ':');
        if Pos(' FRAME:', UpperCase(sn + ':')) > 0 then
        begin
          stnum := StrToIntDef(Trim(sv), -1);
          if (stnum >= 0) and (stnum < statenames.Count - 1) then
            TreeView1.Items.AddChild(Item, sn + ': ' + statenames.Strings[stnum])
          else
            TreeView1.Items.AddChild(Item, lst.Strings[i]);
        end
        else
          TreeView1.Items.AddChild(Item, lst.Strings[i]);
      end;
    end;
  end;

var
  i: integer;
  srch: string;
begin
  if minfo = nil then
    Exit;

  TreeView1.OnChange := nil;
  TreeView1.Items.BeginUpdate;
  try
    TreeView1.Items.Clear;

    srch := LowerCase(Trim(SearchEdit.Text));

    for i := 0 to minfo.Count - 1 do
    begin
      if Length(srch) = 0 then
        AddTreeItem(i)
      else if Pos(srch, LowerCase(minfo.Strings[i])) > 0 then
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
// TFrame_Mobjinfo.FillListView
//
//==============================================================================
procedure TFrame_Mobjinfo.FillListView;

  procedure AddListItem(const idx: integer; const fieldname, fieldvalue: string);
  var
    ListItem: TListItem;
    spr: integer;
    brightstr: string;
    stnum: integer;
  begin
    ListItem := ListView1.Items.Add;
    ListItem.Caption := fieldname;
    if fieldname = 'Sprite Number' then
    begin
      spr := StrToInt(fieldvalue);
      if (spr >= 0) and (spr < sprnames.Count) then
        ListItem.SubItems.Add(sprnames.Strings[spr])
      else
        ListItem.SubItems.Add('Unknown sprite number #' + IntToStr(spr));
    end
    else if fieldname = 'Sprite Subnumber' then
    begin
      spr := StrToInt(fieldvalue);
      if spr and 32768 <> 0 then
      begin
        spr := spr and 32767;
        brightstr := ' BRIGHT'
      end
      else
        brightstr := '';
      if spr in [0..28] then
        ListItem.SubItems.Add(Chr(Ord('A') + spr) + brightstr)
      else
        ListItem.SubItems.Add('Unknown sprite subnumber #' + IntToStr(spr));
    end
    else if fieldname = 'Next Frame' then
    begin
      stnum := StrToIntDef(fieldvalue, -1);
      if (stnum >= 0) and (stnum < statenames.Count - 1) then
        ListItem.SubItems.Add(statenames.Strings[stnum])
      else
        ListItem.SubItems.Add(fieldvalue);
    end
    else
      ListItem.SubItems.Add(fieldvalue);
    ListItem.Data := Pointer(idx);
  end;

var
  it: TTreeNode;
  n, v: string;
  i, stnum: integer;
  lst: TStringList;
begin
  ListView1.Items.BeginUpdate;
  try
    ListView1.Items.Clear;

    it := TreeView1.Selected;
    if it <> nil then
    begin
      splitstring(it.Text, n, v, ':');
      if Pos(' FRAME:', UpperCase(n + ':')) > 0 then
      begin
        stnum := StrToIntDef(v, -1);
        if (stnum < 0) or (stnum >= sinfo.Count) then
          stnum := statenames.IndexOf(v);
        if (stnum >= 0) and (stnum < sinfo.Count) then
        begin
          lst := sinfo.Objects[stnum] as TStringList;
          if lst <> nil then
            for i := 0 to lst.Count - 1 do
            begin
              splitstring(lst.Strings[i], n, v, ':');
              AddListItem(i, n, v);
            end;
        end;
      end;
    end;

  finally
    ListView1.Items.EndUpdate;
  end;
end;

//==============================================================================
//
// TFrame_Mobjinfo.SearchEditChange
//
//==============================================================================
procedure TFrame_Mobjinfo.SearchEditChange(Sender: TObject);
begin
  FillTreeView;
  ClearFilterSpeedButton.Visible := SearchEdit.Text <> '';
end;

procedure TFrame_Mobjinfo.ClearFilterSpeedButtonClick(
  Sender: TObject);
begin
  SearchEdit.Clear;
end;

//==============================================================================
//
// TFrame_Mobjinfo.HintPanelResize
//
//==============================================================================
procedure TFrame_Mobjinfo.HintPanelResize(Sender: TObject);
begin
  HintEdit.Width := HintPanel.Width - 16;
end;

//==============================================================================
//
// TFrame_Mobjinfo.TreeView1Editing
//
//==============================================================================
procedure TFrame_Mobjinfo.TreeView1Editing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;

//==============================================================================
//
// TFrame_Mobjinfo.TreeView1Change
//
//==============================================================================
procedure TFrame_Mobjinfo.TreeView1Change(Sender: TObject; Node: TTreeNode);
var
  it: TTreeNode;
begin
  it := TreeView1.Selected;
  if it <> nil then
  begin
    HintEdit.Text := it.Text;
    HintEdit.Hint := HintEdit.Text;
    HintPanel.Hint := HintEdit.Text;
    FillListView;
  end;
end;

end.
