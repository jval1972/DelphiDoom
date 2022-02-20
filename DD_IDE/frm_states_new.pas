unit frm_states;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, StdCtrls, Buttons, ExtCtrls;

type
  TFrame_States = class(TFrame)
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
  ddc_base, ide_utils;

//==============================================================================
//
// TFrame_States.Create
//
//==============================================================================
constructor TFrame_States.Create(AOwner: TComponent);
begin
  inherited;

  fgame := '';

  sinfo := nil;
  sprnames := nil;
  statenames := nil;
end;

//==============================================================================
//
// TFrame_States.CreateParams
//
//==============================================================================
procedure TFrame_States.CreateParams(var Params: TCreateParams);
begin
  sortcolumn := 0;
  Inherited;
end;

//==============================================================================
//
// TFrame_States.Destroy
//
//==============================================================================
destructor TFrame_States.Destroy;
begin
  ClearMinfo;
  ClearSinfo;
  sprnames.Free;
  statenames.Free;
  inherited;
end;

//==============================================================================
//
// TFrame_States.ClearMinfo
//
//==============================================================================
procedure TFrame_States.ClearMinfo;
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
// TFrame_States.ClearSinfo
//
//==============================================================================
procedure TFrame_States.ClearSinfo;
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
// TFrame_States.FocusAndSelectFirstItem
//
//==============================================================================
procedure TFrame_States.FocusAndSelectFirstItem;
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
// TFrame_States.FocusAndSelectFirstListItem
//
//==============================================================================
procedure TFrame_States.FocusAndSelectFirstListItem;
begin
  if ListView1.Items.Count > 0 then
  begin
    ListView1.Selected := ListView1.Items[0];
    ListView1.ItemFocused := ListView1.Selected;
  end;
end;

//==============================================================================
//
// TFrame_States.UpdateGameControls
//
//==============================================================================
procedure TFrame_States.UpdateGameControls(const game: string);
var
  lst: TStringList;
begin
  if fgame = LowerCase(game) then
    Exit;

  fgame := LowerCase(game);

  ClearMinfo;
  ClearSinfo;
  sprnames.Free;
  statenames.Free;

  sprnames := dll_getspritenames(fgame);

  lst := dll_getpcharfunc(game, 'dd_getstatescsv_');
  statenames := getcolumnfromcsv(lst, 'id');
  lst.Free;

  lst := dll_getpcharfunc(game, 'dd_getmobjinfocsv_');
  minfo := getcolumnfromcsv(lst, 'id');
  lst.Free;

  if sinfo <> nil then
    FillTreeView;

  FillListView;
end;

//==============================================================================
//
// TFrame_States.FillTreeView
//
//==============================================================================
procedure TFrame_States.FillTreeView;

  procedure AddTreeItem(const id: integer);
  var
    Item: TTreeNode;
    i: integer;
    lst: TStringList;
    fieldname, fieldvalue: string;
    spr, stnum: integer;
    brightstr: string;
  begin
    if id = 0 then
      Item := TreeView1.Items.AddFirst(nil, sinfo.Strings[id])
    else
      Item := TreeView1.Items.AddChild(nil, sinfo.Strings[id]);
    lst := sinfo.Objects[id] as TStringList;
    if lst <> nil then
      for i := 0 to lst.Count - 1 do
      begin
        splitstring(lst.Strings[i], fieldname, fieldvalue, ':');

        if fieldname = 'Sprite Number' then
        begin
          spr := StrToInt(fieldvalue);
          if (spr >= 0) and (spr < sprnames.Count) then
            fieldvalue := sprnames.Strings[spr]
          else
            fieldvalue := 'Unknown sprite number #' + fieldvalue;
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
            fieldvalue := Chr(Ord('A') + spr) + brightstr
          else
            fieldvalue := 'Unknown sprite subnumber #' + fieldvalue;
        end
        else if fieldname = 'Next Frame' then
        begin
          stnum := StrToIntDef(fieldvalue, -1);
          if (stnum >= 0) and (stnum < statenames.Count - 1) then
            fieldvalue := statenames.Strings[stnum];
        end;
        TreeView1.Items.AddChild(Item, fieldname + ': ' + fieldvalue);
      end;
  end;

var
  i: integer;
  srch: string;
begin
  if sinfo = nil then
    Exit;

  TreeView1.Items.BeginUpdate;
  try
    TreeView1.Items.Clear;

    srch := LowerCase(Trim(SearchEdit.Text));

    for i := 0 to sinfo.Count - 1 do
    begin
      if Length(srch) = 0 then
        AddTreeItem(i)
      else if Pos(srch, LowerCase(sinfo.Strings[i])) > 0 then
        AddTreeItem(i);
    end;

  finally
    TreeView1.Items.EndUpdate;
    FocusAndSelectFirstItem;
  end;
end;

//==============================================================================
//
// TFrame_States.FillListView
//
//==============================================================================
procedure TFrame_States.FillListView;

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
      if (n = 'Next Frame') or ((v = '') and (statenames.IndexOf(n) >= 0)) then
      begin
        stnum := StrToIntDef(v, -1);
        if (stnum < 0) or (stnum > sinfo.Count) then
          if v = '' then
            stnum := statenames.IndexOf(n)
          else
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
//    ListView1.AlphaSort;
    ListView1.Items.EndUpdate;
//    FocusAndSelectFirstItem;
  end;
end;

//==============================================================================
//
// TFrame_States.SearchEditChange
//
//==============================================================================
procedure TFrame_States.SearchEditChange(Sender: TObject);
begin
  FillTreeView;
  ClearFilterSpeedButton.Visible := SearchEdit.Text <> '';
end;

procedure TFrame_States.ClearFilterSpeedButtonClick(
  Sender: TObject);
begin
  SearchEdit.Clear;
end;

//==============================================================================
//
// TFrame_States.HintPanelResize
//
//==============================================================================
procedure TFrame_States.HintPanelResize(Sender: TObject);
begin
  HintEdit.Width := HintPanel.Width - 16;
end;

//==============================================================================
//
// TFrame_States.TreeView1Editing
//
//==============================================================================
procedure TFrame_States.TreeView1Editing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;

//==============================================================================
//
// TFrame_States.TreeView1Change
//
//==============================================================================
procedure TFrame_States.TreeView1Change(Sender: TObject; Node: TTreeNode);
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
