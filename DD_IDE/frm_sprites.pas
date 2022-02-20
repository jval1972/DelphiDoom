unit frm_sprites;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, StdCtrls, Buttons, ExtCtrls;

type
  TFrame_Sprites = class(TFrame)
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
    sprinfo: TStringList;
    fgame: string;
    sortcolumn: integer;
    procedure ClearSPRinfo;
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
// TFrame_Sprites.Create
//
//==============================================================================
constructor TFrame_Sprites.Create(AOwner: TComponent);
begin
  inherited;

  fgame := '';

  sprinfo := nil;
end;

//==============================================================================
//
// TFrame_Sprites.CreateParams
//
//==============================================================================
procedure TFrame_Sprites.CreateParams(var Params: TCreateParams);
begin
  sortcolumn := 0;
  Inherited;
end;

//==============================================================================
//
// TFrame_Sprites.Destroy
//
//==============================================================================
destructor TFrame_Sprites.Destroy;
begin
  ClearSPRinfo;
  inherited;
end;

//==============================================================================
//
// TFrame_Sprites.ClearSPRinfo
//
//==============================================================================
procedure TFrame_Sprites.ClearSPRinfo;
var
  i: integer;
  j: integer;
  lst: TStringList;
begin
  if sprinfo <> nil then
  begin
    for i := 0 to sprinfo.Count - 1 do
    begin
      lst := sprinfo.Objects[i] as TStringList;
      if lst <> nil then
      begin
        for j := 0 to lst.Count - 1 do
          lst.Objects[j].Free;
        lst.Free;
      end;
    end;
    sprinfo.Free;
    sprinfo := nil;
  end;
end;

//==============================================================================
//
// TFrame_Sprites.FocusAndSelectFirstItem
//
//==============================================================================
procedure TFrame_Sprites.FocusAndSelectFirstItem;
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
// TFrame_Sprites.FocusAndSelectFirstListItem
//
//==============================================================================
procedure TFrame_Sprites.FocusAndSelectFirstListItem;
begin
  if ListView1.Items.Count > 0 then
  begin
    ListView1.Selected := ListView1.Items[0];
    ListView1.ItemFocused := ListView1.Selected;
  end;
end;

//==============================================================================
//
// TFrame_Sprites.UpdateGameControls
//
//==============================================================================
procedure TFrame_Sprites.UpdateGameControls(const game: string);
var
  lst, tmp: TStringList;
  statesprites: TStringList;
  statespritesN: TStringList;
  statesnext: TStringList;
  mobjnames: TStringList;
  mobjstates: TStringList;
  mobjcsvstates: TStringList;
  i, j: integer;
  sprnum: integer;
  sprname, sprnameN: string;
  mobjname: string;
  stnum, stnum1, stnum2: integer;
begin
  if fgame = LowerCase(game) then
    Exit;

  fgame := LowerCase(game);

  ClearSPRinfo;

  lst := dll_getpcharfunc(game, 'dd_getspritescsv_');
  sprinfo := getcolumnfromcsv(lst, 'Sprite');
  lst.Free;

  for i := 0 to sprinfo.Count - 1 do
  begin
    tmp := TStringList.Create;
    tmp.Sorted := True;
    sprinfo.Objects[i] := tmp;
  end;

  lst := dll_getpcharfunc(game, 'dd_getstatescsv_');
  statesprites := getcolumnfromcsv(lst, 'Sprite Number');
  statespritesN := getcolumnfromcsv(lst, 'Sprite Subnumber');
  statesnext := getcolumnfromcsv(lst, 'Next Frame');
  lst.Free;

  for i := 0 to statesprites.Count - 1 do
  begin
    sprnum := StrToIntDef(statesprites.Strings[i], -1);
    if (sprnum >= 0) and (sprnum < sprinfo.Count) then
    begin
      lst := sprinfo.Objects[sprnum] as TStringList;
      sprnameN := sprinfo.Strings[sprnum] + Chr(Ord('A') + StrToIntDef(statespritesN.Strings[i], 0) mod 32768);
      if lst.IndexOf(sprnameN) < 0 then
      begin
        tmp := TStringList.Create;
        tmp.Sorted := True;
        lst.AddObject(sprnameN, tmp);
      end;
    end;
  end;

  for i := 0 to statesprites.Count - 1 do
    statesprites.Strings[i] := sprinfo.Strings[StrToIntDef(statesprites.Strings[i], 0)] + Chr(Ord('A') + StrToIntDef(statespritesN.Strings[i], 0) mod 32768);
  statespritesN.Free;

  lst := dll_getpcharfunc(game, 'dd_getmobjinfocsv_');
  if lst.Count > 0 then
  begin
    mobjcsvstates := csvlinetolist(lst.Strings[0]);
    for i := mobjcsvstates.Count - 1 downto 0 do
      if Pos(' FRAME: ', UpperCase(mobjcsvstates.Strings[i]) + ': ') = 0 then
        mobjcsvstates.Delete(i);

    mobjnames := getcolumnfromcsv(lst, 'Name');

    for i := 0 to mobjcsvstates.Count - 1 do
    begin
      mobjstates := getcolumnfromcsv(lst, mobjcsvstates.Strings[i]);

      for j := 0 to mobjstates.Count - 1 do
      begin
        mobjname := mobjnames.Strings[j];
        stnum := StrToIntDef(mobjstates.Strings[j], -1);
        stnum1 := stnum;
        while true do
        begin
          if (stnum >= 0) and (stnum < statesprites.Count) then
          begin
            sprnameN := statesprites.Strings[stnum];
            if Length(sprnameN) = 5 then
            begin
              sprname := Copy(sprnameN, 1, 4);
              sprnum := sprinfo.IndexOf(sprname);
              if sprnum >= 0 then
              begin
                tmp := sprinfo.Objects[sprnum] as TStringList;
                sprnum := tmp.IndexOf(sprnameN);
                if sprnum >= 0 then
                begin
                  tmp := tmp.Objects[sprnum] as TStringList;
                  if tmp.IndexOf(mobjname) < 0 then
                    tmp.Add(mobjname)
                  else
                    break;
                end;
              end;
            end;
          end;

          stnum2 := stnum;
          stnum := StrToIntDef(statesnext.strings[stnum], 0);
          if (stnum = 0) or (stnum = stnum1) or (stnum = stnum2) then
            break;
        end;
      end;

      mobjstates.Free;
    end;

    mobjcsvstates.Free;
    mobjnames.Free;
  end;
  lst.Free;

  statesprites.Free;
  statesnext.Free;

  sprinfo.Sorted := True;

  FillTreeView;

  FillListView;
end;

//==============================================================================
//
// TFrame_Sprites.FillTreeView
//
//==============================================================================
procedure TFrame_Sprites.FillTreeView;

  procedure AddTreeItem(const id: integer);
  var
    Item1, Item2: TTreeNode;
    i1, i2: integer;
    lst1, lst2: TStringList;
  begin
    if id = 0 then
      Item1 := TreeView1.Items.AddFirst(nil, sprinfo.Strings[id])
    else
      Item1 := TreeView1.Items.AddChild(nil, sprinfo.Strings[id]);
    lst1 := sprinfo.Objects[id] as TStringList;
    if lst1 <> nil then
      for i1 := 0 to lst1.Count - 1 do
      begin
        Item2 := TreeView1.Items.AddChild(Item1, lst1.Strings[i1]);
        lst2 := lst1.Objects[i1] as TStringList;
        if lst2 <> nil then
          for i2 := 0 to lst2.Count - 1 do
            TreeView1.Items.AddChild(Item2, lst2.Strings[i2]);
      end;
  end;

var
  i: integer;
  srch: string;
begin
  if sprinfo = nil then
    Exit;

  TreeView1.OnChange := nil;
  TreeView1.Items.BeginUpdate;
  try
    TreeView1.Items.Clear;

    srch := LowerCase(Trim(SearchEdit.Text));

    for i := 0 to sprinfo.Count - 1 do
    begin
      if Length(srch) = 0 then
        AddTreeItem(i)
      else if Pos(srch, LowerCase(sprinfo.Strings[i])) > 0 then
        AddTreeItem(i);
    end;

  finally
    TreeView1.Items.EndUpdate;
    TreeView1.OnChange := TreeView1Change;
    FocusAndSelectFirstItem;
  end;
end;

//==============================================================================
//
// TFrame_Sprites.FillListView
//
//==============================================================================
procedure TFrame_Sprites.FillListView;
var
  it, dad: TTreeNode;
  i, j, idx4, idx5: integer;
  pool: TStringList;
  lst4, lst5: TStringList;
  sprname, sprname4, sprname5: string;
  ListItem: TListItem;
begin
  ListView1.Items.BeginUpdate;
  try
    ListView1.Items.Clear;

    it := TreeView1.Selected;
    if it <> nil then
    begin
      dad := it.Parent;
      if (dad <> nil) and (dad.Parent <> nil) then
      begin
        ListItem := ListView1.Items.Add;
        ListItem.Caption := it.Text;
        ListItem.Data := Pointer(0);
      end
      else
      begin
        sprname := it.Text;
        sprname4 := '';
        sprname5 := '';
        if Length(sprname) = 4 then
          sprname4 := sprname
        else if Length(sprname) = 5 then
        begin
          sprname4 := Copy(sprname, 1, 4);
          sprname5 := sprname;
        end;

        idx4 := sprinfo.IndexOf(sprname4);
        if (sprname4 <> '') and (idx4 >= 0) then
        begin
          pool := TStringList.Create;
          lst4 := sprinfo.Objects[idx4] as TStringList;
          if sprname5 <> '' then
          begin
            idx5 := lst4.IndexOf(sprname5);
            lst5 := lst4.Objects[idx5] as TStringList;
            for i := 0 to lst5.Count - 1 do
              pool.Add(lst5.Strings[i]);
          end
          else
          begin
            for i := 0 to lst4.Count - 1 do
            begin
              lst5 := lst4.Objects[i] as TStringList;
              if lst5 <> nil then
                for j := 0 to lst5.Count - 1 do
                  if pool.IndexOf(lst5.Strings[j]) < 0 then
                    pool.Add(lst5.Strings[j]);
            end;
          end;

          pool.Sort;

          for i := 0 to pool.Count - 1 do
          begin
            ListItem := ListView1.Items.Add;
            ListItem.Caption := pool.Strings[i];
            ListItem.Data := Pointer(i);
          end;

          pool.Free;
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
// TFrame_Sprites.SearchEditChange
//
//==============================================================================
procedure TFrame_Sprites.SearchEditChange(Sender: TObject);
begin
  FillTreeView;
  ClearFilterSpeedButton.Visible := SearchEdit.Text <> '';
end;

procedure TFrame_Sprites.ClearFilterSpeedButtonClick(
  Sender: TObject);
begin
  SearchEdit.Clear;
end;

//==============================================================================
//
// TFrame_Sprites.HintPanelResize
//
//==============================================================================
procedure TFrame_Sprites.HintPanelResize(Sender: TObject);
begin
  HintEdit.Width := HintPanel.Width - 16;
end;

//==============================================================================
//
// TFrame_Sprites.TreeView1Editing
//
//==============================================================================
procedure TFrame_Sprites.TreeView1Editing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;

//==============================================================================
//
// TFrame_Sprites.TreeView1Change
//
//==============================================================================
procedure TFrame_Sprites.TreeView1Change(Sender: TObject; Node: TTreeNode);
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
