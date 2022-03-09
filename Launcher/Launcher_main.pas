unit Launcher_main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, CheckLst, ExtCtrls, Launcher_defs, Menus,
  XPMan, ImgList;

type
  resolution_t = record
    width, height: integer;
  end;
  Presolution_t = ^resolution_t;
  resolution_tArray = array[0..$FFF] of resolution_t;
  Presolution_tArray = ^resolution_tArray;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Bevel1: TBevel;
    Panel2: TPanel;
    RunDelphiDoomButton: TButton;
    Button2: TButton;
    Image2: TImage;
    HomePageButton: TButton;
    Panel3: TPanel;
    Panel4: TPanel;
    PageControl1: TPageControl;
    TabSheet4: TTabSheet;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    PWAdsCheckListBox: TCheckListBox;
    PaksCheckListBox: TCheckListBox;
    MapsComboBox: TComboBox;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    ScreenResolutionComboBox: TComboBox;
    DetailLevelComboBox: TComboBox;
    FullScreenCheckBox: TCheckBox;
    InterpolationCheckBox: TCheckBox;
    ZAxisShiftCheckBox: TCheckBox;
    Emulate3dCheckBox: TCheckBox;
    AutodetectDetailButton: TButton;
    TabSheet2: TTabSheet;
    NoSoundCheckBox: TCheckBox;
    NoMusicCheckBox: TCheckBox;
    TabSheet3: TTabSheet;
    UseMouseCheckBox: TCheckBox;
    UseJoystickCheckBox: TCheckBox;
    TabSheet5: TTabSheet;
    Label6: TLabel;
    Label7: TLabel;
    PlayerNumLabel: TLabel;
    NETComputersCheckListBox: TCheckListBox;
    GameModeComboBox: TComboBox;
    PlayerNumRadioGroup: TRadioGroup;
    TabSheet6: TTabSheet;
    DevelopmentModeCheckBox: TCheckBox;
    NoMonstersCheckBox: TCheckBox;
    RespawnCheckBox: TCheckBox;
    RandomMonstersCheckBox: TCheckBox;
    FastCheckBox: TCheckBox;
    UseMMXCheckBox: TCheckBox;
    SkillComboBox: TComboBox;
    Label9: TLabel;
    NetUnavailvableLabel: TLabel;
    TabSheet7: TTabSheet;
    PlayDemoCheckBox: TCheckBox;
    PlayDemoComboBox: TComboBox;
    LoadDemoButton: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    RecordDemoEdit: TEdit;
    RecordDemoCheckBox: TCheckBox;
    SaveDemoButton: TButton;
    SpecifyZoneSizeCheckBox: TCheckBox;
    ZoneSizeTrackBar: TTrackBar;
    ZoneSizeLabel: TLabel;
    UseTransparentSpritesCheckBox: TCheckBox;
    UseLightBoostCheckBox: TCheckBox;
    SpecifyLightBoostCheckBox: TCheckBox;
    LightBoostTrackBar: TTrackBar;
    Label10: TLabel;
    AdditionalParametersEdit: TEdit;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    RunDelphiDoom1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Options1: TMenuItem;
    N2: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Contactme1: TMenuItem;
    XPManifest1: TXPManifest;
    ChaseCameraCheckBox: TCheckBox;
    OpenPWADSButton: TButton;
    OpenWADDialog: TOpenDialog;
    OpenPAKDialog: TOpenDialog;
    OpenPK3Button: TButton;
    TabSheet8: TTabSheet;
    IconsImageList: TImageList;
    PageControl2: TPageControl;
    TabSheet9: TTabSheet;
    TabSheet10: TTabSheet;
    ListView1: TListView;
    SelectGameComboBox: TComboBox;
    Label11: TLabel;
    Label3: TLabel;
    IWADsComboBox: TComboBox;
    EngineRadioGroup: TRadioGroup;
    RetrieveNetworkComputersButton: TButton;
    NoSingleDemoCheckBox: TCheckBox;
    OpenGLCheckBox: TCheckBox;
    PopupMenu1: TPopupMenu;
    ItemProperties1: TMenuItem;
    KeyboardModeRadioGroup: TRadioGroup;
    MidiRadioGroup: TRadioGroup;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RunDelphiDoomClick(Sender: TObject);
    procedure HomePageButtonClick(Sender: TObject);
    procedure AutodetectDetailButtonClick(Sender: TObject);
    procedure GameModeComboBoxChange(Sender: TObject);
    procedure IWADsComboBoxChange(Sender: TObject);
    procedure NETComputersCheckListBoxClickCheck(Sender: TObject);
    procedure LoadDemoButtonClick(Sender: TObject);
    procedure PlayDemoCheckBoxClick(Sender: TObject);
    procedure RecordDemoCheckBoxClick(Sender: TObject);
    procedure SaveDemoButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EngineRadioGroupClick(Sender: TObject);
    procedure SpecifyZoneSizeCheckBoxClick(Sender: TObject);
    procedure ZoneSizeTrackBarChange(Sender: TObject);
    procedure SpecifyLightBoostCheckBoxClick(Sender: TObject);
    procedure Contactme1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure SelectGameComboBoxClick(Sender: TObject);
    procedure IWADsComboBoxClick(Sender: TObject);
    procedure OpenPWADSButtonClick(Sender: TObject);
    procedure OpenPK3ButtonClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure RetrieveNetworkComputersButtonClick(Sender: TObject);
    procedure ItemProperties1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    privatedemos: TStringList;
    basedir: string;
    gameengine: gameengine_t;
    closing: boolean;
    resolutions: Presolution_tArray;
    numresolutions: integer;
    { Private declarations }
    procedure Controls_To_Ini;
    procedure Ini_To_Controls;
    function GetExecutableName: string;
    procedure CheckGameEngine;
    procedure UpdateNetwork;
    procedure UpdateWads;
    procedure UpdateZoneParams;
    procedure UpdateLightBoostParams;
    procedure UpdateCurDir;
    procedure UpdateFromListView;
    procedure UpdateFiles;
    procedure UpdateGames;
    procedure InitGames;
    function GetCurrentCmd: string;
    function GetCurrentOptionsCmd: string;
    procedure PopulateIWADS;
    procedure PopulatePWADS;
    procedure PopulatePaks;
    procedure UpdateEasyStart;
    procedure QuickConfigureEasyStart(idx: integer);
    procedure RunCmd(const cmd: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  WinSock, Launcher_utils, Launcher_wads, Launcher_about, Launcher_options,
  Launcher_gameproperties;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.PopulateIWADS;
var
  sr: TSearchRec;
begin
  IWADsComboBox.Items.Clear;
  IWADsComboBox.Items.Add('(default)');
  if FindFirst('*.wad', faAnyFile, sr) = 0 then
  begin
    repeat
      if IsIWAD(sr.Name) then
        if (gameengine = ge_strife) and (UpperCase(sr.Name) = 'VOICES.WAD') then
        else
          IWADsComboBox.Items.Add(sr.Name);
    until FindNext(sr) <> 0;
  end;
  IWADsComboBox.ItemIndex := 0;
end;

procedure TForm1.PopulatePWADS;
var
  sr: TSearchRec;
  i: integer;
begin
  PWAdsCheckListBox.Items.Clear;
  if FindFirst('*.wad', faAnyFile, sr) = 0 then
  begin
    repeat
      if IsPWAD(sr.Name) then
        PWAdsCheckListBox.Items.Add(sr.Name);
    until FindNext(sr) <> 0;
  end;
  for i := 0 to PWAdsCheckListBox.Items.Count - 1 do
    PWAdsCheckListBox.Checked[i] := false;
end;

procedure TForm1.PopulatePaks;
var
  sr: TSearchRec;
  i: integer;

  procedure AddPak(mask: string);
  begin
    if FindFirst(mask, faAnyFile, sr) = 0 then
    begin
      repeat
        if IsPak(sr.Name) then
          PaksCheckListBox.Items.Add(sr.Name);
      until FindNext(sr) <> 0;
    end;
  end;

begin
  form1.PaksCheckListBox.Items.Clear;
  AddPak('*.pak');
  AddPak('*.pk3');
  AddPak('*.pk4');
  AddPak('*.zip');
  AddPak('*.wad');
  for i := 0 to PaksCheckListBox.Items.Count - 1 do
    PaksCheckListBox.Checked[i] := false;
end;

procedure TForm1.UpdateCurDir;
var
  dir: string;
begin
  CheckGameEngine;
  if gameengine < NUMGAMEENGINES then
  begin
    dir := gamepaths[Ord(gameengine)];
    {$I-}
    ChDir(basedir);
    ChDir(dir);
    {$I+}
  end;
end;

procedure TForm1.UpdateFiles;
begin
  UpdateCurDir;
  PopulateIWADs;
  PopulatePWADS;
  PopulatePaks;
end;

procedure TForm1.UpdateGames;
var
  idx: integer;
begin
  idx := SelectGameComboBox.ItemIndex - 1;
  if idx >= 0 then
  begin
    EngineRadioGroup.ItemIndex := Ord(gameinfo[idx].gameengine);
    IWADsComboBox.ItemIndex := 0;
  end;
end;

procedure TForm1.InitGames;
var
  i: integer;
begin
  SelectGameComboBox.Items.Clear;
  SelectGameComboBox.Items.Add('(select main wad below)');
  for i := 0 to Ord(gt_strifeteaser) do
    SelectGameComboBox.Items.Add(gameinfo[i].description);
  SelectGameComboBox.ItemIndex := 0;
end;

var
  defaultsfile: string;

procedure TForm1.FormCreate(Sender: TObject);
var
  dm: TDevMode;
  i: integer;
  s: string;
begin
  scaled := false;
  closing := false;
  privatedemos := TStringList.Create;
  resolutions := nil;
  numresolutions := 0;

  basedir := ExtractFilePath(ParamStr(0));
  if basedir = '' then
    basedir := '.\'
  else if basedir[length(basedir)] <> '\' then
    basedir := basedir + '\';

  defaultsfile := basedir + 'Launcher.ini';
  M_LoadDefaults(defaultsfile);
  UpdateEasyStart;

  ScreenResolutionCombobox.Items.Clear;
  i := 0;
  while EnumDisplaySettings(nil, i, dm) do
  begin
    if {(dm.dmPelsWidth >= 640) and }(dm.dmBitsPerPel = 32) then
    begin
      s := Format('%dx%d', [dm.dmPelsWidth, dm.dmPelsHeight]);
      if ScreenResolutionCombobox.Items.IndexOf(s) = -1 then
      begin
        ScreenResolutionCombobox.Items.Add(s);
        inc(numresolutions);
        ReallocMem(resolutions, numresolutions * SizeOf(resolution_t));
        resolutions[numresolutions - 1].width := dm.dmPelsWidth;
        resolutions[numresolutions - 1].height := dm.dmPelsHeight;
      end;
    end;
    Inc(i);
  end;
  if ScreenResolutionCombobox.Items.Count = 0 then // JVAL -> uneeded :)
    ScreenResolutionCombobox.Items.Add('640x480');
  if numresolutions = 0 then
  begin
    numresolutions := 1;
    ReallocMem(resolutions, numresolutions * SizeOf(resolution_t));
    resolutions[numresolutions - 1].width := 640;
    resolutions[numresolutions - 1].height := 480;
  end;
  ScreenResolutionCombobox.ItemIndex := ScreenResolutionCombobox.Items.Count - 1;

  Ini_To_Controls;

  GameModeComboBox.ItemIndex := 0;
  PageControl2.ActivepageIndex := 0;
  PageControl1.ActivepageIndex := 0;
  UpdateFiles;
  UpdateNetwork;
  UpdateWads;
  InitGames;
  UpdateZoneParams;
  UpdateLightBoostParams;
  PlayDemoComboBox.Enabled := false;
  RecordDemoEdit.Enabled := false;
  LoadDemoButton.Enabled := false;
  SaveDemoButton.Enabled := false;
end;

procedure TForm1.Controls_To_Ini;
begin
  opt_ScreenResolution := ScreenResolutionComboBox.Text;
  opt_DetailLevel := DetailLevelComboBox.ItemIndex;
  opt_Skill := SkillComboBox.ItemIndex;
  opt_DevelopmentMode := Ord(DevelopmentModeCheckBox.State);
  opt_NoMonsters := Ord(NoMonstersCheckBox.State);
  opt_Respawn := Ord(RespawnCheckBox.State);
  opt_UseMMX := Ord(UseMMXCheckBox.State);
  opt_RandomMonsters := Ord(RandomMonstersCheckBox.State);
  opt_Fast := Ord(FastCheckBox.State);
  opt_SpecifyZoneSize := SpecifyZoneSizeCheckBox.Checked;
  opt_ZoneSize := ZoneSizeTrackBar.Position;
  opt_FullScreen := Ord(FullScreenCheckBox.State);
  opt_Interpolation := Ord(InterpolationCheckBox.State);
  opt_ZAxisShift := Ord(ZAxisShiftCheckBox.State);
  opt_Emulate3d := Ord(Emulate3dCheckBox.State);
  opt_UseTransparentSprites := Ord(UseTransparentSpritesCheckBox.State);
  opt_ChaseCamera := Ord(ChaseCameraCheckBox.State);
  opt_UseLightBoost := Ord(UseLightBoostCheckBox.State);
  opt_SpecifyLightBoost := SpecifyLightBoostCheckBox.Checked;
  opt_LightBoost := LightBoostTrackBar.Position;
  opt_NoSound := Ord(NoSoundCheckBox.State);
  opt_NoMusic := Ord(NoMusicCheckBox.State);
  opt_UseMouse := Ord(UseMouseCheckBox.State);
  opt_UseJoystick := Ord(UseJoystickCheckBox.State);
end;

procedure TForm1.Ini_To_Controls;
begin
  ScreenResolutionComboBox.Text := opt_ScreenResolution;
  DetailLevelComboBox.ItemIndex := opt_DetailLevel;
  SkillComboBox.ItemIndex := opt_Skill;
  DevelopmentModeCheckBox.State := TCheckBoxState(opt_DevelopmentMode);
  NoMonstersCheckBox.State := TCheckBoxState(opt_NoMonsters);
  RespawnCheckBox.State := TCheckBoxState(opt_Respawn);
  UseMMXCheckBox.State := TCheckBoxState(opt_UseMMX);
  RandomMonstersCheckBox.State := TCheckBoxState(opt_RandomMonsters);
  FastCheckBox.State := TCheckBoxState(opt_Fast);
  SpecifyZoneSizeCheckBox.Checked := opt_SpecifyZoneSize;
  ZoneSizeTrackBar.Position := opt_ZoneSize;
  FullScreenCheckBox.State := TCheckBoxState(opt_FullScreen);
  InterpolationCheckBox.State := TCheckBoxState(opt_Interpolation);
  ZAxisShiftCheckBox.State := TCheckBoxState(opt_ZAxisShift);
  Emulate3dCheckBox.State := TCheckBoxState(opt_Emulate3d);
  UseTransparentSpritesCheckBox.State := TCheckBoxState(opt_UseTransparentSprites);
  ChaseCameraCheckBox.State := TCheckBoxState(opt_ChaseCamera);
  UseLightBoostCheckBox.State := TCheckBoxState(opt_UseLightBoost);
  SpecifyLightBoostCheckBox.Checked := opt_SpecifyLightBoost;
  LightBoostTrackBar.Position := opt_LightBoost;
  NoSoundCheckBox.State := TCheckBoxState(opt_NoSound);
  NoMusicCheckBox.State := TCheckBoxState(opt_NoMusic);
  UseMouseCheckBox.State := TCheckBoxState(opt_UseMouse);
  UseJoystickCheckBox.State := TCheckBoxState(opt_UseJoystick);
end;

function TForm1.GetExecutableName: string;
var
  mainwad: string;
begin
  if EngineRadioGroup.ItemIndex >= 0 then
  begin
    result := LowerCase(EngineRadioGroup.Items[EngineRadioGroup.ItemIndex]);
    exit;
  end;

  if IWADsComboBox.ItemIndex > 0 then
  begin
    mainwad := UpperCase(Trim(IWADsComboBox.Items[IWADsComboBox.ItemIndex]));
    if (Pos('HERETIC.WAD', mainwad) > 0) or
       (Pos('HERETIC1.WAD', mainwad) > 0) then
    begin
      result := 'heretic';
      exit;
    end
    else if (Pos('HEXEN.WAD', mainwad) > 0) or
            (Pos('HEXEN1.WAD', mainwad) > 0) then
    begin
      result := 'hexen';
      exit;
    end
    else if (Pos('STRIFE0.WAD', mainwad) > 0) or
            (Pos('STRIFE1.WAD', mainwad) > 0) then
    begin
      result := 'strife';
      exit;
    end;
  end;
  if FileExists('doom2f.wad') then
    result := 'doom'
  else if FileExists('doom2.wad') then
    result := 'doom'
  else if FileExists('plutonia.wad') then
    result := 'doom'
  else if FileExists('tnt.wad') then
    result := 'doom'
  else if FileExists('doomu.wad') then
    result := 'doom'
  else if FileExists('doom.wad') then
    result := 'doom'
  else if FileExists('doom1.wad') then
    result := 'doom'
  else if FileExists('heretic.wad') then
    result := 'heretic'
  else if FileExists('heretic1.wad') then
    result := 'heretic'
  else if FileExists('hexen.wad') then
    result := 'hexen'
  else if FileExists('hexen1.wad') then
    result := 'hexen'
  else if FileExists('strife0.wad') then
    result := 'strife'
  else if FileExists('strife1.wad') then
    result := 'strife'
  else
    result := 'doom'; // ????
end;

procedure TForm1.CheckGameEngine;
var
  exename: string;
begin
  exename := UpperCase(GetExecutableName);
  if exename = 'DOOM' then
    gameengine := ge_doom
  else if exename = 'HERETIC' then
    gameengine := ge_heretic
  else if exename = 'HEXEN' then
    gameengine := ge_hexen
  else if exename = 'STRIFE' then
    gameengine := ge_strife
  else
    gameengine := ge_unknown;
end;

const
  exenames: array[0..4] of string = (
    'doom32.exe',
    'heretic32.exe',
    'hexen32.exe',
    'strife32.exe',
    ''
  );

  glexenames: array[0..4] of string = (
    'gldoom32.exe',
    'glheretic32.exe',
    'glhexen32.exe',
    'glstrife32.exe',
    ''
  );

function TForm1.GetCurrentCmd: string;
var
  cmd: string;
  executable: string;

  procedure AddCmd(const s: string);
  begin
    if Pos(' ', s) = 0 then
      cmd := cmd + ' ' + s
    else
      cmd := cmd + ' "' + s + '"';
  end;

begin
  CheckGameEngine;
  if OpenGLCheckBox.Checked then
    executable := glexenames[Ord(gameengine)]
  else
    executable := exenames[Ord(gameengine)];
  cmd := executable;

  if SelectGameComboBox.ItemIndex > 0 then
  begin
    if gameinfo[SelectGameComboBox.ItemIndex - 1].mainwad <> '' then
    begin
      AddCmd('-mainwad');
      AddCmd(gameinfo[SelectGameComboBox.ItemIndex - 1].mainwad);
    end;
    if gameinfo[SelectGameComboBox.ItemIndex - 1].pwad <> '' then
    begin
      AddCmd('-file1');
      AddCmd(gameinfo[SelectGameComboBox.ItemIndex - 1].pwad);
    end;
    if gameinfo[SelectGameComboBox.ItemIndex - 1].extracmdline <> '' then
      cmd := cmd + ' ' + gameinfo[SelectGameComboBox.ItemIndex - 1].extracmdline;
  end
  else
  begin
  // Game
    if IWADsComboBox.ItemIndex > 0 then
    begin
      AddCmd('-mainwad');
      AddCmd(IWADsComboBox.Items[IWADsComboBox.ItemIndex]);
    end;

  end;

  result := cmd + ' ' + GetCurrentOptionsCmd;
end;

function TForm1.GetCurrentOptionsCmd: string;
var
  cmd: string;
  check: boolean;
  i: integer;
  swidth, sheight: integer;
  ep, map: integer;

  procedure AddCmd(const s: string);
  begin
    if Pos(' ', s) = 0 then
      cmd := cmd + ' ' + s
    else
      cmd := cmd + ' "' + s + '"';
  end;

begin
  check := false;
  for i := 0 to PWAdsCheckListBox.Count - 1 do
    if PWAdsCheckListBox.Checked[i] then
    begin
      check := true;
      break;
    end;

  cmd := '';
  if check then
  begin
    AddCmd('-lfile');
    for i := 0 to PWAdsCheckListBox.Count - 1 do
      if PWAdsCheckListBox.Checked[i] then
        AddCmd(PWAdsCheckListBox.Items.Strings[i]);
  end;

  check := false;
  for i := 0 to PaksCheckListBox.Count - 1 do
    if PaksCheckListBox.Checked[i] then
    begin
      check := true;
      break;
    end;

  if check then
  begin
    AddCmd('-lpakfile');
    for i := 0 to PaksCheckListBox.Count - 1 do
      if PaksCheckListBox.Checked[i] then
        AddCmd(PaksCheckListBox.Items.Strings[i]);
  end;

  if MapsComboBox.ItemIndex >= 0 then
    if GetMapNameInfo(MapsComboBox.Items[MapsComboBox.ItemIndex], ep, map) then
    begin
      AddCmd('-warp');
      if ep <> 0 then
        AddCmd(IntToStr(ep));
      AddCmd(IntToStr(map));
    end;

// Display
  if Get2Ints(ScreenResolutionComboBox.Text, swidth, sheight) then
  begin
    AddCmd('-screenwidth');
    AddCmd(IntToStr(swidth));
    AddCmd('-screenheight');
    AddCmd(IntToStr(sheight));
  end;

  if DetailLevelComboBox.ItemIndex > 0 then
  begin
    if DetailLevelComboBox.ItemIndex = 1 then
      AddCmd('-lowestres')
    else if DetailLevelComboBox.ItemIndex = 2 then
      AddCmd('-lowres')
    else if DetailLevelComboBox.ItemIndex = 3 then
      AddCmd('-mediumres')
    else if DetailLevelComboBox.ItemIndex = 4 then
      AddCmd('-normalres')
    else if DetailLevelComboBox.ItemIndex = 5 then
      AddCmd('-hires')
    else if DetailLevelComboBox.ItemIndex = 6 then
      AddCmd('-ultrares')
  end;

  if SkillComboBox.ItemIndex > 0 then
  begin
    AddCmd('-skill');
    AddCmd(IntToStr(SkillComboBox.ItemIndex));
  end;

  if DevelopmentModeCheckBox.State = cbChecked then
    AddCmd('-devparm');

  if NoMonstersCheckBox.State = cbChecked then
    AddCmd('-nomonsters');

  if RespawnCheckBox.State = cbChecked then
    AddCmd('-respawn');

  if UseMMXCheckBox.State = cbChecked then
    AddCmd('-mmx')
  else if UseMMXCheckBox.State = cbUnChecked then
    AddCmd('-nommx');

  if RandomMonstersCheckBox.State = cbChecked then
    AddCmd('-spawnrandommonsters')
  else if RandomMonstersCheckBox.State = cbUnChecked then
    AddCmd('-nospawnrandommonsters');

  if FastCheckBox.State = cbChecked then
    AddCmd('-fast');

  if SpecifyZoneSizeCheckBox.Checked then
  begin
    AddCmd('-zone');
    AddCmd(IntToStr(ZoneSizeTrackBar.Position));
  end;

  if FullScreenCheckBox.State = cbChecked then
    AddCmd('-fullscreen')
  else if FullScreenCheckBox.State = cbUnchecked then
    AddCmd('-nofullscreen');

  if InterpolationCheckBox.State = cbChecked then
    AddCmd('-interpolate')
  else if InterpolationCheckBox.State = cbUnchecked then
    AddCmd('-nointerpolate');

  if ZAxisShiftCheckBox.State = cbChecked then
    AddCmd('-zaxisshift')
  else if ZAxisShiftCheckBox.State = cbUnchecked then
    AddCmd('-nozaxisshift');

  if Emulate3dCheckBox.State = cbChecked then
    AddCmd('-fake3d')
  else if Emulate3dCheckBox.State = cbUnchecked then
    AddCmd('-nofake3d');

  if UseTransparentSpritesCheckBox.State = cbChecked then
    AddCmd('-usetransparentsprites')
  else if UseTransparentSpritesCheckBox.State = cbUnchecked then
    AddCmd('-dontusetransparentsprites');

  if ChaseCameraCheckBox.State = cbChecked then
    AddCmd('-chasecamera')
  else if ChaseCameraCheckBox.State = cbUnchecked then
    AddCmd('-nochasecamera');

  if UseLightBoostCheckBox.State = cbChecked then
    AddCmd('-uselightboost')
  else if UseLightBoostCheckBox.State = cbUnchecked then
    AddCmd('-dontuselightboost');

  if SpecifyLightBoostCheckBox.Checked then
  begin
    AddCmd('-lightboostfactor');
    AddCmd(IntToStr(LightBoostTrackBar.Position));
  end;

// Sound
  if MidiRadioGroup.ItemIndex = 1 then
    AddCmd('-uselegacymidiplayer')
  else if MidiRadioGroup.ItemIndex = 2 then
    AddCmd('-nouselegacymidiplayer')
  else if MidiRadioGroup.ItemIndex = 3 then
    AddCmd('-internalmidiplayer');

  if NoSoundCheckBox.State = cbChecked then
    AddCmd('-nosound');

  if NoMusicCheckBox.State = cbChecked then
    AddCmd('-nomusic');

// Controls
  if UseMouseCheckBox.State = cbChecked then
    AddCmd('-mouse')
  else if UseMouseCheckBox.State = cbUnchecked then
    AddCmd('-nomouse');

  if UseJoystickCheckBox.State = cbChecked then
    AddCmd('-joystick')
  else if UseJoystickCheckBox.State = cbUnchecked then
    AddCmd('-nojoystick');

  if GameModeComboBox.ItemIndex > 0 then
  begin
    if GameModeComboBox.ItemIndex = 1 then
      AddCmd('-deathmatch');
    AddCmd('-net');
    AddCmd(IntToStr(PlayerNumRadioGroup.ItemIndex + 1));
    for i := 0 to NETComputersCheckListBox.Items.Count - 1 do
      if NETComputersCheckListBox.Selected[i] then
        AddCmd(I_GetComputerName(NETComputersCheckListBox.Items.Strings[i]));
  end;

  if PlayDemoCheckBox.Checked then
  begin
    if PlayDemoComboBox.Text <> '' then
    begin
      AddCmd('-playdemo');
      AddCmd(PlayDemoComboBox.Text);
    end;
  end
  else if RecordDemoCheckBox.Checked then
  begin
    if RecordDemoEdit.Text <> '' then
    begin
      AddCmd('-record');
      AddCmd(RecordDemoEdit.Text);
    end;
  end;

  if NoSingleDemoCheckBox.Checked then
    AddCmd('-nosingledemo');

  if KeyboardModeRadioGroup.ItemIndex > 0 then
  begin
    AddCmd('-keyboardmode');
    AddCmd(IntToStr(KeyboardModeRadioGroup.ItemIndex - 1));
  end;

  result := cmd;
  if Trim(AdditionalParametersEdit.Text) <> '' then
    result := result + ' ' + Trim(AdditionalParametersEdit.Text);
end;

procedure TForm1.RunDelphiDoomClick(Sender: TObject);
var
  cmd: string;
begin
  if PageControl2.ActivePageIndex = 0 then
  begin
    ListView1DblClick(Sender);
  end
  else
  begin
    cmd := GetCurrentCmd;
    RunCmd(cmd);
  end;
end;

procedure TForm1.RunCmd(const cmd: string);
var
  t: TextFile;
  weret: integer;
  errmsg: string;
begin
  if not I_IsCDROMDrive then
  begin
  {$I-}
    AssignFile(t, 'Launcher.last.cmd');
    rewrite(t);
    writeln(t, cmd);
    CloseFile(t);
  {$I+}
  end;

  weret := WinExec(PChar(cmd), SW_SHOWNORMAL);
  if weret > 31 then
//    Close
  else
  begin
    if weret = 0 then
      errmsg := 'The system is out of memory or resources.'
    else if weret = ERROR_BAD_FORMAT then
      errmsg := 'The ' + firstword(cmd) + ' file is invalid (non-Win32 .EXE or error in .EXE image).'
    else if weret = ERROR_FILE_NOT_FOUND then
      errmsg := 'The ' + firstword(cmd) + ' file was not found.'
    else if weret = ERROR_PATH_NOT_FOUND then
      errmsg := 'Path not found.'
    else
      errmsg := 'Can not run ' + firstword(cmd) + '.';

    ShowMessage(errmsg);
  end;
end;

procedure TForm1.HomePageButtonClick(Sender: TObject);
begin
  I_GoToWebPage('http://sourceforge.net/projects/delphidoom/');
end;

procedure TForm1.AutodetectDetailButtonClick(Sender: TObject);
var
  cpuspeed: double;
  cpusfactor: double;
  w, h: integer;
  mindiff, diff: integer;
  best: integer;
  i: integer;
begin
  Screen.Cursor := crHourglass;
  try
    GetCPUSpeed(100);
    cpuspeed := GetCPUSpeed(500);
  finally
    Screen.Cursor := crDefault;
  end;

  cpusfactor := sqrt(I_GetNumCPUs);
  if cpusfactor > 8.0 then
    cpusfactor := 8.0;

  cpuspeed := cpuspeed * cpusfactor;

  if cpuspeed < 81 then
  begin
    w := 320;
    h := 200;
    DetailLevelComboBox.ItemIndex := 3;
  end
  else if cpuspeed < 201 then
  begin
    w := 400;
    h := 300;
    DetailLevelComboBox.ItemIndex := 3;
  end
  else if cpuspeed < 601 then
  begin
    w := 640;
    h := 400;
    DetailLevelComboBox.ItemIndex := 3;
  end
  else if cpuspeed < 801 then
  begin
    w := 640;
    h := 480;
    DetailLevelComboBox.ItemIndex := 3;
  end
  else if cpuspeed < 1001 then
  begin
    w := 640;
    h := 480;
    DetailLevelComboBox.ItemIndex := 4;
  end
  else if cpuspeed < 1501 then
  begin
    w := 800;
    h := 600;
    DetailLevelComboBox.ItemIndex := 4;
  end
  else if cpuspeed < 2200 then
  begin
    w := 1280;
    h := 800;
    DetailLevelComboBox.ItemIndex := 4;
  end
  else if cpuspeed < 3000 then
  begin
    w := 1366;
    h := 768;
    DetailLevelComboBox.ItemIndex := 4;
  end
  else if cpuspeed < 4000 then
  begin
    w := 1600;
    h := 900;
    DetailLevelComboBox.ItemIndex := 4;
  end
  else if cpuspeed < 5000 then
  begin
    w := 1920;
    h := 1080;
    DetailLevelComboBox.ItemIndex := 4;
  end
  else if cpuspeed < 6000 then
  begin
    w := 2560;
    h := 1080;
    DetailLevelComboBox.ItemIndex := 4;
  end
  else if cpuspeed < 8000 then
  begin
    w := 2560;
    h := 1440;
    DetailLevelComboBox.ItemIndex := 4;
  end
  else
  begin
    w := 3840;
    h := 2160;
    DetailLevelComboBox.ItemIndex := 4;
  end;

{  if w > I_ScreenWidth then
    w := I_ScreenWidth;
  if h > I_ScreenHeight then
    h := I_ScreenHeight;}

  best := -1;
  mindiff := MAXINT;

  for i := 0 to numresolutions - 1 do
  begin
    diff := abs(resolutions[i].width * resolutions[i].height - w * h);
    if diff < mindiff then
    begin
      best := i;
      mindiff := diff;
      if w = resolutions[i].width then
        if h = resolutions[i].height then
          break;
    end;
  end;

  if best >= 0 then
    ScreenResolutionComboBox.Text := IntToStr(resolutions[best].width) + 'x' + IntToStr(resolutions[best].height)
  else
    ScreenResolutionComboBox.Text := IntToStr(w) + 'x' + IntToStr(h);

end;

procedure TForm1.UpdateNetwork;
var
  i: integer;
  pl: integer;
  maxpl: integer;
  oldidx: integer;
begin
  if not I_WinsockEnabled then
  begin
    GameModeComboBox.Enabled := false;
    NETComputersCheckListBox.Enabled := false;
    PlayerNumLabel.Caption := '(Number of players: 1)';
    PlayerNumRadioGroup.Items.Clear;
    PlayerNumRadioGroup.Items.Add('Player Number 1');
    PlayerNumRadioGroup.ItemIndex := 0;
    PlayerNumRadioGroup.Enabled := false;
    NetUnavailvableLabel.Caption := '(Network Unavailable)';
  end
  else
  begin
    NetUnavailvableLabel.Caption := '';

    if GameModeComboBox.ItemIndex = 0 then
    begin
      NETComputersCheckListBox.Enabled := false;
      PlayerNumLabel.Caption := '(Number of players: 1)';
      PlayerNumRadioGroup.Items.Clear;
      PlayerNumRadioGroup.Items.Add('Player Number 1');
      PlayerNumRadioGroup.ItemIndex := 0;
    end
    else
    begin
      CheckGameEngine;
      if gameengine = ge_hexen then
        maxpl := 8
      else if gameengine = ge_strife then
        maxpl := 8
      else
        maxpl := 4;
      NETComputersCheckListBox.Enabled := true;
      pl := 1;
      for i := 0 to NETComputersCheckListBox.Count - 1 do
        if NETComputersCheckListBox.Checked[i] then
        begin
          if pl < maxpl then
            inc(pl)
          else
            NETComputersCheckListBox.Checked[i] := false;
        end;
      PlayerNumLabel.Caption := '(Number of players: ' + IntToStr(pl) + ')';

      oldidx := PlayerNumRadioGroup.ItemIndex;
      PlayerNumRadioGroup.Items.Clear;
      for i := 1 to pl do
        PlayerNumRadioGroup.Items.Add('Player Number ' + IntToStr(i));
      if oldidx < PlayerNumRadioGroup.Items.Count then
        PlayerNumRadioGroup.ItemIndex := oldidx
      else
        PlayerNumRadioGroup.ItemIndex := 0;
    end;
  end;

  PlayerNumRadioGroup.Height := PlayerNumRadioGroup.Items.Count * 21 + 20;
end;

procedure TForm1.UpdateWads;
var
  wadname: string;
  s: TStringList;
  dem: string;
  idx: integer;
begin
  if closing then
    exit;
  idx := -1;
  if PageControl2.ActivePageIndex = 0 then
    if ListView1.Selected <> nil then
      idx := ListView1.Selected.Index;

  if idx <> -1 then
  begin
    wadname := gameinfo[idx].mainwad;
  end
  else
  if SelectGameComboBox.ItemIndex <> 0 then
  begin
    wadname := gameinfo[SelectGameComboBox.ItemIndex - 1].mainwad;
  end
  else if IWADsComboBox.ItemIndex = 0 then
  begin
    CheckGameEngine;
    if gameengine = ge_hexen then
    begin
      if FileExists('hexen.wad') then
        wadname := 'hexen.wad'
      else if FileExists('hexen1.wad') then
        wadname := 'hexen1.wad'
      else
        exit; // No WADS!
    end
    else if gameengine = ge_heretic then
    begin
      if FileExists('heretic.wad') then
        wadname := 'heretic.wad'
      else if FileExists('heretic1.wad') then
        wadname := 'heretic1.wad'
      else
        exit; // No WADS!
    end
    else if gameengine = ge_strife then
    begin
      if FileExists('strife1.wad') then
        wadname := 'strife1.wad'
      else if FileExists('strife0.wad') then
        wadname := 'strife0.wad'
      else
        exit; // No WADS!
    end
    else if FileExists('doom2f.wad') then
      wadname := 'doom2f.wad'
    else if FileExists('doom2.wad') then
      wadname := 'doom2.wad'
    else if FileExists('plutonia.wad') then
      wadname := 'plutonia.wad'
    else if FileExists('tnt.wad') then
      wadname := 'tnt.wad'
    else if FileExists('doomu.wad') then
      wadname := 'doomu.wad'
    else if FileExists('doom.wad') then
      wadname := 'doom.wad'
    else if FileExists('doom1.wad') then
      wadname := 'doom1.wad'
    else if FileExists('heretic.wad') then
      wadname := 'heretic.wad'
    else if FileExists('heretic1.wad') then
      wadname := 'heretic1.wad'
    else if FileExists('hexen.wad') then
      wadname := 'hexen.wad'
    else if FileExists('hexen1.wad') then
      wadname := 'hexen1.wad'
    else if FileExists('strife1.wad') then
      wadname := 'strife1.wad'
    else if FileExists('strife0.wad') then
      wadname := 'strife0.wad'
    else
      exit; // NO WADS found
  end
  else
    wadname := IWADsComboBox.Items[IWADsComboBox.itemindex];

  Screen.Cursor := crHourglass;

  s := TStringList.Create;
  try
    GetWadMapNames(wadname, s);
    MapsComboBox.Items.Clear;
    MapsComboBox.Items.Add('(default)');
    MapsComboBox.Items.AddStrings(s);
    MapsComboBox.ItemIndex := 0;

    s.Clear;
    if PlayDemoComboBox.ItemIndex > -1 then
      dem := PlayDemoComboBox.Items.Strings[PlayDemoComboBox.ItemIndex]
    else
      dem := '';
    GetWadDemoNames(wadname, s);
    PlayDemoComboBox.Items.Clear;
    PlayDemoComboBox.Items.AddStrings(s);
    PlayDemoComboBox.Items.AddStrings(privatedemos);
    PlayDemoComboBox.ItemIndex := 0;
    if dem <> '' then
    begin
      idx := PlayDemoComboBox.Items.IndexOf(dem);
      if idx > -1 then
        PlayDemoComboBox.ItemIndex := idx;
    end;
  finally
    s.Free;
  end;

  Screen.Cursor := crDefault;

end;

procedure TForm1.GameModeComboBoxChange(Sender: TObject);
begin
  UpdateNetwork;
end;

procedure TForm1.IWADsComboBoxChange(Sender: TObject);
begin
  UpdateWads;
end;

procedure TForm1.NETComputersCheckListBoxClickCheck(Sender: TObject);
begin
  UpdateNetwork;
end;

procedure TForm1.LoadDemoButtonClick(Sender: TObject);
var
  dem: string;
  idx: integer;
begin
  if OpenDialog1.Execute then
  begin
    dem := OpenDialog1.FileName;
    if PlayDemoComboBox.Items.IndexOf(dem) = -1 then
    begin
      PlayDemoComboBox.Items.Add(dem);
      privatedemos.Add(dem);
    end;
    idx := PlayDemoComboBox.Items.IndexOf(dem);
    if idx > -1 then
      PlayDemoComboBox.ItemIndex := idx;
  end;
end;

procedure TForm1.PlayDemoCheckBoxClick(Sender: TObject);
begin
  PlayDemoComboBox.Enabled := PlayDemoCheckBox.Checked;
  LoadDemoButton.Enabled := PlayDemoCheckBox.Checked;
  if PlayDemoCheckBox.Checked then
  begin
    RecordDemoCheckBox.Checked := false;
    RecordDemoEdit.Enabled := false;
    SaveDemoButton.Enabled := false;
  end;
end;

procedure TForm1.RecordDemoCheckBoxClick(Sender: TObject);
begin
  RecordDemoEdit.Enabled := RecordDemoCheckBox.Checked;
  SaveDemoButton.Enabled := RecordDemoCheckBox.Checked;
  if RecordDemoCheckBox.Checked then
  begin
    PlayDemoCheckBox.Checked := false;
    PlayDemoComboBox.Enabled := false;
    LoadDemoButton.Enabled := false;
  end;
end;

procedure TForm1.SaveDemoButtonClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    RecordDemoEdit.Text := SaveDialog1.FileName;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  closing := true;
  privatedemos.Free;
  Controls_To_Ini;
  M_SaveDefaults(defaultsfile);
  ReallocMem(resolutions, 0);
  numresolutions := 0;
end;

procedure TForm1.EngineRadioGroupClick(Sender: TObject);
begin
  SelectGameComboBox.ItemIndex := 0;
  UpdateFiles;
  UpdateWads;
end;

procedure TForm1.UpdateZoneParams;
begin
  CheckGameEngine;
  if gameengine = ge_hexen then
  begin
    if ZoneSizeTrackBar.Position < 8 then
      ZoneSizeTrackBar.Position := 8;
    ZoneSizeTrackBar.Min := 8;
  end
  else
    ZoneSizeTrackBar.Min := 6;
  ZoneSizeTrackBar.Enabled := SpecifyZoneSizeCheckBox.Checked;
  ZoneSizeTrackBar.Visible := SpecifyZoneSizeCheckBox.Checked;
  ZoneSizeLabel.Caption := Format('%d MB', [ZoneSizeTrackBar.Position]);
end;

procedure TForm1.SpecifyZoneSizeCheckBoxClick(Sender: TObject);
begin
  UpdateZoneParams;
end;

procedure TForm1.ZoneSizeTrackBarChange(Sender: TObject);
begin
  UpdateZoneParams;
end;

procedure TForm1.UpdateLightBoostParams;
begin
  LightBoostTrackBar.Enabled := SpecifyLightBoostCheckBox.Checked;
end;

procedure TForm1.SpecifyLightBoostCheckBoxClick(Sender: TObject);
begin
  UpdateLightBoostParams;
end;

procedure TForm1.Contactme1Click(Sender: TObject);
begin
  I_SendMail;
end;

procedure TForm1.About1Click(Sender: TObject);
var
  frm: TForm;
begin
  frm := TAboutForm.Create(self);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TForm1.Options1Click(Sender: TObject);
begin
  if OptionsFormDlg then
  begin
    UpdateFiles;
    UpdateEasyStart;
  end;
end;

procedure TForm1.File1Click(Sender: TObject);
begin
  CheckGameEngine;
  if gameengine = ge_doom then
    RunDelphiDoom1.Caption := '&Run DelphiDoom'
  else if gameengine = ge_heretic then
    RunDelphiDoom1.Caption := '&Run DelphiHeretic'
  else if gameengine = ge_hexen then
    RunDelphiDoom1.Caption := '&Run DelphiHexen'
  else if gameengine = ge_strife then
    RunDelphiDoom1.Caption := '&Run DelphiStrife';
end;

procedure TForm1.SelectGameComboBoxClick(Sender: TObject);
var
  idx: integer;
begin
  idx := SelectGameComboBox.ItemIndex;
  UpdateWads;
  UpdateGames;
  SelectGameComboBox.ItemIndex := idx;
end;

procedure TForm1.IWADsComboBoxClick(Sender: TObject);
begin
  SelectGameComboBox.ItemIndex := 0;
end;

procedure SelectFromListBox(const lb: TCheckListBox; const sl: TStrings);
var
  i, j: Integer;
begin
  for i := 0 to sl.Count - 1 do
  begin
    j := lb.Items.Count - 1;
    while j >= 0 do
    begin
      if sl.Strings[i] = lb.Items.Strings[j] then
      begin
        lb.Checked[j] := True;
        j := 0;
      end;
      dec(j);
    end;
  end;
end;

procedure TForm1.OpenPWADSButtonClick(Sender: TObject);
begin
  if OpenWADDialog.Execute then
  begin
    PWAdsCheckListBox.Items.AddStrings(OpenWADDialog.Files);
    SelectFromListBox(PWAdsCheckListBox, OpenWADDialog.Files);
    UpdateCurDir;
  end;
end;

procedure TForm1.OpenPK3ButtonClick(Sender: TObject);
begin
  if OpenPAKDialog.Execute then
  begin
    PaksCheckListBox.Items.AddStrings(OpenPAKDialog.Files);
    SelectFromListBox(PaksCheckListBox, OpenPAKDialog.Files);
    UpdateCurDir;
  end;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.UpdateEasyStart;
var
  i: integer;
  idx: integer;
  item: TListItem;
begin
  ListView1.Items.Clear;
  idx := 0;
  for i := 0 to Ord(NUMGAMETYPES) - 1 do
  begin
    item := ListView1.Items.Insert(idx);
    item.Caption := gameinfo[idx].description;
    item.ImageIndex := Ord(gameinfo[idx].gameengine);
    inc(idx);
  end;

end;

procedure TForm1.QuickConfigureEasyStart(idx: integer);
begin
  if (idx >= 0) and (idx < Ord(NUMGAMETYPES)) then
    if GamePropertiesDialog(@gameinfo[idx]) then
    begin
      ListView1.Items[idx].Caption := gameinfo[idx].description;
      ListView1.Items[idx].ImageIndex := Ord(gameinfo[idx].gameengine);
    end;
end;

procedure TForm1.ListView1DblClick(Sender: TObject);
var
  idx: integer;
  gi: Pgameinfo_t;
  cmd: string;

  procedure AddCmd(const s: string);
  begin
    if Pos(' ', s) = 0 then
      cmd := cmd + ' ' + s
    else
      cmd := cmd + ' "' + s + '"';
  end;

begin
  if ListView1.Selected = nil then
    idx := -1
  else
    idx := ListView1.Selected.Index;
  if (idx >= 0) and (idx < Ord(NUMGAMETYPES)) then
  begin
    gi := @gameinfo[idx];
    if Ord(gi.gameengine) < Ord(NUMGAMEENGINES) then
    begin
      if OpenGLCheckBox.Checked then
        cmd := glexenames[Ord(gi.gameengine)]
      else
        cmd := exenames[Ord(gi.gameengine)];

      if gi.mainwad <> '' then
      begin
        AddCmd('-mainwad');
        AddCmd(gi.mainwad);
      end;

      if gi.pwad <> '' then
      begin
        AddCmd('-lfile');
        AddCmd(gi.pwad);
      end;

      if gi.extracmdline <> '' then
        cmd := cmd + ' ' + gi.extracmdline;

      {$I-}
      ChDir(basedir);
      ChDir(gamepaths[Ord(gi.gameengine)]);
      {$I+}

      cmd := cmd + GetCurrentOptionsCmd;

      RunCmd(cmd);

      UpdateCurDir;
    end
    else
      QuickConfigureEasyStart(idx);
  end
  else
    MessageBox(Handle, 'Please select game/WAD', PChar(Application.Title), MB_OK or MB_ICONEXCLAMATION);
end;

procedure TForm1.UpdateFromListView;
var
  idx: integer;
begin
  idx := -1;
  if PageControl2.ActivePageIndex = 0 then
    if ListView1.Selected <> nil then
      idx := ListView1.Selected.Index;

  if idx <> -1 then
    if Ord(gameinfo[idx].gameengine) < EngineRadioGroup.Items.Count then
      EngineRadioGroup.ItemIndex := Ord(gameinfo[idx].gameengine);

end;

procedure TForm1.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  UpdateFromListView;
  UpdateCurDir;
  UpdateWads;
end;

procedure TForm1.RetrieveNetworkComputersButtonClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    I_GetNetComputeListWithOutLocal(NETComputersCheckListBox.Items);
    UpdateNetwork;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.ItemProperties1Click(Sender: TObject);
var
  idx: integer;
begin
  if ListView1.Selected <> nil then
  begin
    idx := ListView1.Selected.Index;
    if (idx >= 0) and (idx < Ord(NUMGAMETYPES)) then
    begin
      if GamePropertiesDialog(@gameinfo[idx]) then
      begin
        ListView1.Items[idx].Caption := gameinfo[idx].description;
        ListView1.Items[idx].ImageIndex := Ord(gameinfo[idx].gameengine);
      end;
      exit;
    end;
  end;
  MessageBeep(0);
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
var
  idx: integer;
begin
  if ListView1.Selected <> nil then
  begin
    idx := ListView1.Selected.Index;
    if (idx >= 0) and (idx < Ord(NUMGAMETYPES)) then
    begin
      ItemProperties1.Enabled := true;
      exit;
    end;
  end;
  ItemProperties1.Enabled := false;
end;

end.

