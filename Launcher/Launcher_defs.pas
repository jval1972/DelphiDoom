unit Launcher_defs;

interface

type
  gameengine_t = (
    ge_doom,
    ge_heretic,
    ge_hexen,
    ge_strife,
    NUMGAMEENGINES,
    ge_unknown
  );

type
  gameinfo_t = record
    description: string;
    mainwad: string;
    pwad: string;
    extracmdline: string;
    gameengine: gameengine_t;
  end;
  Pgameinfo_t = ^gameinfo_t;

  gametype_t = (
    gt_sharewaredoom,
    gt_registereddoom,
    gt_retaildoom,
    gt_doom2,
    gt_plutonia,
    gt_tnt,
    gt_sharewareheretic,
    gt_registeredheretic,
    gt_extendedheretic,
    gt_sharewarehexen,
    gt_registeredhexen,
    gt_expansionhexen, // HEXDD.WAD ????
    gt_strife,
    gt_strifeteaser,
    gt_custom01,
    gt_custom02,
    gt_custom03,
    gt_custom04,
    gt_custom05,
    gt_custom06,
    gt_custom07,
    gt_custom08,
    gt_custom09,
    gt_custom10,
    gt_custom11,
    gt_custom12,
    gt_custom13,
    gt_custom14,
    gt_custom15,
    gt_custom16,
    gt_custom17,
    gt_custom18,
    gt_custom19,
    gt_custom20,
    gt_custom21,
    gt_custom22,
    gt_custom23,
    gt_custom24,
    gt_custom25,
    gt_custom26,
    gt_custom27,
    gt_custom28,
    gt_custom29,
    gt_custom30,
    gt_custom31,
    gt_custom32,
    gt_custom33,
    gt_custom34,
    gt_custom35,
    gt_custom36,
    gt_custom37,
    gt_custom38,
    gt_custom39,
    gt_custom40,
    gt_custom41,
    gt_custom42,
    gt_custom43,
    gt_custom44,
    gt_custom45,
    gt_custom46,
    gt_custom47,
    gt_custom48,
    gt_custom49,
    gt_custom50,
    gt_custom51,
    gt_custom52,
    gt_custom53,
    gt_custom54,
    gt_custom55,
    gt_custom56,
    gt_custom57,
    gt_custom58,
    gt_custom59,
    gt_custom60,
    gt_custom61,
    gt_custom62,
    gt_custom63,
    gt_custom64,
    gt_custom65,
    gt_custom66,
    gt_custom67,
    gt_custom68,
    gt_custom69,
    gt_custom70,
    gt_custom71,
    gt_custom72,
    gt_custom73,
    gt_custom74,
    gt_custom75,
    gt_custom76,
    gt_custom77,
    gt_custom78,
    gt_custom79,
    gt_custom80,
    gt_custom81,
    gt_custom82,
    gt_custom83,
    gt_custom84,
    gt_custom85,
    gt_custom86,
    gt_custom87,
    gt_custom88,
    gt_custom89,
    gt_custom90,
    gt_custom91,
    gt_custom92,
    gt_custom93,
    gt_custom94,
    gt_custom95,
    gt_custom96,
    gt_custom97,
    gt_custom98,
    gt_custom99,
    NUMGAMETYPES
  );

var
  gamepaths: array[0..Ord(NUMGAMEENGINES) - 1] of string;
  gameinfo: array[0..Ord(NUMGAMETYPES) - 1] of gameinfo_t;

var
  opt_ScreenResolution: string;
  opt_DetailLevel: integer;
  opt_Skill: integer;
  opt_DevelopmentMode: integer;
  opt_NoMonsters: integer;
  opt_Respawn: integer;
  opt_UseMMX: integer;
  opt_RandomMonsters: integer;
  opt_Fast: integer;
  opt_SpecifyZoneSize: boolean;
  opt_ZoneSize: integer;
  opt_FullScreen: integer;
  opt_Interpolation: integer;
  opt_ZAxisShift: integer;
  opt_Emulate3d: integer;
  opt_UseTransparentSprites: integer;
  opt_ChaseCamera: integer;
  opt_UseLightBoost: integer;
  opt_SpecifyLightBoost: boolean;
  opt_LightBoost: integer;
  opt_NoSound: integer;
  opt_NoMusic: integer;
  opt_UseMouse: integer;
  opt_UseJoystick: integer;

procedure MakeDefaultGameInfo;

type
  ttype_t = (tString, tInteger, tBoolean, tGroup);

  default_t = record
    name: string;
    location: pointer;
    defaultsvalue: string;
    defaultivalue: integer;
    defaultbvalue: boolean;
    _type: ttype_t;
  end;
  Pdefault_t = ^default_t;

const
  NUMDEFAULTS = 30;

  defaults: array[0..NUMDEFAULTS - 1] of default_t = (
    (name: 'Directories';
     location: nil;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'doom_path';
     location: @gamepaths[Ord(ge_doom)];
     defaultsvalue: 'doom\';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString),

    (name: 'heretic_path';
     location: @gamepaths[Ord(ge_heretic)];
     defaultsvalue: 'heretic\';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString),

    (name: 'hexen_path';
     location: @gamepaths[Ord(ge_hexen)];
     defaultsvalue: 'hexen\';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString),

    (name: 'strife_path';
     location: @gamepaths[Ord(ge_strife)];
     defaultsvalue: 'strife\';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString),

    (name: 'Options';
     location: nil;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'screen_resolution';
     location: @opt_ScreenResolution;
     defaultsvalue: '640x480';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString),

    (name: 'detail_level';
     location: @opt_DetailLevel;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'skill';
     location: @opt_Skill;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'development_mode';
     location: @opt_DevelopmentMode;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'no_monsters';
     location: @opt_NoMonsters;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'respawn_monsters';
     location: @opt_Respawn;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'use_mmx';
     location: @opt_UseMMX;
     defaultsvalue: '';
     defaultivalue: 2;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'random_monsters';
     location: @opt_RandomMonsters;
     defaultsvalue: '';
     defaultivalue: 2;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'fast';
     location: @opt_Fast;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'specify_zone';
     location: @opt_SpecifyZoneSize;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'zone_size';
     location: @opt_ZoneSize;
     defaultsvalue: '';
     defaultivalue: 32;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'fullscreen';
     location: @opt_FullScreen;
     defaultsvalue: '';
     defaultivalue: 2;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'interpolate';
     location: @opt_Interpolation;
     defaultsvalue: '';
     defaultivalue: 2;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'zaxisshift';
     location: @opt_ZAxisShift;
     defaultsvalue: '';
     defaultivalue: 2;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'fake3d';
     location: @opt_Emulate3d;
     defaultsvalue: '';
     defaultivalue: 2;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'transparent_sprites';
     location: @opt_UseTransparentSprites;
     defaultsvalue: '';
     defaultivalue: 2;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'chase_camera';
     location: @opt_ChaseCamera;
     defaultsvalue: '';
     defaultivalue: 2;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'use_light_boost';
     location: @opt_UseLightBoost;
     defaultsvalue: '';
     defaultivalue: 2;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'specify_light_boost';
     location: @opt_SpecifyLightBoost;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'light_boost';
     location: @opt_LightBoost;
     defaultsvalue: '';
     defaultivalue: 256;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'no_sound';
     location: @opt_NoSound;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'no_music';
     location: @opt_NoMusic;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'use_mouse';
     location: @opt_UseMouse;
     defaultsvalue: '';
     defaultivalue: 2;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'use_joystick';
     location: @opt_UseJoystick;
     defaultsvalue: '';
     defaultivalue: 2;
     defaultbvalue: false;
     _type: tInteger)

  );

procedure M_LoadDefaults(const defaultfile: string);

procedure M_SaveDefaults(const defaultfile: string);

const
  LAUNCHERVERSION = 'DelphiDoom Launcher version 1.8';

implementation

uses
  SysUtils, Classes;

procedure MakeDefaultGameInfo;
var
  i: integer;
  idx: integer;
begin
  FillChar(gameinfo, SizeOf(gameinfo), Chr(0));

  gameinfo[Ord(gt_sharewaredoom)].description := 'Shareware Doom';
  gameinfo[Ord(gt_sharewaredoom)].mainwad := 'DOOM1.WAD';
  gameinfo[Ord(gt_sharewaredoom)].pwad := '';
  gameinfo[Ord(gt_sharewaredoom)].extracmdline := '';
  gameinfo[Ord(gt_sharewaredoom)].gameengine := ge_doom;

  gameinfo[Ord(gt_registereddoom)].description := 'Registered Doom';
  gameinfo[Ord(gt_registereddoom)].mainwad := 'DOOM.WAD';
  gameinfo[Ord(gt_registereddoom)].pwad := '';
  gameinfo[Ord(gt_registereddoom)].extracmdline := '';
  gameinfo[Ord(gt_registereddoom)].gameengine := ge_doom;

  gameinfo[Ord(gt_retaildoom)].description := 'Ultimate Doom';
  gameinfo[Ord(gt_retaildoom)].mainwad := 'DOOMU.WAD';
  gameinfo[Ord(gt_retaildoom)].pwad := '';
  gameinfo[Ord(gt_retaildoom)].extracmdline := '';
  gameinfo[Ord(gt_retaildoom)].gameengine := ge_doom;

  gameinfo[Ord(gt_doom2)].description := 'Doom2: Hell On Earth';
  gameinfo[Ord(gt_doom2)].mainwad := 'DOOM2.WAD';
  gameinfo[Ord(gt_doom2)].pwad := '';
  gameinfo[Ord(gt_doom2)].extracmdline := '';
  gameinfo[Ord(gt_doom2)].gameengine := ge_doom;

  gameinfo[Ord(gt_plutonia)].description := 'Final Doom: The Plutonia Experiment';
  gameinfo[Ord(gt_plutonia)].mainwad := 'PLUTONIA.WAD';
  gameinfo[Ord(gt_plutonia)].pwad := '';
  gameinfo[Ord(gt_plutonia)].extracmdline := '';
  gameinfo[Ord(gt_plutonia)].gameengine := ge_doom;

  gameinfo[Ord(gt_tnt)].description := 'Final Doom: TNT Evilution';
  gameinfo[Ord(gt_tnt)].mainwad := 'TNT.WAD';
  gameinfo[Ord(gt_tnt)].pwad := '';
  gameinfo[Ord(gt_tnt)].extracmdline := '';
  gameinfo[Ord(gt_tnt)].gameengine := ge_doom;

  gameinfo[Ord(gt_sharewareheretic)].description := 'Shareware Heretic';
  gameinfo[Ord(gt_sharewareheretic)].mainwad := 'HERETIC1.WAD';
  gameinfo[Ord(gt_sharewareheretic)].pwad := '';
  gameinfo[Ord(gt_sharewareheretic)].extracmdline := '';
  gameinfo[Ord(gt_sharewareheretic)].gameengine := ge_heretic;

  gameinfo[Ord(gt_registeredheretic)].description := 'Registered Heretic';
  gameinfo[Ord(gt_registeredheretic)].mainwad := 'HERETIC.WAD';
  gameinfo[Ord(gt_registeredheretic)].pwad := '';
  gameinfo[Ord(gt_registeredheretic)].extracmdline := '';
  gameinfo[Ord(gt_registeredheretic)].gameengine := ge_heretic;

  gameinfo[Ord(gt_extendedheretic)].description := 'Heretic Extended version';
  gameinfo[Ord(gt_extendedheretic)].mainwad := 'HERETIC.WAD';
  gameinfo[Ord(gt_extendedheretic)].pwad := '';
  gameinfo[Ord(gt_extendedheretic)].extracmdline := '';
  gameinfo[Ord(gt_extendedheretic)].gameengine := ge_heretic;

  gameinfo[Ord(gt_sharewarehexen)].description := 'Shareware Hexen';
  gameinfo[Ord(gt_sharewarehexen)].mainwad := 'HEXEN1.WAD';
  gameinfo[Ord(gt_sharewarehexen)].pwad := '';
  gameinfo[Ord(gt_sharewarehexen)].extracmdline := '';
  gameinfo[Ord(gt_sharewarehexen)].gameengine := ge_hexen;

  gameinfo[Ord(gt_registeredhexen)].description := 'Registered Hexen';
  gameinfo[Ord(gt_registeredhexen)].mainwad := 'HEXEN.WAD';
  gameinfo[Ord(gt_registeredhexen)].pwad := '';
  gameinfo[Ord(gt_registeredhexen)].extracmdline := '';
  gameinfo[Ord(gt_registeredhexen)].gameengine := ge_hexen;

  gameinfo[Ord(gt_expansionhexen)].description := 'Hexen: Death Kings of the Dark Citadel';
  gameinfo[Ord(gt_expansionhexen)].mainwad := 'HEXEN.WAD';
  gameinfo[Ord(gt_expansionhexen)].pwad := 'HEXDD.WAD';
  gameinfo[Ord(gt_expansionhexen)].extracmdline := '';
  gameinfo[Ord(gt_expansionhexen)].gameengine := ge_hexen;

  gameinfo[Ord(gt_strife)].description := 'Strife: Quest for the Sigil';
  gameinfo[Ord(gt_strife)].mainwad := 'STRIFE1.WAD';
  gameinfo[Ord(gt_strife)].pwad := '';
  gameinfo[Ord(gt_strife)].extracmdline := '';
  gameinfo[Ord(gt_strife)].gameengine := ge_strife;

  gameinfo[Ord(gt_strifeteaser)].description := 'Strife: Teaser Demo';
  gameinfo[Ord(gt_strifeteaser)].mainwad := 'STRIFE0.WAD';
  gameinfo[Ord(gt_strifeteaser)].pwad := '';
  gameinfo[Ord(gt_strifeteaser)].extracmdline := '';
  gameinfo[Ord(gt_strifeteaser)].gameengine := ge_strife;

  idx := 1;
  for i := Ord(gt_strifeteaser) + 1 to Ord(NUMGAMETYPES) -  1 do
  begin
    gameinfo[i].description := 'Custom Game ' + IntToStr(idx);
    gameinfo[i].mainwad := '';
    gameinfo[i].pwad := '';
    gameinfo[i].gameengine := ge_unknown;
    inc(idx);
  end;

end;

procedure M_LoadDefaults(const defaultfile: string);
var
  i: integer;
  j: integer;
  idx: integer;
  pd: Pdefault_t;
  s: TStringList;
  n: string;
  token: string;

  procedure ParseGameInfo(const idx: string);
  var
    gi: Pgameinfo_t;
  begin
    if StrToIntDef(idx, -1) > 0 then
    begin
      gi := @gameinfo[StrToIntDef(idx, -1) - 1];

      inc(i);
      token := s.Strings[i];
      if Pos(UpperCase('description='), UpperCase(token)) <> 1 then
        exit;
      gi.description := Copy(token, 13, length(token) - 12);

      inc(i);
      token := s.Strings[i];
      if Pos(UpperCase('mainwad='), UpperCase(token)) <> 1 then
        exit;
      gi.mainwad := Copy(token, 9, length(token) - 8);

      inc(i);
      token := s.Strings[i];
      if Pos(UpperCase('PWADs='), UpperCase(token)) <> 1 then
        exit;
      gi.pwad := Copy(token, 7, length(token) - 6);

      inc(i);
      token := s.Strings[i];
      if Pos(UpperCase('extra params='), UpperCase(token)) <> 1 then
        exit;
      gi.extracmdline := Copy(token, 14, length(token) - 13);

      inc(i);
      token := s.Strings[i];
      if Pos(UpperCase('gameengine='), UpperCase(token)) <> 1 then
        exit;
      token := Copy(token, 12, length(token) - 11);
      if token = '0' then
        gi.gameengine := gameengine_t(0)
      else if token = '1' then
        gi.gameengine := gameengine_t(1)
      else if token = '2' then
        gi.gameengine := gameengine_t(2)
      else if token = '3' then
        gi.gameengine := gameengine_t(3);
    end;
  end;

begin
  MakeDefaultGameInfo;

  // set everything to base values
  for i := 0 to NUMDEFAULTS - 1 do
    if defaults[i]._type = tInteger then
      PInteger(defaults[i].location)^ := defaults[i].defaultivalue
    else if defaults[i]._type = tBoolean then
      PBoolean(defaults[i].location)^ := defaults[i].defaultbvalue
    else if defaults[i]._type = tString then
      PString(defaults[i].location)^ := defaults[i].defaultsvalue;

  s := TStringList.Create;
  try
    // read the file in, overriding any set defaults
    if FileExists(defaultfile) then
      s.LoadFromFile(defaultfile);

    for i := s.Count - 1 downto 0 do
    begin
      s.Strings[i] := trim(s.Strings[i]);
      if s.Strings[i] = '' then
        s.Delete(i);
    end;

    if s.Count > 1 then
    begin

      for i := 0 to s.Count - 1 do
      begin
        idx := -1;
        n := LowerCase(s.Names[i]);
        for j := 0 to NUMDEFAULTS - 1 do
          if defaults[j].name = n then
          begin
            idx := j;
            break;
          end;

        if idx > -1 then
        begin
          pd := @defaults[idx];
          if pd._type = tInteger then
            PInteger(pd.location)^ := StrToInt(s.Values[n])
          else if pd._type = tBoolean then
            PBoolean(pd.location)^ := StrToInt(s.Values[n]) <> 0
          else if pd._type = tString then
            PString(pd.location)^ := s.Values[n];
        end;
      end;

      i := 0;
      while i < s.Count do
      begin
        token := s[i];
        if Pos(UpperCase('[Gameinfo #'), UpperCase(token)) = 1 then
        begin
          token := Copy(token, 12, Length(token) - 11);
          if length(token) > 0 then
          begin
            if token[length(token)] = ']' then
              SetLength(token, length(token) - 1);
            ParseGameInfo(token);
          end;
        end;
        inc(i);
      end;
    end;

  finally
    s.Free;
  end;
end;

procedure M_SaveDefaults(const defaultfile: string);
var
  i: integer;
  pd: Pdefault_t;
  s: TStringList;
  verstr: string;
begin
  s := TStringList.Create;
  try
    verstr := '[' + LAUNCHERVERSION + ']';
    s.Add(verstr);

    pd := @defaults[0];
    for i := 0 to NUMDEFAULTS - 1 do
    begin
      if pd._type = tInteger then
        s.Add(pd.name + '=' + IntToStr(PInteger(pd.location)^))
      else if pd._type = tString then
        s.Add(pd.name + '=' + PString(pd.location)^)
      else if pd._type = tBoolean then
        s.Add(pd.name + '=' + IntToStr(integer(PBoolean(pd.location)^)))
      else if pd._type = tGroup then
      begin
        s.Add('');
        s.Add('[' + pd.name + ']');
      end;
      inc(pd);
    end;

    for i := 0 to Ord(NUMGAMETYPES) - 1 do
    begin
      s.Add('');
      s.Add('[Gameinfo #' + IntToStr(i + 1) + ']');
      s.Add('description=' + gameinfo[i].description);
      s.Add('mainwad=' + gameinfo[i].mainwad);
      s.Add('PWADs=' + gameinfo[i].pwad);
      s.Add('extra params=' + gameinfo[i].extracmdline);
      s.Add('gameengine=' + IntToStr(Ord(gameinfo[i].gameengine)));
    end;

    s.SaveToFile(defaultfile);

  finally
    s.Free;
  end;
end;

end.
