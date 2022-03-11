//------------------------------------------------------------------------------
//
//  DelphiStrife is a source port of the game Strife.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2005 Simon Howard
//  Copyright (C) 2010 James Haley, Samuel Villarreal
//  Copyright (C) 2004-2022 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//    Door animation code (opening/closing)
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_doors;

interface

uses
  doomdef,
  p_mobj_h,
  p_spec,
  r_defs,
  s_sound,
  sounddata;

//==============================================================================
//
// T_VerticalDoor
//
//==============================================================================
procedure T_VerticalDoor(door: Pvldoor_t);

//==============================================================================
//
// EV_DoLockedDoor
//
//==============================================================================
function EV_DoLockedDoor(line: Pline_t; _type: vldoor_e; thing: Pmobj_t): integer;

//==============================================================================
//
// EV_DoDoor
//
//==============================================================================
function EV_DoDoor(line: Pline_t; _type: vldoor_e): integer;

//==============================================================================
//
// EV_VerticalDoor
//
//==============================================================================
procedure EV_VerticalDoor(line: Pline_t; thing: Pmobj_t);

//==============================================================================
//
// EV_SlidingDoor
//
//==============================================================================
procedure EV_SlidingDoor(line: Pline_t; thing: Pmobj_t);

//==============================================================================
//
// EV_ClearForceFields
//
//==============================================================================
function EV_ClearForceFields(line: Pline_t): integer;

//==============================================================================
//
// EV_RemoteSlidingDoor
//
//==============================================================================
function EV_RemoteSlidingDoor(line: Pline_t; thing: Pmobj_t): integer;

//==============================================================================
//
// P_SpawnDoorCloseIn30
//
//==============================================================================
procedure P_SpawnDoorCloseIn30(sec: Psector_t);

//==============================================================================
//
// P_SpawnDoorRaiseIn5Mins
//
//==============================================================================
procedure P_SpawnDoorRaiseIn5Mins(sec: Psector_t; secnum: integer);

//==============================================================================
//
// P_InitSlidingDoorFrames
//
//==============================================================================
procedure P_InitSlidingDoorFrames;

//==============================================================================
//
// T_SlidingDoor
//
//==============================================================================
procedure T_SlidingDoor(door: Pslidedoor_t);

implementation

uses
  d_delphi,
  deh_main,
  i_system,
  doomdata,
  d_player,
  g_game,
  m_fixed,
  info_h,
  r_data,
  p_dialog,
  p_genlin,
  p_lights,
  p_tick,
  p_setup,
  p_floor,
  w_wad,
  z_zone;

//==============================================================================
//
// VERTICAL DOORS
//
// T_VerticalDoor
//
//==============================================================================
procedure T_VerticalDoor(door: Pvldoor_t);
var
  res1, res2: result_e;
begin
  case door.direction of
    0:
      begin
  // WAITING
        dec(door.topcountdown);
        if door.topcountdown = 0 then
        begin
          case door._type of
            vld_blazeRaise,
            vld_genBlazeRaise:
              begin
                door.direction := -1; // time to go back down
                S_StartSound(@door.sector.soundorg, Ord(sfx_bdcls));
              end;
            vld_normal,
            vld_genRaise:
              begin
                door.direction := -1; // time to go back down
                // villsa [STRIFE] closesound added
                S_StartSound(@door.sector.soundorg, door.closesound);
              end;

            // villsa [STRIFE]
            vld_shopClose:
              begin
                door.direction := 1;
                door.speed := 2 * FRACUNIT;
                S_StartSound(@door.sector.soundorg, door.opensound);
              end;

            vld_close30ThenOpen,
            vld_genCdO:
              begin
                door.direction := 1;
                // villsa [STRIFE] opensound added
                S_StartSound(@door.sector.soundorg, door.opensound);
              end;
            vld_genBlazeCdO:
              begin
                door.direction := 1;
                S_StartSound(@door.sector.soundorg, Ord(sfx_bdopn));
              end;
          end;
        end;
      end;
    2:
      begin
  //  INITIAL WAIT
        dec(door.topcountdown);
        if door.topcountdown = 0 then
        begin
          case door._type of
            vld_raiseIn5Mins:
              begin
                door.direction := 1;
                door._type := vld_normal;
                S_StartSound(@door.sector.soundorg, door.opensound);
              end;
          end;
        end;
      end;

        // villsa [STRIFE]
   -2:
      begin
        // SPLIT
        res1 := T_MovePlane(door.sector, door.speed, door.topheight, false, 1, 1);
        res2 := T_MovePlane(door.sector, door.speed, door.topwait, false, 0, -1);

        if (res1 = pastdest) and (res2 = pastdest) then
        begin
            door.sector.ceilingdata := nil;
            P_RemoveThinker(@door.thinker);  // unlink and free
        end;
      end;

   -1:
      begin
  // DOWN
        res1 := T_MovePlane(door.sector, door.speed, door.sector.floorheight,
                  false, 1, door.direction);
        if res1 = pastdest then
        begin
          case door._type of
            vld_blazeRaise,
            vld_blazeClose,
            vld_genBlazeRaise,
            vld_genBlazeClose,
            vld_normal,
            vld_close,
            vld_genRaise,
            vld_genClose:
              begin
                door.sector.ceilingdata := nil;
                P_RemoveThinker(@door.thinker); // unlink and free
              end;

            vld_close30ThenOpen:
              begin
                door.direction := 0;
                door.topcountdown := TICRATE * 30;
              end;

            vld_genCdO,
            vld_genBlazeCdO:
              begin
                door.direction := 0;
                door.topcountdown := door.topwait; // jff 5/8/98 insert delay
              end;

                // villsa [STRIFE]
            vld_shopClose:
              begin
                door.direction := 0;
                door.topcountdown := TICRATE * 120;
              end;

          end;

          if not compatibilitymode then
            if door.line <> nil then
              if door.line.tag <> 0 then
              begin
                if (door.line.special > CGENLOCKEDBASE) and
                   (door.line.special and 6 = 6) then //jff 3/9/98 all manual doors
                  EV_TurnTagLightsOff(door.line)
                else
                  case door.line.special of
                    1,
                    26,
                    27,
                    28,
                    31,
                    32,
                    33,
                    34,
                    117,
                    118:
                      EV_TurnTagLightsOff(door.line);
                  end;
              end;
        end
        else if res1 = crushed then
        begin
          case door._type of
            vld_blazeClose,
            vld_close,      // DO NOT GO BACK UP!
            vld_shopClose,  // villsa [STRIFE]
            vld_genBlazeClose,
            vld_genClose:
              begin
              end;
          else
            begin
              door.direction := 1;
              // villsa [STRIFE] opensound added
              S_StartSound(@door.sector.soundorg, door.opensound);
            end;
          end;
        end;
      end;
    1:
      begin
  // UP
        res1 := T_MovePlane(door.sector, door.speed, door.topheight,
                  false, 1, door.direction);
        if res1 = pastdest then
        begin
          case door._type of
            vld_blazeRaise,
            vld_normal,
            vld_genRaise,
            vld_genBlazeRaise:
              begin
                door.direction := 0; // wait at top
                door.topcountdown := door.topwait;
              end;
            vld_close30ThenOpen,
            vld_blazeOpen,
            vld_open,
            vld_shopClose,  // villsa [STRIFE]
            vld_genBlazeOpen,
            vld_genOpen,
            vld_genCdO,
            vld_genBlazeCdO:
              begin
                door.sector.ceilingdata := nil;
                P_RemoveThinker(@door.thinker); // unlink and free
              end;
          end;

          if not compatibilitymode then
            if door.line <> nil then
              if door.line.tag <> 0 then
              begin
                if (door.line.special > CGENLOCKEDBASE) and
                   (door.line.special and 6 = 6) then //jff 3/9/98 all manual doors
                  EV_LightTurnOn(door.line, 0)
                else
                  case door.line.special of
                    1,
                    26,
                    27,
                    28,
                    31,
                    32,
                    33,
                    34,
                    117,
                    118:
                      EV_LightTurnOn(door.line, 0);
                  end;
              end;

        end;
      end;
  end;
end;

//==============================================================================
//
// EV_DoLockedDoor
// Move a locked door up/down
//
// [STRIFE] This game has a crap load of keys. And this function doesn't even
// deal with all of them...
//
//==============================================================================
function EV_DoLockedDoor(line: Pline_t; _type: vldoor_e; thing: Pmobj_t): integer;
var
  p: Pplayer_t;
begin
  p := thing.player;

  // only players can open locked doors
  if p = nil then
  begin
    result := 0;
    exit;
  end;

  case line.special of
    99,
   133:
      begin
        if not p.cards[Ord(key_IDCard)] then
        begin
          p._message := DEH_GetString('You need an id card');
          S_StartSound(nil, Ord(sfx_oof));
          result := 0;
          exit;
        end;
      end;

   134,
   135:
      begin
        if not p.cards[Ord(key_IDBadge)] then
        begin
          p._message := DEH_GetString('You need an id badge');
          S_StartSound(nil, Ord(sfx_oof));
          result := 0;
          exit;
        end;
      end;

   136,
   137:
      begin
        if not p.cards[Ord(key_Passcard)] then
        begin
          p._message := DEH_GetString('You need a pass card');
          S_StartSound(nil, Ord(sfx_oof));
          result := 0;
          exit;
        end;
      end;

   151,
   164:
      begin
        if not p.cards[Ord(key_GoldKey)] then
        begin
          p._message := DEH_GetString('You need a gold key');
          S_StartSound(nil, Ord(sfx_oof));
          result := 0;
          exit;
        end;
      end;

   153,
   163:
      begin
        if not p.cards[Ord(key_SilverKey)] then
        begin
          p._message := DEH_GetString('You need a silver key');
          S_StartSound(nil, Ord(sfx_oof));
          result := 0;
          exit;
        end;
      end;

   152,
   162:
      begin
        if not p.cards[Ord(key_BrassKey)] then
        begin
          p._message := DEH_GetString('You need a brass key');
          S_StartSound(nil, Ord(sfx_oof));
          result := 0;
          exit;
        end;
      end;

   167,
   168:
      begin
        if not p.cards[Ord(key_SeveredHand)] then
        begin
          p._message := DEH_GetString('Hand print not on file');
          S_StartSound(nil, Ord(sfx_oof));
          result := 0;
          exit;
        end;
      end;

   171:
      begin
        if not p.cards[Ord(key_PrisonKey)] then
        begin
          p._message := DEH_GetString('You don''t have the key to the prison');
          S_StartSound(nil, Ord(sfx_oof));
          result := 0;
          exit;
        end;
      end;

   172:
      begin
        if not p.cards[Ord(key_Power1Key)] then
        begin
          p._message := DEH_GetString('You don''t have the key');
          S_StartSound(nil, Ord(sfx_oof));
          result := 0;
          exit;
        end;
      end;

   173:
      begin
        if not p.cards[Ord(key_Power2Key)] then
        begin
          p._message := DEH_GetString('You don''t have the key');
          S_StartSound(nil, Ord(sfx_oof));
          result := 0;
          exit;
        end;
      end;

   176:
      begin
        if not p.cards[Ord(key_Power3Key)] then
        begin
          p._message := DEH_GetString('You don''t have the key');
          S_StartSound(nil, Ord(sfx_oof));
          result := 0;
          exit;
        end;
      end;

   189:
      begin
        if not p.cards[Ord(key_OracleKey)] then
        begin
          p._message := DEH_GetString('You don''t have the key');
          S_StartSound(nil, Ord(sfx_oof));
          result := 0;
          exit;
        end;
      end;

   191:
      begin
        if not p.cards[Ord(key_MilitaryID)] then
        begin
          p._message := DEH_GetString('You don''t have the key');
          S_StartSound(nil, Ord(sfx_oof));
          result := 0;
          exit;
        end;
      end;

   192:
      begin
        if not p.cards[Ord(key_WarehouseKey)] then
        begin
          p._message := DEH_GetString('You don''t have the key');
          S_StartSound(nil, Ord(sfx_oof));
          result := 0;
          exit;
        end;
      end;

   223:
      begin
        if not p.cards[Ord(key_MineKey)] then
        begin
          p._message := DEH_GetString('You don''t have the key');
          S_StartSound(nil, Ord(sfx_oof));
          result := 0;
          exit;
        end;
      end;

  end;

  result := EV_DoDoor(line, _type);
end;

//==============================================================================
//
// R_SoundNumForDoor
//
// villsa [STRIFE] - new function
// Set sounds associated with door though why
// on earth is this function placed here?
//
// JVAL: changed to P_SoundNumForDoor
//
//==============================================================================
procedure P_SoundNumForDoor(door: Pvldoor_t);
var
  i: integer;
  sector: Psector_t;
  line: Pline_t;
  texture: Ptexture_t;
  name: string;
  c1, c2: char;
begin
  // set default sounds
  door.opensound := Ord(sfx_drsmto);
  door.closesound := Ord(sfx_drsmtc);

  sector := door.sector;
  for i := 0 to sector.linecount - 1 do
  begin
    line := sector.lines[i];

    if line.flags and ML_TWOSIDED = 0 then
      continue;

    texture := textures[sides[line.sidenum[0]].toptexture];
    name := char8tostring(texture.name);

    if Pos('DOR', name) <> 1 then
      continue;

    while length(name) < 8 do
      name := name + ' ';

    c1 := name[4];
    c2 := name[5];

    // S type
    if c1 = 'S' then
    begin
      door.opensound := Ord(sfx_drston);
      door.closesound := Ord(sfx_drston);
      exit;
    end;

    // M type
    if c1 = 'M' then
    begin
      // L subtype
      if c2 = 'L' then
      begin
        door.opensound := Ord(sfx_drlmto);
        door.closesound := Ord(sfx_drlmtc);
      end
      // S subtype
      else if c2 = 'S' then
      begin
        door.opensound := Ord(sfx_drsmto);
        door.closesound := Ord(sfx_drsmtc);
      end;
      exit;
    end
    // W type
    else if c1 = 'W' then
    begin
      // L subtype
      if c2 = 'L' then
      begin
        door.opensound := Ord(sfx_drlwud);
        door.closesound := Ord(sfx_drlwud);
      end
      // S subtype
      else if c2 = 'S' then
      begin
        door.opensound := Ord(sfx_drswud);
        door.closesound := Ord(sfx_drswud);
      end;
      exit;
    end;
  end;
end;

//==============================================================================
//
// EV_DoDoor
//
//==============================================================================
function EV_DoDoor(line: Pline_t; _type: vldoor_e): integer;
var
  initial: boolean;
  secnum: integer;
  sec: Psector_t;
  door: Pvldoor_t;
begin
  secnum := -1;
  result := 0;

  initial := true;
  while (secnum >= 0) or initial do
  begin
    initial := false;
    secnum := P_FindSectorFromLineTag(line, secnum);
    if secnum < 0 then
      break;

    sec := @sectors[secnum];
    if P_SectorActive(ceiling_special, sec) then
      continue;

    // new door thinker
    result := 1;
    door := Z_Malloc(SizeOf(vldoor_t), PU_LEVSPEC, nil);
    P_AddThinker(@door.thinker);
    sec.ceilingdata := door;

    door.thinker._function.acp1 := @T_VerticalDoor;
    door.sector := sec;
    door._type := _type;
    door.topwait := VDOORWAIT;
    door.speed := VDOORSPEED;
    door.line := nil;
    P_SoundNumForDoor(door);    // villsa [STRIFE] set door sounds

    case _type of
      // villsa [STRIFE] new door type
      vld_splitOpen:
        begin
          door.direction := -2;
          door.topheight := P_FindLowestCeilingSurrounding(sec);
          door.topheight := door.topheight - 4 * FRACUNIT;
          door.speed := FRACUNIT;
          // yes, it using topwait to get the floor height
          door.topwait := P_FindLowestFloorSurrounding(sec);
          if door.topheight = sec.ceilingheight then
            continue;

          S_StartSound(@door.sector.soundorg, door.opensound);
        end;

      // villsa [STRIFE] new door type
      vld_splitRaiseNearest:
        begin
          door.direction := -2;
          door.topheight := P_FindLowestCeilingSurrounding(sec);
          door.topheight := door.topheight - 4 * FRACUNIT;
          door.speed := FRACUNIT;
          // yes, it using topwait to get the floor height
          door.topwait := P_FindHighestFloorSurrounding(sec);
          if door.topheight = sec.ceilingheight then
            continue;

          S_StartSound(@door.sector.soundorg, door.opensound);
        end;

      vld_blazeClose,
      vld_shopClose:     // villsa [STRIFE]
        begin
          door.topheight := P_FindLowestCeilingSurrounding(sec);
          door.topheight := door.topheight - 4 * FRACUNIT;
          door.direction := -1;
          door.speed := VDOORSPEED * 4;
          S_StartSound(@door.sector.soundorg, Ord(sfx_bdcls));
        end;

      vld_close:
        begin
          door.topheight := P_FindLowestCeilingSurrounding(sec);
          door.topheight := door.topheight - 4 * FRACUNIT;
          door.direction := -1;
          // villsa [STRIFE] set door sounds
          S_StartSound(@door.sector.soundorg, door.opensound);
        end;

      vld_close30ThenOpen:
        begin
          door.topheight := sec.ceilingheight;
          door.direction := -1;
          // villsa [STRIFE] set door sounds
          S_StartSound(@door.sector.soundorg, door.closesound);
        end;

      vld_blazeRaise,
      vld_blazeOpen:
        begin
          door.direction := 1;
          door.topheight := P_FindLowestCeilingSurrounding(sec);
          door.topheight := door.topheight - 4 * FRACUNIT;
          door.speed := VDOORSPEED * 4;
          if door.topheight <> sec.ceilingheight then
            S_StartSound(@door.sector.soundorg, Ord(sfx_bdopn));
        end;

      vld_normal,
      vld_open:
        begin
          door.direction := 1;
          door.topheight := P_FindLowestCeilingSurrounding(sec);
          door.topheight := door.topheight - 4 * FRACUNIT;
          if door.topheight <> sec.ceilingheight then
            S_StartSound(@door.sector.soundorg, door.opensound);
        end;
    end;
  end;
end;

//==============================================================================
//
// EV_ClearForceFields
//
// villsa [STRIFE] new function
//
//==============================================================================
function EV_ClearForceFields(line: Pline_t): integer;
var
  secnum: integer;
  sec: Psector_t;
  i: integer;
  secline: Pline_t;
  initial: boolean;
  sn: integer;
begin
  secnum := -1;
  result := 0;

  initial := true;
  while (secnum >= 0) or initial do
  begin
    initial := false;
    secnum := P_FindSectorFromLineTag(line, secnum);
    if secnum < 0 then
      break;

    sec := @sectors[secnum];

    line.special := 0;
    result := 1;

    // haleyjd 09/18/10: fixed to continue w/linecount == 0, not return
    for i := 0 to sec.linecount - 1 do
    begin
      secline := sec.lines[i];
      if secline.flags and ML_TWOSIDED = 0 then
        continue;
      if secline.special <> 148 then
        continue;

      secline.flags := secline.flags and not ML_BLOCKING;
      secline.special := 0;

      sn := secline.sidenum[0];
      if sn >= 0 then
        sides[sn].midtexture := 0;
      sn := secline.sidenum[1];
      if sn >= 0 then
        sides[sn].midtexture := 0;
    end;
  end;
end;

//==============================================================================
//
// EV_VerticalDoor : open a door manually, no tag value
//
// [STRIFE] Tons of new door types were added.
//
//==============================================================================
procedure EV_VerticalDoor(line: Pline_t; thing: Pmobj_t);
var
  player: Pplayer_t;
  sec: Psector_t;
  door: Pvldoor_t;
begin
  // Check for locks
  player := thing.player;

  case line.special of
    26, // DR ID Card door
    32: // D1 ID Card door
      begin
        if player = nil then
          exit;

        if not player.cards[Ord(key_IDCard)] then
        begin
          player._message := DEH_GetString('You need an id card to open this door');
          S_StartSound(nil, Ord(sfx_oof));
          exit;
        end;
      end;

    27, // DR Pass Card door
    34: // D1 Pass Card door
      begin
        if player = nil then
          exit;

        if not player.cards[Ord(key_Passcard)] then
        begin
          player._message := DEH_GetString('You need a pass card key to open this door');
          S_StartSound(nil, Ord(sfx_oof));
          exit;
        end;
      end;

    28, // DR ID Badge door
    33: // D1 ID Badge door
      begin
        if player = nil then
          exit;

        if not player.cards[Ord(key_IDBadge)] then
        begin
          player._message := DEH_GetString('You need an id badge to open this door');
          S_StartSound(nil, Ord(sfx_oof));
          exit;
        end;
      end;

   156, // D1 brass key door
   161: // DR brass key door
      begin
        if not player.cards[Ord(key_BrassKey)] then
        begin
          player._message := DEH_GetString('You need a brass key');
          S_StartSound(nil, Ord(sfx_oof));
          exit;
        end;
      end;

   157, // D1 silver key door
   160: // DR silver key door
      begin
        if not player.cards[Ord(key_SilverKey)] then
        begin
          player._message := DEH_GetString('You need a silver key');
          S_StartSound(nil, Ord(sfx_oof));
          exit;
        end;
      end;

   158, // D1 gold key door
   159: // DR gold key door
      begin
        if not player.cards[Ord(key_GoldKey)] then
        begin
          player._message := DEH_GetString('You need a gold key');
          S_StartSound(nil, Ord(sfx_oof));
          exit;
        end;
      end;

   // villsa [STRIFE] added 09/15/10
   165:
      begin
        player._message := DEH_GetString('That doesn''t seem to work');
        S_StartSound(nil, Ord(sfx_oof));
        exit;
      end;

   166: // DR Hand Print door
      begin
        if not player.cards[Ord(key_SeveredHand)] then
        begin
          player._message := DEH_GetString('Hand print not on file');
          S_StartSound(nil, Ord(sfx_oof));
          exit;
        end;
      end;

   169: // DR Base key door
      begin
        if not player.cards[Ord(key_BaseKey)] then
        begin
          player._message := DEH_GetString('You don''t have the key');
          S_StartSound(nil, Ord(sfx_oof));
          exit;
        end;
      end;

   170: // DR Gov's Key door
      begin
        if not player.cards[Ord(key_GovsKey)] then
        begin
          player._message := DEH_GetString('You don''t have the key');
          S_StartSound(nil, Ord(sfx_oof));
          exit;
        end;
      end;

   190: // DR Order Key door
      begin
        if not player.cards[Ord(key_OrderKey)] then
        begin
          player._message := DEH_GetString('You don''t have the key');
          S_StartSound(nil, Ord(sfx_oof));
          exit;
        end;
      end;

   205: // DR "Only in retail"
      begin
        player._message := DEH_GetString('THIS AREA IS ONLY AVAILABLE IN THE RETAIL VERSION OF STRIFE');
        S_StartSound(nil, Ord(sfx_oof));
        exit;
      end;

   213: // DR Chalice door
      begin
        if P_PlayerHasItem(player, MT_INV_CHALICE) = 0 then
        begin
          player._message := DEH_GetString('You need the chalice!');
          S_StartSound(nil, Ord(sfx_oof));
          exit;
        end;
      end;

   217: // DR Core Key door
      begin
        if not player.cards[Ord(key_CoreKey)] then
        begin
          player._message := DEH_GetString('You don''t have the key');
          S_StartSound(nil, Ord(sfx_oof));
          exit;
        end;
      end;

   221: // DR Mauler Key door
      begin
        if not player.cards[Ord(key_MaulerKey)] then
        begin
          player._message := DEH_GetString('You don''t have the key');
          S_StartSound(nil, Ord(sfx_oof));
          exit;
        end;
      end;

   224: // DR Chapel Key door
      begin
        if not player.cards[Ord(key_ChapelKey)] then
        begin
          player._message := DEH_GetString('You don''t have the key');
          S_StartSound(nil, Ord(sfx_oof));
          exit;
        end;
      end;

   225: // DR Catacomb Key door
      begin
        if not player.cards[Ord(key_CatacombKey)] then
        begin
          player._message := DEH_GetString('You don''t have the key');
          S_StartSound(nil, Ord(sfx_oof));
          exit;
        end;
      end;

   232: // DR Oracle Pass door
      begin
        if player.questflags and QF_QUEST18 = 0 then
        begin
          player._message := DEH_GetString('You need the Oracle Pass!');
          S_StartSound(nil, Ord(sfx_oof));
          exit;
        end;
      end;

  end;

  // if the wrong side of door is pushed, give oof sound
  if line.sidenum[1] = -1 then  // killough
  begin
    S_StartSound(player.mo, Ord(sfx_oof));  // killough 3/20/98
    exit;
  end;

  // if the sector has an active thinker, use it
  // back sides
  sec := sides[line.sidenum[1]].sector;

  if P_SectorActive(ceiling_special, sec) then
  begin
    door := sec.ceilingdata;
    // [STRIFE] Adjusted to handle linetypes handled here by Strife.
    // BUG: Not all door types are checked here. This means that certain
    // door lines are allowed to fall through and start a new thinker on the
    // sector! This is why some doors can become jammed in Strife - stuck in
    // midair, or unable to be opened at all. Multiple thinkers will fight
    // over how to move the door. They should have added a default return if
    // they weren't going to handle this unconditionally...
    // JVAL: Added special 221 from svstrife
    case line.special of
       1, // ONLY FOR "RAISE" DOORS, NOT "OPEN"s
      26,
      27,
      28,
     117,
     159,       // villsa
     160,       // haleyjd
     161,       // villsa
     166,       // villsa
     169,       // villsa
     170,       // villsa
     190,       // villsa
     213,       // villsa
     221,       // kaiser // JVAL: added from svstrife 1.4
     232:       // villsa

        begin
          if door.direction = -1 then
            door.direction := 1 // go back up
          else
          begin
            if thing.player = nil then
              exit; // JDC: bad guys never close doors

            door.direction := -1; // start going down immediately
          end;
          exit;
        end;
    end;
  end;

  // new door thinker
  door := Z_Malloc(SizeOf(vldoor_t), PU_LEVSPEC, nil);
  P_AddThinker(@door.thinker);
  sec.ceilingdata := door;
  door.thinker._function.acp1 := @T_VerticalDoor;
  door.sector := sec;
  door.direction := 1;
  door.speed := VDOORSPEED;
  door.topwait := VDOORWAIT;
  door.line := nil;
  P_SoundNumForDoor(door);   // haleyjd 09/15/10: [STRIFE] Get door sounds

  // for proper sound - [STRIFE] - verified complete
  case line.special of
    117,  // BLAZING DOOR RAISE
    118:  // BLAZING DOOR OPEN
        S_StartSound(@sec.soundorg, Ord(sfx_bdopn));
  else    // NORMAL DOOR SOUND
    S_StartSound(@sec.soundorg, door.opensound);
  end;

    // haleyjd: [STRIFE] - verified all.
  case line.special of
     1,
    26,
    27,
    28:
      door._type := vld_normal;

    31,
    32,
    33,
    34,
   156,   // villsa [STRIFE]
   157,   // villsa [STRIFE]
   158:   // villsa [STRIFE]
      begin
        door._type := vld_open;
        line.special := 0;
      end;

   117: // blazing door raise
      begin
        door._type := vld_blazeRaise;
        door.speed := VDOORSPEED * 4;
      end;

   118: // blazing door open
      begin
        door._type := vld_blazeOpen;
        line.special := 0;
        door.speed := VDOORSPEED * 4;
      end;
  else
    // haleyjd: [STRIFE] pretty important to have this here!
    door._type := vld_normal;

  end;

  // find the top and bottom of the movement range
  door.topheight := P_FindLowestCeilingSurrounding(sec) - 4 * FRACUNIT;
end;

//==============================================================================
// P_SpawnDoorCloseIn30
//
// Spawn a door that closes after 30 seconds
//
//==============================================================================
procedure P_SpawnDoorCloseIn30(sec: Psector_t);
var
  door: Pvldoor_t;
begin
  door := Z_Malloc(SizeOf(vldoor_t), PU_LEVSPEC, nil);

  P_AddThinker(@door.thinker);

  sec.ceilingdata := door;
  sec.special := 0;

  door.thinker._function.acp1 := @T_VerticalDoor;
  door.sector := sec;
  door.direction := 0;
  door._type := vld_normal;
  door.speed := VDOORSPEED;
  door.topcountdown := 30 * TICRATE;
  door.line := nil; // remember line that triggered us
end;

//==============================================================================
// P_SpawnDoorRaiseIn5Mins
//
// Spawn a door that opens after 5 minutes
//
//==============================================================================
procedure P_SpawnDoorRaiseIn5Mins(sec: Psector_t; secnum: integer);
var
  door: Pvldoor_t;
begin
  door := Z_Malloc(SizeOf(vldoor_t), PU_LEVSPEC, nil);

  P_AddThinker(@door.thinker);

  sec.ceilingdata := door;
  sec.special := 0;

  door.thinker._function.acp1 := @T_VerticalDoor;
  door.sector := sec;
  door.direction := 2;
  door._type := vld_raiseIn5Mins;
  door.speed := VDOORSPEED;
  door.topheight := P_FindLowestCeilingSurrounding(sec) - 4 * FRACUNIT;
  door.topwait := VDOORWAIT;
  door.topcountdown := 5 * 60 * TICRATE;
  door.line := nil; // remember line that triggered us
end;

// villsa [STRIFE] resurrected sliding doors
//

//
// villsa [STRIFE]
//
// Sliding door name information
//
var
  slideFrameNames: array[0..MAXSLIDEDOORS - 1] of slidename_t = (
    // SIGLDR
   (
    frame1: 'SIGLDR01';  // frame1
    frame2: 'SIGLDR02';  // frame2
    frame3: 'SIGLDR03';  // frame3
    frame4: 'SIGLDR04';  // frame4
    frame5: 'SIGLDR05';  // frame5
    frame6: 'SIGLDR06';  // frame6
    frame7: 'SIGLDR07';  // frame7
    frame8: 'SIGLDR08';  // frame8
   ),

    // DORSTN
   (
    frame1: 'DORSTN01';  // frame1
    frame2: 'DORSTN02';  // frame2
    frame3: 'DORSTN03';  // frame3
    frame4: 'DORSTN04';  // frame4
    frame5: 'DORSTN05';  // frame5
    frame6: 'DORSTN06';  // frame6
    frame7: 'DORSTN07';  // frame7
    frame8: 'DORSTN08';  // frame8
   ),

    // DORQTR
   (
    frame1: 'DORQTR01';  // frame1
    frame2: 'DORQTR02';  // frame2
    frame3: 'DORQTR03';  // frame3
    frame4: 'DORQTR04';  // frame4
    frame5: 'DORQTR05';  // frame5
    frame6: 'DORQTR06';  // frame6
    frame7: 'DORQTR07';  // frame7
    frame8: 'DORQTR08';  // frame8
   ),

    // DORCRG
   (
    frame1: 'DORCRG01';  // frame1
    frame2: 'DORCRG02';  // frame2
    frame3: 'DORCRG03';  // frame3
    frame4: 'DORCRG04';  // frame4
    frame5: 'DORCRG05';  // frame5
    frame6: 'DORCRG06';  // frame6
    frame7: 'DORCRG07';  // frame7
    frame8: 'DORCRG08';  // frame8
   ),

    // DORCHN
   (
    frame1: 'DORCHN01';  // frame1
    frame2: 'DORCHN02';  // frame2
    frame3: 'DORCHN03';  // frame3
    frame4: 'DORCHN04';  // frame4
    frame5: 'DORCHN05';  // frame5
    frame6: 'DORCHN06';  // frame6
    frame7: 'DORCHN07';  // frame7
    frame8: 'DORCHN08';  // frame8
   ),

    // DORIRS
   (
    frame1: 'DORIRS01';  // frame1
    frame2: 'DORIRS02';  // frame2
    frame3: 'DORIRS03';  // frame3
    frame4: 'DORIRS04';  // frame4
    frame5: 'DORIRS05';  // frame5
    frame6: 'DORIRS06';  // frame6
    frame7: 'DORIRS07';  // frame7
    frame8: 'DORIRS08';  // frame8
   ),

    // DORALN
   (
    frame1: 'DORALN01';  // frame1
    frame2: 'DORALN02';  // frame2
    frame3: 'DORALN03';  // frame3
    frame4: 'DORALN04';  // frame4
    frame5: 'DORALN05';  // frame5
    frame6: 'DORALN06';  // frame6
    frame7: 'DORALN07';  // frame7
    frame8: 'DORALN08';  // frame8
   ),

   (
    frame1: '';  // frame1
    frame2: '';  // frame2
    frame3: '';  // frame3
    frame4: '';  // frame4
    frame5: '';  // frame5
    frame6: '';  // frame6
    frame7: '';  // frame7
    frame8: '';  // frame8
   )

  );

//
// villsa [STRIFE]
//
// Sliding door open sounds
//
var
  slideOpenSounds: array[0..MAXSLIDEDOORS - 1] of sfxenum_t = (
    sfx_drlmto, sfx_drston, sfx_airlck, sfx_drsmto,
    sfx_drchno, sfx_airlck, sfx_airlck, sfx_None
  );

//
// villsa [STRIFE]
//
// Sliding door close sounds
//
var
  slideCloseSounds: array[0..MAXSLIDEDOORS - 1] of sfxenum_t = (
    sfx_drlmtc, sfx_drston, sfx_airlck, sfx_drsmtc,
    sfx_drchnc, sfx_airlck, sfx_airlck, sfx_None
  );

var
  slideFrames: array[0..MAXSLIDEDOORS - 1] of slideframe_t;

//==============================================================================
//
// P_InitSlidingDoorFrames
//
// villsa [STRIFE] resurrected
//
// JVAL: Allow missing from IWAD, calls R_CheckTextureNumForName instead of R_TextureNumForName
//
//==============================================================================
procedure P_InitSlidingDoorFrames;
var
  i: integer;
begin
  ZeroMemory(@slideFrames, SizeOf(slideframe_t) * MAXSLIDEDOORS);

  for i := 0 to MAXSLIDEDOORS - 1 do
  begin
    slideFrames[i].frames[0] := R_CheckTextureNumForName(DEH_GetString(slideFrameNames[i].frame1));
    if slideFrames[i].frames[0] = -1 then
    begin
      slideFrames[i].frames[1] := -1;
      slideFrames[i].frames[2] := -1;
      slideFrames[i].frames[3] := -1;
      slideFrames[i].frames[4] := -1;
      slideFrames[i].frames[5] := -1;
      slideFrames[i].frames[6] := -1;
      slideFrames[i].frames[7] := -1;
    end
    else
    begin
      slideFrames[i].frames[1] := R_TextureNumForName(DEH_GetString(slideFrameNames[i].frame2));
      slideFrames[i].frames[2] := R_TextureNumForName(DEH_GetString(slideFrameNames[i].frame3));
      slideFrames[i].frames[3] := R_TextureNumForName(DEH_GetString(slideFrameNames[i].frame4));
      slideFrames[i].frames[4] := R_TextureNumForName(DEH_GetString(slideFrameNames[i].frame5));
      slideFrames[i].frames[5] := R_TextureNumForName(DEH_GetString(slideFrameNames[i].frame6));
      slideFrames[i].frames[6] := R_TextureNumForName(DEH_GetString(slideFrameNames[i].frame7));
      slideFrames[i].frames[7] := R_TextureNumForName(DEH_GetString(slideFrameNames[i].frame8));
    end;
  end;
end;

//==============================================================================
//
// P_FindSlidingDoorType
//
// Return index into "slideFrames" array
// for which door type to use
//
// villsa [STRIFE] resurrected
//
//==============================================================================
function P_FindSlidingDoorType(line: Pline_t): integer;
var
  i: integer;
begin
  for i := 0 to MAXSLIDEDOORS - 1 do
    if sides[line.sidenum[0]].toptexture = slideFrames[i].frames[0] then
    begin
      result := i;
      exit;
    end;

  result := -1;
end;

//==============================================================================
//
// T_SlidingDoor
//
// villsa [STRIFE] resurrected
//
//==============================================================================
procedure T_SlidingDoor(door: Pslidedoor_t);
var
  sec: Psector_t;
  speed: fixed_t;
  cheight: fixed_t;
begin
  sec := door.frontsector;

  case door.status of
    sd_opening:
      begin
        dec(door.timer);
        if door.timer <= 0 then
        begin
          inc(door.frame);
          if door.frame = SNUMFRAMES then
          begin
            // IF DOOR IS DONE OPENING...
            door.line1.flags := door.line1.flags and not ML_BLOCKING;
            door.line2.flags := door.line2.flags and not ML_BLOCKING;

            if door._type = sdt_openOnly then
            begin
              door.frontsector.ceilingdata := nil;
              P_RemoveThinker(@door.thinker);
              exit;
            end;

            door.timer := SDOORWAIT;
            door.status := sd_waiting;
          end
          else
          begin
            // IF DOOR NEEDS TO ANIMATE TO NEXT FRAME...
            door.timer := SWAITTICS;

            sides[door.line2.sidenum[0]].midtexture :=
                    slideFrames[door.whichDoorIndex].frames[door.frame];

            sides[door.line2.sidenum[1]].midtexture :=
                    slideFrames[door.whichDoorIndex].frames[door.frame];

            sides[door.line1.sidenum[0]].midtexture :=
                    slideFrames[door.whichDoorIndex].frames[door.frame];

            sides[door.line1.sidenum[1]].midtexture :=
                    slideFrames[door.whichDoorIndex].frames[door.frame];
          end;
        end;
      end;

    sd_waiting:
      begin
        // IF DOOR IS DONE WAITING...
        dec(door.timer);
        if door.timer <= -1 then
        begin
          sec := door.frontsector;

          // CAN DOOR CLOSE?
          if sec.thinglist <> nil then
          begin
            door.timer := SDOORWAIT;
            exit;
          end
          else
          begin
            cheight := sec.ceilingheight;
            speed := cheight - sec.floorheight - (10 * FRACUNIT);

            // something blocking it?
            if T_MovePlane(sec, speed, sec.floorheight, false, 1, -1) = crushed then
            begin
              door.timer := SDOORWAIT;
              exit;
            end
            else
            begin
              // Instantly move plane
              T_MovePlane(sec, 128 * FRACUNIT, cheight, false, 1, 1);

              // turn line blocking back on
              door.line1.flags := door.line1.flags or ML_BLOCKING;
              door.line2.flags := door.line2.flags or ML_BLOCKING;

              // play close sound
              S_StartSound(@sec.soundorg, Ord(slideCloseSounds[door.whichDoorIndex]));

              door.status := sd_closing;
              door.timer := SWAITTICS;
            end;
          end;
        end;
      end;

    sd_closing:
      begin
        dec(door.timer);
        if door.timer <= -1 then
        begin
          dec(door.frame);
          if door.frame < 0 then
          begin
            // IF DOOR IS DONE CLOSING...
            T_MovePlane(sec, 128 * FRACUNIT, sec.floorheight, false, 1, -1);
            door.frontsector.ceilingdata := nil;
            P_RemoveThinker(@door.thinker);
            exit;
          end
          else
          begin
            // IF DOOR NEEDS TO ANIMATE TO NEXT FRAME...
            door.timer := SWAITTICS;

            sides[door.line2.sidenum[0]].midtexture :=
                    slideFrames[door.whichDoorIndex].frames[door.frame];

            sides[door.line2.sidenum[1]].midtexture :=
                    slideFrames[door.whichDoorIndex].frames[door.frame];

            sides[door.line1.sidenum[0]].midtexture :=
                    slideFrames[door.whichDoorIndex].frames[door.frame];

            sides[door.line1.sidenum[1]].midtexture :=
                    slideFrames[door.whichDoorIndex].frames[door.frame];
          end;
        end;
      end;

  end;
end;

//==============================================================================
//
// EV_RemoteSlidingDoor
//
// villsa [STRIFE] new function
//
//==============================================================================
function EV_RemoteSlidingDoor(line: Pline_t; thing: Pmobj_t): integer;
var
  secnum: integer;
  sec: Psector_t;
  i: integer;
  secline: Pline_t;
begin
  result := 0;

  secnum := -1;
  while P_FindSectorFromLineTag2(line, secnum) >= 0 do
  begin
    sec := @sectors[secnum];
    if sec.ceilingdata <> nil then
      continue;

    // JVAL: Check sector linecount
    if sec.linecount < 4 then
      continue;

    for i := 0 to 3 do
    begin
      secline := sec.lines[i];

      if P_FindSlidingDoorType(secline) < 0 then
        continue;

      sec.renderflags := sec.renderflags or SRF_NO_INTERPOLATE;
      EV_SlidingDoor(secline, thing);
      result := 1;
    end;
  end;
end;

//==============================================================================
//
// EV_SlidingDoor
//
// villsa [STRIFE]
//
//==============================================================================
procedure EV_SlidingDoor(line: Pline_t; thing: Pmobj_t);
var
  sec: Psector_t;
  door: Pslidedoor_t;
  i: integer;
  secline: Pline_t;
  side1, side2: Pside_t;
begin
  // Make sure door isn't already being animated
  sec := sides[line.sidenum[1]].sector;
  door := nil;
  if sec.ceilingdata <> nil then
  begin
    if thing.player = nil then
      exit;

    door := sec.ceilingdata;

    if door._type = sdt_openAndClose then
    begin
      if door.status = sd_waiting then
      begin
        door.status := sd_closing;
        door.timer := SWAITTICS;    // villsa [STRIFE]
      end
    end
    else
      exit;
  end;

  sec.renderflags := sec.renderflags or SRF_NO_INTERPOLATE;

  // Init sliding door vars
  if door = nil then
  begin
    door := Z_Malloc(SizeOf(slidedoor_t), PU_LEVSPEC, nil);
    P_AddThinker(@door.thinker);

    sec.ceilingdata := door;

    door._type := sdt_openAndClose;
    door.status := sd_opening;
    door.whichDoorIndex := P_FindSlidingDoorType(line);

    // villsa [STRIFE] different error message
    if door.whichDoorIndex < 0 then
      I_Error('EV_SlidingDoor(): Textures are not defined for sliding door!');

    sides[line.sidenum[0]].midtexture := sides[line.sidenum[0]].toptexture;

    // villsa [STRIFE]
    door.line1 := line;
    door.line2 := line;

    // villsa [STRIFE] this loop assumes that the sliding door is made up
    // of only four linedefs!
    for i := 0 to 3 do
    begin
      secline := sec.lines[i];
      if secline <> line then
      begin
        side1 := @sides[secline.sidenum[0]];
        side2 := @sides[line.sidenum[0]];

        if side1.toptexture = side2.toptexture then
          door.line2 := secline;
      end;
    end;

    door.thinker._function.acp1 := @T_SlidingDoor;
    door.timer := SWAITTICS;
    door.frontsector := sec;
    door.frame := 0;

    // villsa [STRIFE] preset flags
    door.line1.flags := door.line1.flags or ML_BLOCKING;
    door.line2.flags := door.line2.flags or ML_BLOCKING;

    // villsa [STRIFE] set the closing sector
    T_MovePlane(
            door.frontsector,
            128 * FRACUNIT,
            P_FindLowestCeilingSurrounding(door.frontsector),
            false,
            1,
            1);

    // villsa [STRIFE] play open sound
    S_StartSound(@door.frontsector.soundorg, Ord(slideOpenSounds[door.whichDoorIndex]));
  end;
end;

end.
