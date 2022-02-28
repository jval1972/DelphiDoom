//------------------------------------------------------------------------------
//
//  DelphiHexen is a source port of the game Hexen and it is
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit g_demo;

interface

uses
  d_ticcmd;

//==============================================================================
//
// G_FinishedDemoPlayback
//
//==============================================================================
procedure G_FinishedDemoPlayback;

//==============================================================================
//
// G_DoPlayDemo
//
//==============================================================================
procedure G_DoPlayDemo;

//==============================================================================
//
// G_ReadDemoTiccmd
//
//==============================================================================
procedure G_ReadDemoTiccmd(cmd: Pticcmd_t);

//==============================================================================
//
// G_WriteDemoTiccmd
//
//==============================================================================
procedure G_WriteDemoTiccmd(cmd: Pticcmd_t);

//==============================================================================
//
// G_DeferedPlayDemo
//
//==============================================================================
function G_DeferedPlayDemo(const name: string): boolean;

//==============================================================================
//
// G_CmdPlayDemo
//
//==============================================================================
procedure G_CmdPlayDemo(const name: string);

{ Only called by startup code. }

//==============================================================================
//
// G_RecordDemo
//
//==============================================================================
procedure G_RecordDemo(const name: string);

//==============================================================================
//
// G_BeginRecording
//
//==============================================================================
procedure G_BeginRecording;

//==============================================================================
//
// G_TimeDemo
//
//==============================================================================
procedure G_TimeDemo(const name: string);

//==============================================================================
//
// G_CheckDemoStatus
//
//==============================================================================
function G_CheckDemoStatus: boolean;

var
  netdemo: boolean;
  demoplayback: boolean;

var
  demoversion: byte;

implementation

uses
  d_delphi,
  c_cmds,
  d_main,
  d_event,
  g_game,
  i_system,
  info,
  m_misc,
  m_rnd,
  m_argv,
  p_mobj_h,
  doomdef,
  w_wad,
  z_zone;
//
// DEMO PLAYBACK
//
const
  DEMOMARKER = $80;

var
  compatibility_done: boolean;

var
  demoname: string;
  demobuffer: PByteArray;
  demo_p: PByteArray;
  demoend: PByte;

//==============================================================================
//
// G_FinishedDemoPlayback
//
//==============================================================================
procedure G_FinishedDemoPlayback;
begin
  demoplayback := false;
  // Restore old compatibility mode
  compatibilitymode := oldcompatibilitymode;
end;

(*
===================
=
= G_CheckDemoStatus
=
= Called after a death or level completion to allow demos to be cleaned up
= Returns true if a new demo loop action will take place
===================
*)

var
  timingdemo: boolean;       // if true, exit with report on completion

//==============================================================================
//
// G_CheckDemoStatus
//
//==============================================================================
function G_CheckDemoStatus: boolean;
var
  endtime: integer;
  i: integer;
begin
  if timingdemo then
  begin
    endtime := I_GetTime;
    I_Error('G_CheckDemoStatus(): timed %d gametics in %d realtics', [gametic, endtime - starttime]);
  end;

  if demoplayback then
  begin
    if singledemo then
      I_Quit;

    Z_Free(demoBuffer);
    G_FinishedDemoPlayback;
    netdemo := false;
    netgame := false;
    deathmatch := 0;
    for i := 1 to MAXPLAYERS - 1 do
      playeringame[i] := false;
    respawnparm := false;
    fastparm := false;
    nomonsters := false;
    consoleplayer := 0;
    D_AdvanceDemo;
    result := true;
    exit;
  end;

  if demorecording then
  begin
    demo_p[0] := DEMOMARKER;
    demo_p := @demo_p[1];

    M_WriteFile(demoname, demobuffer, pDiff(demo_p, demobuffer, SizeOf(byte)));
    Z_Free(demobuffer);
    demorecording := false;
    I_Error('G_CheckDemoStatus(): Demo %s recorded', [demoname]);
  end;

  result := false;
end;

//==============================================================================
//
// G_ReadDemoTiccmd
//
//==============================================================================
procedure G_ReadDemoTiccmd(cmd: Pticcmd_t);
begin
  if demo_p[0] = DEMOMARKER then
  begin
    // end of demo data stream
    G_CheckDemoStatus;
    exit;
  end;

  cmd.forwardmove := shortint(demo_p[0]);
  demo_p := @demo_p[1];

  cmd.sidemove := shortint(demo_p[0]);
  demo_p := @demo_p[1];

  cmd.angleturn := PSmallInt(demo_p)^;
  demo_p := @demo_p[2];

  cmd.buttons := demo_p[0] and not BT_SPECIAL;
  demo_p := @demo_p[1];

  cmd.lookfly := demo_p[0];
  demo_p := @demo_p[1];

  cmd.arti := demo_p[0];
  demo_p := @demo_p[1];

  cmd.lookleftright := demo_p[0];
  demo_p := @demo_p[1];

  cmd.jump_crouch := demo_p[0];
  demo_p := @demo_p[1];

  if demoversion < VERSION203 then
  begin
    cmd.lookupdown16 := (cmd.lookfly and 15) * 256; // JVAL Smooth Look Up/Down
  end
  else
  begin
    cmd.lookupdown16 := PWord(demo_p)^;
    demo_p := @demo_p[2];
  end;
  cmd.lookupdown := cmd.lookupdown16 div 256; // JVAL unused :)
end;

//==============================================================================
// G_IncreaseDemoBuffer
//
// DEMO RECORDING
//
// Increase the size of the demo buffer to allow unlimited demos
//
//==============================================================================
procedure G_IncreaseDemoBuffer;
var
  current_length: integer;
  new_demobuffer: PByteArray;
  new_demop: PByteArray;
  new_length: integer;
begin
  // Find the current size

  current_length := integer(demoend) - integer(demobuffer);

  // Generate a new buffer twice the size
  new_length := current_length + $80000;

  new_demobuffer := Z_Malloc2(new_length, PU_STATIC, nil);
  if new_demobuffer = nil then
    G_CheckDemoStatus;

  new_demop := @new_demobuffer[integer(demo_p) - integer(demobuffer)];

  // Copy over the old data

  memcpy(new_demobuffer, demobuffer, current_length);

  // Free the old buffer and point the demo pointers at the new buffer.

  Z_Free(demobuffer);

  demobuffer := new_demobuffer;
  demo_p := new_demop;
  demoend := @demobuffer[new_length];
end;

//==============================================================================
//
// G_WriteDemoTiccmd
//
//==============================================================================
procedure G_WriteDemoTiccmd(cmd: Pticcmd_t);
var
  demo_start: PByteArray;
begin
  if gamekeydown[Ord('q')] then // press q to end demo recording
    G_CheckDemoStatus;

  demo_start := demo_p;

  demo_p[0] := Ord(cmd.forwardmove);
  demo_p := @demo_p[1];

  demo_p[0] := Ord(cmd.sidemove);
  demo_p := @demo_p[1];

  PSmallInt(demo_p)^ := cmd.angleturn;
  demo_p := @demo_p[2];

  demo_p[0] := cmd.buttons and not BT_SPECIAL;
  demo_p := @demo_p[1];

  demo_p[0] := cmd.lookfly;
  demo_p := @demo_p[1];

  demo_p[0] := cmd.arti;
  demo_p := @demo_p[1];

  demo_p[0] := cmd.lookleftright;
  demo_p := @demo_p[1];

  demo_p[0] := cmd.jump_crouch;
  demo_p := @demo_p[1];

  // JVAL Smooth Look Up/Down
  PWord(demo_p)^ := cmd.lookupdown16;
//  demo_p := @demo_p[2];

  demo_p := demo_start;

  if integer(demo_p) >= integer(demoend) - 2 * SizeOf(ticcmd_t) then
    G_IncreaseDemoBuffer;

  G_ReadDemoTiccmd(cmd);  // make SURE it is exactly the same
end;

//==============================================================================
//
// G_RecordDemo
//
//==============================================================================
procedure G_RecordDemo(const name: string);
var
  i: integer;
  maxsize: integer;
begin
  usergame := false;
  demoname := name;
  if CharPos('.', demoname) = 0 then
    demoname := demoname + '.lmp';

  i := M_CheckParm ('-maxdemo');
  if (i <> 0) and (i < myargc - 1) then
    maxsize := atoi(myargv[i + 1]) * 1024
  else
    maxsize := SAVEGAMESIZE;

  repeat
    demobuffer := Z_Malloc2(maxsize, PU_STATIC, nil);
    if demobuffer = nil then
      maxsize := maxsize * 3 div 4;
  until demobuffer <> nil;

  demoend := @demobuffer[maxsize];

  demorecording := true;
  Info_Init(true); // JVAL: Start thinkers

end;

const
  DEMOHDR: integer = $4D454458; // JVAL: XDEM in hex

//==============================================================================
//
// G_BeginRecording
//
//==============================================================================
procedure G_BeginRecording;
var
  i: integer;
begin
  demo_p := demobuffer;
  demoversion := VERSION;

  PInteger(demo_p)^ := DEMOHDR;
  demo_p := @demo_p[4];

  demo_p[0] := VERSION;
  demo_p := @demo_p[1];

  demo_p[0] := Ord(gameskill);
  demo_p := @demo_p[1];

  demo_p[0] := gameepisode;
  demo_p := @demo_p[1];

  demo_p[0] := gamemap;
  demo_p := @demo_p[1];

  demo_p[0] := deathmatch;
  demo_p := @demo_p[1];

  demo_p[0] := intval(respawnparm);
  demo_p := @demo_p[1];

  demo_p[0] := intval(fastparm);
  demo_p := @demo_p[1];

  demo_p[0] := intval(nomonsters);
  demo_p := @demo_p[1];

  demo_p[0] := intval(compatibilitymode);
  demo_p := @demo_p[1];

  demo_p[0] := intval(spawnrandommonsters);
  demo_p := @demo_p[1];

  demo_p[0] := sysrndseed;
  demo_p := @demo_p[1];

  demo_p[0] := consoleplayer;
  demo_p := @demo_p[1];

  for i := 0 to MAXPLAYERS - 1 do
  begin
    demo_p[0] := intval(playeringame[i]);
    demo_p := @demo_p[1];
  end;

end;

//
// G_PlayDemo
//
var
  defdemoname: string;
  externaldemo: boolean = false;

//==============================================================================
//
// G_DoPlayExternalDemo
//
//==============================================================================
function G_DoPlayExternalDemo(const name: string): boolean;
var
  dmname: string;
begin
  dmname := name;
  if not fexists(dmname) then
    if M_CheckParmCDROM then
      dmname := CD_WORKDIR + dmname;
  if fexists(dmname) then
  begin
    externaldemo := true;
    defdemoname := dmname;
    gameaction := ga_playdemo;
    printf('Playing demo %s'#13#10, [defdemoname]);
    result := true;
    exit;
  end;
  result := false;
end;

//==============================================================================
//
// G_DeferedPlayDemo
//
//==============================================================================
function G_DeferedPlayDemo(const name: string): boolean;
var
  dmname: string;
  dmname2: string;
  defdemoname8: char8_t;
  i: integer;
  pdot: integer;
begin
  dmname := name;
  for i := 0 to 9 do
    if itoa(i) = name then
    begin
      sprintf(dmname, 'demo%d', [i]);
      break;
    end;

  pdot := CharPos('.', dmname);
  if ((Length(dmname) <= 8) and (pdot = 0)) or ((pdot < 9) and (pdot <> 0)) then
  begin
    ExtractFileBase8(dmname, defdemoname8);
    defdemoname := char8tostring(defdemoname8);
    if W_CheckNumForName(defdemoname) >= 0 then
    begin
      externaldemo := false;
      gameaction := ga_playdemo;
      printf('Playing demo %s'#13#10, [defdemoname]);
      result := true;
      exit;
    end;
  end;

  // JVAL
  // Playdemo command also looks for a filename in current or CD_WORK directory
  dmname2 := dmname;
  if pdot = 0 then
    dmname := dmname + '.lmp';
  if G_DoPlayExternalDemo(dmname) then
  begin
    result := true;
    exit;
  end;

  if pdot = 0 then
  begin
    dmname := dmname2 + '.dem';
    if G_DoPlayExternalDemo(dmname) then
    begin
      result := true;
      exit;
    end;
  end;

  I_Warning('G_DeferedPlayDemo(): Can not find demo %s.'#13#10, [defdemoname]);
  result := false;
end;

//==============================================================================
//
// G_CmdPlayDemo
//
//==============================================================================
procedure G_CmdPlayDemo(const name: string);
begin
  if G_DeferedPlayDemo(name) then
    C_ExecuteCmd('closeconsole', '1');
end;

//==============================================================================
//
// G_DoPlayDemo
//
//==============================================================================
procedure G_DoPlayDemo;
var
  skill: skill_t;
  i, episode, map: integer;
  lump: integer;
  len: integer;
  oldspawnrandommonsters: boolean;
begin
  gameaction := ga_nothing;
  if externaldemo then
    len := M_ReadFile(defdemoname, pointer(demobuffer))
  else
  begin
    lump := W_GetNumForName(defdemoname);
    demobuffer := W_CacheLumpNum(lump, PU_STATIC);
    len := W_LumpLength(lump);
  end;

  demo_p := demobuffer;
  demoend := @demo_p[len];

  if PInteger(demo_p)^ = DEMOHDR then
  begin
    demo_p := @demo_p[4];
    demoversion := demo_p[0];
  end
  else
  begin
    demoversion := demobuffer[0];
  end;

  if demoversion < VERSION142 then
  begin
    I_Warning('G_DoPlayDemo(): Demo is from an unsupported game version = %d.%.*d' + #13#10,
      [demo_p[0] div 100, 2, demo_p[0] mod 100]);
    gameaction := ga_nothing;
    exit;
  end
  else if demoversion <> VERSION then
    I_Warning('G_DoPlayDemo(): Demo is from a partial supported game version = %d.%.*d' + #13#10,
      [demo_p[0] div 100, 2, demo_p[0] mod 100]);

  demo_p := @demo_p[1];

  skill := skill_t(demo_p[0]);
  demo_p := @demo_p[1];

  episode := demo_p[0];
  demo_p := @demo_p[1];

  map := demo_p[0];
  demo_p := @demo_p[1];

  deathmatch := demo_p[0];
  demo_p := @demo_p[1];

  respawnparm := demo_p[0] <> 0;
  demo_p := @demo_p[1];

  fastparm := demo_p[0] <> 0;
  demo_p := @demo_p[1];

  nomonsters := demo_p[0] <> 0;
  demo_p := @demo_p[1];

  oldcompatibilitymode := compatibilitymode;
  compatibilitymode := demo_p[0] <> 0;
  demo_p := @demo_p[1];

  oldspawnrandommonsters := spawnrandommonsters;
  spawnrandommonsters := demo_p[0] <> 0;
  demo_p := @demo_p[1];

  if demoversion >= VERSION207 then
  begin
    sysrndseed := demo_p[0];
    demo_p := @demo_p[1];
  end
  else
    sysrndseed := 0;

  consoleplayer := demo_p[0];
  demo_p := @demo_p[1];

  for i := 0 to MAXPLAYERS - 1 do
  begin
    playeringame[i] := demo_p[0] <> 0;
    demo_p := @demo_p[1];
  end;

  if playeringame[1] then
  begin
    netgame := true;
    netdemo := true;
  end;

  preparingdemoplayback := true;
  // don't spend a lot of time in loadlevel if not singledemo
  precache := singledemo; // JVAL original code: precache := false
  G_InitNew(skill, episode, map);
  preparingdemoplayback := false;
  spawnrandommonsters := oldspawnrandommonsters;  // Back to default
  precache := true;
  usergame := false;
  demoplayback := true;
  Info_Init(true); // JVAL: Start thinkers

  compatibility_done := false;

end;

//==============================================================================
//
// G_TimeDemo
//
//==============================================================================
procedure G_TimeDemo(const name: string);
begin
  timingdemo := true;
  singletics := true;

  defdemoname := name;
  gameaction := ga_playdemo;
end;

end.
