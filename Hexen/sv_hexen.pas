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
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Binary serializer
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit sv_hexen;

interface

uses
  p_serializer;

var
  MobjSerializer206: TSerializer;
  MobjSerializer: TSerializer;
  SectorSerializer: TSerializer;
  LineSerializer: TSerializer;
  SideSerializer: TSerializer;

//==============================================================================
//
// SV_InitializeSerializers
//
//==============================================================================
procedure SV_InitializeSerializers;

//==============================================================================
//
// SV_ShutDownSerializers
//
//==============================================================================
procedure SV_ShutDownSerializers;

implementation

uses
  d_delphi,
  m_fixed,
  info,
  info_h,
  p_mobj_h,
  p_udmf,
  s_sndseq,
  r_defs,
  r_renderstyle;

//==============================================================================
//
// OnLoadMobjInfoType
//
//==============================================================================
procedure OnLoadMobjInfoType(const struct: Pointer);
var
  amobj: Pmobj_t;
  info: Pmobjinfo_t;
begin
  amobj := struct;
  if not IsIntegerInRange(amobj._type, 0, nummobjtypes - 1) then
    exit;

  // Default values
  info := @mobjinfo[amobj._type];
  amobj.info := info;
  amobj.painchance := info.painchance;
  amobj.spriteDX := info.spriteDX;
  amobj.spriteDY := info.spriteDY;
  amobj.infighting_group := info.infighting_group;
  amobj.projectile_group := info.projectile_group;
  amobj.splash_group := info.splash_group;
  amobj.bloodcolor := info.bloodcolor;
  amobj.translationname := info.translationname;
end;

//==============================================================================
//
// SV_InitializeSerializers
//
//==============================================================================
procedure SV_InitializeSerializers;
var
  mo206: Pmobj_t206;
  mo: Pmobj_t;
  sec: Psector_t;
  ln: Pline_t;
  sd: Pside_t;
begin
  mo206 := nil;
  MobjSerializer206 := TSerializer.Create;
  MobjSerializer206.AddTypedItem(@mo206.x, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.y, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.z, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.angle, st_longword, 0);
  MobjSerializer206.AddTypedItem(@mo206.viewangle, st_longword, 0);
  MobjSerializer206.AddTypedItem(@mo206.sprite, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.frame, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.floorz, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.ceilingz, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.floorpic, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.radius, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.height, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.momx, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.momy, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.momz, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206._type, st_integer, -1, @OnLoadMobjInfoType);
  MobjSerializer206.AddTypedItem(@mo206.tics, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.state, st_integer, 0);  // State translated to id
  MobjSerializer206.AddTypedItem(@mo206.flags, st_integer, MF_SOLID);
  MobjSerializer206.AddTypedItem(@mo206.flags2, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.flags_ex, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.flags2_ex, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.damage, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.special1, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.special2, st_integer, 0);
  MobjSerializer206.AddItem(@mo206.renderstyle, SizeOf(mobjrenderstyle_t));
  MobjSerializer206.AddTypedItem(@mo206.alpha, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.bob, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.health, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.movedir, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.movecount, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.target, st_integer, 0);  // Target translated key
  MobjSerializer206.AddTypedItem(@mo206.reactiontime, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.threshold, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.player, st_integer, 0); // Player translated to id
  MobjSerializer206.AddTypedItem(@mo206.lastlook, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.spawnpoint.x, st_smallint, 0);
  MobjSerializer206.AddTypedItem(@mo206.spawnpoint.y, st_smallint, 0);
  MobjSerializer206.AddTypedItem(@mo206.spawnpoint.angle, st_smallint, 0);
  MobjSerializer206.AddTypedItem(@mo206.spawnpoint._type, st_word, 0);
  MobjSerializer206.AddTypedItem(@mo206.spawnpoint.options, st_smallint, 0);
  MobjSerializer206.AddTypedItem(@mo206.tracer, st_integer, 0);  // Tracer translated key
  MobjSerializer206.AddTypedItem(@mo206.fastchasetics, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.archivenum, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.tid, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.floorclip, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.friction, st_integer, 0);
  MobjSerializer206.AddItem(@mo206.customparams, SizeOf(pointer));
  MobjSerializer206.AddTypedItem(@mo206.key, st_longword, 0);
  MobjSerializer206.AddTypedItem(@mo206.prevx, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.prevy, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.prevz, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.prevangle, st_longword, 0);
  MobjSerializer206.AddTypedItem(@mo206.nextx, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.nexty, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.nextz, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.nextangle, st_longword, 0);
  MobjSerializer206.AddTypedItem(@mo206.intrplcnt, st_longword, 0);
  MobjSerializer206.AddTypedItem(@mo206.dropitem, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.scale, st_integer, FRACUNIT);
  MobjSerializer206.AddTypedItem(@mo206.pushfactor, st_integer, DEFPUSHFACTOR);
  MobjSerializer206.AddTypedItem(@mo206.gravity, st_integer, FRACUNIT);
  MobjSerializer206.AddTypedItem(@mo206.flags3_ex, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.flags4_ex, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.mass, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.args[0], st_byte, 0);
  MobjSerializer206.AddTypedItem(@mo206.args[1], st_byte, 0);
  MobjSerializer206.AddTypedItem(@mo206.args[2], st_byte, 0);
  MobjSerializer206.AddTypedItem(@mo206.args[3], st_byte, 0);
  MobjSerializer206.AddTypedItem(@mo206.args[4], st_byte, 0);
  MobjSerializer206.AddTypedItem(@mo206.special, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.master, st_integer, 0);  // Master translated key
  MobjSerializer206.AddTypedItem(@mo206.WeaveIndexXY, st_integer, 0);
  MobjSerializer206.AddTypedItem(@mo206.WeaveIndexZ, st_integer, 0);

  mo := nil;
  MobjSerializer := TSerializer.Create;
  MobjSerializer.AddTypedItem(@mo.x, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.y, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.z, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.angle, st_longword, 0);
  MobjSerializer.AddTypedItem(@mo.viewangle, st_longword, 0);
  MobjSerializer.AddTypedItem(@mo.sprite, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.frame, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.floorz, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.ceilingz, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.floorpic, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.radius, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.height, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.momx, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.momy, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.momz, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo._type, st_integer, -1, @OnLoadMobjInfoType);
  MobjSerializer.AddTypedItem(@mo.tics, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.state, st_integer, 0);  // Hexen save system needs untranslated
  MobjSerializer.AddTypedItem(@mo.flags, st_integer, MF_SOLID);
  MobjSerializer.AddTypedItem(@mo.flags2, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.flags_ex, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.flags2_ex, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.damage, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.special1, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.special2, st_integer, 0);
  MobjSerializer.AddItem(@mo.renderstyle, SizeOf(mobjrenderstyle_t));
  MobjSerializer.AddTypedItem(@mo.alpha, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.bob, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.health, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.movedir, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.movecount, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.target, st_integer, 0); // Hexen save system needs untranslated
  MobjSerializer.AddTypedItem(@mo.reactiontime, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.threshold, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.player, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.lastlook, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.spawnpoint.x, st_smallint, 0);
  MobjSerializer.AddTypedItem(@mo.spawnpoint.y, st_smallint, 0);
  MobjSerializer.AddTypedItem(@mo.spawnpoint.angle, st_smallint, 0);
  MobjSerializer.AddTypedItem(@mo.spawnpoint._type, st_word, 0);
  MobjSerializer.AddTypedItem(@mo.spawnpoint.options, st_smallint, 0);
  MobjSerializer.AddTypedItem(@mo.tracer, st_integer, 0); // Hexen save system needs untranslated
  MobjSerializer.AddTypedItem(@mo.fastchasetics, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.archivenum, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.tid, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.floorclip, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.friction, st_integer, 0);
  MobjSerializer.AddItem(@mo.customparams, SizeOf(pointer));
  MobjSerializer.AddTypedItem(@mo.key, st_longword, 0);
  MobjSerializer.AddTypedItem(@mo.prevx, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.prevy, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.prevz, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.prevangle, st_longword, 0);
  MobjSerializer.AddTypedItem(@mo.nextx, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.nexty, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.nextz, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.nextangle, st_longword, 0);
  MobjSerializer.AddTypedItem(@mo.intrplcnt, st_longword, 0);
  MobjSerializer.AddTypedItem(@mo.dropitem, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.scale, st_integer, FRACUNIT);
  MobjSerializer.AddTypedItem(@mo.pushfactor, st_integer, DEFPUSHFACTOR);
  MobjSerializer.AddTypedItem(@mo.gravity, st_integer, FRACUNIT);
  MobjSerializer.AddTypedItem(@mo.flags3_ex, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.flags4_ex, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.mass, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.args[0], st_byte, 0);
  MobjSerializer.AddTypedItem(@mo.args[1], st_byte, 0);
  MobjSerializer.AddTypedItem(@mo.args[2], st_byte, 0);
  MobjSerializer.AddTypedItem(@mo.args[3], st_byte, 0);
  MobjSerializer.AddTypedItem(@mo.args[4], st_byte, 0);
  MobjSerializer.AddTypedItem(@mo.special, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.master, st_integer, 0); // Hexen save system needs untranslated
  MobjSerializer.AddTypedItem(@mo.WeaveIndexXY, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.WeaveIndexZ, st_integer, 0);
  // Version 207
  MobjSerializer.AddTypedItem(@mo.painchance, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.spriteDX, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.spriteDY, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.flags5_ex, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.flags6_ex, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.playerfollowtime, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.tracefollowtimestamp, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.tracex, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.tracey, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.tracez, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.infighting_group, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.projectile_group, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.splash_group, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.strafecount, st_integer, 0);
  MobjSerializer.AddTypedItem(@mo.bloodcolor, st_integer, 0);
  MobjSerializer.AddStringItem(@mo.translationname, '');

  sec := nil;
  SectorSerializer := TSerializer.Create;
  SectorSerializer.AddTypedItem(@sec.floorheight, st_integer, 0);
  SectorSerializer.AddTypedItem(@sec.ceilingheight, st_integer, 128 * FRACUNIT);
  SectorSerializer.AddTypedItem(@sec.lightlevel, st_smallint, 160);
  SectorSerializer.AddTypedItem(@sec.special, st_smallint, 0);
  SectorSerializer.AddTypedItem(@sec.tag, st_smallint, 0);
  SectorSerializer.AddTypedItem(@sec.renderflags, st_longword, 0);
  SectorSerializer.AddTypedItem(@sec.flags, st_longword, 0);
  SectorSerializer.AddTypedItem(@sec.midsec, st_integer, -1);
  SectorSerializer.AddTypedItem(@sec.midline, st_integer, -1);
  SectorSerializer.AddTypedItem(@sec.gravity, st_integer, FRACUNIT);
  SectorSerializer.AddTypedItem(@sec.floorangle, st_longword, 0);
  SectorSerializer.AddTypedItem(@sec.flooranglex, st_integer, 0);
  SectorSerializer.AddTypedItem(@sec.floorangley, st_integer, 0);
  SectorSerializer.AddTypedItem(@sec.ceilingangle, st_longword, 0);
  SectorSerializer.AddTypedItem(@sec.ceilinganglex, st_integer, 0);
  SectorSerializer.AddTypedItem(@sec.ceilingangley, st_integer, 0);
  SectorSerializer.AddFloatItem(@sec.fa, 0.0);
  SectorSerializer.AddFloatItem(@sec.fb, 0.0);
  SectorSerializer.AddFloatItem(@sec.fd, 0.0);
  SectorSerializer.AddFloatItem(@sec.fic, 0.0);
  SectorSerializer.AddFloatItem(@sec.ca, 0.0);
  SectorSerializer.AddFloatItem(@sec.cb, 0.0);
  SectorSerializer.AddFloatItem(@sec.cd, 0.0);
  SectorSerializer.AddFloatItem(@sec.cic, 0.0);
  SectorSerializer.AddTypedItem(@sec.num_saffectees, st_integer, 1);
  SectorSerializer.AddItem(@sec.moreids, SizeOf(moreids_t));
  SectorSerializer.AddItem(@sec.seqType, SizeOf(seqtype_t));
  SectorSerializer.AddTypedItem(@sec.windthrust, st_integer, 0);
  SectorSerializer.AddTypedItem(@sec.windangle, st_longword, 0);

  ln := nil;
  LineSerializer := TSerializer.Create;
  LineSerializer.AddTypedItem(@ln.flags, st_word, 1);
  LineSerializer.AddTypedItem(@ln.special, st_byte, 0);
  LineSerializer.AddTypedItem(@ln.renderflags, st_longword, 0);
  LineSerializer.AddTypedItem(@ln.arg1, st_byte, 0);
  LineSerializer.AddTypedItem(@ln.arg2, st_byte, 0);
  LineSerializer.AddTypedItem(@ln.arg3, st_byte, 0);
  LineSerializer.AddTypedItem(@ln.arg4, st_byte, 0);
  LineSerializer.AddTypedItem(@ln.arg5, st_byte, 0);
  LineSerializer.AddItem(@ln.moreids, SizeOf(moreids_t));

  sd := nil;
  SideSerializer := TSerializer.Create;
  SideSerializer.AddTypedItem(@sd.textureoffset, st_integer, 0);
  SideSerializer.AddTypedItem(@sd.rowoffset, st_integer, 0);
  SideSerializer.AddTypedItem(@sd.toptextureoffset, st_integer, 0);
  SideSerializer.AddTypedItem(@sd.bottomtextureoffset, st_integer, 0);
  SideSerializer.AddTypedItem(@sd.midtextureoffset, st_integer, 0);
  SideSerializer.AddTypedItem(@sd.toprowoffset, st_integer, 0);
  SideSerializer.AddTypedItem(@sd.bottomrowoffset, st_integer, 0);
  SideSerializer.AddTypedItem(@sd.midrowoffset, st_integer, 0);
  SideSerializer.AddTypedItem(@sd.flags, st_integer, 0);
end;

//==============================================================================
//
// SV_ShutDownSerializers
//
//==============================================================================
procedure SV_ShutDownSerializers;
begin
  MobjSerializer206.Free;
  MobjSerializer.Free;
  SectorSerializer.Free;
  LineSerializer.Free;
  SideSerializer.Free;
end;

end.
