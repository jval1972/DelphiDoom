//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2012 by Jim Valavanis
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
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://delphidoom.sitesled.com/
//------------------------------------------------------------------------------

{$IFDEF FPC}
{$Error: Use you must use Delphi to compile this project. Use Doom32f.dpr with FPC}
{$ENDIF}

{$IFDEF OPENGL}
{$Error: This project uses software renderer, please undef "OPENGL"}
{$ENDIF}

{$IFNDEF DOOM}
{$Error: To compile this project you must define "DOOM"}
{$ENDIF}

{$I Doom32.inc}
{$D Doom to Delphi Total Conversion}

program Doom32;

{$R *.RES}

uses
  FastMM4 in 'FASTMM4\FastMM4.pas',
  FastMM4Messages in 'FASTMM4\FastMM4Messages.pas',
  jpg_utils in 'JPEGLIB\jpg_utils.pas',
  jpg_COMapi in 'JPEGLIB\jpg_comapi.pas',
  jpg_dAPImin in 'JPEGLIB\jpg_dapimin.pas',
  jpg_dAPIstd in 'JPEGLIB\jpg_dapistd.pas',
  jpg_DCoefCt in 'JPEGLIB\jpg_dcoefct.pas',
  jpg_dColor in 'JPEGLIB\jpg_dcolor.pas',
  jpg_dct in 'JPEGLIB\jpg_dct.pas',
  jpg_dDctMgr in 'JPEGLIB\jpg_ddctmgr.pas',
  jpg_defErr in 'JPEGLIB\jpg_deferr.pas',
  jpg_dHuff in 'JPEGLIB\jpg_dhuff.pas',
  jpg_dInput in 'JPEGLIB\jpg_dinput.pas',
  jpg_dMainCt in 'JPEGLIB\jpg_dmainct.pas',
  jpg_dMarker in 'JPEGLIB\jpg_dmarker.pas',
  jpg_dMaster in 'JPEGLIB\jpg_dmaster.pas',
  jpg_dMerge in 'JPEGLIB\jpg_dmerge.pas',
  jpg_dpHuff in 'JPEGLIB\jpg_dphuff.pas',
  jpg_dPostCt in 'JPEGLIB\jpg_dpostct.pas',
  jpg_dSample in 'JPEGLIB\jpg_dsample.pas',
  jpg_error in 'JPEGLIB\jpg_error.pas',
  jpg_IDctAsm in 'JPEGLIB\jpg_idctasm.pas',
  jpg_IDctFlt in 'JPEGLIB\jpg_idctflt.pas',
  jpg_IDctFst in 'JPEGLIB\jpg_idctfst.pas',
  jpg_IDctRed in 'JPEGLIB\jpg_IDctRed.pas',
  jpg_Lib in 'JPEGLIB\jpg_lib.pas',
  jpg_MemMgr in 'JPEGLIB\jpg_memmgr.pas',
  jpg_memnobs in 'JPEGLIB\jpg_memnobs.pas',
  jpg_moreCfg in 'JPEGLIB\jpg_morecfg.pas',
  jpg_Quant1 in 'JPEGLIB\jpg_quant1.pas',
  jpg_Quant2 in 'JPEGLIB\jpg_quant2.pas',
  mp3_SynthFilter in 'MP3LIB\mp3_SynthFilter.pas',
  mp3_Args in 'MP3LIB\mp3_Args.pas',
  mp3_BitReserve in 'MP3LIB\mp3_BitReserve.pas',
  mp3_BitStream in 'MP3LIB\mp3_BitStream.pas',
  mp3_CRC in 'MP3LIB\mp3_CRC.pas',
  mp3_Header in 'MP3LIB\mp3_Header.pas',
  mp3_Huffman in 'MP3LIB\mp3_Huffman.pas',
  mp3_InvMDT in 'MP3LIB\mp3_InvMDT.pas',
  mp3_L3Tables in 'MP3LIB\mp3_L3Tables.pas',
  mp3_L3Type in 'MP3LIB\mp3_L3Type.pas',
  mp3_Layer3 in 'MP3LIB\mp3_Layer3.pas',
  mp3_MPEGPlayer in 'MP3LIB\mp3_MPEGPlayer.pas',
  mp3_OBuffer in 'MP3LIB\mp3_OBuffer.pas',
  mp3_OBuffer_MCI in 'MP3LIB\mp3_OBuffer_MCI.pas',
  mp3_OBuffer_Wave in 'MP3LIB\mp3_OBuffer_Wave.pas',
  mp3_Player in 'MP3LIB\mp3_Player.pas',
  mp3_ScaleFac in 'MP3LIB\mp3_ScaleFac.pas',
  mp3_Shared in 'MP3LIB\mp3_Shared.pas',
  mp3_SubBand1 in 'MP3LIB\mp3_SubBand1.pas',
  mp3_SubBand2 in 'MP3LIB\mp3_SubBand2.pas',
  mp3_SubBand in 'MP3LIB\mp3_SubBand.pas',
  t_bmp in 'TEXLIB\t_bmp.pas',
  t_colors in 'TEXLIB\t_colors.pas',
  t_draw in 'TEXLIB\t_draw.pas',
  t_jpeg in 'TEXLIB\t_jpeg.pas',
  t_main in 'TEXLIB\t_main.pas',
  t_png in 'TEXLIB\t_png.pas',
  t_tga in 'TEXLIB\t_tga.pas',
  z_files in 'ZLIB\z_files.pas',
  DirectX in 'Common\DirectX.pas',
  am_map in 'Doom\am_map.pas',
  c_cmds in 'Base\c_cmds.pas',
  c_con in 'Doom\c_con.pas',
  c_utils in 'Doom\c_utils.pas',
  d_delphi in 'Common\d_delphi.pas',
  d_englsh in 'Doom\d_englsh.pas',
  d_event in 'Doom\d_event.pas',
  d_fpc in 'Doom\d_fpc.pas',
  d_items in 'Doom\d_items.pas',
  d_main in 'Doom\d_main.pas',
  d_net in 'Doom\d_net.pas',
  d_net_h in 'Doom\d_net_h.pas',
  d_player in 'Doom\d_player.pas',
  d_textur in 'Doom\d_textur.pas',
  d_think in 'Doom\d_think.pas',
  d_ticcmd in 'Doom\d_ticcmd.pas',
  deh_main in 'Doom\deh_main.pas',
  doomdata in 'Doom\doomdata.pas',
  doomdef in 'Doom\doomdef.pas',
  doomstat in 'Doom\doomstat.pas',
  doomtype in 'Doom\doomtype.pas',
  dstrings in 'Doom\dstrings.pas',
  e_endoom in 'Doom\e_endoom.pas',
  f_finale in 'Doom\f_finale.pas',
  f_wipe in 'Doom\f_wipe.pas',
  g_game in 'Doom\g_game.pas',
  hu_lib in 'Doom\hu_lib.pas',
  hu_stuff in 'Doom\hu_stuff.pas',
  i_input in 'Doom\i_input.pas',
  i_io in 'Base\i_io.pas',
  i_main in 'Doom\i_main.pas',
  i_midi in 'Doom\i_midi.pas',
  i_mp3 in 'Doom\i_mp3.pas',
  i_music in 'Doom\i_music.pas',
  i_net in 'Doom\i_net.pas',
  i_sound in 'Doom\i_sound.pas',
  i_system in 'Doom\i_system.pas',
  i_video in 'Doom\i_video.pas',
  info in 'Doom\info.pas',
  info_h in 'Doom\info_h.pas',
  info_rnd in 'Doom\info_rnd.pas',
  m_argv in 'Doom\m_argv.pas',
  m_bbox in 'Doom\m_bbox.pas',
  m_cheat in 'Doom\m_cheat.pas',
  m_defs in 'Doom\m_defs.pas',
  m_fixed in 'Base\m_fixed.pas',
  m_menu in 'Doom\m_menu.pas',
  m_misc in 'Doom\m_misc.pas',
  m_rnd in 'Base\m_rnd.pas',
  m_stack in 'Base\m_stack.pas',
  m_vectors in 'Base\m_vectors.pas',
  p_ceilng in 'Doom\p_ceilng.pas',
  p_doors in 'Doom\p_doors.pas',
  p_enemy in 'Doom\p_enemy.pas',
  p_extra in 'Doom\p_extra.pas',
  p_floor in 'Doom\p_floor.pas',
  p_genlin in 'Doom\p_genlin.pas',
  p_inter in 'Doom\p_inter.pas',
  p_lights in 'Doom\p_lights.pas',
  p_local in 'Doom\p_local.pas',
  p_map in 'Doom\p_map.pas',
  p_maputl in 'Doom\p_maputl.pas',
  p_mobj in 'Doom\p_mobj.pas',
  p_mobj_h in 'Doom\p_mobj_h.pas',
  p_plats in 'Doom\p_plats.pas',
  p_pspr in 'Doom\p_pspr.pas',
  p_pspr_h in 'Doom\p_pspr_h.pas',
  p_saveg in 'Doom\p_saveg.pas',
  p_scroll in 'Doom\p_scroll.pas',
  p_setup in 'Doom\p_setup.pas',
  p_sight in 'Doom\p_sight.pas',
  p_sounds in 'Doom\p_sounds.pas',
  p_spec in 'Doom\p_spec.pas',
  p_switch in 'Doom\p_switch.pas',
  p_telept in 'Doom\p_telept.pas',
  p_terrain in 'Doom\p_terrain.pas',
  p_tick in 'Doom\p_tick.pas',
  p_user in 'Doom\p_user.pas',
  r_bsp in 'Doom\r_bsp.pas',
  r_cache in 'Doom\r_cache.pas',
  r_ccache in 'Doom\r_ccache.pas',
  r_col_al in 'Doom\r_col_al.pas',
  r_col_av in 'Doom\r_col_av.pas',
  r_col_fz in 'Doom\r_col_fz.pas',
  r_col_l in 'Doom\r_col_l.pas',
  r_col_ms in 'Doom\r_col_ms.pas',
  r_col_sk in 'Doom\r_col_sk.pas',
  r_col_tr in 'Doom\r_col_tr.pas',
  r_column in 'Doom\r_column.pas',
  r_data in 'Doom\r_data.pas',
  r_defs in 'Doom\r_defs.pas',
  r_draw in 'Doom\r_draw.pas',
  r_fake3d in 'Doom\r_fake3d.pas',
  r_grow in 'Doom\r_grow.pas',
  r_hires in 'Base\r_hires.pas',
  r_intrpl in 'Doom\r_intrpl.pas',
  r_lights in 'Doom\r_lights.pas',
  r_main in 'Doom\r_main.pas',
  r_mmx in 'Doom\r_mmx.pas',
  r_plane in 'Doom\r_plane.pas',
  r_scache in 'Doom\r_scache.pas',
  r_segs in 'Doom\r_segs.pas',
  r_sky in 'Doom\r_sky.pas',
  r_span32 in 'Doom\r_span32.pas',
  r_span in 'Doom\r_span.pas',
  r_things in 'Doom\r_things.pas',
  rtl_types in 'Doom\rtl_types.pas',
  s_sound in 'Doom\s_sound.pas',
  sc_decorate in 'Doom\sc_decorate.pas',
  sc_engine in 'Doom\sc_engine.pas',
  sc_params in 'Base\sc_params.pas',
  sounds in 'Doom\sounds.pas',
  st_lib in 'Doom\st_lib.pas',
  st_stuff in 'Doom\st_stuff.pas',
  tables in 'Doom\tables.pas',
  v_data in 'Doom\v_data.pas',
  v_video in 'Doom\v_video.pas',
  w_pak in 'Base\w_pak.pas',
  w_utils in 'Doom\w_utils.pas',
  w_wad in 'Base\w_wad.pas',
  wi_stuff in 'Doom\wi_stuff.pas',
  z_zone in 'Base\z_zone.pas',
  r_trans8 in 'Doom\r_trans8.pas',
  i_exec in 'Base\i_exec.pas',
  i_tmp in 'Base\i_tmp.pas',
  i_startup in 'Base\i_startup.pas' {StartUpConsoleForm},
  t_material in 'TEXLIB\t_material.pas';

var
  Saved8087CW: Word;

begin
  { Save the current FPU state and then disable FPU exceptions }
  Saved8087CW := Default8087CW;
  Set8087CW($133f); { Disable all fpu exceptions }

  try
    DoomMain;
  except
    I_FlashCachedOutput;
  end;

  { Reset the FPU to the previous state }
  Set8087CW(Saved8087CW);

end.
