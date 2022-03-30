//------------------------------------------------------------------------------
//
//  DelphiDoom is a source port of the game Doom and it is
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
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
//  Rendering of moving objects, sprites.
//  Refresh of things, i.e. objects represented by sprites.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_things;

interface

uses
  d_delphi,
  doomdef,
  p_mobj_h,
  m_fixed,
  r_defs;

var
  // JVAL Note about visprites
  // Now visprites allocated dynamically using Zone memory
  // (Original MAXVISSPRITES was 128)
  maxvissprite: integer;

{$IFNDEF OPENGL}

//==============================================================================
//
// R_DrawMaskedColumn
//
//==============================================================================
procedure R_DrawMaskedColumn(column: Pcolumn_t; const depthscale: fixed_t; const mo: Pmobj_t = nil;
  baseclip: integer = -1; const renderflags: LongWord = 0);

//==============================================================================
//
// R_DrawMaskedColumn2
//
//==============================================================================
procedure R_DrawMaskedColumn2(const mc2h: integer; const depthscale: fixed_t); // Use dc_source32
{$ENDIF}

//==============================================================================
//
// R_AddSprites
//
//==============================================================================
procedure R_AddSprites(sec: Psector_t);

//==============================================================================
//
// R_InitSprites
//
//==============================================================================
procedure R_InitSprites(namelist: PIntegerArray);

//==============================================================================
//
// R_InitNegoArray
//
//==============================================================================
procedure R_InitNegoArray;

//==============================================================================
//
// R_ClearSprites
//
//==============================================================================
procedure R_ClearSprites;
{$IFNDEF OPENGL}

//==============================================================================
//
// R_DrawMasked_SingleThread
//
//==============================================================================
procedure R_DrawMasked_SingleThread;

//==============================================================================
//
// R_DrawMasked_MultiThread
//
//==============================================================================
procedure R_DrawMasked_MultiThread;

//==============================================================================
//
// R_PrepareMasked
//
//==============================================================================
procedure R_PrepareMasked;

//==============================================================================
//
// R_SignalPrepareMasked
//
//==============================================================================
procedure R_SignalPrepareMasked;
{$ENDIF}

//==============================================================================
//
// R_DrawPlayer
//
//==============================================================================
procedure R_DrawPlayer;

var
  pspritescale: fixed_t;
  pspriteiscale: fixed_t;
  pspritescalep: fixed_t;
  pspriteiscalep: fixed_t;
  pspriteyscale: fixed_t;

var
  mfloorclip: PSmallIntArray;
  mceilingclip: PSmallIntArray;
  spryscale: fixed_t;
  sprtopscreen: fixed_t;

// constant arrays
//  used for psprite clipping and initializing clipping
  negonearray: packed array[0..MAXWIDTH - 1] of smallint;
  screenheightarray: packed array[0..MAXWIDTH - 1] of smallint;

// variables used to look up
//  and range check thing_t sprites patches
  sprites: Pspritedef_tArray;
  numspritespresent: integer;

//==============================================================================
//
// R_ShutDownSprites
//
//==============================================================================
procedure R_ShutDownSprites;

const
  MINZ = FRACUNIT * 4;
  BASEYCENTER = 100;

//==============================================================================
//
// R_NewVisSprite
//
//==============================================================================
function R_NewVisSprite: Pvissprite_t;

//==============================================================================
//
// R_ProjectAdditionalThings
//
//==============================================================================
procedure R_ProjectAdditionalThings;

var
  spritelights: PBytePArray;

var
  clipbot: packed array[0..MAXWIDTH - 1] of smallint;
  cliptop: packed array[0..MAXWIDTH - 1] of smallint;

var
  vissprites: visspritebuffer_p;
  vissprite_p: integer;
  visspritessize: Integer = 0;

var
  domaskedzbuffer: boolean;

{$IFNDEF OPENGL}
const
  SPRITECACHESIZE = 8192 * 1024;

var
  spritecache: array[0..SPRITECACHESIZE - 1] of Byte;
  spritecachepos: integer;
  maskedpreparesignal: boolean;

const
  CACHE_OP_THICK = 1;
  CACHE_OP_MASKED = 2;
  CACHE_OP_THICK_AND_MASKED = 3;
  CACHE_OP_SIL1 = 4;
  CACHE_OP_SIL2 = 5;
  CACHE_OP_SIL3 = 6;

type
  visspritecacheitem_t = record
    ds: Pdrawseg_t;
    r1, r2: integer;
    operation: integer;
  end;
  Pvisspritecacheitem_t = ^visspritecacheitem_t;

  visspritecache_t = record
    fds_p: integer;
    fdrawsegs: Pdrawsegsbuffer_t;
    cachesize: integer;
    cache: array[0..0] of visspritecacheitem_t;
  end;
  Pvisspritecache_t = ^visspritecache_t;

//==============================================================================
//
// R_ExecuteSpriteCache
//
//==============================================================================
procedure R_ExecuteSpriteCache(const cache: Pvisspritecache_t);
{$ENDIF}

implementation

uses
  tables,
  {$IFDEF HEXEN}
  d_player,
  {$ENDIF}
  info_h,
  i_system,
{$IFDEF OPENGL}
  gl_render, // JVAL OPENGL
  gl_voxels,
  gl_models,
{$ENDIF}
  p_pspr,
  p_pspr_h,
  p_local,
  p_maputl,
  r_data,
  r_draw,
  r_main,
  p_setup,
{$IFNDEF OPENGL}
  r_sprite,
  r_segs,
  r_segs2,
  r_column,
  r_batchcolumn,
  r_trans8,
  r_hires,
  r_lights,
  r_bsp,
  r_voxels, // JVAL voxel support
  r_fake3d,
  r_3dfloors, // JVAL: 3d Floors
  r_depthbuffer, // JVAL: 3d Floors
  r_things_sortvissprites,
  r_dynlights,
  r_vislight,
  r_softlights,
  r_zbuffer,
{$ENDIF}
  r_camera,
  r_renderstyle,
  z_zone,
  w_sprite,
  w_wad,
  doomstat;

//
// Sprite rotation 0 is facing the viewer,
//  rotation 1 is one angle turn CLOCKWISE around the axis.
// This is not the same as the angle,
//  which increases counter clockwise (protractor).
// There was a lot of stuff grabbed wrong, so I changed it...
//

//
// INITIALIZATION FUNCTIONS
//
const
  MAXFRAMES = 29; // Maximun number of frames in sprite

var
  sprtemp: array[0..MAXFRAMES - 1] of spriteframe_t;
  maxframe: integer;
  spritename: string;

//==============================================================================
//
// R_InstallSpriteLump
// Local function for R_InitSprites.
//
//==============================================================================
procedure R_InstallSpriteLump(lump: integer;
  frame: LongWord; rotation: LongWord; flipped: boolean);
var
  r: integer;
begin
  if (frame >= MAXFRAMES) or (rotation > 32) then // JVAL: Up to 32 sprite rotations
    I_DevError('R_InstallSpriteLump(): Bad frame characters in lump %d (frame = %d, rotation = %d)'#13#10, [lump, frame, rotation]);

  if integer(frame) > maxframe then
    maxframe := frame;

  if rotation = 0 then
  begin
    // the lump should be used for all rotations
    if sprtemp[frame].rotate = 0 then
      I_DevWarning('R_InitSprites(): Sprite %s frame %s has multip rot=0 lump'#13#10,
        [spritename, Chr(Ord('A') + frame)]);

    if sprtemp[frame].rotate in [1..3] then // JVAL: Up to 32 sprite rotations
      I_DevWarning('R_InitSprites(): Sprite %s frame %s has rotations and a rot=0 lump'#13#10,
        [spritename, Chr(Ord('A') + frame)]);

    sprtemp[frame].rotate := 0;
    for r := 0 to 7 do
    begin
      sprtemp[frame].lump[r] := lump - firstspritelump;
      sprtemp[frame].flip[r] := flipped;
    end;
    exit;
  end;

  // the lump is only used for one rotation
  if sprtemp[frame].rotate = 0 then
    I_DevWarning('R_InitSprites(): Sprite %s frame %s has rotations and a rot=0 lump'#13#10,
      [spritename, Chr(Ord('A') + frame)]);

  // JVAL: Up to 32 sprite rotations
  if rotation in [1..8] then
    if sprtemp[frame].rotate = -1 then
      if sprtemp[frame].rotate < 1 then
        sprtemp[frame].rotate := 1;

  if rotation in [9..16] then
    if (sprtemp[frame].rotate = -1) or (sprtemp[frame].rotate = 1) then
      if sprtemp[frame].rotate < 2 then
        sprtemp[frame].rotate := 2;

  if rotation in [17..32] then
    if (sprtemp[frame].rotate = -1) or (sprtemp[frame].rotate = 1) or (sprtemp[frame].rotate = 2) then
      sprtemp[frame].rotate := 3;

  // make 0 based
  dec(rotation);
  if sprtemp[frame].lump[rotation] <> -1 then
    I_DevWarning('R_InitSprites(): Sprite %s : %s : %s has two lumps mapped to it'#13#10,
      [spritename, Chr(Ord('A') + frame), decide(rotation > 9, Chr(Ord('A') + rotation - 10), Chr(Ord('1') + rotation))]); // JVAL: Up to 32 sprite rotations

  sprtemp[frame].lump[rotation] := lump - firstspritelump;
  sprtemp[frame].flip[rotation] := flipped;
end;

//==============================================================================
//
// R_InitSpriteDefs
// Pass a null terminated list of sprite names
//  (4 chars exactly) to be used.
// Builds the sprite rotation matrixes to account
//  for horizontally flipped sprites.
// Will report an error if the lumps are inconsistant.
// Only called at startup.
//
// Sprite lump names are 4 characters for the actor,
//  a letter for the frame, and a number for the rotation.
// A sprite that is flippable will have an additional
//  letter/number appended.
// The rotation character can be 0 to signify no rotations.
//
//==============================================================================
procedure R_InitSpriteDefs(namelist: PIntegerArray);

  procedure sprtempreset;
  var
    i: integer;
    j: integer;
  begin
    for i := 0 to MAXFRAMES - 1 do
    begin
      sprtemp[i].rotate := -1;
      for j := 0 to 31 do // JVAL: Up to 32 sprite rotations
      begin
        sprtemp[i].lump[j] := -1;
        sprtemp[i].flip[j] := false;
      end;
    end;
  end;

  // JVAL: Up to 32 sprite rotations
  function rotationfromchar(const ch: char): integer;
  begin
    if ch in ['0'..'9'] then
    begin
      result := Ord(ch) - Ord('0');
      exit;
    end;
    if ch in ['A'..'W'] then
    begin
      result := Ord(ch) - Ord('A') + 10;
      exit;
    end;
    result := 33; // will trigger error
  end;

var
  i: integer;
  l: integer;
  intname: integer;
  frame: integer;
  rotation: integer;
  start: integer;
  finish: integer;
  patched: integer;
begin
  // count the number of sprite names

  numspritespresent := 0;
  while namelist[numspritespresent] <> 0 do
    inc(numspritespresent);

  if numspritespresent = 0 then
    exit;

  sprites := Z_Malloc(numspritespresent * SizeOf(spritedef_t), PU_STATIC, nil);
  ZeroMemory(sprites, numspritespresent * SizeOf(spritedef_t));

  start := firstspritelump - 1;
  finish := lastspritelump + 1;

  // scan all the lump names for each of the names,
  //  noting the highest frame letter.
  // Just compare 4 characters as ints
  for i := 0 to numspritespresent - 1 do
  begin
    spritename := Chr(namelist[i]) + Chr(namelist[i] shr 8) + Chr(namelist[i] shr 16) + Chr(namelist[i] shr 24);

    sprtempreset;

    maxframe := -1;
    intname := namelist[i];

    // scan the lumps,
    //  filling in the frames for whatever is found
    for l := start + 1 to finish - 1 do
    begin
      if spritepresent[l - firstspritelump] then

        if lumpinfo[l].v1 = intname then // JVAL
        begin
          frame := Ord(lumpinfo[l].name[4]) - Ord('A');
          rotation := rotationfromchar(lumpinfo[l].name[5]); // JVAL: Up to 32 sprite rotations

          if modifiedgame then
            patched := W_GetNumForName(lumpinfo[l].name)
          else
            patched := l;

          R_InstallSpriteLump(patched, frame, rotation, false);

          if lumpinfo[l].name[6] <> #0 then
          begin
            frame := Ord(lumpinfo[l].name[6]) - Ord('A');
            rotation := rotationfromchar(lumpinfo[l].name[7]); // JVAL: Up to 32 sprite rotations
            R_InstallSpriteLump(l, frame, rotation, true);
          end;
        end;
    end;

    // check the frames that were found for completeness
    if maxframe = -1 then
    begin
      sprites[i].numframes := 0;
      continue;
    end;

    inc(maxframe);

    for frame := 0 to maxframe - 1 do
    begin
      case sprtemp[frame].rotate of
        -1:
          begin
            // no rotations were found for that frame at all
            // JVAL: Changed from I_Error to I_Warning
            I_Warning('R_InitSprites(): No patches found for %s frame %s'#13#10,
              [spritename, Chr(frame + Ord('A'))]);
          end;
         0:
          begin
            // only the first rotation is needed
          end;
         1:
          begin
            // must have all 8 frames
            for rotation := 0 to 7 do
              if sprtemp[frame].lump[rotation] = -1 then
                I_Error('R_InitSprites(): Sprite %s frame %s is missing rotations',
                  [spritename, Chr(frame + Ord('A'))]);
          end;
         2: // JVAL: Up to 32 sprite rotations
          begin
            // must have all 16 frames
            for rotation := 0 to 15 do
              if sprtemp[frame].lump[rotation] = -1 then
                I_Error('R_InitSprites(): Sprite %s frame %s is missing rotations',
                  [spritename, Chr(frame + Ord('A'))]);
          end;
         3: // JVAL: Up to 32 sprite rotations
          begin
            // must have all 32 frames
            for rotation := 0 to 31 do
              if sprtemp[frame].lump[rotation] = -1 then
                I_Error('R_InitSprites(): Sprite %s frame %s is missing rotations',
                  [spritename, Chr(frame + Ord('A'))]);
          end;
      end;
    end;

    // allocate space for the frames present and copy sprtemp to it
    sprites[i].numframes := maxframe;
    sprites[i].spriteframes :=
      Z_Malloc(maxframe * SizeOf(spriteframe_t), PU_STATIC, nil);
    memcpy(sprites[i].spriteframes, @sprtemp, maxframe * SizeOf(spriteframe_t));
  end;
end;

var
  negosize: Integer = 0;

//==============================================================================
//
// GAME FUNCTIONS
//
// R_InitNegoArray
//
//==============================================================================
procedure R_InitNegoArray;
var
  i: integer;
begin
  if negosize < SCREENWIDTH then
  begin
    for i := negosize to SCREENWIDTH - 1 do
      negonearray[i] := -1;
    negosize := SCREENWIDTH;
  end;
end;

var
  funny_rotations: TDStringList;
  wrong_frames: TDStringList;

//==============================================================================
//
// R_InitSprites
// Called at program start.
//
//==============================================================================
procedure R_InitSprites(namelist: PIntegerArray);
begin
  R_InitNegoArray;

  funny_rotations := TDStringList.Create;
  wrong_frames := TDStringList.Create;
  R_InitSpriteDefs(namelist);
{$IFNDEF OPENGL}
  R_InitSpriteSort;
{$ENDIF}
end;

//==============================================================================
//
// R_ClearSprites
// Called at frame start.
//
//==============================================================================
procedure R_ClearSprites;
begin
  vissprite_p := 0;
end;

//==============================================================================
//
// R_NewVisSprite
//
// JVAL Now we allocate a new visprite dynamically
//
//==============================================================================
function R_NewVisSprite: Pvissprite_t;
begin
  if vissprite_p = visspritessize then
  begin
    realloc(pointer(vissprites), visspritessize * SizeOf(Pvissprite_t), (128 +
      visspritessize) * SizeOf(Pvissprite_t));
    visspritessize := visspritessize + 128;
  end;
  if vissprite_p > maxvissprite then
  begin
    maxvissprite := vissprite_p;
    vissprites[vissprite_p] := Z_Malloc(SizeOf(vissprite_t), PU_LEVEL, nil);
  end;
  result := vissprites[vissprite_p];
  inc(vissprite_p);
end;

//==============================================================================
//
// R_ShutDownSprites
//
//==============================================================================
procedure R_ShutDownSprites;
begin
  realloc(pointer(vissprites), visspritessize * SizeOf(Pvissprite_t), 0);
{$IFNDEF OPENGL}
  R_ShutDownSpriteSort;
{$ENDIF}
  funny_rotations.Free;
  wrong_frames.Free;
end;

{$IFNDEF OPENGL}

//==============================================================================
//
// R_MaskedAdjustY
//
//==============================================================================
procedure R_MaskedAdjustY(var yl: integer; const yh: integer);
var
  testfrac: fixed_t;
begin
  testfrac := dc_texturemid + (yl - centery) * dc_iscale;
  while true do
  begin
    if testfrac >= 0 then
      exit;
    inc(yl);
    if yl > yh then
      exit;
    testfrac := testfrac + dc_iscale;
  end;
end;

//
// R_DrawMaskedColumn
// Used for sprites and masked mid textures.
// Masked means: partly transparent, i.e. stored
//  in posts/runs of opaque pixels.
//
type
  R_DrawMaskedColumn_t = procedure (column: Pcolumn_t; const depthscale: fixed_t; const mo: Pmobj_t = nil; baseclip: integer = -1; const renderflags: LongWord = 0);

//==============================================================================
//
// R_DrawMaskedColumn
//
//==============================================================================
procedure R_DrawMaskedColumn(column: Pcolumn_t; const depthscale: fixed_t; const mo: Pmobj_t = nil;
  baseclip: integer = -1; const renderflags: LongWord = 0);
var
  topscreen: int64;
  bottomscreen: int64;
  basetexturemid: fixed_t;
  fc_x, cc_x: integer;
  delta, prevdelta: integer;
  tallpatch: boolean;
  dodepthbuffer: boolean;
begin
  basetexturemid := dc_texturemid;

  fc_x := mfloorclip[dc_x];
  cc_x := mceilingclip[dc_x];

  delta := 0;
  tallpatch := false;

  if baseclip = -1 then
    baseclip := viewheight - 1;

  dodepthbuffer := depthbufferactive;
  while column.topdelta <> $ff do
  begin
    // calculate unclipped screen coordinates
    // for post
    delta := delta + column.topdelta;
    topscreen := sprtopscreen + int64(spryscale * delta);
    bottomscreen := topscreen + int64(spryscale * column.length);

    dc_yl := FixedInt64(topscreen + (FRACUNIT - 1));
    if dc_yl >= fc_x then
      break;
    if dc_yl <= cc_x then
      dc_yl := cc_x + 1;

    dc_yh := FixedInt64(bottomscreen - 1);
    if dc_yh >= fc_x then
      dc_yh := fc_x - 1;
    if dc_yh >= baseclip then
      dc_yh := baseclip;

    if dc_yl <= dc_yh then
    begin
      dc_source := PByteArray(integer(column) + 3);
      dc_texturemid := basetexturemid - (delta * FRACUNIT);
      R_MaskedAdjustY(dc_yl, dc_yh);
      // Drawn by either R_DrawColumn
      //  or (SHADOW) R_DrawFuzzColumn
      //  or R_DrawColumnAverage
      //  or R_DrawTranslatedColumn
      if dodepthbuffer then // JVAL: 3d Floors
      begin
        if renderflags and VSF_TRANSPARENCY <> 0 then
          R_DrawColumnWithDepthBufferCheckOnly(colfunc, depthscale) // JVAL: 3d Floors
        else
          R_DrawColumnWithDepthBufferCheckWrite(colfunc, depthscale) // JVAL: 3d Floors
      end
      else
        colfunc;

      if domaskedzbuffer then
        if renderflags and VSF_TRANSPARENCY = 0 then
          R_DrawMaskedColumnToZBuffer(mo, depthscale);
    end;
    if not tallpatch then
    begin
      prevdelta := column.topdelta;
      column := Pcolumn_t(integer(column) + column.length + 4);
      if column.topdelta > prevdelta then
        delta := 0
      else
        tallpatch := true;
    end
    else
      column := Pcolumn_t(integer(column) + column.length + 4);
  end;
  dc_texturemid := basetexturemid;
end;

//==============================================================================
// R_DrawMaskedColumn2
//
// For Walls only
//
//==============================================================================
procedure R_DrawMaskedColumn2(const mc2h: integer; const depthscale: fixed_t); // Use dc_source32
var
  topscreen: int64;
  bottomscreen: int64;
  basetexturemid: fixed_t;
  fc_x, cc_x: integer;
begin
  basetexturemid := dc_texturemid;

  fc_x := mfloorclip[dc_x];
  cc_x := mceilingclip[dc_x];

  topscreen := sprtopscreen;
  bottomscreen := topscreen + int64(spryscale * mc2h);

  dc_yl := FixedInt64(topscreen + (FRACUNIT - 1));
  if dc_yl <= cc_x then
    dc_yl := cc_x + 1;

  dc_yh := FixedInt64(bottomscreen - 1);
  if dc_yh >= fc_x then
    dc_yh := fc_x - 1;

  if dc_yl <= dc_yh then
  begin
    // Drawn by either R_DrawColumn
    //  or (SHADOW) R_DrawFuzzColumn
    //  or R_DrawColumnAverage
    //  or R_DrawTranslatedColumn
    if depthbufferactive then                   // JVAL: 3d Floors
      R_DrawColumnWithDepthBufferCheckWrite(colfunc) // JVAL: 3d Floors
    else
      colfunc;

    if domaskedzbuffer then
      R_DrawMaskedColumnToZBuffer(nil, depthscale);
  end;

  dc_texturemid := basetexturemid;
end;

// JVAL: batch column drawing
type
  DrawMaskedColumn_Batch_t = procedure (column: Pcolumn_t; mo: Pmobj_t; const depthscale: integer; baseclip: integer = -1; const renderflags: LongWord = 0);

//==============================================================================
//
// R_DrawMaskedColumn_Batch
//
//==============================================================================
procedure R_DrawMaskedColumn_Batch(column: Pcolumn_t; mo: Pmobj_t; const depthscale: integer; baseclip: integer = -1; const renderflags: LongWord = 0);
var
  topscreen: int64;
  bottomscreen: int64;
  basetexturemid: fixed_t;
  fc_x, cc_x: integer;
  delta, prevdelta: integer;
  tallpatch: boolean;
begin
  basetexturemid := dc_texturemid;

  fc_x := mfloorclip[dc_x];
  cc_x := mceilingclip[dc_x];

  delta := 0;
  tallpatch := false;

  if baseclip = -1 then
    baseclip := viewheight - 1;

  while column.topdelta <> $ff do
  begin
    // calculate unclipped screen coordinates
    // for post
    delta := delta + column.topdelta;
    topscreen := sprtopscreen + int64(spryscale * delta);
    bottomscreen := topscreen + int64(spryscale * column.length);

    dc_yl := FixedInt64(topscreen + (FRACUNIT - 1));
    if dc_yl >= fc_x then
      break;
    if dc_yl <= cc_x then
      dc_yl := cc_x + 1;

    dc_yh := FixedInt64(bottomscreen - 1);
    if dc_yh >= fc_x then
      dc_yh := fc_x - 1;
    if dc_yh >= baseclip then
      dc_yh := baseclip;

    if dc_yl <= dc_yh then
    begin
      dc_source := PByteArray(integer(column) + 3);
      dc_texturemid := basetexturemid - (delta * FRACUNIT);
      R_MaskedAdjustY(dc_yl, dc_yh);
      // Drawn by either R_DrawColumn
      //  or (SHADOW) R_DrawFuzzColumn
      //  or R_DrawColumnAverage
      //  or R_DrawTranslatedColumn
      batchcolfunc;

      if domaskedzbuffer then
        if renderflags and VSF_TRANSPARENCY = 0 then
          R_DrawBatchMaskedColumnToZBuffer(mo, depthscale);
    end;
    if not tallpatch then
    begin
      prevdelta := column.topdelta;
      column := Pcolumn_t(integer(column) + column.length + 4);
      if column.topdelta > prevdelta then
        delta := 0
      else
        tallpatch := true;
    end
    else
      column := Pcolumn_t(integer(column) + column.length + 4);
  end;

  dc_texturemid := basetexturemid;
end;

var
  spritefunc_mt: spritefunc_t;
  batchspritefunc_mt: spritefunc_t;

//==============================================================================
//
// R_FillSpriteInfo_MT
//
//==============================================================================
procedure R_FillSpriteInfo_MT(const parms: Pspriterenderinfo_t);
begin
  parms.dc_x := dc_x;
  parms.dc_yh := dc_yh;
  parms.dc_yl := dc_yl;
  parms.dc_iscale := dc_iscale;
  parms.dc_texturemid := dc_texturemid;
  parms.dc_source := dc_source;
  parms.dc_alpha := dc_alpha;
  parms.dc_fog := dc_fog; // JVAL: Mars fog sectors
  parms.num_batch_columns := num_batch_columns;
  parms.dc_colormap := dc_colormap;
  parms.dc_colormap32 := dc_colormap32;
  parms.proc := spritefunc_mt;
end;

//==============================================================================
//
// R_FillSpriteInfo_BatchMT
//
//==============================================================================
procedure R_FillSpriteInfo_BatchMT(const parms: Pspriterenderinfo_t);
begin
  parms.dc_x := dc_x;
  parms.dc_yh := dc_yh;
  parms.dc_yl := dc_yl;
  parms.dc_iscale := dc_iscale;
  parms.dc_texturemid := dc_texturemid;
  parms.dc_source := dc_source;
  parms.dc_alpha := dc_alpha;
  parms.dc_fog := dc_fog; // JVAL: Mars fog sectors
  parms.num_batch_columns := num_batch_columns;
  parms.dc_colormap := dc_colormap;
  parms.dc_colormap32 := dc_colormap32;
  parms.proc := batchspritefunc_mt;
end;

//==============================================================================
//
// R_DrawMaskedColumn_BatchMT
//
//==============================================================================
procedure R_DrawMaskedColumn_BatchMT(column: Pcolumn_t; mo: Pmobj_t; const depthscale: integer; baseclip: integer = -1; const renderflags: LongWord = 0);
var
  topscreen: int64;
  bottomscreen: int64;
  basetexturemid: fixed_t;
  fc_x, cc_x: integer;
  delta, prevdelta: integer;
  tallpatch: boolean;
begin
  basetexturemid := dc_texturemid;

  fc_x := mfloorclip[dc_x];
  cc_x := mceilingclip[dc_x];

  delta := 0;
  tallpatch := false;

  if baseclip = -1 then
    baseclip := viewheight - 1;

  while column.topdelta <> $ff do
  begin
    // calculate unclipped screen coordinates
    // for post
    delta := delta + column.topdelta;
    topscreen := sprtopscreen + int64(spryscale * delta);
    bottomscreen := topscreen + int64(spryscale * column.length);

    dc_yl := FixedInt64(topscreen + (FRACUNIT - 1));
    if dc_yl >= fc_x then
      break;
    if dc_yl <= cc_x then
      dc_yl := cc_x + 1;

    dc_yh := FixedInt64(bottomscreen - 1);
    if dc_yh >= fc_x then
      dc_yh := fc_x - 1;
    if dc_yh >= baseclip then
      dc_yh := baseclip;

    if dc_yl <= dc_yh then
    begin
      dc_source := PByteArray(integer(column) + 3);
      dc_texturemid := basetexturemid - (delta * FRACUNIT);
      R_MaskedAdjustY(dc_yl, dc_yh);

      R_FillSpriteInfo_BatchMT(R_SpriteAddMTInfo);

      if domaskedzbuffer then
        if renderflags and VSF_TRANSPARENCY = 0 then
          R_DrawBatchMaskedColumnToZBuffer(mo, depthscale);
    end;
    if not tallpatch then
    begin
      prevdelta := column.topdelta;
      column := Pcolumn_t(integer(column) + column.length + 4);
      if column.topdelta > prevdelta then
        delta := 0
      else
        tallpatch := true;
    end
    else
      column := Pcolumn_t(integer(column) + column.length + 4);
  end;

  dc_texturemid := basetexturemid;
end;

procedure _add_sprite_task;
begin
  R_FillSpriteInfo_MT(R_SpriteAddMTInfo);
end;

//==============================================================================
//
// R_DrawMaskedColumnMT
//
//==============================================================================
procedure R_DrawMaskedColumnMT(column: Pcolumn_t; const depthscale: fixed_t; mo: Pmobj_t;
  baseclip: integer = -1; const renderflags: LongWord = 0);
var
  topscreen: int64;
  bottomscreen: int64;
  basetexturemid: fixed_t;
  fc_x, cc_x: integer;
  delta, prevdelta: integer;
  tallpatch: boolean;
  dodepthbuffer: boolean;
begin
  basetexturemid := dc_texturemid;

  fc_x := mfloorclip[dc_x];
  cc_x := mceilingclip[dc_x];

  delta := 0;
  tallpatch := false;

  if baseclip = -1 then
    baseclip := viewheight - 1;

  dodepthbuffer := depthbufferactive;
  while column.topdelta <> $ff do
  begin
    // calculate unclipped screen coordinates
    // for post
    delta := delta + column.topdelta;
    topscreen := sprtopscreen + int64(spryscale * delta);
    bottomscreen := topscreen + int64(spryscale * column.length);

    dc_yl := FixedInt64(topscreen + (FRACUNIT - 1));
    if dc_yl >= fc_x then
      break;
    if dc_yl <= cc_x then
      dc_yl := cc_x + 1;

    dc_yh := FixedInt64(bottomscreen - 1);
    if dc_yh >= fc_x then
      dc_yh := fc_x - 1;
    if dc_yh >= baseclip then
      dc_yh := baseclip;

    if dc_yl <= dc_yh then
    begin
      dc_source := PByteArray(integer(column) + 3);
      dc_texturemid := basetexturemid - (delta * FRACUNIT);
      R_MaskedAdjustY(dc_yl, dc_yh);
      // Drawn by either R_DrawColumn
      //  or (SHADOW) R_DrawFuzzColumn
      //  or R_DrawColumnAverage
      //  or R_DrawTranslatedColumn
      if dodepthbuffer then // JVAL: 3d Floors
      begin
        if renderflags and VSF_TRANSPARENCY <> 0 then
          R_DrawColumnWithDepthBufferCheckOnly(colfunc, depthscale) // JVAL: 3d Floors
        else
          R_DrawColumnWithDepthBufferCheckWrite(colfunc, depthscale) // JVAL: 3d Floors
      end
      else
        R_FillSpriteInfo_MT(R_SpriteAddMTInfo);

      if domaskedzbuffer then
        if renderflags and VSF_TRANSPARENCY = 0 then
          R_DrawMaskedColumnToZBuffer(mo, depthscale);
    end;
    if not tallpatch then
    begin
      prevdelta := column.topdelta;
      column := Pcolumn_t(integer(column) + column.length + 4);
      if column.topdelta > prevdelta then
        delta := 0
      else
        tallpatch := true;
    end
    else
      column := Pcolumn_t(integer(column) + column.length + 4);
  end;

  dc_texturemid := basetexturemid;
end;

// R_DrawVisSprite
//  mfloorclip and mceilingclip should also be set.
//
const
  MIN_SPRITE_SIZE_MT = 64;

//==============================================================================
//
// R_DrawVisSprite
//
//==============================================================================
procedure R_DrawVisSprite(vis: Pvissprite_t; const playerweapon: boolean = false);
var
  column: Pcolumn_t;
  texturecolumn: integer;
  checkcolumn: integer;
  vismo: Pmobj_t;
  renderflags: LongWord;
  frac: fixed_t;
  patch: Ppatch_t;
  xiscale: integer;
  baseclip: fixed_t;
  last_dc_x: integer;
  last_texturecolumn: integer;
  last_fc_x: smallint;
  last_cc_x: smallint;
  save_dc_x: integer;
  dmcproc: R_DrawMaskedColumn_t;
  dmcproc_batch: DrawMaskedColumn_Batch_t;
  do_mt: boolean;
  dbscale: fixed_t;
begin
  vismo := vis.mo;
  renderflags := vis.renderflags;
  if playerweapon then
    patch := W_CacheSpriteNum(vis.patch + firstspritelump, nil, PU_STATIC) // JVAL: Images as sprites
  else
    patch := W_CacheSpriteNum(vis.patch + firstspritelump, vismo.translationtable, PU_STATIC); // JVAL: Images as sprites

  dc_colormap := vis.colormap;

  spritefunc_mt := nil;
  batchspritefunc_mt := nil;

  if videomode = vm32bit then
  begin
    dc_colormap32 := R_GetColormap32(dc_colormap);
    if fixedcolormapnum = INVERSECOLORMAP then
      dc_lightlevel := -1
    else
      dc_lightlevel := R_GetColormapLightLevel(dc_colormap);
  end;

  if dc_colormap = nil then
  begin
    // NULL colormap = shadow draw
  {$IFDEF DOOM_OR_HERETIC}
    colfunc := fuzzcolfunc;
    batchcolfunc := batchfuzzcolfunc;
  {$ENDIF}
  {$IFDEF HEXEN}
    colfunc := fuzzcolfunc;
    batchcolfunc := nil;
  end
  else if vis.mobjflags and (MF_SHADOW or MF_ALTSHADOW) <> 0 then
  begin
    // NULL colormap = shadow draw
    colfunc := fuzzcolfunc;
    batchcolfunc := nil;
  {$ENDIF}
  {$IFDEF STRIFE}
    // NULL colormap = shadow draw
    colfunc := fuzzcolfunc1;
    batchcolfunc := batchfuzzcolfunc1;
  end
  else if vis.mobjflags and MF_SHADOW <> 0 then
  begin
    if vis.mobjflags and MF_TRANSLATION = 0 then
    begin
      if vis.mobjflags and MF_MVIS <> 0 then
      begin
        colfunc := fuzzcolfunc2;
        batchcolfunc := batchfuzzcolfunc2;
      end
      else
      begin
        colfunc := fuzzcolfunc1;
        batchcolfunc := batchfuzzcolfunc1;
      end;
    end
    else
    begin
      dc_translation := PByteArray(integer(translationtables) - 256 +
        (_SHR((vis.mobjflags and MF_TRANSLATION), (MF_TRANSSHIFT - 8))));
      colfunc := fuzztranscolfunc;
      batchcolfunc := batchfuzztranscolfunc;
    end;
  end
  else if vis.mobjflags and MF_MVIS <> 0 then
  begin
    colfunc := fuzzcolfunc2;
    batchcolfunc := batchfuzzcolfunc2;
  {$ENDIF}
  end
  else if vis.mobjflags and MF_TRANSLATION <> 0 then
  begin
    colfunc := transcolfunc;
    batchcolfunc := batchtranscolfunc;
    dc_translation := PByteArray(integer(translationtables) - 256 +
    {$IFDEF HEXEN}vis._class * ((MAXPLAYERS - 1) * 256) + {$ENDIF}
      (_SHR((vis.mobjflags and MF_TRANSLATION), (MF_TRANSSHIFT - 8))));
  end
  else if usetransparentsprites and (vismo <> nil) and (vismo.renderstyle = mrs_translucent) then
  begin
    dc_alpha := vismo.alpha;
    {$IFDEF DOOM_OR_STRIFE}
    curtrans8table := R_GetTransparency8table(dc_alpha);
    {$ENDIF}
    colfunc := alphacolfunc;
    spritefunc_mt := alphacolfunc_mt;
    batchcolfunc := batchtalphacolfunc;
    batchspritefunc_mt := batchtalphacolfunc_mt;
  end
  else if usetransparentsprites and (vismo <> nil) and (vismo.renderstyle = mrs_add) then
  begin
    dc_alpha := vismo.alpha;
    curadd8table := R_GetAdditive8table(dc_alpha);
    if vis.scale > FRACUNIT then
    begin
      colfunc := addcolfunc_smallstep;
      spritefunc_mt := addcolfunc_smallstep_mt;
    end
    else
    begin
      colfunc := addcolfunc;
      spritefunc_mt := addcolfunc_mt;
    end;
    batchcolfunc := batchaddcolfunc;
    batchspritefunc_mt := batchaddcolfunc_mt;
  end
  else if usetransparentsprites and (vismo <> nil) and (vismo.renderstyle = mrs_subtract) then
  begin
    dc_alpha := vismo.alpha;
    cursubtract8table := R_GetSubtractive8table(dc_alpha);
    colfunc := subtractcolfunc;
    spritefunc_mt := subtractcolfunc_mt;
    batchcolfunc := batchsubtractcolfunc;
    batchspritefunc_mt := batchsubtractcolfunc_mt;
  end
  else if usetransparentsprites and (vis.mobjflags_ex and MF_EX_TRANSPARENT <> 0) then
  begin
    colfunc := averagecolfunc;
    spritefunc_mt := averagecolfunc_mt;
    batchcolfunc := batchtaveragecolfunc;
    batchspritefunc_mt := batchtaveragecolfunc_mt;
  end
  else
  begin
    colfunc := maskedcolfunc;
    spritefunc_mt := maskedcolfunc_mt;
    batchcolfunc := basebatchcolfunc;
    batchspritefunc_mt := basebatchcolfunc_mt;
  end;

  dc_iscale := FixedDivEx(FRACUNIT, vis.scale);
  if vis.infoscale = FRACUNIT then
    dbscale := vis.scale
  else
    dbscale := trunc((FRACUNIT / dc_iscale) * (FRACUNIT / vis.infoscale) * FRACUNIT);

  dc_fog := vis.fog; // JVAL: Mars fog sectors

  dc_texturemid := vis.texturemid;
  frac := vis.startfrac;
  spryscale := vis.scale;
  sprtopscreen := centeryfrac - FixedMul(dc_texturemid, spryscale);

  if (vis.footclip <> 0) and not playerweapon then
    baseclip := FixedInt((sprtopscreen + FixedMul(patch.height * FRACUNIT, spryscale) - FixedMul(vis.footclip, spryscale)))
  else
    baseclip := viewheight - 1;

  xiscale := vis.xiscale;
  dc_x := vis.x1;

  do_mt := usemultithread and (vis.x2 - vis.x1 > MIN_SPRITE_SIZE_MT);

  if do_mt and Assigned(spritefunc_mt) then
    dmcproc := @R_DrawMaskedColumnMT
  else
    dmcproc := @R_DrawMaskedColumn;

  if depthbufferactive or (xiscale > FRACUNIT div 2) or (xiscale < -FRACUNIT div 2) or not optimizedthingsrendering or not Assigned(batchcolfunc) then
  begin
    while dc_x <= vis.x2 do
    begin
      texturecolumn := LongWord(frac) shr FRACBITS;

      column := Pcolumn_t(integer(patch) + patch.columnofs[texturecolumn]);
      dmcproc(column, dbscale, vismo, baseclip, renderflags);
      frac := frac + xiscale;
      inc(dc_x);
    end;
    R_SpriteRenderMT;
  end
  else
  begin // JVAL: batch column drawing
    if do_mt and Assigned(batchspritefunc_mt) then
      dmcproc_batch := @R_DrawMaskedColumn_BatchMT
    else
      dmcproc_batch := @R_DrawMaskedColumn_Batch;
    last_dc_x := dc_x;
    last_texturecolumn := LongWord(frac) shr FRACBITS;
    last_fc_x := mfloorclip[last_dc_x];
    last_cc_x := mceilingclip[last_dc_x];
    while dc_x <= vis.x2 do
    begin
      checkcolumn := LongWord(frac) shr FRACBITS;
      if (last_fc_x <> mfloorclip[dc_x]) or
         (last_cc_x <> mceilingclip[dc_x]) or
         (last_texturecolumn <> checkcolumn) then
      begin
        num_batch_columns := dc_x - last_dc_x;
        texturecolumn := last_texturecolumn;
        last_texturecolumn := checkcolumn;
        last_fc_x := mfloorclip[dc_x];
        last_cc_x := mceilingclip[dc_x];
        save_dc_x := last_dc_x;
        last_dc_x := dc_x;
        column := Pcolumn_t(integer(patch) + patch.columnofs[texturecolumn]);
        dc_x := save_dc_x;
        if num_batch_columns > 1 then
          dmcproc_batch(column, vismo, dbscale, baseclip, renderflags)
        else
          dmcproc(column, dbscale, vismo, baseclip, renderflags);
        dc_x := last_dc_x;
      end;
      frac := frac + xiscale;
      inc(dc_x);
    end;
    num_batch_columns := dc_x - last_dc_x;
    if num_batch_columns > 0 then
    begin
      column := Pcolumn_t(integer(patch) + patch.columnofs[last_texturecolumn]);
      dc_x := last_dc_x;
      if num_batch_columns > 1 then
        dmcproc_batch(column, vismo, dbscale, baseclip, renderflags)
      else
        dmcproc(column, dbscale, vismo, baseclip, renderflags);
    end;
    R_SpriteRenderMT;
  end;

  Z_ChangeTag(patch, PU_CACHE);
end;

//==============================================================================
//
// R_DrawVisSpriteLight
// Used for sprites that emit light
//
//==============================================================================
procedure R_DrawVisSpriteLight(vis: Pvissprite_t; x1: integer; x2: integer);
var
  texturecolumn: integer;
  frac: fixed_t;
  fracstep: fixed_t;
  patch: Ppatch_t;
  topscreen: int64;
  bottomscreen: int64;
  basetexturemid: fixed_t;
  last_dc_x: integer;
  save_dc_x: integer;
  last_texturecolumn: integer;
  last_floorclip, last_ceilingclip: SmallInt;
  checkcolumn: integer;
  ltopdelta: integer;
  llength: integer;
begin
  patch := W_CacheSpriteNum(vis.patch + firstspritelump, nil, PU_STATIC); // JVAL: Images as sprites

  frac := vis.startfrac * LIGHTBOOSTSIZE div patch.width;
  fracstep := vis.xiscale * (LIGHTBOOSTSIZE shr 1) div patch.width;
  spryscale := vis.scale * patch.height div (LIGHTBOOSTSIZE shr 1);
  dc_iscale := FixedDivEx(FRACUNIT, spryscale);
  sprtopscreen := centeryfrac - FixedMul(vis.texturemid2, vis.scale);

  if fixedcolormapnum = INVERSECOLORMAP then
  // JVAL: if in invulnerability mode use white color
  begin
    lightcolfunc := whitelightcolfunc;
    batchlightcolfunc := batchwhitelightcolfunc;
  end
  else if vis.fog then  // JVAL: Mars fog sectors
  begin
    lightcolfunc := whitelightcolfunc;
    batchlightcolfunc := batchwhitelightcolfunc;
  end
  else if vis.mobjflags_ex and MF_EX_LIGHT <> 0 then
  begin
    if vis.mobjflags_ex and MF_EX_REDLIGHT <> 0 then
    begin
      lightcolfunc := redlightcolfunc;
      batchlightcolfunc := batchredlightcolfunc;
    end
    else if vis.mobjflags_ex and MF_EX_GREENLIGHT <> 0 then
    begin
      lightcolfunc := greenlightcolfunc;
      batchlightcolfunc := batchgreenlightcolfunc;
    end
    else if vis.mobjflags_ex and MF_EX_BLUELIGHT <> 0 then
    begin
      lightcolfunc := bluelightcolfunc;
      batchlightcolfunc := batchbluelightcolfunc;
    end
    else if vis.mobjflags_ex and MF_EX_YELLOWLIGHT <> 0 then
    begin
      lightcolfunc := yellowlightcolfunc;
      batchlightcolfunc := batchyellowlightcolfunc;
    end
    else
    begin
      lightcolfunc := whitelightcolfunc;
      batchlightcolfunc := batchwhitelightcolfunc;
    end;
  end
  else
  begin
    lightcolfunc := whitelightcolfunc;
    batchlightcolfunc := batchwhitelightcolfunc;
  end;

  dc_x := x1;
  if depthbufferactive or (fracstep > FRACUNIT div 2) or not optimizedthingsrendering or not Assigned(batchlightcolfunc) then
  begin
    while dc_x <= x2 do
    begin
      texturecolumn := LongWord(frac) shr FRACBITS;
      if LongWord(texturecolumn) < LIGHTBOOSTSIZE then
      begin
        ltopdelta := lighboostlookup[texturecolumn].topdelta;
        llength := lighboostlookup[texturecolumn].length;
        dc_source32 := @lightboost[texturecolumn * LIGHTBOOSTSIZE + ltopdelta];

        basetexturemid := dc_texturemid;

        topscreen := sprtopscreen + int64(spryscale * ltopdelta);
        bottomscreen := topscreen + int64(spryscale * llength);

        dc_yl := FixedInt64(topscreen + (FRACUNIT - 1));
        dc_texturemid := (centery - dc_yl) * dc_iscale;
        if dc_yl <= mceilingclip[dc_x] then
          dc_yl := mceilingclip[dc_x] + 1;

        dc_yh := FixedInt64(bottomscreen - 1);
        if dc_yh >= mfloorclip[dc_x] then
          dc_yh := mfloorclip[dc_x] - 1;

        if frac < 256 * FRACUNIT then
          if dc_yl <= dc_yh then
            if depthbufferactive then                             // JVAL: 3d Floors
              R_DrawColumnWithDepthBufferCheckOnly(lightcolfunc)  // JVAL: 3d Floors
            else
              lightcolfunc;

        dc_texturemid := basetexturemid;
      end;

      frac := frac + fracstep;
      inc(dc_x);
    end;
  end
  else
  begin
    last_dc_x := dc_x;
    last_texturecolumn := LongWord(frac) shr FRACBITS;
    last_floorclip := mfloorclip[dc_x];
    last_ceilingclip := mceilingclip[dc_x];
    while dc_x <= x2 do
    begin
      checkcolumn := LongWord(frac) shr FRACBITS;
      if (last_floorclip <> mfloorclip[dc_x]) or
         (last_ceilingclip <> mceilingclip[dc_x]) or
         (last_texturecolumn <> checkcolumn) or
         (dc_x = x2) then
      begin
        num_batch_columns := dc_x - last_dc_x;
        texturecolumn := last_texturecolumn;
        last_texturecolumn := checkcolumn;
        last_floorclip := mfloorclip[dc_x];
        last_ceilingclip := mceilingclip[dc_x];
        save_dc_x := last_dc_x;
        last_dc_x := dc_x;

        if LongWord(texturecolumn) < LIGHTBOOSTSIZE then
        begin
          ltopdelta := lighboostlookup[texturecolumn].topdelta;
          llength := lighboostlookup[texturecolumn].length;
          dc_source32 := @lightboost[texturecolumn * LIGHTBOOSTSIZE + ltopdelta];

          basetexturemid := dc_texturemid;

          topscreen := sprtopscreen + int64(spryscale * ltopdelta);
          bottomscreen := topscreen + int64(spryscale * llength);

          dc_yl := FixedInt64(topscreen + (FRACUNIT - 1));
          dc_texturemid := (centery - dc_yl) * dc_iscale;
          if dc_yl <= mceilingclip[dc_x] then
            dc_yl := mceilingclip[dc_x] + 1;

          dc_yh := FixedInt64(bottomscreen - 1);
          if dc_yh >= mfloorclip[dc_x] then
            dc_yh := mfloorclip[dc_x] - 1;

          if frac < 256 * FRACUNIT then
            if dc_yl <= dc_yh then
            begin
              dc_x := save_dc_x;
              if num_batch_columns > 1 then
                batchlightcolfunc
              else
                lightcolfunc;
              dc_x := last_dc_x;
            end;

          dc_texturemid := basetexturemid;
        end;

      end;

      frac := frac + fracstep;
      inc(dc_x);
    end;
  end;

  Z_ChangeTag(patch, PU_CACHE);
end;
{$ENDIF}

// JVAL: Up to 32 sprite rotations
const
  translaterotations: array[1..3] of array [0..31] of integer = (
    ( 0,  1,  2,  3,  4,  5,  6,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    ( 0,  8,  1,  9,  2, 10,  3, 11,  4, 12,  5, 13,  6, 14,  7, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    ( 0, 16,  8, 17,  1, 18,  9, 19,  2, 20, 10, 21,  3, 22, 11, 23,  4, 24, 12, 25,  5, 26, 13, 27,  6, 28, 14, 29,  7, 30, 15, 31)
  );

//==============================================================================
//
// R_ProjectSprite
// Generates a vissprite for a thing
//  if it might be visible.
//
//==============================================================================
procedure R_ProjectSprite(thing: Pmobj_t);
var
  tr_x: fixed_t;
  tr_y: fixed_t;
  gxt: fixed_t;
  gyt: fixed_t;
  tx: fixed_t;
  tz: fixed_t;
  xscale: fixed_t;
  x1: integer;
  x2: integer;
  sprdef: Pspritedef_t;
  sprframe: Pspriteframe_t;
  lump: integer;
  rot: LongWord;
  flip: boolean;
  vis: Pvissprite_t;
  ang: angle_t;
  sec: Psector_t;
{$IFDEF OPENGL}
  checksides: boolean;
  modelflag: integer;
{$ELSE}
  index: integer;
  iscale: fixed_t;
  {$IFDEF DOOM_OR_STRIFE}
  heightsec: integer;
  plheightsec: integer;
  {$ENDIF}
  gzt: integer;
  voxelflag: integer;
  vx1, vx2: integer;
  mid: Psector_t; // JVAL: 3d floors
  midn: integer;  // JVAL: 3d floors
  sprlights: PBytePArray; // JVAL: 3d floors
  scaledtop: fixed_t;
  vr3: fixed_t;
{$ENDIF}
  soffset, swidth: fixed_t;
  infoscale: fixed_t;
  frm: integer;
begin
  if (thing.player = viewplayer) and not chasecamera then
    exit;

  if thing.rendervalidcount = rendervalidcount then
    exit;

  thing.rendervalidcount := rendervalidcount;

  // Never make a vissprite when MF2_DONTDRAW is flagged.
  {$IFDEF HERETIC_OR_HEXEN}
  if thing.flags2 and MF2_DONTDRAW <> 0 then
  {$ELSE}
  if thing.flags2_ex and MF2_EX_DONTDRAW <> 0 then
  {$ENDIF}
    exit;

  // transform the origin point
  tr_x := thing.x - viewx;
  tr_y := thing.y - viewy;

  gxt := FixedMul(tr_x, viewcos);
  gyt := -FixedMul(tr_y, viewsin);

  tz := gxt - gyt;

{$IFNDEF OPENGL}
  // JVAL voxel support
  voxelflag := 0;
  if r_drawvoxels then
    if thing.state.voxels <> nil then
      voxelflag := 1;
{$ENDIF}

  // thing is behind view plane?
{$IFDEF OPENGL}
  if ((thing.state.voxels <> nil) and gl_drawvoxels) or ((thing.state.models <> nil) and gl_drawmodels) then
    modelflag := 1
  else
    modelflag := 0;
  checksides := (absviewpitch < 35) and (thing.state.dlights = nil) and
   (thing.state.models = nil) and (thing.state.voxels = nil);

  if checksides then
{$ENDIF}
    if tz < MINZ - 128 * FRACUNIT * {$IFDEF OPENGL}modelflag {$ELSE} voxelflag {$ENDIF} then
      exit;

  xscale := FixedDiv(projection, tz);
  {$IFDEF OPENGL}
  if modelflag = 0 then
  {$ENDIF}
    if xscale <= 0 then
      exit;

  gxt := -FixedMul(tr_x, viewsin);
  gyt := FixedMul(tr_y, viewcos);
  tx := -(gyt + gxt);

  // too far off the side?
{$IFDEF OPENGL}
  if checksides then
{$ENDIF}
    if abs(tx) > 4 * tz then
      exit;

  // decide which patch to use for sprite relative to player
  sprdef := @sprites[thing.sprite];

  if sprdef.numframes <= 0 then
  begin
    sprdef := @sprites[Ord(SPR_TNT1)];
    sprframe := @sprdef.spriteframes[0];
  end
  else
  begin
    frm := thing.frame and FF_FRAMEMASK;
    if IsIntegerInRange(frm, 0, sprdef.numframes) then
      sprframe := @sprdef.spriteframes[frm]
    else
    begin
      if wrong_frames.IndexOf(thing.info.name) < 0 then
      begin
        wrong_frames.Add(thing.info.name);
        I_Warning('R_ProjectSprite(): Sprite for "%s" is missing frame ' + Chr(Ord('A') + frm) + '.'#13#10, [thing.info.name]);
      end;
      sprdef := @sprites[Ord(SPR_TNT1)];
      sprframe := @sprdef.spriteframes[0];
    end;
  end;

  if sprframe = nil then
  begin
    I_DevError('R_ProjectSprite(): Sprite for "%s" is NULL.'#13#10, [thing.info.name]);
    exit;
  end;

  // JVAL: Up to 32 sprite rotations
  case sprframe.rotate of
  0:  // 1 angle
    begin
      // use single rotation for all views
      lump := sprframe.lump[0];
      flip := sprframe.flip[0];
    end;
  1:  // 8 angles
    begin
      // choose a different rotation based on player view
      ang := R_PointToAngle(thing.x, thing.y);
      rot := (ang - thing.angle + LongWord(ANG45 div 2) * 9) shr 29;
      lump := sprframe.lump[rot];
      flip := sprframe.flip[rot];
    end;
  2:  // 16 angles
    begin
      // choose a different rotation based on player view
      ang := R_PointToAngle(thing.x, thing.y);
      rot := translaterotations[2, (ang - thing.angle + LongWord(ANG45 div 4) * 17) shr 28];
      lump := sprframe.lump[rot];
      flip := sprframe.flip[rot];
    end;
  3:  // 32 angles
    begin
      // choose a different rotation based on player view
      ang := R_PointToAngle(thing.x, thing.y);
      rot := translaterotations[3, (ang - thing.angle + LongWord(ANG45 div 8) * 33) shr 27];
      lump := sprframe.lump[rot];
      flip := sprframe.flip[rot];
    end;
  else
    if funny_rotations.IndexOf(thing.info.name) < 0 then
    begin
      funny_rotations.Add(thing.info.name);
      I_Warning('R_ProjectSprite(): Sprite for "%s" has funny rotations.'#13#10, [thing.info.name]);
    end;
    // use single rotation for all views
    lump := sprframe.lump[0];
    flip := sprframe.flip[0];
  end;

  infoscale := thing.scale;
  soffset := FixedMul(spriteoffset[lump] + thing.spriteDX, infoscale);
  tx := tx - soffset;
  x1 := FixedInt(centerxfrac + FixedMul(tx, xscale));
{$IFNDEF OPENGL}
  // JVAL: 20200528 - Approximate sqrt(2) with 3 div 2 ;-)
  vr3 := thing.state.voxelradius * 3;
  vx1 := FixedInt(centerxfrac + FixedMul(tx + soffset - vr3 div 2, xscale));
{$ENDIF}

  // off the right side?
{$IFDEF OPENGL}
  if checksides then
    if modelflag = 0 then
{$ELSE}
  if voxelflag = 0 then
{$ENDIF}
    if x1 > viewwidth then
      exit;

  swidth := FixedMul(spritewidth[lump], infoscale);
  tx := tx + swidth;
  x2 := FixedInt(centerxfrac + FixedMul(tx, xscale)) - 1;
{$IFNDEF OPENGL}
  // JVAL: 20200528 - Approximate sqrt(2) with 3 div 2 ;-)
  vx2 := FixedInt(centerxfrac + FixedMul(tx - swidth + vr3, xscale));
{$ENDIF}

  // off the left side
{$IFDEF OPENGL}
  if checksides then
    if modelflag = 0 then
{$ELSE}
  if voxelflag = 0 then
{$ENDIF}
    if x2 < 0 then
      exit;

{$IFDEF OPENGL}
  if modelflag = 0 then
{$ENDIF}
  if x2 < x1 then
    exit; // SOS
  sec := Psubsector_t(thing.subsector).sector;
{$IFNDEF OPENGL}
  scaledtop := FixedMul(spritetopoffset[lump] + thing.spriteDY, infoscale);
  gzt := thing.z + scaledtop;
  {$IFDEF DOOM_OR_STRIFE}
  heightsec := sec.heightsec;

  if heightsec <> -1 then   // only clip things which are in special sectors
  begin
    plheightsec := Psubsector_t(viewplayer.mo.subsector).sector.heightsec;
    if plheightsec <> -1 then
    begin
      if viewz < sectors[plheightsec].floorheight then
      begin
        if thing.z >= sectors[heightsec].floorheight then
          Exit;
      end
      else
      begin
        if gzt < sectors[heightsec].floorheight then
          Exit;
      end;

      if viewz > sectors[plheightsec].ceilingheight then
      begin
        if (gzt < sectors[heightsec].ceilingheight) and
           (viewz >= sectors[heightsec].ceilingheight) then
           Exit;
      end
      else
      begin
        if thing.z >= sectors[heightsec].ceilingheight then
           Exit;
      end;
    end;
  end;
  {$ENDIF}
{$ENDIF}

  // store information in a vissprite
  vis := R_NewVisSprite;
  vis.mobjflags := thing.flags;
  vis.mobjflags_ex := thing.flags_ex or (thing.state.flags_ex and MF_EX_STATE_MASK); // JVAL: extended flags passed to vis
  vis.mo := thing;
  vis.scale := FixedDiv(projectiony, tz); // JVAL For correct aspect
  vis.infoscale := infoscale;
  {$IFDEF HEXEN}
  if thing.flags and MF_TRANSLATION <> 0 then
  begin
    if thing.player <> nil then
      vis._class := Ord(Pplayer_t(thing.player)._class)
    else
      vis._class := thing.special1;
    if vis._class > 2 then
      vis._class := 0
    else if vis._class < 0 then
      vis._class := 0;
  end
  else
    vis._class := 0;
  {$ENDIF}
  {$IFNDEF OPENGL}
  vis.cache := nil;
  vis.lightcache := nil;
  {$IFDEF DOOM_OR_STRIFE}
  vis.heightsec := heightsec;
  {$ENDIF}
  if voxelflag <> 0 then
    vis.renderflags := VSF_VOXEL  // JVAL voxel support
  else
    vis.renderflags := 0;
  vis.gx := thing.x;
  vis.gy := thing.y;
  vis.gz := thing.z;
  vis.gzt := gzt;
  // foot clipping
  {$IFDEF HERETIC}
  if (thing.flags2 and MF2_FEETARECLIPPED <> 0) and (thing.z <= sec.floorheight) then
    vis.footclip := 10 * FRACUNIT
  else
    vis.footclip := 0;
  {$ELSE}
  vis.footclip := thing.floorclip;
  {$ENDIF}
  vis.texturemid := FixedDiv(thing.z - viewz - vis.footclip, infoscale) + spritetopoffset[lump] + thing.spriteDY;
  vis.texturemid2 := vis.texturemid + spritetopoffset[lump] + thing.spriteDY;
  if x1 <= 0 then
    vis.x1 := 0
  else
    vis.x1 := x1;
  if x2 >= viewwidth then
    vis.x2 := viewwidth - 1
  else
    vis.x2 := x2;
  if vx1 <= 0 then
    vis.vx1 := 0
  else
    vis.vx1 := vx1;
  if vx2 >= viewwidth then
    vis.vx2 := viewwidth - 1
  else
    vis.vx2 := vx2;

  // JVAL: 20200528
  if vis.vx1 > vis.x1 then
    vis.vx1 := vis.x1;
  if vis.vx2 < vis.x2 then
    vis.vx2 := vis.x2;

  iscale := FixedDiv(FRACUNIT, xscale);

  // JVAL: 3d Floors
  sprlights := spritelights;
  if hasExtrafloors then
  begin
    if scaledtop < 60 * FRACUNIT then
      vis.ceilingz := vis.gz + 60 * FRACUNIT
    else
      vis.ceilingz := vis.gzt;

    if thing.ceilingz < vis.ceilingz then
      vis.ceilingz := thing.ceilingz;

    midn := sec.midsec;
    if midn >= 0 then
    begin
      mid := @sectors[midn];
      if vis.gz < mid.floorheight then
        sprlights := spritelights2;
    end;
  end;

  if flip <> (thing.flags3_ex and MF3_EX_FLIPSPRITE <> 0) then
  begin
    vis.startfrac := spritewidth[lump] - 1;
    vis.xiscale := -iscale;
  end
  else
  begin
    vis.startfrac := 0;
    vis.xiscale := iscale;
  end;
  {$ENDIF}
{$IFDEF OPENGL}
  vis.flip := flip <> (thing.flags3_ex and MF3_EX_FLIPSPRITE <> 0);
{$ENDIF}

{$IFNDEF OPENGL}
  if vis.x1 > x1 then
    vis.startfrac := vis.startfrac + FixedDiv(vis.xiscale, infoscale) * (vis.x1 - x1);
{$ENDIF}
  vis.patch := lump;

  vis.fog := sec.renderflags and SRF_FOG <> 0; // JVAL: Mars fog sectors

{$IFNDEF OPENGL}  // JVAL: 3d Floors
  // get light level
  {$IFDEF DOOM_OR_HERETIC}
  if thing.flags and MF_SHADOW <> 0 then
  begin
    // shadow draw
    vis.colormap := nil;
  end
  else{$ENDIF} if fixedcolormap <> nil then
  begin
    // fixed map
    vis.colormap := fixedcolormap;
  end
  else if thing.frame and FF_FULLBRIGHT <> 0 then
  begin
    // full bright
    if vis.fog then // JVAL: Mars fog sectors
      vis.colormap := fog_colormaps
    else
      vis.colormap := colormaps;
  end
  else
  begin
    // diminished light
    index := _SHR(xscale, LIGHTSCALESHIFT);

    if index >= MAXLIGHTSCALE then
      index := MAXLIGHTSCALE - 1
    else if index < 0 then
      index := 0;

    vis.colormap := sprlights[index]; // JVAL: 3d Floors
  end;

  if (vis.colormap = nil) or
     {$IFDEF HEXEN}(vis.mobjflags and (MF_SHADOW or MF_ALTSHADOW) <> 0) or {$ENDIF}
     {$IFDEF STRIFE}(vis.mobjflags and MF_SHADOW <> 0) or {$ENDIF}
     (usetransparentsprites and (vis.mo <> nil) and (vis.mo.renderstyle in [mrs_translucent, mrs_add, mrs_subtract])) or
     (usetransparentsprites and (vis.mobjflags_ex and MF_EX_TRANSPARENT <> 0)) then
    vis.renderflags := vis.renderflags or VSF_TRANSPARENCY;
{$ENDIF}

{$IFDEF OPENGL}
  gld_AddSprite(vis); // JVAL: OPENGL
{$ENDIF}
end;

//==============================================================================
//
// R_AddSprites
// During BSP traversal, this adds sprites by sector.
//
//==============================================================================
procedure R_AddSprites(sec: Psector_t);
var
  thing: Pmobj_t;
  lightnum: integer;
{$IFNDEF OPENGL}
  lightnum2: integer;
{$ENDIF}
begin
  // BSP is traversed by subsector.
  // A sector might have been split into several
  // subsectors during BSP building.
  // Thus we check whether its already added.
  if sec.validcount = validcount then
    exit;

  // Well, now it will be done.
  sec.validcount := validcount;

{$IFNDEF OPENGL}
  hasExtraFloors := hasExtraFloors or (sec.midsec >= 0);  // JVAL: 3d Floors
{$ENDIF}

  lightnum := _SHR(sec.lightlevel, LIGHTSEGSHIFT) + extralight;

  if sec.renderflags and SRF_FOG <> 0 then // JVAL: Mars fog sectors
  begin
    {$IFNDEF OPENGL}
    dc_fog := true;
    {$ENDIF}
    if lightnum <= 0 then
      spritelights := @fog_scalelight[0]
    else if lightnum >= LIGHTLEVELS then
      spritelights := @fog_scalelight[LIGHTLEVELS - 1]
    else
      spritelights := @fog_scalelight[lightnum];
  end
  else
  begin
    {$IFNDEF OPENGL}
    dc_fog := false;
    {$ENDIF}
    if lightnum <= 0 then
      spritelights := @scalelight[0]
    else if lightnum >= LIGHTLEVELS then
      spritelights := @scalelight[LIGHTLEVELS - 1]
    else
      spritelights := @scalelight[lightnum];
  end;

  // JVAL: 3d Floors
  {$IFNDEF OPENGL}
  if sec.midsec >= 0 then
  begin
    lightnum2 := _SHR(sectors[sec.midsec].lightlevel, LIGHTSEGSHIFT) + extralight;
    if sectors[sec.midsec].renderflags and SRF_FOG <> 0 then // JVAL: Mars fog sectors
    begin
      if lightnum2 <= 0 then
        spritelights2 := @fog_scalelight[0]
      else if lightnum2 >= LIGHTLEVELS then
        spritelights2 := @fog_scalelight[LIGHTLEVELS - 1]
      else
        spritelights2 := @fog_scalelight[lightnum2];
    end
    else
    begin
      if lightnum2 <= 0 then
        spritelights2 := @scalelight[0]
      else if lightnum2 >= LIGHTLEVELS then
        spritelights2 := @scalelight[LIGHTLEVELS - 1]
      else
        spritelights2 := @scalelight[lightnum2];
    end;
  end;
  {$ENDIF}

  // Handle all things in sector.
  thing := sec.thinglist;
  while thing <> nil do
  begin
    R_ProjectSprite(thing);
    thing := thing.snext;
  end;
end;

//
// R_DrawPSprite
//
{$IFDEF HERETIC}
const
  PSpriteSY: array[0..Ord(NUMWEAPONS) - 1] of fixed_t = (
    0, // staff
    5 * FRACUNIT, // goldwand
    15 * FRACUNIT, // crossbow
    15 * FRACUNIT, // blaster
    15 * FRACUNIT, // skullrod
    15 * FRACUNIT, // phoenix rod
    15 * FRACUNIT, // mace
    15 * FRACUNIT, // gauntlets
    15 * FRACUNIT // beak
    );
{$ENDIF}
{$IFDEF HEXEN}
// Y-adjustment values for full screen (4 weapons)
const
  FPSpriteSY: array[0..Ord(NUMCLASSES) - 1, 0..Ord(NUMWEAPONS) - 1] of fixed_t = (
    (-10 * FRACUNIT,              0,  20 * FRACUNIT, 10 * FRACUNIT),  // Fighter
    (             0,  10 * FRACUNIT,  10 * FRACUNIT,             0),  // Cleric
    (  9 * FRACUNIT,  20 * FRACUNIT,  20 * FRACUNIT, 20 * FRACUNIT),  // Mage
    ( 10 * FRACUNIT,  10 * FRACUNIT,  10 * FRACUNIT, 10 * FRACUNIT)   // Pig
  );
{$ENDIF}

//==============================================================================
//
// R_DrawPSprite
//
//==============================================================================
procedure R_DrawPSprite(psp: Ppspdef_t);
var
  tx: fixed_t;
  x1: integer;
  x2: integer;
  sprdef: Pspritedef_t;
  sprframe: Pspriteframe_t;
  lump: integer;
{$IFNDEF OPENGL}
  flip: boolean;
{$ENDIF}
  vis: Pvissprite_t;
  avis: vissprite_t;
{$IFDEF OPENGL}
  lightlevel: integer; // JVAL OPENGL
{$ENDIF}
begin
  // decide which patch to use

  sprdef := @sprites[Ord(psp.state.sprite)];

  sprframe := @sprdef.spriteframes[psp.state.frame and FF_FRAMEMASK];

  lump := sprframe.lump[0];
{$IFNDEF OPENGL}
  flip := sprframe.flip[0];
{$ENDIF}

  // calculate edges of the shape
  tx := psp.sx - 160 * FRACUNIT;

  tx := tx - spriteoffset[lump];
  if viewplayer.mo <> nil then
    tx := tx - viewplayer.mo.spriteDX;
  x1 := FixedInt(centerxfrac + centerxshift + FixedMul(tx, pspritescalep));

  // off the right side
  if x1 > viewwidth then
    exit;

  tx := tx + spritewidth[lump];
  x2 := FixedInt(centerxfrac + centerxshift + FixedMul(tx, pspritescalep)) - 1;

  // off the left side
  if x2 < 0 then
    exit;

  // store information in a vissprite
  vis := @avis;
  ZeroMemory(vis, SizeOf(vissprite_t));
  vis.mo := viewplayer.mo;
  vis.texturemid := (BASEYCENTER * FRACUNIT) - (psp.sy - spritetopoffset[lump]);
  if viewplayer.mo <> nil then
    vis.texturemid := vis.texturemid + viewplayer.mo.spriteDY;

{$IFDEF HERETIC}
  if screenblocks > 10 then
    vis.texturemid := vis.texturemid - PSpriteSY[Ord(viewplayer.readyweapon)];
{$ENDIF}
{$IFDEF HEXEN}
  if screenblocks > 10 then
    vis.texturemid := vis.texturemid - FPSpriteSY[Ord(viewplayer._class), Ord(viewplayer.readyweapon)];
  vis._class := 0;
{$ENDIF}
  if x1 < 0 then
    vis.x1 := 0
  else
    vis.x1 := x1;
  if x2 >= viewwidth then
    vis.x2 := viewwidth - 1
  else
    vis.x2 := x2;

  vis.scale := pspriteyscale;

{$IFNDEF OPENGL}
  if flip then
  begin
    vis.xiscale := -pspriteiscalep;
    vis.startfrac := spritewidth[lump] - 1;
  end
  else
  begin
    vis.xiscale := pspriteiscalep;
    vis.startfrac := 0;
  end;

  if vis.x1 > x1 then
    vis.startfrac := vis.startfrac + vis.xiscale * (vis.x1 - x1);
{$ENDIF}
  vis.patch := lump;

  vis.fog := false; // JVAL: Mars fog sectors

{$IFNDEF OPENGL}{$IFDEF DOOM_OR_HERETIC}
  if (viewplayer.powers[Ord(pw_invisibility)] > 4 * 32) or
     (viewplayer.powers[Ord(pw_invisibility)] and 8 <> 0) then
  begin
    // shadow draw
    vis.colormap := nil;
  end
  else
{$ENDIF}{$ENDIF}if fixedcolormap <> nil then
  begin
    // fixed color
{$IFNDEF OPENGL}  // JVAL: 3d Floors
    vis.colormap := fixedcolormap;
{$ENDIF}
{$IFDEF OPENGL}
    lightlevel := 255;
{$ENDIF}
  end
  else if psp.state.frame and FF_FULLBRIGHT <> 0 then
  begin
    // full bright
{$IFNDEF OPENGL}  // JVAL: 3d Floors
    vis.colormap := colormaps;
{$ENDIF}
{$IFDEF OPENGL}
    lightlevel := 255;
{$ENDIF}
  end
  else
  begin
    // local light
{$IFNDEF OPENGL}  // JVAL: 3d Floors
    vis.colormap := spritelights[MAXLIGHTSCALE - 1];
{$ENDIF}
{$IFDEF OPENGL}
    lightlevel := Psubsector_t(vis.mo.subsector).sector.lightlevel + extralight shl LIGHTSEGSHIFT
{$ENDIF}
  end;

  vis.infoscale := FRACUNIT;
{$IFDEF OPENGL}
  gld_DrawWeapon(lump, vis, lightlevel); // JVAL OPENGL
{$ELSE}
  R_DrawVisSprite(vis, true); // JVAL OPENGL
{$ENDIF}
end;

//==============================================================================
//
// R_DrawPlayerSprites
//
//==============================================================================
procedure R_DrawPlayerSprites;
var
  i: integer;
  lightnum: integer;
  playerlightlevel: integer;  // JVAL: 3d Floors
  s: Psector_t; // JVAL: 3d Floors
begin
  // get light level
  s := Psubsector_t(viewplayer.mo.subsector).sector;
  if s.midsec >= 0 then
  begin
    if viewplayer.mo.z > sectors[s.midsec].floorheight then
      playerlightlevel := s.lightlevel
    else
      playerlightlevel := sectors[s.midsec].lightlevel;
  end
  else
    playerlightlevel := s.lightlevel;

  lightnum :=
    _SHR(playerlightlevel, LIGHTSEGSHIFT) + // JVAL: 3d Floors
    extralight;

  if lightnum <= 0 then
    spritelights := @scalelight[0]
  else if lightnum >= LIGHTLEVELS then
    spritelights := @scalelight[LIGHTLEVELS - 1]
  else
    spritelights := @scalelight[lightnum];

  // clip to screen bounds
  mfloorclip := @screenheightarray;
  mceilingclip := @negonearray;

  // add all active psprites
  if shiftangle < 128 then
    centerxshift := shiftangle * FRACUNIT div 40 * viewwidth
  else
    centerxshift := - (255 - shiftangle) * FRACUNIT div 40 * viewwidth;
  for i := 0 to Ord(NUMPSPRITES) - 1 do
  begin
    if viewplayer.psprites[i].state <> nil then
      R_DrawPSprite(@viewplayer.psprites[i]);
  end;
end;

{$IFNDEF OPENGL}
//==============================================================================
//
// R_SignalPrepareMasked;
//
//==============================================================================
procedure R_SignalPrepareMasked;
begin
  maskedpreparesignal := true;
end;

//==============================================================================
//
// R_PrepareSprite
//
//==============================================================================
function R_PrepareSprite(const spr: Pvissprite_t; const islight: Boolean = False): boolean;
var
  sx1: integer;
  sx2: integer;
  x: integer;
  cache: Pvisspritecache_t;
  item: Pvisspritecacheitem_t;
  ds: Pdrawseg_t;
  scale: fixed_t;
  lowscale: fixed_t;
  silhouette: integer;
begin
  if spritecachepos + SizeOf(visspritecache_t) > SPRITECACHESIZE then
  begin
    Result := false;
    exit;
  end;

  cache := @spritecache[spritecachepos];
  if islight then
    spr.lightcache := cache
  else
    spr.cache := cache;

  cache.cachesize := 0;

  if islight then
  begin
    x := (spr.x2 - spr.x1) div 2;
    sx1 := spr.x1 - x;
    sx2 := spr.x2 + x;
    if sx1 < 0 then
      sx1 := 0;
    if sx2 >= viewwidth then
      sx2 := viewwidth - 1;

    if sx2 < sx1 then
    begin
      Result := True;
      exit; // SOS
    end;
  end
  else if spr.renderflags and VSF_VOXEL <> 0 then
  begin
    sx1 := spr.vx1;
    sx2 := spr.vx2;
  end
  else
  begin
    sx1 := spr.x1;
    sx2 := spr.x2;
  end;

  R_GetDrawsegsForRange(sx1, sx2, cache.fdrawsegs, cache.fds_p);

  // Scan drawsegs from end to start for obscuring segs.
  // The first drawseg that has a greater scale
  //  is the clip seg.
  while cache.fds_p > 0 do
  begin
    dec(cache.fds_p);
    ds := cache.fdrawsegs[cache.fds_p];
    // determine if the drawseg obscures the sprite
    if (ds.x1 > sx2) or (ds.x2 < sx1) or ds.maskedquery then
    begin
      // does not cover sprite
      continue;
    end;

    if spritecachepos + SizeOf(visspritecacheitem_t) > SPRITECACHESIZE then
    begin
      Result := false;
      exit;
    end;

    item := @cache.cache[cache.cachesize];
    item.ds := ds;

    if ds.x1 < sx1 then
      item.r1 := sx1
    else
      item.r1 := ds.x1;
    if ds.x2 > sx2 then
      item.r2 := sx2
    else
      item.r2 := ds.x2;

    if ds.scale1 > ds.scale2 then
    begin
      lowscale := ds.scale2;
      scale := ds.scale1;
    end
    else
    begin
      lowscale := ds.scale1;
      scale := ds.scale2;
    end;

    if (scale < spr.scale) or
       ((lowscale < spr.scale) and not R_PointOnSegSide(spr.gx, spr.gy, ds.curline)) then
    begin
      if (ds.thicksidecol <> nil) and (ds.maskedtexturecol <> nil) then
      begin
        item.operation := CACHE_OP_THICK_AND_MASKED;
        Inc(cache.cachesize);
        continue;
      end;
      if ds.thicksidecol <> nil then
      begin
        item.operation := CACHE_OP_THICK;
        Inc(cache.cachesize);
        continue;
      end;
      if ds.maskedtexturecol <> nil then
      begin
        item.operation := CACHE_OP_MASKED;
        Inc(cache.cachesize);
        continue;
      end;
      continue;
    end;

    // clip this piece of the sprite
    silhouette := ds.silhouette;

    if spr.gz >= ds.bsilheight then
      silhouette := silhouette and not SIL_BOTTOM;

    if spr.gzt <= ds.tsilheight then
      silhouette := silhouette and not SIL_TOP;

    if silhouette = 1 then
    begin
      item.operation := CACHE_OP_SIL1;
      Inc(cache.cachesize);
      continue;
    end
    else if silhouette = 2 then
    begin
      item.operation := CACHE_OP_SIL2;
      Inc(cache.cachesize);
      continue;
    end
    else if silhouette = 3 then
    begin
      item.operation := CACHE_OP_SIL3;
      Inc(cache.cachesize);
      continue;
    end;
  end;

  spritecachepos := spritecachepos + SizeOf(visspritecache_t) + (cache.cachesize - 1) * SizeOf(visspritecacheitem_t);
  Result := True;
end;

//==============================================================================
//
// R_ExecuteSpriteCache
//
//==============================================================================
procedure R_ExecuteSpriteCache(const cache: Pvisspritecache_t);
var
  i: integer;
  ds: Pdrawseg_t;
  x: integer;
  item: Pvisspritecacheitem_t;
begin
  for i := 0 to cache.cachesize - 1 do
  begin
    item := @cache.cache[i];
    case item.operation of
      CACHE_OP_THICK:
        begin
          R_RenderThickSideRange(item.ds, item.r1, item.r2);
        end;
      CACHE_OP_MASKED:
        begin
          R_RenderMaskedSegRange(item.ds, item.r1, item.r2);
        end;
      CACHE_OP_THICK_AND_MASKED:
        begin
          R_RenderThickSideRange(item.ds, item.r1, item.r2);
          R_RenderMaskedSegRange(item.ds, item.r1, item.r2);
        end;
      CACHE_OP_SIL1:
        begin
          ds := item.ds;
          for x := item.r1 to item.r2 do
            if clipbot[x] = -2 then
              clipbot[x] := ds.sprbottomclip[x];
        end;
      CACHE_OP_SIL2:
        begin
          ds := item.ds;
          for x := item.r1 to item.r2 do
            if cliptop[x] = -2 then
              cliptop[x] := ds.sprtopclip[x];
        end;
      CACHE_OP_SIL3:
        begin
          ds := item.ds;
          for x := item.r1 to item.r2 do
          begin
            if clipbot[x] = -2 then
              clipbot[x] := ds.sprbottomclip[x];
            if cliptop[x] = -2 then
              cliptop[x] := ds.sprtopclip[x];
          end;
        end;
    end;
  end;
end;

//==============================================================================
//
// R_PrepareMaked
//
//==============================================================================
procedure R_PrepareMasked;
var
  i: integer;
  spr: Pvissprite_t;
  dolight: Boolean;
begin
  spritecachepos := 0;
  maskedpreparesignal := false;
  dolight := (uselightboost and (videomode = vm32bit) and (fixedcolormapnum <> INVERSECOLORMAP)) or
       (uselightboostgodmode and (videomode = vm32bit) and (fixedcolormapnum = INVERSECOLORMAP));
  for i := 0 to vissprite_p - 1 do
  begin
    spr := vissprites[i];
    if dolight then
      if spr.mobjflags_ex and MF_EX_LIGHT <> 0 then
        if not R_PrepareSprite(spr, True) then
          Break;
    if not R_PrepareSprite(spr) then
      Break;
    if maskedpreparesignal then
      Break;
  end;
end;

//==============================================================================
//
// R_DrawSprite
//
//==============================================================================
procedure R_DrawSprite(spr: Pvissprite_t);
var
  ds: Pdrawseg_t;
  x: integer;
  sx1: integer;
  sx2: integer;
  r1: integer;
  r2: integer;
  scale: fixed_t;
  lowscale: fixed_t;
  silhouette: integer;
  i: integer;
  cache: Pvisspritecache_t;
  size: integer;
  {$IFDEF DOOM_OR_STRIFE}
  h, mh: fixed_t;
  plheightsec: integer;
  {$ENDIF}
  fds_p: integer;
  fdrawsegs: Pdrawsegsbuffer_t;
begin
  sx1 := spr.x1;
  sx2 := spr.x2;
  size := sx2 - sx1 + 1;
  memsetsi(@clipbot[sx1], - 2, size);
  memsetsi(@cliptop[sx1], - 2, size);

  if spr.cache <> nil then
  begin
    cache := spr.cache;
    R_ExecuteSpriteCache(cache);
    fdrawsegs := cache.fdrawsegs;
    fds_p := cache.fds_p;
  end
  else
    R_GetDrawsegsForVissprite(spr, fdrawsegs, fds_p);

  // Scan drawsegs from end to start for obscuring segs.
  // The first drawseg that has a greater scale
  //  is the clip seg.
  for i := fds_p - 1 downto 0 do
  begin
    ds := fdrawsegs[i];
    // determine if the drawseg obscures the sprite
    if (ds.x1 > sx2) or (ds.x2 < sx1) or ds.maskedquery then
    begin
      // does not cover sprite
      continue;
    end;

    if ds.x1 < sx1 then
      r1 := sx1
    else
      r1 := ds.x1;
    if ds.x2 > sx2 then
      r2 := sx2
    else
      r2 := ds.x2;

    if ds.scale1 > ds.scale2 then
    begin
      lowscale := ds.scale2;
      scale := ds.scale1;
    end
    else
    begin
      lowscale := ds.scale1;
      scale := ds.scale2;
    end;

    if (scale < spr.scale) or
       ((lowscale < spr.scale) and not R_PointOnSegSide(spr.gx, spr.gy, ds.curline)) then
    begin
      // masked mid texture?
      if ds.thicksidecol <> nil then        // JVAL: 3d Floors
        R_RenderThickSideRange(ds, r1, r2); // JVAL: 3d Floors
      if ds.maskedtexturecol <> nil then
        R_RenderMaskedSegRange(ds, r1, r2);
      // seg is behind sprite
      continue;
    end;

    // clip this piece of the sprite
    silhouette := ds.silhouette;

    if spr.gz >= ds.bsilheight then
      silhouette := silhouette and not SIL_BOTTOM;

    if spr.gzt <= ds.tsilheight then
      silhouette := silhouette and not SIL_TOP;

    if silhouette = 1 then
    begin
      // bottom sil
      for x := r1 to r2 do
        if clipbot[x] = -2 then
          clipbot[x] := ds.sprbottomclip[x];
    end
    else if silhouette = 2 then
    begin
      // top sil
      for x := r1 to r2 do
        if cliptop[x] = -2 then
          cliptop[x] := ds.sprtopclip[x];
    end
    else if silhouette = 3 then
    begin
      // both
      for x := r1 to r2 do
      begin
        if clipbot[x] = -2 then
          clipbot[x] := ds.sprbottomclip[x];
        if cliptop[x] = -2 then
          cliptop[x] := ds.sprtopclip[x];
      end;
    end;
  end;

  {$IFDEF DOOM_OR_STRIFE}
  // killough 3/27/98:
  // Clip the sprite against deep water and/or fake ceilings.
  // killough 4/9/98: optimize by adding mh
  // killough 4/11/98: improve sprite clipping for underwater/fake ceilings

  if spr.heightsec <> -1 then  // only things in specially marked sectors
  begin
    plheightsec := Psubsector_t(viewplayer.mo.subsector).sector.heightsec;
    mh := sectors[spr.heightsec].floorheight;
    if mh > spr.gz then
    begin
      mh := mh - viewz;
      h := centeryfrac - FixedMul(mh, spr.scale);
      if h >= 0 then
      begin
        h := h div FRACUNIT;
        if h < viewheight then
        begin
          if (mh <= 0) or ((plheightsec <> -1) and (viewz > sectors[plheightsec].floorheight)) then
          begin
            for x := sx1 to sx2 do
              if (clipbot[x] = -2) or (h < clipbot[x]) then
                clipbot[x] := h;
          end
          else if (plheightsec <> -1) and (viewz <= sectors[plheightsec].floorheight) then
          begin
            for x := sx1 to sx2 do
              if (cliptop[x] = -2) or (h > cliptop[x]) then
                cliptop[x] := h;
          end;
        end;
      end;
    end;

    mh := sectors[spr.heightsec].ceilingheight;
    if mh < spr.gzt then
    begin
      h := centeryfrac - FixedMul(mh - viewz, spr.scale);
      if h >= 0 then
      begin
        h := h div FRACUNIT;
        if h < viewheight then
        begin
          if (plheightsec <> -1) and (viewz >= sectors[plheightsec].ceilingheight) then
          begin
            for x := sx1 to sx2 do
              if (clipbot[x] = -2) or (h < clipbot[x]) then
                clipbot[x] := h;
          end
          else
          begin
            for x := sx1 to sx2 do
              if (cliptop[x] = -2) or (h > cliptop[x]) then
                cliptop[x] := h;
          end;
        end;
      end;
    end;
  end;

  // killough 3/27/98: end special clipping for deep water / fake ceilings
  {$ENDIF}

  // all clipping has been performed, so draw the sprite

  // check for unclipped columns
  for x := sx1 to sx2 do
  begin
    if clipbot[x] = -2 then
      clipbot[x] := fake3dbottomclip
    else if clipbot[x] > fake3dbottomclip then
      clipbot[x] := fake3dbottomclip;

    if cliptop[x] = -2 then
      cliptop[x] := fake3dtopclip
    else if cliptop[x] < fake3dtopclip then
      cliptop[x] := fake3dtopclip;
  end;

  mfloorclip := @clipbot;
  mceilingclip := @cliptop;

  if spr.infoscale <> FRACUNIT then
  begin
    spr.scale := FixedMul(spr.scale, spr.infoscale);
    spr.xiscale := FixedDiv(spr.xiscale, spr.infoscale);
  end;

  R_DrawVisSprite(spr);
end;

//==============================================================================
//
// R_DrawSpriteLight
//
//==============================================================================
procedure R_DrawSpriteLight(spr: Pvissprite_t);
var
  ds: Pdrawseg_t;
  x: integer;
  r1: integer;
  r2: integer;
  cache: Pvisspritecache_t;
  scale: fixed_t;
  lowscale: fixed_t;
  silhouette: integer;
  i: integer;
  x1, x2: integer;
  size: integer;
  fds_p: integer;
  fdrawsegs: Pdrawsegsbuffer_t;
begin
  x := (spr.x2 - spr.x1) div 2;
  x1 := spr.x1 - x;
  x2 := spr.x2 + x;

  if x1 < 0 then
    x1 := 0;
  if x2 >= viewwidth then
    x2 := viewwidth - 1;

  if x2 < x1 then
    exit; // sos
  size := x2 - x1 + 1;
  memsetsi(@clipbot[x1], - 2, size);
  memsetsi(@cliptop[x1], - 2, size);

  if spr.lightcache <> nil then
  begin
    cache := spr.lightcache;
    R_ExecuteSpriteCache(cache);
    fdrawsegs := cache.fdrawsegs;
    fds_p := cache.fds_p;
  end
  else
    R_GetDrawsegsForRange(x1, x2, fdrawsegs, fds_p);

  // Scan drawsegs from end to start for obscuring segs.
  // The first drawseg that has a greater scale
  //  is the clip seg.
  for i := fds_p - 1 downto 0 do
  begin
    ds := fdrawsegs[i];
    // determine if the drawseg obscures the sprite
    if (ds.x1 > x2) or (ds.x2 < x1) or ds.maskedquery then
    begin
      // does not cover sprite
      continue;
    end;

    if ds.x1 <= x1 then
      r1 := x1
    else
      r1 := ds.x1;
    if ds.x2 >= x2 then
      r2 := x2
    else
      r2 := ds.x2;

    if ds.scale1 > ds.scale2 then
    begin
      lowscale := ds.scale2;
      scale := ds.scale1;
    end
    else
    begin
      lowscale := ds.scale1;
      scale := ds.scale2;
    end;

    if (scale < spr.scale) or
       ((lowscale < spr.scale) and not R_PointOnSegSide(spr.gx, spr.gy, ds.curline)) then
    begin
      // masked mid texture?
      if ds.thicksidecol <> nil then        // JVAL: 3d Floors
        R_RenderThickSideRange(ds, r1, r2); // JVAL: 3d Floors
      if ds.maskedtexturecol <> nil then
        R_RenderMaskedSegRange(ds, r1, r2);
      // seg is behind sprite
      continue;
    end;

    // clip this piece of the sprite
    silhouette := ds.silhouette;

    if spr.gz >= ds.bsilheight then
      silhouette := silhouette and not SIL_BOTTOM;

    if spr.gzt <= ds.tsilheight then
      silhouette := silhouette and not SIL_TOP;

    if silhouette = 1 then
    begin
      // bottom sil
      for x := r1 to r2 do
        if clipbot[x] = -2 then
          clipbot[x] := ds.sprbottomclip[x];
    end
    else if silhouette = 2 then
    begin
      // top sil
      for x := r1 to r2 do
        if cliptop[x] = -2 then
          cliptop[x] := ds.sprtopclip[x];
    end
    else if silhouette = 3 then
    begin
      // both
      for x := r1 to r2 do
      begin
        if clipbot[x] = -2 then
          clipbot[x] := ds.sprbottomclip[x];
        if cliptop[x] = -2 then
          cliptop[x] := ds.sprtopclip[x];
      end;
    end;
  end;

  // all clipping has been performed, so draw the sprite

  // check for unclipped columns
  for x := x1 to x2 do
  begin
    if clipbot[x] = -2 then
      clipbot[x] := fake3dbottomclip
    else if clipbot[x] > fake3dbottomclip then
      clipbot[x] := fake3dbottomclip;

    if cliptop[x] = -2 then
      cliptop[x] := fake3dtopclip
    else if cliptop[x] < fake3dtopclip then
      cliptop[x] := fake3dtopclip;
  end;

  mfloorclip := @clipbot;
  mceilingclip := @cliptop;

  R_DrawVisSpriteLight(spr, x1, x2);
end;

//==============================================================================
//
// R_MarkLights
//
//==============================================================================
procedure R_MarkLights;
var
  i: integer;
begin
  numdlitems := 0;
  vislight_p := 0;
  for i := 0 to vissprite_p - 1 do
    R_MarkDLights(vissprites[i].mo);
  R_AddAdditionalLights;
end;

//==============================================================================
// R_DoDrawMasked
//
// R_DrawMasked
//
//==============================================================================
procedure R_DoDrawMasked;
var
  pds: Pdrawseg_t;
  spr: Pvissprite_t;
  i: integer;
  restsprites: integer;
begin
  if vissprite_p > 0 then
  begin
    // draw all vissprites back to front
    if (uselightboost and (videomode = vm32bit) and (fixedcolormapnum <> INVERSECOLORMAP)) or
       (uselightboostgodmode and (videomode = vm32bit) and (fixedcolormapnum = INVERSECOLORMAP)) then
    begin
      // 32bit color
      for i := 0 to vissprite_p - 1 do
      begin
        spr := vissprites[i];
        if spr.mobjflags_ex and MF_EX_LIGHT <> 0 then
          R_DrawSpriteLight(spr);
        if spr.renderflags and VSF_VOXEL <> 0 then
          R_DrawVoxel(spr)
        else
          R_DrawSprite(spr);
      end;
    end
    else if depthbufferactive then
    begin
      restsprites := 0;
      for i := vissprite_p - 1 downto 0 do
      begin
        spr := vissprites[i];
        if spr.renderflags and VSF_TRANSPARENCY <> 0 then
        begin
          restsprites := i + 1;
          Break;
        end
        else if spr.renderflags and VSF_VOXEL <> 0 then
          R_DrawVoxel(spr)
        else
          R_DrawSprite(spr);
      end;

      for i := 0 to restsprites - 1 do
      begin
        spr := vissprites[i];
        if spr.renderflags and VSF_VOXEL <> 0 then
          R_DrawVoxel(spr)
        else
          R_DrawSprite(spr);
      end;
    end
    else
    begin
      for i := 0 to vissprite_p - 1 do
      begin
        spr := vissprites[i];
        if spr.renderflags and VSF_VOXEL <> 0 then
          R_DrawVoxel(spr)
        else
          R_DrawSprite(spr);
      end;
    end;
  end;

  // render any remaining masked mid textures
  // colfunc := maskedcolfunc; // JVAL: 3d Floors
  for i := ds_p - 1 downto 0 do
  begin
    pds := drawsegs[i];
    if pds.thicksidecol <> nil then                 // JVAL: 3d Floors
      R_RenderThickSideRange(pds,  pds.x1, pds.x2); // JVAL: 3d Floors
    if pds.maskedtexturecol <> nil then
      R_RenderMaskedSegRange(pds, pds.x1, pds.x2);
  end;

  R_StopDepthBuffer;  // JVAL: 3d Floors
end;

//==============================================================================
//
// R_DrawMasked_SingleThread
//
//==============================================================================
procedure R_DrawMasked_SingleThread;
begin
  R_SortVisSprites;
  domaskedzbuffer := false;
  if r_uselightmaps then
  begin
    R_MarkLights;
    if numdlitems > 0 then
    begin
      // Draw lights - First pass - only world
      R_DrawLightsSingleThread(lp_solid);
      domaskedzbuffer := r_lightmaponmasked;
      R_DoDrawMasked;
      // Draw lights - Second pass - masked
      if r_lightmaponmasked then
        R_DrawLightsSingleThread(lp_masked);
    end
    else
      R_DoDrawMasked;
  end
  else
    R_DoDrawMasked;
end;

//==============================================================================
//
// R_DrawMasked_MultiThread
//
//==============================================================================
procedure R_DrawMasked_MultiThread;
begin
  domaskedzbuffer := false;
  if r_uselightmaps then
  begin
    R_MarkLights;
    if numdlitems > 0 then
    begin
      // Draw lights - First pass - only world
      R_DrawLightsMultiThread(lp_solid);
      domaskedzbuffer := r_lightmaponmasked;
      R_DoDrawMasked;
      // Draw lights - Second pass - masked
      if r_lightmaponmasked then
        R_DrawLightsMultiThread(lp_masked);
    end
    else
      R_DoDrawMasked;
  end
  else
    R_DoDrawMasked;
end;
{$ENDIF}

//==============================================================================
//
// RIT_ProjectAdditionalThing
//
//==============================================================================
function RIT_ProjectAdditionalThing(mo: Pmobj_t): boolean;
begin
  if (mo.state.voxels <> nil) {$IFDEF OPENGL} or (mo.state.models <> nil) {$ENDIF} then
    R_ProjectSprite(mo);
  // keep checking
  result := true;
end;

const
  MAXMODELRADIUS = 384 * FRACUNIT;

//==============================================================================
//
// R_ProjectAdditionalThings
//
//==============================================================================
procedure R_ProjectAdditionalThings;
var
  x: integer;
  y: integer;
  xl: integer;
  xh: integer;
  yl: integer;
  yh: integer;
begin
  if internalblockmapformat then
  begin
    yh := MapBlockIntY(int64(viewy) + MAXMODELRADIUS - int64(bmaporgy));
    yl := MapBlockIntY(int64(viewy) - MAXMODELRADIUS - int64(bmaporgy));
    xh := MapBlockIntX(int64(viewx) + MAXMODELRADIUS - int64(bmaporgx));
    xl := MapBlockIntX(int64(viewx) - MAXMODELRADIUS - int64(bmaporgx));
  end
  else
  begin
    yh := MapBlockInt(viewy + MAXMODELRADIUS - bmaporgy);
    yl := MapBlockInt(viewy - MAXMODELRADIUS - bmaporgy);
    xh := MapBlockInt(viewx + MAXMODELRADIUS - bmaporgx);
    xl := MapBlockInt(viewx - MAXMODELRADIUS - bmaporgx);
  end;

  for y := yl to yh do
    for x := xl to xh do
      P_BlockThingsIterator(x, y, RIT_ProjectAdditionalThing);
end;

//==============================================================================
//
// R_DrawPlayer
//
//==============================================================================
procedure R_DrawPlayer;
var
  old_centery: fixed_t;
  old_centeryfrac: fixed_t;
begin
  // draw the psprites on top of everything
  //  but does not draw on side views
  if (viewangleoffset = 0) and not chasecamera then
  begin
    // Restore z-axis shift
    if zaxisshift then
    begin
      old_centery := centery;
      old_centeryfrac := centeryfrac;
      centery := viewheight div 2;
      centeryfrac := centery * FRACUNIT;
      R_DrawPlayerSprites;
      centery := old_centery;
      centeryfrac := old_centeryfrac;
    end
    else
      R_DrawPlayerSprites;
  end;
end;

end.

