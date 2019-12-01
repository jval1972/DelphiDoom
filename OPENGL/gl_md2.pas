//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2016 by Jim Valavanis
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
//  DESCRIPTION:
//    Dynamic lights for OpenGL rendering (why not in software mode??)
//    LIGHTDEF lump parsing, light animation
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit gl_md2;

interface

//------------------------------------------------------------------------------
//--------------------------- MD2 File Format ----------------------------------
//------------------------------------------------------------------------------

const
  // Magic number that identifies MD2 files (ASCII: 'IDP2').
  MD2_MAGIC = $32504449;

type
  TMD2_Index_List = record
    a, b, c: Integer;
    a_s, a_t,
    b_s, b_t,
    c_s, c_t: Single;
  end;
  TMD2_Index_List_Array = array[0..$FFFF] of TMD2_Index_List;
  PMD2_Index_List_Array = ^TMD2_Index_List_Array;

  TMD2_Vertex_List = record
    x, y, z: Single;
  end;
  TMD2_Vertex_List_Array = array[0..$FFFF] of TMD2_Vertex_List;
  PMD2_Vertex_List_Array = ^TMD2_Vertex_List_Array;

  TMD2_Frame_List = record
    Vertex: PMD2_Vertex_List_Array;
  end;
  TMD2_Frame_List_Array = array[0..$FFFF] of TMD2_Frame_List;
  PMD2_Frame_List_Array = ^TMD2_Frame_List_Array;

  TMD2DstVert_T = record
    s: SmallInt;
    t: SmallInt;
  end;
  TMD2DstVert_TArray = array[0..$FFFF] of TMD2DstVert_T;
  PMD2DstVert_TArray = ^TMD2DstVert_TArray;

  TMD2Triangle_T = record
    index_xyz: array[0..2] of SmallInt;
    index_st: array[0..2] of SmallInt;
  end;

  TMD2Trivertx_T = record
    v: array[0..2] of Byte;
    lightnormalindex: byte;
  end;

  PMD2AliasFrame_T = ^TMD2AliasFrame_T;
  TMD2AliasFrame_T = record
    scale: array[0..2] of Single;
    translate: array[0..2] of Single;
    name: array[0..15] of Char;
    verts: array[0..0] of TMD2Trivertx_T;
  end;

  TDmd2_T = record
    ident: Integer;
    version: Integer;

    skinWidth: Integer;
    skinHeight: Integer;
    framesize: Integer;

    num_skins: Integer;
    num_xyz: Integer;
    num_st: Integer;
    num_tris: Integer;
    num_glcmds: Integer;
    num_frames: Integer;

    ofs_skins: Integer;
    ofs_st: Integer;
    ofs_tris: Integer;
    ofs_frames: Integer;
    ofs_glcmds: Integer;
    ofs_end: Integer;
  end;

implementation

end.


