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

{$I Doom32.inc}

unit p_adjust;

interface

procedure P_AdjustMissingTextures;

var
  autoadjustmissingtextures: boolean;

implementation

uses
  r_defs,
  p_setup;
//
// P_AdjustMissingTextures()
//
// jval: Try to adjust missing textures
//
procedure P_AdjustMissingTextures;
var
  i, j: integer;
  ld, sld: Pline_t;
  sec: Psector_t;
begin
  ld := @lines[0];
  for i := 0 to numlines - 1 do
  begin
    if (ld.frontsector <> nil) and (ld.backsector <> nil) then
    begin
      // First pass: if missing toptexture/bottomtexture then
      // if the opposite sidedef has texture then use it!
      if (ld.frontsector.ceilingheight < ld.backsector.ceilingheight) then
        if sides[ld.sidenum[1]].toptexture = 0 then
           sides[ld.sidenum[1]].toptexture := sides[ld.sidenum[0]].toptexture;
      if (ld.frontsector.floorheight > ld.backsector.floorheight) then
        if sides[ld.sidenum[1]].bottomtexture = 0 then
           sides[ld.sidenum[1]].bottomtexture := sides[ld.sidenum[0]].bottomtexture;
      if (ld.frontsector.ceilingheight > ld.backsector.ceilingheight) then
        if sides[ld.sidenum[0]].toptexture = 0 then
           sides[ld.sidenum[0]].toptexture := sides[ld.sidenum[1]].toptexture;
      if (ld.frontsector.floorheight < ld.backsector.floorheight) then
        if sides[ld.sidenum[0]].bottomtexture = 0 then
           sides[ld.sidenum[0]].bottomtexture := sides[ld.sidenum[1]].bottomtexture;

      // Second pass, try to find a toptexture from other lines in sector
      if (ld.frontsector.ceilingheight < ld.backsector.ceilingheight) then
        if sides[ld.sidenum[1]].toptexture = 0 then
        begin
          sec := ld.backsector;
          sld := @sec.lines[0];
          for j := 0 to sec.linecount - 1 do
          begin
            if sld.frontsector = ld.frontsector then
              if sides[sld.sidenum[1]].toptexture > 0 then
              begin
                sides[ld.sidenum[1]].toptexture := sides[sld.sidenum[1]].toptexture;
                break;
              end;
            inc(sld);
          end;
          if sides[ld.sidenum[1]].toptexture = 0 then
          begin
            sld := @sec.lines[0];
            for j := 0 to sec.linecount - 1 do
            begin
              if sides[sld.sidenum[1]].toptexture > 0 then
              begin
                sides[ld.sidenum[1]].toptexture := sides[sld.sidenum[1]].toptexture;
                break;
              end;
              if sides[sld.sidenum[0]].toptexture > 0 then
              begin
                sides[ld.sidenum[1]].toptexture := sides[sld.sidenum[0]].toptexture;
                break;
              end;
              inc(sld);
            end;
          end;
        end;

      if (ld.frontsector.ceilingheight > ld.backsector.ceilingheight) then
        if sides[ld.sidenum[0]].toptexture = 0 then
        begin
          sec := ld.frontsector;
          sld := @sec.lines[0];
          for j := 0 to sec.linecount - 1 do
          begin
            if sld.backsector = ld.backsector then
              if sides[sld.sidenum[0]].toptexture > 0 then
              begin
                sides[ld.sidenum[0]].toptexture := sides[sld.sidenum[0]].toptexture;
                break;
              end;
            inc(sld);
          end;
          if sides[ld.sidenum[0]].toptexture = 0 then
          begin
            sld := @sec.lines[0];
            for j := 0 to sec.linecount - 1 do
            begin
              if sides[sld.sidenum[0]].toptexture > 0 then
              begin
                sides[ld.sidenum[0]].toptexture := sides[sld.sidenum[0]].toptexture;
                break;
              end;
              if sides[sld.sidenum[0]].toptexture > 0 then
              begin
                sides[ld.sidenum[0]].toptexture := sides[sld.sidenum[1]].toptexture;
                break;
              end;
              inc(sld);
            end;
          end;
        end;

    end;
    Inc(ld);
  end;

end;


end.
 