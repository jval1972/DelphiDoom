//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2021 by Jim Valavanis
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
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
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
// JVAL: Try to adjust missing textures
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
  if i = 740 then
  begin
    inc(ld);
    dec(ld);
  end;
    if (ld.frontsector <> nil) and (ld.backsector <> nil) then
      if (ld.sidenum[0] >= 0) and (ld.sidenum[1] >= 0) then
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
            for j := 0 to sec.linecount - 1 do
            begin
              sld := sec.lines[j];
              if sld.frontsector = ld.frontsector then
                if sld.sidenum[1] >= 0 then
                  if sides[sld.sidenum[1]].toptexture > 0 then
                  begin
                    sides[ld.sidenum[1]].toptexture := sides[sld.sidenum[1]].toptexture;
                    break;
                  end;
            end;
            if sides[ld.sidenum[1]].toptexture = 0 then
            begin
              for j := 0 to sec.linecount - 1 do
              begin
                sld := sec.lines[j];
                if sld.sidenum[1] >= 0 then
                  if sides[sld.sidenum[1]].toptexture > 0 then
                  begin
                    sides[ld.sidenum[1]].toptexture := sides[sld.sidenum[1]].toptexture;
                    break;
                  end;
                if sld.sidenum[0] >= 0 then
                  if sides[sld.sidenum[0]].toptexture > 0 then
                  begin
                    sides[ld.sidenum[1]].toptexture := sides[sld.sidenum[0]].toptexture;
                    break;
                  end;
              end;
            end;
          end;

        if (ld.frontsector.floorheight > ld.backsector.floorheight) then
          if sides[ld.sidenum[1]].bottomtexture = 0 then
          begin
            sec := ld.backsector;
            for j := 0 to sec.linecount - 1 do
            begin
              sld := sec.lines[j];
              if sld.frontsector = ld.frontsector then
                if sld.sidenum[1] >= 0 then
                  if sides[sld.sidenum[1]].bottomtexture > 0 then
                  begin
                    sides[ld.sidenum[1]].bottomtexture := sides[sld.sidenum[1]].bottomtexture;
                    break;
                  end;
            end;
            if sides[ld.sidenum[1]].bottomtexture = 0 then
            begin
              for j := 0 to sec.linecount - 1 do
              begin
                sld := sec.lines[j];
                if sld.sidenum[1] >= 0 then
                  if sides[sld.sidenum[1]].bottomtexture > 0 then
                  begin
                    sides[ld.sidenum[1]].bottomtexture := sides[sld.sidenum[1]].bottomtexture;
                    break;
                  end;
                if sld.sidenum[0] >= 0 then
                  if sides[sld.sidenum[0]].bottomtexture > 0 then
                  begin
                    sides[ld.sidenum[1]].bottomtexture := sides[sld.sidenum[0]].bottomtexture;
                    break;
                  end;
              end;
            end;
          end;

        if (ld.frontsector.ceilingheight > ld.backsector.ceilingheight) then
          if sides[ld.sidenum[0]].toptexture = 0 then
          begin
            sec := ld.frontsector;
            for j := 0 to sec.linecount - 1 do
            begin
              sld := sec.lines[j];
              if sld.backsector = ld.backsector then
                if sld.sidenum[0] >= 0 then
                  if sides[sld.sidenum[0]].toptexture > 0 then
                  begin
                    sides[ld.sidenum[0]].toptexture := sides[sld.sidenum[0]].toptexture;
                    break;
                  end;
            end;
            if sides[ld.sidenum[0]].toptexture = 0 then
            begin
              for j := 0 to sec.linecount - 1 do
              begin
                sld := sec.lines[j];
                if sld.sidenum[0] >= 0 then
                  if sides[sld.sidenum[0]].toptexture > 0 then
                  begin
                    sides[ld.sidenum[0]].toptexture := sides[sld.sidenum[0]].toptexture;
                    break;
                  end;
                if sld.sidenum[1] >= 0 then
                  if sides[sld.sidenum[1]].toptexture > 0 then
                  begin
                    sides[ld.sidenum[0]].toptexture := sides[sld.sidenum[1]].toptexture;
                    break;
                  end;
              end;
            end;
          end;

        if (ld.frontsector.floorheight < ld.backsector.floorheight) then
          if sides[ld.sidenum[0]].bottomtexture = 0 then
          begin
            sec := ld.frontsector;
            for j := 0 to sec.linecount - 1 do
            begin
              sld := sec.lines[j];
              if sld.backsector = ld.backsector then
                if sld.sidenum[0] >= 0 then
                  if sides[sld.sidenum[0]].bottomtexture > 0 then
                  begin
                    sides[ld.sidenum[0]].bottomtexture := sides[sld.sidenum[0]].bottomtexture;
                    break;
                  end;
            end;
            if sides[ld.sidenum[0]].bottomtexture = 0 then
            begin
              for j := 0 to sec.linecount - 1 do
              begin
                sld := sec.lines[j];
                if sld.sidenum[0] >= 0 then
                  if sides[sld.sidenum[0]].bottomtexture > 0 then
                  begin
                    sides[ld.sidenum[0]].bottomtexture := sides[sld.sidenum[0]].bottomtexture;
                    break;
                  end;
                if sld.sidenum[1] >= 0 then
                  if sides[sld.sidenum[1]].bottomtexture > 0 then
                  begin
                    sides[ld.sidenum[0]].bottomtexture := sides[sld.sidenum[1]].bottomtexture;
                    break;
                  end;
              end;
            end;
          end;

      end;
    inc(ld);
  end;

end;

end.
