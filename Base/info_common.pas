//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2018 by Jim Valavanis
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
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit info_common;

interface

uses
  d_delphi;

function Info_GetMobjNumForDoomNum(const dn: integer): integer;

procedure Info_InitDnLookUp;

procedure Info_ShutDownDnLookUp;

procedure Info_CheckStates;

var
  dnLookUp: PLongWordArray = nil; // JVAL: Doom Editor Number LookUp

const
  DNLOOKUPSIZE = $10000;

implementation

uses
  i_system,
  info,
  info_h;

procedure Info_InitDnLookUp;
begin
  if dnLookUp = nil then
    dnLookUp := mallocz(DNLOOKUPSIZE * SizeOf(LongWord));
end;

procedure Info_ShutDownDnLookUp;
begin
  memfree(pointer(dnLookUp), DNLOOKUPSIZE * SizeOf(LongWord));
end;

function Info_GetMobjNumForDoomNum(const dn: integer): integer;
var
  i: integer;
  idx: integer;
begin
  if dnLookUp <> nil then
  begin
    idx := dn mod DNLOOKUPSIZE;
    idx := dnLookUp[idx];
    if idx < nummobjtypes then
      if mobjinfo[idx].doomednum = dn then
      begin
        result := idx;
        Exit;
      end;
  end;

  for i := 0 to nummobjtypes - 1 do
  begin
    if mobjinfo[i].doomednum = dn then
    begin
      result := i;
      if dnLookUp <> nil then
        dnLookUp[dn mod DNLOOKUPSIZE] := i;
      Exit;
    end;
  end;
  result := -1;
end;

procedure Info_CheckStates;
var
  i: integer;
  loops: integer;
  st: Pstate_t;
begin
  for i := 0 to numstates - 1 do
  begin
    loops := 0;
    st := @states[i];
    repeat
      if st.nextstate = S_NULL then
        break;
      st := @states[Ord(st.nextstate)];
      inc(loops);
      if loops > $FFFF then
      begin
        I_Warning('Info_CheckStates(): State %d has possible infinite loop.'#13#10, [i]);
        break;
      end;
    until st.tics <> 0;
  end;
end;

end.
