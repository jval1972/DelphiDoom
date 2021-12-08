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

unit p_obituaries;

interface

uses
  p_mobj_h;

procedure P_Obituary(const victim, inflictor, attacker: Pmobj_t);

var
  show_obituaries: boolean = false;

implementation

uses
  SysUtils,
  d_delphi,
  d_player,
  p_common,
  p_gender,
  w_wad;

procedure P_Obituary(const victim, inflictor, attacker: Pmobj_t);
var
  pv, pif, pa: Pplayer_t;
  messagefmt: string;
  vname, aname: string;
  agender: gender_t;
  lst: TDStringList;
  lump: integer;
  i: integer;
  s1, s2: string;
  check: string;
begin
  if not show_obituaries then
    exit;

  if victim = nil then  // Sanity check
    exit;

  if attacker = nil then
    exit;

  pv := victim.player;
  if pv = nil then // No obituaries for non players
    exit;

  if pv.mo <> victim then // No obituaries for voodoo dolls
    exit;

  if inflictor <> nil then
  begin
    pif := inflictor.player;
    if pif <> nil then
      if pif.mo <> inflictor then // Treat voodoo dolls as unknown deaths
        exit;
  end;

  messagefmt := '';

  if (inflictor = attacker) or (attacker.info.hitobituary = '') then
    messagefmt := attacker.info.obituary
  else
    messagefmt := attacker.info.hitobituary;

  if messagefmt = '' then
    exit;

  if Pos('$OB_', strupper(messagefmt)) = 1 then
  begin
    lst := TDStringList.Create;
    try
      lump := W_CheckNumForName('OBITUARY');
      if lump >= 0 then
      begin
        lst.Text := strupper(W_TextLumpNum(lump));
        check := strtrim(strupper(Copy(messagefmt, 2, Length(messagefmt) - 1)));
        for i := 0 to lst.Count - 1 do
        begin
          splitstring(lst.Strings[i], s1, s2, '=');
          s1 := strtrim(s1);
          if s1 = check then
          begin
            messagefmt := s2;
            break;
          end;
        end;
      end;
    finally
      lst.Free
    end;
  end;

  vname := 'Player ' + itoa(PlayerToId(pv));

  pa := attacker.player;
  if pa <> nil then
    aname := 'Player ' + itoa(PlayerToId(pa))
  else
    aname := attacker.info.name;

  messagefmt := StringReplace(messagefmt, '%o', vname, [rfReplaceAll, rfIgnoreCase]);
  messagefmt := StringReplace(messagefmt, '%k', aname, [rfReplaceAll, rfIgnoreCase]);

  messagefmt := StringReplace(messagefmt, '%g', GENDERINFO[Ord(victim.info.gender)].ob_g, [rfReplaceAll, rfIgnoreCase]);

  agender := attacker.info.gender;
  messagefmt := StringReplace(messagefmt, '%h', GENDERINFO[Ord(agender)].ob_h, [rfReplaceAll, rfIgnoreCase]);
  messagefmt := StringReplace(messagefmt, '%p', GENDERINFO[Ord(agender)].ob_p, [rfReplaceAll, rfIgnoreCase]);
  messagefmt := StringReplace(messagefmt, '%s', GENDERINFO[Ord(agender)].ob_s, [rfReplaceAll, rfIgnoreCase]);
  messagefmt := StringReplace(messagefmt, '%r', GENDERINFO[Ord(agender)].ob_r, [rfReplaceAll, rfIgnoreCase]);

  pv._message := messagefmt;
end;

end.
