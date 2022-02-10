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
//  THINKER keyword for ACTORDEF lumps.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit sc_thinker;

interface

type
  rtl_thinker_t = record
    name: string[255];
    script: string[255];
    dn: integer;
    repeatcnt: integer;
    interval: integer;
  end;
  Prtl_thinker_t = ^rtl_thinker_t;

//==============================================================================
//
// SC_SubmitThinker
//
//==============================================================================
procedure SC_SubmitThinker(const th: Prtl_thinker_t);

implementation

uses
  d_delphi,
  i_system,
  sc_actordef;

//==============================================================================
//
// SC_SubmitThinker
//
//==============================================================================
procedure SC_SubmitThinker(const th: Prtl_thinker_t);
var
  res: string;
  i: integer;

  procedure AddRes(const x: string);
  begin
    res := res + #13#10 + x;
  end;

begin
  if th.repeatcnt < 0 then
  begin
    I_Warning('SC_SubmitThinker(): Thinker %s has negative repeat count=%d'#13#10, [th.name, th.repeatcnt]);
    Exit;
  end;

  if th.interval < 1 then
  begin
    I_Warning('SC_SubmitThinker(): Thinker %s has invalid interval=%d'#13#10, [th.name, th.interval]);
    Exit;
  end;

  res := '';
  AddRes('actor ' + th.name + ' ' + itoa(th.dn));
  AddRes('{');
  AddRes('    Health 10000');
  AddRes('    Radius 128');
  AddRes('    Height 128');
  AddRes('    Speed 0');
  AddRes('    Mass 1000');
  AddRes('    DONTDRAW');
  AddRes('    States');
  AddRes('    {');
  AddRes('    Spawn:');
  if th.repeatcnt = 0 then
  begin
    AddRes('        NULL A ' + itoa(th.interval) + ' A_RunScript(' + th.script + ')');
    AddRes('        Loop');
  end
  else
  begin
    for i := 0 to th.repeatcnt - 1 do
      AddRes('        NULL A ' + itoa(th.interval) + ' A_RunScript(' + th.script + ')');
    AddRes('        Stop');
  end;
  AddRes('    }');
  SC_ParseActordefLump(res);
end;

end.
