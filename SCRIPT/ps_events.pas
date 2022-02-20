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
//  Pascal Script Events
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit ps_events;

interface

uses
  ps_import,
  ps_compiler,
  ps_utils;

const
  EventExportedProcs: array [0..9] of record
    Name: AnsiString;
    ParamCount: Byte;
    Typ: array [0..4] of Byte;
    Dir: array [0..3] of TPSParameterMode;
    {$IFDEF DLL}
    Template: AnsiString;
    {$ENDIF}
  end = // Must be in sync with TScriptEvents
  (
    (
      Name: 'ONACTORDIED';
      ParamCount: 2;
      Typ: (0, btU32, btU32, 0,     0    );
      Dir: (pmIn, pmIn, pmIn, pmIn){$IFDEF DLL};
      Template: 'event OnActorDied(const actorKEY: LongWord; const killer: LongWord);|' +
                'begin|' +
                '// Executes every time an actor dies, before dropping the dropitem|' +
                '  |' +
                'end;||'{$ENDIF}
    ),
    (
      Name: 'ONPLAYERENTER';
      ParamCount: 1;
      Typ: (0, btS32, 0,     0,     0    );
      Dir: (pmIn, pmIn, pmIn, pmIn){$IFDEF DLL};
      Template: 'event OnPlayerEnter(const playerNO: integer);|' +
                'begin|' +
                '// Executes the first time a player spawns to the map, just after OnMapStart Event|' +
                '  |' +
                'end;||'{$ENDIF}
    ),
    (
      Name: 'ONPLAYERDIED';
      ParamCount: 2;
      Typ: (0, btS32, btU32, 0,     0    );
      Dir: (pmIn, pmIn, pmIn, pmIn){$IFDEF DLL};
      Template: 'event OnPlayerDied(const playerNO: integer; const killer: LongWord);|' +
                'begin|' +
                '// Executes every time a player dies, before OnActorDied event|' +
                '  |' +
                'end;||'{$ENDIF}
    ),
    (
      Name: 'ONCROSSLINE';
      ParamCount: 3;
      Typ: (0, btU32, btS32, btS32, 0    );
      Dir: (pmIn, pmIn, pmIn, pmIn){$IFDEF DLL};
      Template: 'event OnCrossLine(const actorKEY: LongWord; const lineNO: Integer;|' +
                '  const oldside: Integer);|' +
                'begin|' +
                '// Executes when the actor "actorKEY" crosses the line "lineNO"|' +
                '// line must have the "Trigger PascalScript" setting enabled|' +
                '// actor must not have the MF2_EX_DONTRUNSCRIPTS flag|' +
                '  |' +
                'end;||'{$ENDIF}
    ),
    (
      Name: 'ONSHOOTLINE';
      ParamCount: 3;
      Typ: (0, btU32, btS32, btS32, 0    );
      Dir: (pmIn, pmIn, pmIn, pmIn){$IFDEF DLL};
      Template: 'event OnShootLine(const actorKEY: LongWord; const lineNO: Integer;|' +
                '  const side: Integer);|' +
                'begin|' +
                '// Executes when the actor "actorKEY" shoots the line "lineNO"|' +
                '// line must have the "Trigger PascalScript" setting enabled|' +
                '// actor must not have the MF2_EX_DONTRUNSCRIPTS flag|' +
                '  |' +
                'end;||'{$ENDIF}
    ),
    (
      Name: 'ONUSELINE';
      ParamCount: 3;
      Typ: (0, btU32, btS32, btS32, 0    );
      Dir: (pmIn, pmIn, pmIn, pmIn){$IFDEF DLL};
      Template: 'event OnUseLine(const actorKEY: LongWord; const lineNO: Integer;|' +
                '  const side: Integer);|' +
                'begin|' +
                '// Executes when the actor "actorKEY" uses the line "lineNO"|' +
                '// line must have the "Trigger PascalScript" setting enabled|' +
                '// actor must not have the MF2_EX_DONTRUNSCRIPTS flag|' +
                '  |' +
                'end;||'{$ENDIF}
    ),
    (
      Name: 'ONTICK';
      ParamCount: 1;
      Typ: (0, btS32, 0,     0,     0    );
      Dir: (pmIn, pmIn, pmIn, pmIn){$IFDEF DLL};
      Template: 'event OnTick(const tick: Integer);|' +
                'begin|' +
                '// Executes in every tick (35 times/second), after running all thinkers|' +
                '  |' +
                'end;||'{$ENDIF}
    ),
    (
      Name: 'ONTIMEREVERYSECOND';
      ParamCount: 1;
      Typ: (0, btS32, 0,     0,     0    );
      Dir: (pmIn, pmIn, pmIn, pmIn){$IFDEF DLL};
      Template: 'event OnTimerEverySecond(const second: Integer);|' +
                'begin|' +
                '// Executes every second (35 ticks), after OnTick Event|' +
                '  |' +
                'end;||'{$ENDIF}
    ),
    (
      Name: 'ONTIMEREVERYMINUTE';
      ParamCount: 1;
      Typ: (0, btS32, 0,     0,     0    );
      Dir: (pmIn, pmIn, pmIn, pmIn){$IFDEF DLL};
      Template: 'event OnTimerEveryMinute(const minute: integer);|' +
                'begin|' +
                '// Executes every minute (35 * 60 ticks), after OnTimerEverySecond Event|' +
                '  |' +
                'end;||'{$ENDIF}
    ),
    (
      Name: 'ONMAPSTART';
      ParamCount: 0;
      Typ: (0, 0,     0,     0,     0    );
      Dir: (pmIn, pmIn, pmIn, pmIn){$IFDEF DLL};
      Template: 'event OnMapStart;|' +
                'begin|' +
                '// Executes the first time the map loads, before running thinkers|' +
                '  |' +
                'end;||'{$ENDIF}
    )
  );

type
  TScriptEvents = class(TObject)
  private
    fExec: TDoomExec;
    fProcActorDied: TMethod;
    fProcPlayerEnter: TMethod;
    fProcPlayerDied: TMethod;
    fProcCrossLine: TMethod;
    fProcShootLine: TMethod;
    fProcUseLine: TMethod;
    fProcTick: TMethod;
    fProcTimerEverySecond: TMethod;
    fProcTimerEveryMinute: TMethod;
    fProcMapStart: TMethod;
    procedure RunProc(const aProc: TMethod; const aParams: array of Integer);
  public
    constructor Create(const aExec: TDoomExec);
    procedure Clear;
    procedure LinkEvents;
    // Must be in sync with ScriptOnExportCheck and ps_main events
    procedure ProcActorDied(actor: LongWord; killer: LongWord);
    procedure ProcPlayerEnter(playerNO: Integer);
    procedure ProcPlayerDied(playerNO: Integer; killer: LongWord);
    procedure ProcCrossLine(actor: LongWord; line: Integer; oldside: Integer);
    procedure ProcShootLine(actor: LongWord; line: Integer; oldside: Integer);
    procedure ProcUseLine(actor: LongWord; line: Integer; oldside: Integer);
    procedure ProcTick(tick: Integer);
    procedure ProcTimerEverySecond(second: Integer);
    procedure ProcTimerEveryMinute(minute: Integer);
    procedure ProcMapStart;
  end;

implementation

uses
  i_system,
  SysUtils,
  ps_runtime;

type
  TScriptEvent = procedure of object;
  TScriptEvent1I = procedure (aParam1: Integer) of object;
  TScriptEvent2I = procedure (aParam1, aParam2: Integer) of object;
  TScriptEvent3I = procedure (aParam1, aParam2, aParam3: Integer) of object;
  TScriptEvent4I = procedure (aParam1, aParam2, aParam3, aParam4: Integer) of object;

//==============================================================================
//
// TScriptEvents.Create
//
//==============================================================================
constructor TScriptEvents.Create(const aExec: TDoomExec);
begin
  fExec := aExec;
  Clear;
  inherited Create;
end;

//==============================================================================
//
// TScriptEvents.Clear
//
//==============================================================================
procedure TScriptEvents.Clear;
begin
  fProcActorDied.Code := nil;
  fProcPlayerEnter.Code := nil;
  fProcPlayerDied.Code := nil;
  fProcCrossLine.Code := nil;
  fProcShootLine.Code := nil;
  fProcUseLine.Code := nil;
  fProcTick.Code := nil;
  fProcTimerEverySecond.Code := nil;
  fProcTimerEveryMinute.Code := nil;
  fProcMapStart.Code := nil;
end;

//==============================================================================
//
// TScriptEvents.LinkEvents
//
//==============================================================================
procedure TScriptEvents.LinkEvents;
begin
  fProcActorDied := fExec.GetProcAsMethodN('ONACTORDIED');
  fProcPlayerEnter := fExec.GetProcAsMethodN('ONPLAYERENTER');
  fProcPlayerDied := fExec.GetProcAsMethodN('ONPLAYERDIED');
  fProcCrossLine := fExec.GetProcAsMethodN('ONCROSSLINE');
  fProcShootLine := fExec.GetProcAsMethodN('ONSHOOTLINE');
  fProcUseLine := fExec.GetProcAsMethodN('ONUSELINE');
  fProcTick := fExec.GetProcAsMethodN('ONTICK');
  fProcTimerEverySecond := fExec.GetProcAsMethodN('ONTIMEREVERYSECOND');
  fProcTimerEveryMinute := fExec.GetProcAsMethodN('ONTIMEREVERYMINUTE');
  fProcMapStart := fExec.GetProcAsMethodN('ONMAPSTART');
end;

//==============================================================================
//
// TScriptEvents.RunProc
//
//==============================================================================
procedure TScriptEvents.RunProc(const aProc: TMethod; const aParams: array of Integer);
var
  ExceptionProc: TPSProcRec;
  s: string;
begin
  if aProc.Code = nil then
    Exit;

  try
    case Length(aParams) of
      0: TScriptEvent(aProc);
      1: TScriptEvent1I(aProc)(aParams[0]);
      2: TScriptEvent2I(aProc)(aParams[0], aParams[1]);
      3: TScriptEvent3I(aProc)(aParams[0], aParams[1], aParams[2]);
      4: TScriptEvent4I(aProc)(aParams[0], aParams[1], aParams[2], aParams[3]);
    else
      I_Warning('TScriptEvents.RunProc(): Invalid number of parameters %d'#13#10, [Length(aParams)]);
      Exit;
    end;
  except
    on E: Exception do
    begin
      s := 'Exception in script: ''' + E.Message + '''';
      ExceptionProc := fExec.GetProcNo(fExec.ExceptionProcNo);
      if ExceptionProc is TPSInternalProcRec then
        s := s + ' in procedure ''' + TPSInternalProcRec(ExceptionProc).ExportName + '''';
      I_Warning('TScriptEvents.RunProc():' + S);
    end;
  end;
end;

//==============================================================================
//
// TScriptEvents.ProcActorDied
//
//==============================================================================
procedure TScriptEvents.ProcActorDied(actor: LongWord; killer: LongWord);
begin
  RunProc(fProcActorDied, [actor, killer]);
end;

//==============================================================================
//
// TScriptEvents.ProcPlayerEnter
//
//==============================================================================
procedure TScriptEvents.ProcPlayerEnter(playerNO: Integer);
begin
  RunProc(fProcPlayerEnter, [playerNO]);
end;

//==============================================================================
//
// TScriptEvents.ProcPlayerDied
//
//==============================================================================
procedure TScriptEvents.ProcPlayerDied(playerNO: Integer; killer: LongWord);
begin
  RunProc(fProcPlayerDied, [playerNO, killer]);
end;

//==============================================================================
//
// TScriptEvents.ProcCrossLine
//
//==============================================================================
procedure TScriptEvents.ProcCrossLine(actor: LongWord; line: Integer; oldside: Integer);
begin
  RunProc(fProcCrossLine, [actor, line, oldside]);
end;

//==============================================================================
//
// TScriptEvents.ProcShootLine
//
//==============================================================================
procedure TScriptEvents.ProcShootLine(actor: LongWord; line: Integer; oldside: Integer);
begin
  RunProc(fProcShootLine, [actor, line, oldside]);
end;

//==============================================================================
//
// TScriptEvents.ProcUseLine
//
//==============================================================================
procedure TScriptEvents.ProcUseLine(actor: LongWord; line: Integer; oldside: Integer);
begin
  RunProc(fProcUseLine, [actor, line, oldside]);
end;

//==============================================================================
//
// TScriptEvents.ProcTick
//
//==============================================================================
procedure TScriptEvents.ProcTick(tick: Integer);
begin
  RunProc(fProcTick, [tick]);
end;

//==============================================================================
//
// TScriptEvents.ProcTimerEverySecond
//
//==============================================================================
procedure TScriptEvents.ProcTimerEverySecond(second: Integer);
begin
  RunProc(fProcTimerEverySecond, [second]);
end;

//==============================================================================
//
// TScriptEvents.ProcTimerEveryMinute
//
//==============================================================================
procedure TScriptEvents.ProcTimerEveryMinute(minute: Integer);
begin
  RunProc(fProcTimerEveryMinute, [minute]);
end;

//==============================================================================
//
// TScriptEvents.ProcMapStart
//
//==============================================================================
procedure TScriptEvents.ProcMapStart;
begin
  RunProc(fProcMapStart, []);
end;

end.

