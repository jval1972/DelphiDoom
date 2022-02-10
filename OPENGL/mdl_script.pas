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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//  DESCRIPTION:
//  Script main execute function for DDMODELs
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit mdl_script;

interface

uses
  SysUtils,
  Classes,
  i_system;

//==============================================================================
//
// MDL_ExecuteScript
//
//==============================================================================
function MDL_ExecuteScript(const Script: string): boolean;

implementation

uses
  d_delphi,
  mdl_script_proclist,
  mdl_script_functions,
  ps_keywords,
  ps_compiler,
  ps_runtime;

//==============================================================================
//
// ScriptOnUses
//
//==============================================================================
function ScriptOnUses(Sender: TPSPascalCompiler; const Name: string): Boolean;
var
  uT_integer: TPSType;
  uT_longword: TPSType;
  uT_extended: TPSType;
begin
  if Name = 'SYSTEM' then
  begin
    Sender.AddTypeS('TIntegerArray', 'array of integer');
    Sender.AddTypeS('TInt64Array', 'array of int64');
    Sender.AddTypeS('TLongWordArray', 'array of LongWord');
    Sender.AddTypeS('TSingleArray', 'array of single');
    Sender.AddTypeS('TFloatArray', 'array of single');
    Sender.AddTypeS('TDoubleArray', 'array of double');
    Sender.AddTypeS('TExtendedArray', 'array of extended');
    Sender.AddTypeS('float', 'single');
    Sender.AddTypeS('real', 'double');
    Sender.AddTypeS('GLenum', 'longword');
    Sender.AddTypeS('GLboolean', 'boolean');
    Sender.AddTypeS('GLbitfield', 'longword');
    Sender.AddTypeS('GLbyte', 'Shortint');
    Sender.AddTypeS('GLshort', 'SmallInt');
    Sender.AddTypeS('GLint', 'Integer');
    Sender.AddTypeS('GLsizei', 'Integer');
    Sender.AddTypeS('GLubyte', 'Byte');
    Sender.AddTypeS('GLushort', 'Word');
    Sender.AddTypeS('GLuint', 'longword');
    Sender.AddTypeS('GLfloat', 'Single');
    Sender.AddTypeS('GLclampf', 'Single');
    Sender.AddTypeS('GLdouble', 'Double');
    Sender.AddTypeS('GLclampd', 'Double');
    Sender.AddTypeS('GLint64', 'int64');

    uT_extended := Sender.FindType('extended');
    uT_integer := Sender.FindType('integer');
    uT_longword := Sender.FindType('longword');
    Sender.AddConstant('NaN', uT_extended).Value.textended := 0.0 / 0.0;
    Sender.AddConstant('Infinity', uT_extended).Value.textended := 1.0 / 0.0;
    Sender.AddConstant('NegInfinity', uT_extended).Value.textended := - 1.0 / 0.0;
    Sender.AddConstant('MAXINT', uT_integer).Value.ts32 := MAXINT;

    Sender.AddConstant('GL_LINES', uT_longword).Value.tu32 := $0001;
    Sender.AddConstant('GL_LINE_LOOP', uT_longword).Value.tu32 := $0002;
    Sender.AddConstant('GL_LINE_STRIP', uT_longword).Value.tu32 := $0003;
    Sender.AddConstant('GL_TRIANGLES', uT_longword).Value.tu32 := $0004;
    Sender.AddConstant('GL_TRIANGLE_STRIP', uT_longword).Value.tu32 := $0005;
    Sender.AddConstant('GL_TRIANGLE_FAN', uT_longword).Value.tu32 := $0006;
    Sender.AddConstant('GL_QUADS', uT_longword).Value.tu32 := $0007;
    Sender.AddConstant('GL_QUAD_STRIP', uT_longword).Value.tu32 := $0008;
    Sender.AddConstant('GL_POLYGON', uT_longword).Value.tu32 := $0009;
    Sender.AddConstant('GL_MODELVIEW', uT_longword).Value.tu32 := $1700;
    Sender.AddConstant('GL_PROJECTION', uT_longword).Value.tu32 := $1701;
    Sender.AddConstant('GL_TEXTURE', uT_longword).Value.tu32 := $1702;
    Sender.AddConstant('GL_COLOR', uT_longword).Value.tu32 := $1800;

    MDL_RegisterProcsCompiler(Sender);

    Result := True;
  end
  else
    Result := False;
end;

//==============================================================================
//
// MDL_ExecuteScript
//
//==============================================================================
function MDL_ExecuteScript(const Script: string): boolean;
var
  i: integer;
  Compiler: TPSPascalCompiler;
  Exec: TPSExec;
  {$IFDEF UNICODE}Data: AnsiString;{$ELSE}Data: string;{$ENDIF}
begin
  MDL_InitProcList;

  Compiler := TPSPascalCompiler.Create(@DDModelScriptLookupTable, DDMODELSCRIPT_KEYWORD_COUNT); // create an instance of the compiler.
  Compiler.OnUses := ScriptOnUses; // assign the OnUses event.
  if not Compiler.Compile(Script) then  // Compile the Pascal script into bytecode.
  begin
    for i := 0 to Compiler.MsgCount - 1 do
      I_Warning(Format('MDL_ExecuteScript(): Pos: %.*d, "%s"', [6,  compiler.Msg[i].Pos, compiler.Msg[i].MessageToString]));

    Compiler.Free;
    MDL_ShutDownProcList;
    Result := False;
    Exit;
  end;

  Compiler.GetOutput(Data); // Save the output of the compiler in the string Data.
  Compiler.Free; // After compiling the script, there is no need for the compiler anymore.

  printf('Compile successful'#13#10);

  Exec := TPSExec.Create;  // Create an instance of the executer.

  MDL_RegisterProcsExec(Exec);

  if not Exec.LoadData(Data) then // Load the data from the Data string.
  begin
    I_Warning('MDL_ExecuteScript(): Can not run script, internal error!');
    { For some reason the script could not be loaded. This is usually the case when a
      library that has been used at compile time isn't registered at runtime. }
    Exec.Free;
     // You could raise an exception here.
    MDL_ShutDownProcList;
    Result := False;
    Exit;
  end;

  printf('Running...'#13#10);
  Exec.RunScript; // Run the script.
  Exec.Free; // Free the executer.

  MDL_ShutDownProcList;

  Result := True;

  printf('Done'#13#10);
end;

end.

