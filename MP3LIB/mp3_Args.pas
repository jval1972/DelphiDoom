//------------------------------------------------------------------------------
//
//  DelphiDoom is a source port of the game Doom and it is
//  based on original Linux Doom as published by "id Software"
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
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

(*
 *  File:     $RCSfile: Args.pas,v $
 *  Revision: $Revision: 1.1.1.1 $
 *  Version : $Id: Args.pas,v 1.1.1.1 2002/04/21 12:57:16 fobmagog Exp $
 *  Author:   $Author: fobmagog $
 *  Homepage: http://delphimpeg.sourceforge.net/
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *)
unit mp3_Args;

interface
uses
  MMSystem, mp3_BitStream, mp3_Header, mp3_Shared;

type
  TOutput = (WAVEMAPPER, DIRECTSOUND, WAVEFILE);

  TArgs = class
  public
    Stop: Boolean;
    Pause: Boolean;
    Done: Boolean;
    NonSeekable: Boolean;
    DesiredPosition: Integer;
    PositionChange: Boolean;
    PlayMutex: THandle;

    constructor Create; virtual;
  end;

  // A class to contain arguments for maplay.
  TMPEGArgs = class(TArgs)
  private
    FErrorCode: Cardinal;

  public
    Stream: TBitStream;
    MPEGHeader: THeader;
    WhichC: TChannels;
    UseOwnScaleFactor: Boolean;
    ScaleFactor: Single;
    StartPos: Cardinal;  // start and finish positions (in frames)
    EndPos: Cardinal;
    MusicPos: Cardinal;  // current position (in frames)
    PlayMode: Cardinal;  // -1 - not initialized, 0 - closed, 1 - opened, 2 - stopped
                         //  3 - playing, 4 - paused

    phwo: HWAVEOUT;
    OutputMode: TOutput;
    OutputFileName: array[0..255] of char;

    constructor Create; override;

    function ErrorCode: Cardinal;
  end;

implementation

{ TArgs }

//==============================================================================
//
// TArgs.Create
//
//==============================================================================
constructor TArgs.Create;
begin
  Stop            := false;
  Pause           := false;
  Done            := false;
  NonSeekable     := false;
  DesiredPosition := 0;
  PositionChange  := false;
end;

{ TMPEGArgs }

//==============================================================================
//
// TMPEGArgs.Create
//
//==============================================================================
constructor TMPEGArgs.Create;
begin
  inherited Create;

  Stream := nil;
  MPEGHeader := nil;
  WhichC := Both;
  UseOwnScalefactor := false;
  ScaleFactor := 32768.0;

  OutputMode := WAVEFILE;
  OutputFileName := '';
end;

//==============================================================================
//
// TMPEGArgs.ErrorCode
//
//==============================================================================
function TMPEGArgs.ErrorCode: Cardinal;
begin
  result := FErrorcode;
  FErrorcode := 0;
end;

end.
