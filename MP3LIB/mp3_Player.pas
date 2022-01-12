//------------------------------------------------------------------------------
//
//  DelphiDoom is a source port of the game Doom and it is
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
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

(*
 *  File:     $RCSfile: Player.pas,v $
 *  Revision: $Revision: 1.1.1.1 $
 *  Version : $Id: Player.pas,v 1.1.1.1 2002/04/21 12:57:22 fobmagog Exp $
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
unit mp3_Player;

interface

uses
  d_delphi,
  mp3_OBuffer,
  mp3_Shared,
  mp3_Header;

type
  TPlayer = class
  protected
    function GetPosition: Integer; virtual; abstract;
    function GetLength: Integer; virtual; abstract;
    function GetMode: TMode; virtual; abstract;
    function GetChannels: TChannels; virtual; abstract;
    function GetVersion: TVersion; virtual; abstract;
    function GetLayer: Integer; virtual; abstract;
    function GetFrequency: Cardinal; virtual; abstract;
    function GetBitrate: Integer; virtual; abstract;
    function GetIsPlaying: Boolean; virtual; abstract;
    function GetDoRepeat: Boolean; virtual; abstract;
    procedure SetDoRepeat(Value: Boolean); virtual; abstract;

  public
    property Position: Integer read GetPosition;
    property Length: Integer read GetLength;
    property Mode: TMode read GetMode;
    property Channels: TChannels read GetChannels;
    property Version: TVersion read GetVersion;
    property Layer: Integer read GetLayer;
    property Frequency: Cardinal read GetFrequency;
    property Bitrate: Integer read GetBitrate;
    property IsPlaying: Boolean read GetIsPlaying;
    property DoRepeat: Boolean read GetDoRepeat write SetDoRepeat;

    procedure LoadStream(AStream: TDStream); virtual; abstract;
    procedure SetOutput(Output: TOBuffer); virtual; abstract;
    procedure Play; virtual; abstract;
    procedure Pause; virtual; abstract;
    procedure Resume; virtual; abstract;
    procedure Stop; virtual; abstract;
  end;

implementation

end.
