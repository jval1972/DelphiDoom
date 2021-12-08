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
// DESCRIPTION:
//  DOOM strings, by language.
//
// DESCRIPTION:
// Globally defined strings.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit dstrings;

interface

// Misc. other strings.
var
  SAVEGAMENAME: string = 'doomsav';

const
//
// File locations,
// relative to current position.
// Path names are OS-sensitive.
//
  DEVMAPS = 'devmaps\';
  DEVDATA = 'devdata\';

// Not done in french?

// QuitDOOM messages
  NUM_QUITMESSAGES = 22;
// Start-up messages
  NUM_STARTUPMESSAGES = 5;

var
  endmsg: array[0..NUM_QUITMESSAGES] of string;
  startmsg: array[0..NUM_STARTUPMESSAGES - 1] of string;

implementation

uses
  d_englsh;

initialization

  // DOOM1
  endmsg[0] := QUITMSG;
  endmsg[1] := 'please don''t leave, there''s more' + #13#10 + 'demons to toast!';
  endmsg[2] := 'let''s beat it -- this is turning' + #13#10 + 'into a bloodbath!';
  endmsg[3] := 'i wouldn''t leave if i were you.' + #13#10 + 'dos is much worse.';
  endmsg[4] := 'you''re trying to say you like dos' + #13#10 + 'better than me, right?';
  endmsg[5] := 'don''t leave yet -- there''s a' + #13#10 + 'demon around that corner!';
  endmsg[6] := 'ya know, next time you come in here' + #13#10 + 'i''m gonna toast ya.';
  endmsg[7] := 'go ahead and leave. see if i care.';

  // QuitDOOM II messages
  endmsg[8] := 'you want to quit?' + #13#10 + 'then, thou hast lost an eighth!';
  endmsg[9] := 'don''t go now, there''s a ' + #13#10 + 'dimensional shambler waiting' + #13#10 + 'at the dos prompt!';
  endmsg[10] := 'get outta here and go back' + #13#10 + 'to your boring programs.';
  endmsg[11] := 'if i were your boss, i''d ' + #13#10 + ' deathmatch ya in a minute!';
  endmsg[12] := 'look, bud. you leave now' + #13#10 + 'and you forfeit your body count!';
  endmsg[13] := 'just leave. when you come' + #13#10 + 'back, i''ll be waiting with a bat.';
  endmsg[14] := 'you''re lucky i don''t smack' + #13#10 + 'you for thinking about leaving.';

  // FinalDOOM?
  endmsg[15] := 'fuck you, pussy!' + #13#10 + 'get the fuck out!';
  endmsg[16] := 'you quit and i''ll jizz' + #13#10 + 'in your cystholes!';
  endmsg[17] := 'if you leave, i''ll make' + #13#10 + 'the lord drink my jizz.';
  endmsg[18] := 'hey, ron! can we say' + #13#10 + '''fuck'' in the game?';
  endmsg[19] := 'i''d leave: this is just' + #13#10 + 'more monsters and levels.' + #13#10 + 'what a load.';
  endmsg[20] := 'suck it down, asshole!' + #13#10 + 'you''re a fucking wimp!';
  endmsg[21] := 'don''t quit now! we''re ' + #13#10 + 'still spending your money!';

  // Internal debug. Different style, too.
  endmsg[22] := 'THIS IS NO MESSAGE!' + #13#10 + 'Page intentionally left blank.';

  startmsg[0] := '';
  startmsg[1] := '';
  startmsg[2] := '';
  startmsg[3] := '';
  startmsg[4] := '';

end.

