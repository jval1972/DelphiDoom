unit vxe_rotate;

 

interface

 

uses

  Windows, Classes, Graphics;

 

procedure RotateBitmap90DegreesCounterClockwise(var ABitmap: TBitmap);

procedure RotateBitmap90DegreesClockwise(var ABitmap: TBitmap);

procedure RotateBitmap180Degrees(var ABitmap: TBitmap);

 

implementation



(*$IFNDEF Win32*)

 

type

  DWORD = LongInt;

  TSelOfs = record

  L, H: Word;

  end;

 

procedure Win16Dec(var P: Pointer; const N: LongInt); forward;

 

procedure Win16Inc(var P: Pointer; const N: LongInt);

begin

  if N < 0 then

  Win16Dec(P, -N)

  else if N > 0 then begin

  Inc( TSelOfs(P).H, TSelOfs(N).H * SelectorInc );

  Inc( TSelOfs(P).L, TSelOfs(N).L );

  if TSelOfs(P).L < TSelOfs(N).L then Inc( TSelOfs(P).H, SelectorInc );

  end;

end;

 

procedure Win16Dec(var P: Pointer; const N: LongInt);

begin

  if N < 0 then

  Win16Inc(P, -N)

  else if N > 0 then begin

  if TSelOfs(N).L > TSelOfs(P).L then Dec( TSelOfs(P).H, SelectorInc );

  Dec( TSelOfs(P).L, TSelOfs(N).L );

  Dec( TSelOfs(P).H, TSelOfs(N).H * SelectorInc );

  end;

end;

 

(*

procedure HugeShift; far; external 'KERNEL' index 113;

procedure Win16Dec(var P: Pointer; const N: LongInt); forward;

 

procedure Win16Inc(var HugePtr: Pointer; Amount: LongInt);

  procedure HugeInc; assembler;

  asm

  mov ax, Amount.Word[0] { Store Amount in DX:AX. }

  mov dx, Amount.Word[2]

  les bx, HugePtr { Get the reference to HugePtr. }

  add ax, es:[bx] { Add the offset parts. }

  adc dx, 0 { Propagate carry to the high word of Amount. }

  mov cx, Offset HugeShift

  shl dx, cl { Shift high word of Amount for segment. }

  add es:[bx+2], dx { Increment the segment of HugePtr. }

  mov es:[bx], ax

  end;

 

begin

  if Amount > 0 then HugeInc else if Amount < 0 then Win16Dec(HugePtr, -Amount);

end;

 

procedure Win16Dec(var P: Pointer; const N: LongInt);

begin

  if N < 0 then

  Win16Inc(P, -N)

  else if N > 0 then begin

  if TSelOfs(N).L > TSelOfs(P).L then Dec( TSelOfs(P).H, SelectorInc );

  Dec( TSelOfs(P).L, TSelOfs(N).L );

  Dec( TSelOfs(P).H, TSelOfs(N).H * SelectorInc );

  end;

end;

*)

 

(*$ENDIF*)

 

procedure RotateBitmap90DegreesCounterClockwise(var ABitmap: TBitmap);

const

  BitsPerByte = 8;

 

var

  {

  A whole pile of variables. Some deal with one- and four-bit bitmaps only,

  some deal with eight- and 24-bit bitmaps only, and some deal with both.

  Any variable that ends in 'R' refers to the rotated bitmap, e.g. MemoryStream

  holds the original bitmap, and MemoryStreamR holds the rotated one.

  }

  PbmpInfoR: PBitmapInfoHeader;

  bmpBuffer, bmpBufferR: PByte;

  MemoryStream, MemoryStreamR: TMemoryStream;

  PbmpBuffer, PbmpBufferR: PByte;

  BytesPerPixel, PixelsPerByte: LongInt;

  BytesPerScanLine, BytesPerScanLineR: LongInt;

  PaddingBytes: LongInt;

  BitmapOffset: LongInt;

  BitCount: LongInt;

  WholeBytes, ExtraPixels: LongInt;

  SignificantBytes, SignificantBytesR: LongInt;

  ColumnBytes: LongInt;

  AtLeastEightBitColor: Boolean;

  T: LongInt;

 

procedure NonIntegralByteRotate; (* nested *)

{

 This routine rotates bitmaps with fewer than 8 bits of information per pixel,

 namely monochrome (1-bit) and 16-color (4-bit) bitmaps. Note that there are

 no such things as 2-bit bitmaps, though you might argue that Microsoft's bitmap

 format is worth about 2 bits.

}

var

  X, Y: LongInt;

  I: LongInt;

  MaskBits, CurrentBits: Byte;

  FirstMask, LastMask: Byte;

  PFirstScanLine: PByte;

  FirstIndex, CurrentBitIndex: LongInt;

  ShiftRightAmount, ShiftRightStart: LongInt;

 

begin

  (*$IFDEF Win32*)

  Inc(PbmpBuffer, BytesPerScanLine * (PbmpInfoR^.biHeight - 1) );

  (*$ELSE*)

  Win16Inc( Pointer(PbmpBuffer), BytesPerScanLine * (PbmpInfoR^.biHeight - 1) );

  (*$ENDIF*)

 

  { PFirstScanLine advances along the first scan line of bmpBufferR. }

  PFirstScanLine := bmpBufferR;

 

  { Set up the indexing. }

  FirstIndex := BitsPerByte - BitCount;

 

  {

  Set up the bit masks:

 

  For a monochrome bitmap,

  LastMask := 00000001 and

  FirstMask := 10000000

 

  For a 4-bit bitmap,

  LastMask := 00001111 and

  FirstMask := 11110000

 

  We'll shift through these such that the CurrentBits and the MaskBits will go

  For a monochrome bitmap:

  10000000, 01000000, 00100000, 00010000, 00001000, 00000100, 00000010, 00000001

  For a 4-bit bitmap:

  11110000, 00001111

 

  The CurrentBitIndex denotes how far over the right-most bit would need to

  shift to get to the position of CurrentBits. For example, if we're on the

  eleventh column of a monochrome bitmap, then CurrentBits will equal

  11 mod 8 := 3, or the 3rd-to-the-leftmost bit. Thus, the right-most bit

  would need to shift four places over to get anded correctly with

  CurrentBits. CurrentBitIndex will store this value.

  }

  LastMask := 1 shl BitCount - 1;

  FirstMask := LastMask shl FirstIndex;

 

  CurrentBits := FirstMask;

  CurrentBitIndex := FirstIndex;

 

  ShiftRightStart := BitCount * (PixelsPerByte - 1);

 

  { Here's the meat. Loop through the pixels and rotate appropriately. }

  { Remember that DIBs have their origins opposite from DDBs. }

 

  { The Y counter holds the current row of the source bitmap. }

  for Y := 1 to PbmpInfoR^.biHeight do begin

  PbmpBufferR := PFirstScanLine;

 

  {

  The X counter holds the current column of the source bitmap. We only

  deal with completely filled bytes here. Should there be an extra 'partial'

  byte, we'll deal with that below.

  }

  for X := 1 to WholeBytes do begin

  {

  Pick out the bits, starting with 10000000 for monochromes and

  11110000 for 4-bit guys.

  }

  MaskBits := FirstMask;

  {

  ShiftRightAmount is the amount we need to shift the current bit all

  the way to the right.

  }

  ShiftRightAmount := ShiftRightStart;

  for I := 1 to PixelsPerByte do begin

  {

  Here's the doozy. Take the rotated bitmap's current byte and mask it

  with not CurrentBits. This zeros out the CurrentBits only, and leaves

  everything else unchanged. Example: For a monochrome bitmap, if we

  were on the 11th column as above, we would need to zero out the

  3rd-to-left bit, so we would take PbmpBufferR^ and 11011111.

 

  Now consider our current source byte. For monochrome bitmaps, we're

  going to loop through each bit, for a total of eight pixels. For

  4-bit bitmaps, we're going to loop through the bits four at a time,

  for a total of two pixels. Either way, we do this by masking it with

  MaskBits ('PbmpBuffer^ and MaskBits'). Now we need to get the bit(s)

  into the same column(s) that CurrentBits reflects. We do this by

  shifting them to the right-most part of the byte ('shr

  ShiftRightAmount'), and then shifting left by our aforementioned

  CurrentBitIndex ('shl CurrentBitIndex'). This is because, although a

  right-shift of -n should just be a left-shift of +n, it doesn't work

  that way, at least in Delphi. So we just start from scratch by putting

  everything as far right as we can.

 

  Finally, we have our source bit(s) shifted to the appropriate place,

  with nothing but zeros around. Simply or it with PbmpBufferR^ (which

  had its CurrentBits zeroed out, remember?) and we're done.

 

  Yeah, sure. "Simply". Duh.

  }

 

  PbmpBufferR^ := ( PbmpBufferR^ and not CurrentBits ) or

  ( (PbmpBuffer^ and MaskBits) shr ShiftRightAmount shl CurrentBitIndex );

 

  { Move the MaskBits over for the next iteration. }

  MaskBits := MaskBits shr BitCount;

  (*$IFDEF Win32*)

  { Move our pointer to the rotated-bitmap buffer up one scan line. }

  Inc(PbmpBufferR, BytesPerScanLineR);

  { We don't need to shift as far to the right the next time around. }

  Dec(ShiftRightAmount, BitCount);

  (*$ELSE*)

  Win16Inc( Pointer(PbmpBufferR), BytesPerScanLineR );

  Win16Dec( Pointer(ShiftRightAmount), BitCount );

  (*$ENDIF*)

  end;

  (*$IFDEF Win32*)

  Inc(PbmpBuffer);

  (*$ELSE*)

  Win16Inc( Pointer(PbmpBuffer), 1 );

  (*$ENDIF*)

  end;

 

  { If there's a partial byte, take care of it now. }

  if ExtraPixels <> 0 then begin

  { Do exactly the same crap as in the loop above. }

  MaskBits := FirstMask;

  ShiftRightAmount := ShiftRightStart;

  for I := 1 to ExtraPixels do begin

  PbmpBufferR^ := ( PbmpBufferR^ and not CurrentBits ) or

  ( (PbmpBuffer^ and MaskBits) shr ShiftRightAmount shl CurrentBitIndex );

 

  MaskBits := MaskBits shr BitCount;

  (*$IFDEF Win32*)

  Inc(PbmpBufferR, BytesPerScanLineR);

  (*$ELSE*)

  Win16Inc( Pointer(PbmpBufferR), BytesPerScanLineR );

  (*$ENDIF*)

  Dec(ShiftRightAmount, BitCount);

  end;

  (*$IFDEF Win32*)

  Inc(PbmpBuffer);

  (*$ELSE*)

  Win16Inc( Pointer(PbmpBuffer), 1 );

  (*$ENDIF*)

  end;

 

  (*$IFDEF Win32*)

  { Skip the padding. }

  Inc(PbmpBuffer, PaddingBytes);

  {

  Back up the scan line you just traversed, and go one more to get set for

  the next row.

  }

  Dec(PbmpBuffer, BytesPerScanLine shl 1);

  (*$ELSE*)

  Win16Inc( Pointer(PbmpBuffer), PaddingBytes );

  Win16Dec( Pointer(PbmpBuffer), BytesPerScanLine shl 1 );

  (*$ENDIF*)

 

  if CurrentBits = LastMask then begin

  { We're at the end of this byte. Start over on another column. }

  CurrentBits := FirstMask;

  CurrentBitIndex := FirstIndex;

  { Go to the bottom of the rotated bitmap's column, but one column over. }

  (*$IFDEF Win32*)

  Inc(PFirstScanLine);

  (*$ELSE*)

  Win16Inc( Pointer(PFirstScanLine), 1 );

  (*$ENDIF*)

  end

  else begin

  { Continue filling this byte. }

  CurrentBits := CurrentBits shr BitCount;

  Dec(CurrentBitIndex, BitCount);

  end;

  end;

end; { procedure NonIntegralByteRotate (* nested *) }

 

procedure IntegralByteRotate; (* nested *)

var

  X, Y: LongInt;

  (*$IFNDEF Win32*)

  I: Integer;

  (*$ENDIF*)

 

begin

  { Advance PbmpBufferR to the last column of the first scan line of bmpBufferR. }

  (*$IFDEF Win32*)

  Inc(PbmpBufferR, SignificantBytesR - BytesPerPixel);

  (*$ELSE*)

  Win16Inc( Pointer(PbmpBufferR), SignificantBytesR - BytesPerPixel );

  (*$ENDIF*)

 

  { Here's the meat. Loop through the pixels and rotate appropriately. }

  { Remember that DIBs have their origins opposite from DDBs. }

  for Y := 1 to PbmpInfoR^.biHeight do begin

  for X := 1 to PbmpInfoR^.biWidth do begin

  { Copy the pixels. }

  (*$IFDEF Win32*)

  Move(PbmpBuffer^, PbmpBufferR^, BytesPerPixel);

  Inc(PbmpBuffer, BytesPerPixel);

  Inc(PbmpBufferR, BytesPerScanLineR);

  (*$ELSE*)

  for I := 1 to BytesPerPixel do begin

  PbmpBufferR^ := PbmpBuffer^;

  Win16Inc( Pointer(PbmpBuffer), 1 );

  Win16Inc( Pointer(PbmpBufferR), 1 );

  end;

  Win16Inc( Pointer(PbmpBufferR), BytesPerScanLineR - BytesPerPixel);

  (*$ENDIF*)

  end;

  (*$IFDEF Win32*)

  { Skip the padding. }

  Inc(PbmpBuffer, PaddingBytes);

  { Go to the top of the rotated bitmap's column, but one column over. }

  Dec(PbmpBufferR, ColumnBytes + BytesPerPixel);

  (*$ELSE*)

  Win16Inc( Pointer(PbmpBuffer), PaddingBytes);

  Win16Dec( Pointer(PbmpBufferR), ColumnBytes + BytesPerPixel);

  (*$ENDIF*)

  end;

end;

 

{ This is the body of procedure RotateBitmap90DegreesCounterClockwise. }

begin

  { Don't *ever* call GetDIBSizes! It screws up your bitmap. }

 

  MemoryStream := TMemoryStream.Create;

 

  {

  To do: Set the size before-hand. This will eliminate ReAlloc overhead

  for the MemoryStream. Calling GetDIBSizes would be nice, but, as mentioned

  above, it corrupts the Bitmap in some cases. Some API calls will probably

  take care of things, but I'm not going to mess with it right now.

  }

 

  { An undocumented method. Nice to have around, though. }

  ABitmap.SaveToStream(MemoryStream);

 

  { Don't need you anymore. We'll make a new one when the time comes. }

  ABitmap.Free;

 

  bmpBuffer := MemoryStream.Memory;

  { Get the offset bits. This may or may not include palette information. }

  BitmapOffset := PBitmapFileHeader(bmpBuffer)^.bfOffBits;

 

  { Set PbmpInfoR to point to the source bitmap's info header. }

  { Boy, these headers are getting annoying. }

  (*$IFDEF Win32*)

  Inc( bmpBuffer, SizeOf(TBitmapFileHeader) );

  (*$ELSE*)

  Win16Inc( Pointer(bmpBuffer), SizeOf(TBitmapFileHeader) );

  (*$ENDIF*)

  PbmpInfoR := PBitmapInfoHeader(bmpBuffer);

 

  { Set bmpBuffer and PbmpBuffer to point to the original bitmap bits. }

  bmpBuffer := MemoryStream.Memory;

  (*$IFDEF Win32*)

  Inc(bmpBuffer, BitmapOffset);

  (*$ELSE*)

  Win16Inc( Pointer(bmpBuffer), BitmapOffset );

  (*$ENDIF*)

  PbmpBuffer := bmpBuffer;

 

  {

  Note that we don't need to worry about version 4 vs. version 3 bitmaps,

  because the fields we use -- namely biWidth, biHeight, and biBitCount --

  occur in exactly the same place in both structs. So we're a bit lucky. OS/2

  bitmaps, by the way, cause this to crash heinously. Sorry.

  }

  with PbmpInfoR^ do begin

  { ShowMessage('Compression := ' + IntToStr(biCompression)); }

  BitCount := biBitCount;

  { ShowMessage('BitCount := ' + IntToStr(BitCount)); }

 

  { ScanLines are DWORD aligned. }

  BytesPerScanLine := ((((biWidth * BitCount) + 31) div 32) * SizeOf(DWORD));

  BytesPerScanLineR := ((((biHeight * BitCount) + 31) div 32) * SizeOf(DWORD));

 

  AtLeastEightBitColor := BitCount >= BitsPerByte;

  if AtLeastEightBitColor then begin

  { Don't have to worry about bit-twiddling. Cool. }

  BytesPerPixel := biBitCount shr 3;

  SignificantBytes := biWidth * BitCount shr 3;

  SignificantBytesR := biHeight * BitCount shr 3;

  { Extra bytes required for DWORD aligning. }

  PaddingBytes := BytesPerScanLine - SignificantBytes;

  ColumnBytes := BytesPerScanLineR * biWidth;

  end

  else begin

  { One- or four-bit bitmap. Ugh. }

  PixelsPerByte := SizeOf(Byte) * BitsPerByte div BitCount;

  { The number of bytes entirely filled with pixel information. }

  WholeBytes := biWidth div PixelsPerByte;

  {

  Any extra bits that might partially fill a byte. For instance, a

  monochrome bitmap that is 14 pixels wide has one whole byte and a

  partial byte which has six bits actually used (the rest are garbage).

  }

  ExtraPixels := biWidth mod PixelsPerByte;

  {

  The number of extra bytes -- if any -- required to DWORD-align a

  scanline.

  }

  PaddingBytes := BytesPerScanLine - WholeBytes;

  {

  If there are extra bits (i.e., they run over into a 'partial byte'),

  then one of the padding bytes has already been accounted for.

  }

  if ExtraPixels <> 0 then Dec(PaddingBytes);

  end; { if AtLeastEightBitColor then }

 

  { The TMemoryStream that will hold the rotated bits. }

  MemoryStreamR := TMemoryStream.Create;

  {

  Set size for rotated bitmap. Might be different from source size

  due to DWORD aligning.

  }

  MemoryStreamR.SetSize(BitmapOffset + BytesPerScanLineR * biWidth);

  end; { with PbmpInfoR^ do }

 

  { Copy the headers from the source bitmap. }

  MemoryStream.Seek(0, soFromBeginning);

  MemoryStreamR.CopyFrom(MemoryStream, BitmapOffset);

 

  { Here's the buffer we're going to rotate. }

  bmpBufferR := MemoryStreamR.Memory;

  { Skip the headers, yadda yadda yadda... }

  (*$IFDEF Win32*)

  Inc(bmpBufferR, BitmapOffset);

  (*$ELSE*)

  Win16Inc( Pointer(bmpBufferR), BitmapOffset );

  (*$ENDIF*)

  PbmpBufferR := bmpBufferR;

 

  { Do it. }

  if AtLeastEightBitColor then

  IntegralByteRotate

  else

  NonIntegralByteRotate;

 

  { Done with the source bits. }

  MemoryStream.Free;

 

  { Now set PbmpInfoR to point to the rotated bitmap's info header. }

  PbmpBufferR := MemoryStreamR.Memory;

  (*$IFDEF Win32*)

  Inc( PbmpBufferR, SizeOf(TBitmapFileHeader) );

  (*$ELSE*)

  Win16Inc( Pointer(PbmpBufferR), SizeOf(TBitmapFileHeader) );

  (*$ENDIF*)

  PbmpInfoR := PBitmapInfoHeader(PbmpBufferR);

 

  { Swap the width and height of the rotated bitmap's info header. }

  with PbmpInfoR^ do begin

  T := biHeight;

  biHeight := biWidth;

  biWidth := T;

  biSizeImage := 0;

  end;

 

  ABitmap := TBitmap.Create;

 

  { Spin back to the very beginning. }

  MemoryStreamR.Seek(0, soFromBeginning);

  { Load it back into ABitmap. }

  ABitmap.LoadFromStream(MemoryStreamR);

 

  MemoryStreamR.Free;

end;

 

procedure RotateBitmap90DegreesClockwise(var ABitmap: TBitmap);

const

  BitsPerByte = 8;

 

var

  {

  A whole pile of variables. Some deal with one- and four-bit bitmaps only,

  some deal with eight- and 24-bit bitmaps only, and some deal with both.

  Any variable that ends in 'R' refers to the rotated bitmap, e.g. MemoryStream

  holds the original bitmap, and MemoryStreamR holds the rotated one.

  }

  PbmpInfoR: PBitmapInfoHeader;

  bmpBuffer, bmpBufferR: PByte;

  MemoryStream, MemoryStreamR: TMemoryStream;

  PbmpBuffer, PbmpBufferR: PByte;

  BytesPerPixel, PixelsPerByte: LongInt;

  BytesPerScanLine, BytesPerScanLineR: LongInt;

  PaddingBytes: LongInt;

  BitmapOffset: LongInt;

  BitCount: LongInt;

  WholeBytes, ExtraPixels: LongInt;

  SignificantBytes: LongInt;

  ColumnBytes: LongInt;

  AtLeastEightBitColor: Boolean;

  T: LongInt;

 

procedure NonIntegralByteRotate; (* nested *)

{

 This routine rotates bitmaps with fewer than 8 bits of information per pixel,

 namely monochrome (1-bit) and 16-color (4-bit) bitmaps. Note that there are

 no such things as 2-bit bitmaps, though you might argue that Microsoft's bitmap

 format is worth about 2 bits.

}

var

  X, Y: LongInt;

  I: LongInt;

  MaskBits, CurrentBits: Byte;

  FirstMask, LastMask: Byte;

  PLastScanLine: PByte;

  FirstIndex, CurrentBitIndex: LongInt;

  ShiftRightAmount, ShiftRightStart: LongInt;

 

begin

  { Advance PLastScanLine to the first column of the last scan line of bmpBufferR. }

  PLastScanLine := bmpBufferR; (*$IFDEF Win32*) Inc(PLastScanLine, BytesPerScanLineR *

  (PbmpInfoR^.biWidth - 1) ); (*$ELSE*) Win16Inc( Pointer(PLastScanLine),

  BytesPerScanLineR * (PbmpInfoR^.biWidth - 1) ); (*$ENDIF*)

 

  { Set up the indexing. }

  FirstIndex := BitsPerByte - BitCount;

 

  {

  Set up the bit masks:

 

  For a monochrome bitmap,

  LastMask := 00000001 and

  FirstMask := 10000000

 

  For a 4-bit bitmap,

  LastMask := 00001111 and

  FirstMask := 11110000

 

  We'll shift through these such that the CurrentBits and the MaskBits will go

  For a monochrome bitmap:

  10000000, 01000000, 00100000, 00010000, 00001000, 00000100, 00000010, 00000001

  For a 4-bit bitmap:

  11110000, 00001111

 

  The CurrentBitIndex denotes how far over the right-most bit would need to

  shift to get to the position of CurrentBits. For example, if we're on the

  eleventh column of a monochrome bitmap, then CurrentBits will equal

  11 mod 8 := 3, or the 3rd-to-the-leftmost bit. Thus, the right-most bit

  would need to shift four places over to get anded correctly with

  CurrentBits. CurrentBitIndex will store this value.

  }

  LastMask := 1 shl BitCount - 1;

  FirstMask := LastMask shl FirstIndex;

 

  CurrentBits := FirstMask;

  CurrentBitIndex := FirstIndex;

 

  ShiftRightStart := BitCount * (PixelsPerByte - 1);

 

  { Here's the meat. Loop through the pixels and rotate appropriately. }

  { Remember that DIBs have their origins opposite from DDBs. }

 

  { The Y counter holds the current row of the source bitmap. }

  for Y := 1 to PbmpInfoR^.biHeight do begin

  PbmpBufferR := PLastScanLine;

 

  {

  The X counter holds the current column of the source bitmap. We only

  deal with completely filled bytes here. Should there be an extra 'partial'

  byte, we'll deal with that below.

  }

  for X := 1 to WholeBytes do begin

  {

  Pick out the bits, starting with 10000000 for monochromes and

  11110000 for 4-bit guys.

  }

  MaskBits := FirstMask;

  {

  ShiftRightAmount is the amount we need to shift the current bit all

  the way to the right.

  }

  ShiftRightAmount := ShiftRightStart;

  for I := 1 to PixelsPerByte do begin

  {

  Here's the doozy. Take the rotated bitmap's current byte and mask it

  with not CurrentBits. This zeros out the CurrentBits only, and leaves

  everything else unchanged. Example: For a monochrome bitmap, if we

  were on the 11th column as above, we would need to zero out the

  3rd-to-left bit, so we would take PbmpBufferR^ and 11011111.

 

  Now consider our current source byte. For monochrome bitmaps, we're

  going to loop through each bit, for a total of eight pixels. For

  4-bit bitmaps, we're going to loop through the bits four at a time,

  for a total of two pixels. Either way, we do this by masking it with

  MaskBits ('PbmpBuffer^ and MaskBits'). Now we need to get the bit(s)

  into the same column(s) that CurrentBits reflects. We do this by

  shifting them to the right-most part of the byte ('shr

  ShiftRightAmount'), and then shifting left by our aforementioned

  CurrentBitIndex ('shl CurrentBitIndex'). This is because, although a

  right-shift of -n should just be a left-shift of +n, it doesn't work

  that way, at least in Delphi. So we just start from scratch by putting

  everything as far right as we can.

 

  Finally, we have our source bit(s) shifted to the appropriate place,

  with nothing but zeros around. Simply or it with PbmpBufferR^ (which

  had its CurrentBits zeroed out, remember?) and we're done.

 

  Yeah, sure. "Simply". Duh.

  }

 

  PbmpBufferR^ := ( PbmpBufferR^ and not CurrentBits ) or

  ( (PbmpBuffer^ and MaskBits) shr ShiftRightAmount shl CurrentBitIndex );

 

  { Move the MaskBits over for the next iteration. }

  MaskBits := MaskBits shr BitCount;

  (*$IFDEF Win32*)

  { Move our pointer to the rotated-bitmap buffer up one scan line. }

  Dec(PbmpBufferR, BytesPerScanLineR);

  (*$ELSE*)

  Win16Dec( Pointer(PbmpBufferR), BytesPerScanLineR );

  (*$ENDIF*)

  { We don't need to shift as far to the right the next time around. }

  Dec(ShiftRightAmount, BitCount);

  end;

  (*$IFDEF Win32*)

  Inc(PbmpBuffer);

  (*$ELSE*)

  Win16Inc( Pointer(PbmpBuffer), 1 );

  (*$ENDIF*)

  end;

 

  { If there's a partial byte, take care of it now. }

  if ExtraPixels <> 0 then begin

  { Do exactly the same crap as in the loop above. }

  MaskBits := FirstMask;

  ShiftRightAmount := ShiftRightStart;

  for I := 1 to ExtraPixels do begin

  PbmpBufferR^ := ( PbmpBufferR^ and not CurrentBits ) or

  ( (PbmpBuffer^ and MaskBits) shr ShiftRightAmount shl CurrentBitIndex );

 

  MaskBits := MaskBits shr BitCount;

  (*$IFDEF Win32*)

  Dec(PbmpBufferR, BytesPerScanLineR);

  (*$ELSE*)

  Win16Dec( Pointer(PbmpBufferR), BytesPerScanLineR );

  (*$ENDIF*)

  Dec(ShiftRightAmount, BitCount);

  end;

  (*$IFDEF Win32*)

  Inc(PbmpBuffer);

  (*$ELSE*)

  Win16Inc( Pointer(PbmpBuffer), 1 );

  (*$ENDIF*)

  end;

 

  { Skip the padding. }

  (*$IFDEF Win32*)

  Inc(PbmpBuffer, PaddingBytes);

  (*$ELSE*)

  Win16Inc( Pointer(PbmpBuffer), PaddingBytes );

  (*$ENDIF*)

 

  if CurrentBits = LastMask then begin

  { We're at the end of this byte. Start over on another column. }

  CurrentBits := FirstMask;

  CurrentBitIndex := FirstIndex;

  { Go to the bottom of the rotated bitmap's column, but one column over. }

  (*$IFDEF Win32*)

  Inc(PLastScanLine);

  (*$ELSE*)

  Win16Inc( Pointer(PLastScanLine), 1 );

  (*$ENDIF*)

  end

  else begin

  { Continue filling this byte. }

  CurrentBits := CurrentBits shr BitCount;

  Dec(CurrentBitIndex, BitCount);

  end;

  end;

end; { procedure NonIntegralByteRotate (* nested *) }

 

procedure IntegralByteRotate; (* nested *)

var

  X, Y: LongInt;

  (*$IFNDEF Win32*)

  I: Integer;

  (*$ENDIF*)

 

begin

  { Advance PbmpBufferR to the first column of the last scan line of bmpBufferR. }

  (*$IFDEF Win32*)

  Inc( PbmpBufferR, BytesPerScanLineR * (PbmpInfoR^.biWidth - 1) );

  (*$ELSE*)

  Win16Inc( Pointer(PbmpBufferR) , BytesPerScanLineR * (PbmpInfoR^.biWidth - 1) );

  (*$ENDIF*)

 

  { Here's the meat. Loop through the pixels and rotate appropriately. }

  { Remember that DIBs have their origins opposite from DDBs. }

  for Y := 1 to PbmpInfoR^.biHeight do begin

  for X := 1 to PbmpInfoR^.biWidth do begin

  { Copy the pixels. }

  (*$IFDEF Win32*)

  Move(PbmpBuffer^, PbmpBufferR^, BytesPerPixel);

  Inc(PbmpBuffer, BytesPerPixel);

  Dec(PbmpBufferR, BytesPerScanLineR);

  (*$ELSE*)

  for I := 1 to BytesPerPixel do begin

  PbmpBufferR^ := PbmpBuffer^;

  Win16Inc( Pointer(PbmpBuffer), 1 );

  Win16Inc( Pointer(PbmpBufferR), 1 );

  end;

  Win16Dec( Pointer(PbmpBufferR), BytesPerScanLineR + BytesPerPixel);

  (*$ENDIF*)

  end;

  (*$IFDEF Win32*)

  { Skip the padding. }

  Inc(PbmpBuffer, PaddingBytes);

  { Go to the top of the rotated bitmap's column, but one column over. }

  Inc(PbmpBufferR, ColumnBytes + BytesPerPixel);

  (*$ELSE*)

  Win16Inc( Pointer(PbmpBuffer), PaddingBytes );

  Win16Inc( Pointer(PbmpBufferR), ColumnBytes + BytesPerPixel );

  (*$ENDIF*)

  end;

end;

 

{ This is the body of procedure RotateBitmap90DegreesCounterClockwise. }

begin

  { Don't *ever* call GetDIBSizes! It screws up your bitmap. }

 

  MemoryStream := TMemoryStream.Create;

 

  {

  To do: Set the size before-hand. This will eliminate ReAlloc overhead

  for the MemoryStream. Calling GetDIBSizes would be nice, but, as mentioned

  above, it corrupts the Bitmap in some cases. Some API calls will probably

  take care of things, but I'm not going to mess with it right now.

  }

 

  { An undocumented method. Nice to have around, though. }

  ABitmap.SaveToStream(MemoryStream);

 

  { Don't need you anymore. We'll make a new one when the time comes. }

  ABitmap.Free;

 

  bmpBuffer := MemoryStream.Memory;

  { Get the offset bits. This may or may not include palette information. }

  BitmapOffset := PBitmapFileHeader(bmpBuffer)^.bfOffBits;

 

  { Set PbmpInfoR to point to the source bitmap's info header. }

  { Boy, these headers are getting annoying. }

  (*$IFDEF Win32*)

  Inc( bmpBuffer, SizeOf(TBitmapFileHeader) );

  (*$ELSE*)

  Win16Inc( Pointer(bmpBuffer), SizeOf(TBitmapFileHeader) );

  (*$ENDIF*)

  PbmpInfoR := PBitmapInfoHeader(bmpBuffer);

 

  { Set bmpBuffer and PbmpBuffer to point to the original bitmap bits. }

  bmpBuffer := MemoryStream.Memory;

  (*$IFDEF Win32*)

  Inc(bmpBuffer, BitmapOffset);

  (*$ELSE*)

  Win16Inc( Pointer(bmpBuffer), BitmapOffset );

  (*$ENDIF*)

  PbmpBuffer := bmpBuffer;

 

  {

  Note that we don't need to worry about version 4 vs. version 3 bitmaps,

  because the fields we use -- namely biWidth, biHeight, and biBitCount --

  occur in exactly the same place in both structs. So we're a bit lucky. OS/2

  bitmaps, by the way, cause this to crash heinously. Sorry.

  }

  with PbmpInfoR^ do begin

  { ShowMessage('Compression := ' + IntToStr(biCompression)); }

  BitCount := biBitCount;

  { ShowMessage('BitCount := ' + IntToStr(BitCount)); }

 

  { ScanLines are DWORD aligned. }

  BytesPerScanLine := ((((biWidth * BitCount) + 31) div 32) * SizeOf(DWORD));

  BytesPerScanLineR := ((((biHeight * BitCount) + 31) div 32) * SizeOf(DWORD));

 

  AtLeastEightBitColor := BitCount >= BitsPerByte;

  if AtLeastEightBitColor then begin

  { Don't have to worry about bit-twiddling. Cool. }

  BytesPerPixel := biBitCount shr 3;

  SignificantBytes := biWidth * BitCount shr 3;

  { Extra bytes required for DWORD aligning. }

  PaddingBytes := BytesPerScanLine - SignificantBytes;

  ColumnBytes := BytesPerScanLineR * biWidth;

  end

  else begin

  { One- or four-bit bitmap. Ugh. }

  PixelsPerByte := SizeOf(Byte) * BitsPerByte div BitCount;

  { The number of bytes entirely filled with pixel information. }

  WholeBytes := biWidth div PixelsPerByte;

  {

  Any extra bits that might partially fill a byte. For instance, a

  monochrome bitmap that is 14 pixels wide has one whole byte and a

  partial byte which has six bits actually used (the rest are garbage).

  }

  ExtraPixels := biWidth mod PixelsPerByte;

  {

  The number of extra bytes -- if any -- required to DWORD-align a

  scanline.

  }

  PaddingBytes := BytesPerScanLine - WholeBytes;

  {

  If there are extra bits (i.e., they run over into a 'partial byte'),

  then one of the padding bytes has already been accounted for.

  }

  if ExtraPixels <> 0 then Dec(PaddingBytes);

  end; { if AtLeastEightBitColor then }

 

  { The TMemoryStream that will hold the rotated bits. }

  MemoryStreamR := TMemoryStream.Create;

  {

  Set size for rotated bitmap. Might be different from source size

  due to DWORD aligning.

  }

  MemoryStreamR.SetSize(BitmapOffset + BytesPerScanLineR * biWidth);

  end; { with PbmpInfoR^ do }

 

  { Copy the headers from the source bitmap. }

  MemoryStream.Seek(0, soFromBeginning);

  MemoryStreamR.CopyFrom(MemoryStream, BitmapOffset);

 

  { Here's the buffer we're going to rotate. }

  bmpBufferR := MemoryStreamR.Memory;

  { Skip the headers, yadda yadda yadda... }

  (*$IFDEF Win32*)

  Inc(bmpBufferR, BitmapOffset);

  (*$ELSE*)

  Win16Inc( Pointer(bmpBufferR), BitmapOffset );

  (*$ENDIF*)

  PbmpBufferR := bmpBufferR;

 

  { Do it. }

  if AtLeastEightBitColor then

  IntegralByteRotate

  else

  NonIntegralByteRotate;

 

  { Done with the source bits. }

  MemoryStream.Free;

 

  { Now set PbmpInfoR to point to the rotated bitmap's info header. }

  PbmpBufferR := MemoryStreamR.Memory;

  (*$IFDEF Win32*)

  Inc( PbmpBufferR, SizeOf(TBitmapFileHeader) );

  (*$ELSE*)

  Win16Inc( Pointer(PbmpBufferR), SizeOf(TBitmapFileHeader) );

  (*$ENDIF*)

  PbmpInfoR := PBitmapInfoHeader(PbmpBufferR);

 

  { Swap the width and height of the rotated bitmap's info header. }

  with PbmpInfoR^ do begin

  T := biHeight;

  biHeight := biWidth;

  biWidth := T;

  biSizeImage := 0;

  end;

 

  ABitmap := TBitmap.Create;

 

  { Spin back to the very beginning. }

  MemoryStreamR.Seek(0, soFromBeginning);

  { Load it back into ABitmap. }

  ABitmap.LoadFromStream(MemoryStreamR);

 

  MemoryStreamR.Free;

end;

 

procedure RotateBitmap180Degrees(var ABitmap: TBitmap);

var

  RotatedBitmap: TBitmap;

 

begin

  RotatedBitmap := TBitmap.Create;

  with RotatedBitmap do begin

  Width := ABitmap.Width;

  Height := ABitmap.Height;

  Canvas.StretchDraw( Rect(ABitmap.Width, ABitmap.Height, 0, 0), ABitmap );

  end;

  ABitmap.Free;

  ABitmap := RotatedBitmap;

end;

 

end.

