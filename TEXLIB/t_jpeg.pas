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
//  JPEG image format.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit t_jpeg;

interface

uses
  d_delphi,
  t_main,
  t_bmp;

type
  TJPGTextureManager = object(TBMPTextureManager)
  private
    bmpstream: TDStream;
  public
    constructor Create(const ext: string);
    destructor Destroy; virtual;
    function LoadHeader(stream: TDStream): boolean; virtual;
    function LoadImage(stream: TDStream): boolean; virtual;
  end;

implementation

uses
  i_system,
  jpg_morecfg,
  jpg_lib,
  jpg_error,
  jpg_deferr,
  jpg_dmarker,
  jpg_dmaster,
  jpg_dapimin,
  jpg_dapistd;

{.$I jconfig.inc}

{ ---------------------------------------------------------------------- }
{   source manager to read compressed data                               }
{   for reference: JDATASRC.PAS in PASJPG10 library                      }
{ ---------------------------------------------------------------------- }

type
  my_src_ptr = ^my_source_mgr;
  my_source_mgr = record
    pub: jpeg_source_mgr;  {public fields}
    infile: TDStream;    {source stream}
    buffer: JOCTET_FIELD_PTR;  {start of buffer}
    start_of_file: boolean;  {have we gotten any data yet?}
  end;

const
  INPUT_BUF_SIZE = 4096;

//==============================================================================
//
// init_source
//
//==============================================================================
procedure init_source(cinfo: j_decompress_ptr); far;
var
  src: my_src_ptr;
begin
  src := my_src_ptr(cinfo^.src);
  src^.start_of_file := TRUE;
end;

//==============================================================================
//
// fill_input_buffer
//
//==============================================================================
function fill_input_buffer(cinfo: j_decompress_ptr): boolean; far;
var
  src: my_src_ptr;
  nbytes: size_t;
begin
  src := my_src_ptr(cinfo^.src);
  nbytes := src^.infile.Read(src^.buffer^, INPUT_BUF_SIZE);
  if nbytes <= 0 then
  begin
    if (src^.start_of_file) then   {Treat empty input file as fatal error}
      ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EMPTY);
    WARNMS(j_common_ptr(cinfo), JWRN_JPEG_EOF);
    {Insert a fake EOI marker}
    src^.buffer^[0] := JOCTET ($FF);
    src^.buffer^[1] := JOCTET (JPEG_EOI);
    nbytes := 2;
  end;
  src^.pub.next_input_byte := JOCTETptr(src^.buffer);
  src^.pub.bytes_in_buffer := nbytes;
  src^.start_of_file := FALSE;
  fill_input_buffer := TRUE;
end;

//==============================================================================
//
// skip_input_data
//
//==============================================================================
procedure skip_input_data(cinfo: j_decompress_ptr;
                      num_bytes: long); far;
var
  src: my_src_ptr;
begin
  src := my_src_ptr (cinfo^.src);
  if (num_bytes > 0) then
  begin
    while (num_bytes > long(src^.pub.bytes_in_buffer)) do
    begin
      dec(num_bytes, long(src^.pub.bytes_in_buffer));
      fill_input_buffer(cinfo);
      { note we assume that fill_input_buffer will never return FALSE,
        so suspension need not be handled. }
    end;
    inc( src^.pub.next_input_byte, size_t(num_bytes) );
    dec( src^.pub.bytes_in_buffer, size_t(num_bytes) );
  end;
end;

//==============================================================================
//
// term_source
//
//==============================================================================
procedure term_source(cinfo: j_decompress_ptr); far;
begin
  { no work necessary here }
end;

//==============================================================================
//
// jpeg_stream_src
//
//==============================================================================
procedure jpeg_stream_src(cinfo: j_decompress_ptr; const infile: TDStream);
var
  src: my_src_ptr;
begin
  if (cinfo^.src = nil) then
  begin {first time for this JPEG object?}
    cinfo^.src := jpeg_source_mgr_ptr(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
          SizeOf(my_source_mgr)) );
    src := my_src_ptr (cinfo^.src);
    src^.buffer := JOCTET_FIELD_PTR(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
          INPUT_BUF_SIZE * SizeOf(JOCTET)) );
  end;
  src := my_src_ptr (cinfo^.src);
  {override pub's method pointers}
  src^.pub.init_source := init_source;
  src^.pub.fill_input_buffer := fill_input_buffer;
  src^.pub.skip_input_data := skip_input_data;
  src^.pub.resync_to_restart := jpeg_resync_to_restart; {use default method}
  src^.pub.term_source := term_source;
  {define our fields}
  src^.infile := infile;
  src^.pub.bytes_in_buffer := 0;   {forces fill_input_buffer on first read}
  src^.pub.next_input_byte := nil; {until buffer loaded}
end;

{ ------------------------------------------------------------------------ }
{   Bitmap writing routines                                                }
{   for reference: WRBMP.PAS in PASJPG10 library                           }
{ ------------------------------------------------------------------------ }
{   NOTE: we always write BMP's in Windows format, no OS/2 formats!        }
{         however, we read all bitmap flavors (see bitmap reading)         }
{ ------------------------------------------------------------------------ }

{ To support 12-bit JPEG data, we'd have to scale output down to 8 bits.
  This is not yet implemented. }

{.$ifndef BITS_IN_JSAMPLE_IS_8}
//  Sorry, this code only copes with 8-bit JSAMPLEs. { deliberate syntax err }
{.$endif}

type
  BGRptr = ^BGRtype;
  BGRtype = packed record
    b,g,r: byte;
  end;

  RGBptr = ^RGBtype;
  RGBtype = packed record
    r,g,b: JSAMPLE;
  end;

  bmp_dest_ptr = ^bmp_dest_struct;
  bmp_dest_struct = record
    outfile: TDStream;               {Stream to write to}
    inmemory: boolean;              {keep whole image in memory}
    {image info}
    data_width: JDIMENSION;         {JSAMPLEs per row}
    row_width: JDIMENSION;          {physical width of one row in the BMP file}
    pad_bytes: INT;                 {number of padding bytes needed per row}
    grayscale: boolean;             {grayscale or quantized color table ?}
    {pixelrow buffer}
    buffer: JSAMPARRAY;             {pixelrow buffer}
    buffer_height: JDIMENSION;      {normally, we'll use 1}
    {image buffer}
    image_buffer: jvirt_sarray_ptr; {needed to reverse row order BMP<>JPG}
    image_buffer_height: JDIMENSION;
    cur_output_row: JDIMENSION;     {next row# to write to virtual array}
    row_offset: INT32;              {position of next row to write to BMP}
  end;

//==============================================================================
//
// write_bmp_header
//
//==============================================================================
procedure write_bmp_header(cinfo: j_decompress_ptr; dest: bmp_dest_ptr);
{Write a Windows-style BMP file header, including colormap if needed}
var
  bmpfileheader: TBitmapFileHeader;
  bmpinfoheader: TBitmapInfoHeader;
  headersize: INT32;
  bits_per_pixel, cmap_entries, num_colors, i: INT;
  output_ext_color_map: array[0..255] of record b, g, r, a: byte; end;
begin
  {colormap size and total file size}
  if (cinfo^.out_color_space = JCS_RGB) then
  begin
    if (cinfo^.quantize_colors) then
    begin {colormapped RGB}
      bits_per_pixel := 8;
      cmap_entries := 256;
    end
    else
    begin {unquantized, full color RGB}
      bits_per_pixel := 24;
      cmap_entries := 0;
    end;
  end
  else
  begin {grayscale output. We need to fake a 256-entry colormap.}
    bits_per_pixel := 8;
    cmap_entries := 256;
  end;
  headersize := SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader) + cmap_entries * 4;
  {define headers}
  FillChar(bmpfileheader, SizeOf(bmpfileheader), $0);
  FillChar(bmpinfoheader, SizeOf(bmpinfoheader), $0);
  with bmpfileheader do
  begin
    bfType := $4D42; {BM}
    bfSize := headersize + INT32(dest^.row_width) * INT32(cinfo^.output_height);
    bfOffset := headersize;
  end;
  with bmpinfoheader do
  begin
    Size := SizeOf(TBitmapInfoHeader);
    Width := cinfo^.output_width;
    Height := cinfo^.output_height;
    Planes := 1;
    BitCount := bits_per_pixel;
    if (cinfo^.density_unit = 2) then
    begin
      XPelsPerMeter := INT32(cinfo^.X_density * 100);
      YPelsPerMeter := INT32(cinfo^.Y_density * 100);
    end;
    ClrUsed := cmap_entries;
  end;
  if dest^.outfile.Write(bmpfileheader, SizeOf(bmpfileheader))
       <> size_t(SizeOf(bmpfileheader)) then
    ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
  if dest^.outfile.Write(bmpinfoheader, SizeOf(bmpinfoheader))
       <> size_t(SizeOf(bmpinfoheader)) then
    ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
  {colormap}
  if cmap_entries > 0 then
  begin
    num_colors := cinfo^.actual_number_of_colors;
    if cinfo^.colormap <> nil then
    begin
      if cinfo^.out_color_components = 3 then
        for i := 0 to pred(num_colors) do
          with output_ext_color_map[i] do
          begin
            b := GETJSAMPLE(cinfo^.colormap^[2]^[i]);
            g := GETJSAMPLE(cinfo^.colormap^[1]^[i]);
            r := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
            a := 0;
          end
      else
        {grayscale colormap (only happens with grayscale quantization)}
        for i := 0 to pred(num_colors) do
          with output_ext_color_map[i] do
          begin
            b := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
            g := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
            r := GETJSAMPLE(cinfo^.colormap^[0]^[i]);
            a := 0;
          end;
      i := num_colors;
    end
    else
    begin
      {if no colormap, must be grayscale data. Generate a linear "map".}
      {Nomssi: do not use "num_colors" here, it should be 0}
      for i := 0 to pred(256) do
        with output_ext_color_map[i] do
        begin
          b := i;
          g := i;
          r := i;
          a := 0;
        end;
      i := 256;
    end;
    {pad colormap with zeros to ensure specified number of colormap entries}
    if i > cmap_entries then
      ERREXIT1(j_common_ptr(cinfo), JERR_TOO_MANY_COLORS, i);
    while i < cmap_entries do
    begin
      with output_ext_color_map[i] do
      begin
        b := 0;
        g := 0;
        r := 0;
        a := 0;
      end;
      inc(i);
    end;
    if dest^.outfile.Write(output_ext_color_map, cmap_entries * 4) <> cmap_entries * 4 then
      ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
  end;
  dest^.row_offset := bmpfileheader.bfSize;
end;

//==============================================================================
//
// write_bmp_pixelrow
//
//==============================================================================
procedure write_bmp_pixelrow(cinfo: j_decompress_ptr; dest: bmp_dest_ptr;
  rows_supplied: JDIMENSION);
var
  image_ptr: JSAMPARRAY;
  inptr, outptr: JSAMPLE_PTR;
  BGR: BGRptr;
  col,row: JDIMENSION;
  pad: int;
begin
  if dest^.inmemory then
  begin
    row := dest^.cur_output_row;
    inc(dest^.cur_output_row);
  end
  else
  begin
    row := 0;
    dec(dest^.row_offset, dest^.row_width);
  end;
  image_ptr := cinfo^.mem^.access_virt_sarray ( j_common_ptr(cinfo),
     dest^.image_buffer, row, JDIMENSION (1), TRUE);
  inptr := JSAMPLE_PTR(dest^.buffer^[0]);
  if not dest^.grayscale then
  begin
    BGR := BGRptr(image_ptr^[0]);
    inc(BGR, cinfo^.output_width);
    for col := pred(cinfo^.output_width) downto 0 do
    begin
      dec(BGR);
      BGR^.b := inptr^;
      inc(inptr);
      BGR^.g := inptr^;
      inc(inptr);
      BGR^.r := inptr^;
      inc(inptr);
    end;
    outptr := JSAMPLE_PTR(BGR);
  end
  else
  begin
    outptr := JSAMPLE_PTR(image_ptr^[0]);
    inc(outptr, cinfo^.output_width);
    for col := pred(cinfo^.output_width) downto 0 do
    begin
      dec(outptr);
      outptr^ := inptr^;
      inc(inptr);
    end;
  end;
  {zero out the pad bytes}
  pad := dest^.pad_bytes;
  while (pad > 0) do
  begin
    dec(pad);
    outptr^ := 0;
    inc(outptr);
  end;
  if not dest^.inmemory then
  begin
    {store row in output stream}
    image_ptr := cinfo^.mem^.access_virt_sarray ( j_common_ptr(cinfo),
         dest^.image_buffer, 0, JDIMENSION(1), FALSE);
    outptr := JSAMPLE_PTR(image_ptr^[0]);
    if dest^.outfile.Seek(dest^.row_offset, 0) <> dest^.row_offset then
      ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
    if dest^.outfile.Write(outptr^, dest^.row_width) <> dest^.row_width then
      ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
  end;
end;

//==============================================================================
//
// write_bmp_image
//
//==============================================================================
procedure write_bmp_image(cinfo: j_decompress_ptr; dest: bmp_dest_ptr);
var
  row: JDIMENSION;
  image_ptr: JSAMPARRAY;
  data_ptr: JSAMPLE_PTR;
begin
  if dest^.inmemory then {write the image data from our virtual array}
    for row := cinfo^.output_height downto 1 do
    begin
      image_ptr := cinfo^.mem^.access_virt_sarray( j_common_ptr(cinfo),
         dest^.image_buffer, row-1, JDIMENSION(1), FALSE);
      data_ptr := JSAMPLE_PTR(image_ptr^[0]);
      {Nomssi - This won't work for 12bit samples}
      if dest^.outfile.Write(data_ptr^, dest^.row_width) <> dest^.row_width then
        ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
    end;
end;

//==============================================================================
//
// jinit_write_bmp
//
//==============================================================================
function jinit_write_bmp(cinfo: j_decompress_ptr; outfile: TDStream;
  inmemory: boolean): bmp_dest_ptr;
var
  dest: bmp_dest_ptr;
begin
  dest := bmp_dest_ptr (
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
          SizeOf(bmp_dest_struct)) );
  dest^.outfile := outfile;
  dest^.inmemory := inmemory;
  {image info}
  jpeg_calc_output_dimensions(cinfo);
  dest^.data_width := cinfo^.output_width * cinfo^.output_components;
  dest^.row_width := dest^.data_width;
  while ((dest^.row_width and 3) <> 0) do
    inc(dest^.row_width);
  dest^.pad_bytes := int(dest^.row_width-dest^.data_width);
  if (cinfo^.out_color_space = JCS_GRAYSCALE) then
    dest^.grayscale := True
  else if (cinfo^.out_color_space = JCS_RGB) then
    if (cinfo^.quantize_colors) then
      dest^.grayscale := True
    else
      dest^.grayscale := False
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BMP_COLORSPACE);
  {decompress buffer}
  dest^.buffer := cinfo^.mem^.alloc_sarray
    (j_common_ptr(cinfo), JPOOL_IMAGE, dest^.row_width, JDIMENSION (1));
  dest^.buffer_height := 1;
  {image buffer}
  if inmemory then
    dest^.image_buffer_height := cinfo^.output_height
  else
    dest^.image_buffer_height := 1;
  dest^.image_buffer := cinfo^.mem^.request_virt_sarray (
     j_common_ptr(cinfo), JPOOL_IMAGE, FALSE, dest^.row_width,
     dest^.image_buffer_height, JDIMENSION (1) );
  dest^.cur_output_row := 0;
  {result}
  jinit_write_bmp := dest;
end;

{ ------------------------------------------------------------------------ }
{   JPEG error handler                                                     }
{   for reference: JERROR.PAS in PASJPG10 library                          }
{                  LIPJPEG.DOC in \JPEG\C directory                        }
{   NOTE: we have replaced jpeg_std_error because it stores a static       }
{         message table (JDEFERR.PAS) in the jpeg_message_table field.     }
{ ------------------------------------------------------------------------ }

type
  my_error_ptr = ^my_error_mgr;
  my_error_mgr = record
    pub: jpeg_error_mgr;
  end;

//==============================================================================
//
// error_exit 
//
//==============================================================================
procedure error_exit (cinfo: j_common_ptr); far;
var
  buffer: string;
begin
  cinfo^.err^.format_message(cinfo, buffer);
  I_Error('TJPGTextureManager(): %s', [buffer]);
end;

//==============================================================================
//
// emit_message 
//
//==============================================================================
procedure emit_message (cinfo: j_common_ptr; msg_level: int); far;
var
  err: jpeg_error_mgr_ptr;
begin
  err := cinfo^.err;
  if (msg_level < 0) then
  begin
    {It's a warning message. Since corrupt files may generate many warnings,}
    {the policy implemented here is to show only the first warning,}
    {unless trace_level >= 3}
    if (err^.num_warnings = 0) or (err^.trace_level >= 3) then
      err^.output_message(cinfo);
    {Always count warnings in num_warnings}
    inc( err^.num_warnings );
  end
  else
    {It's a trace message. Show it if trace_level >= msg_level}
    if (err^.trace_level >= msg_level) then
      err^.output_message (cinfo);
end;

//==============================================================================
//
// output_message
//
//==============================================================================
procedure output_message(cinfo: j_common_ptr); far;
var
  buffer: string;
begin
  cinfo^.err^.format_message (cinfo, buffer);
  {message dialog}
  I_Warning('TJPGTextureManager(): %s', [buffer]);
end;

//==============================================================================
//
// format_message
//
//==============================================================================
procedure format_message(cinfo: j_common_ptr; var buffer: string); far;
begin
  buffer :=
    'JPEG ERROR -- #' + itoa(cinfo^.err^.msg_code);
end;

//==============================================================================
//
// reset_error_mgr 
//
//==============================================================================
procedure reset_error_mgr (cinfo: j_common_ptr); far;
begin
  cinfo^.err^.num_warnings := 0;
  {trace_level is not reset since it is an application-supplied parameter}
  cinfo^.err^.msg_code := 0;      {may be useful as a flag for "no error"}
end;

//==============================================================================
//
// jpeg_my_error
//
//==============================================================================
function jpeg_my_error(var err: my_error_mgr): jpeg_error_mgr_ptr;
begin
  {methods}
  err.pub.error_exit := error_exit;
  err.pub.emit_message := emit_message;
  err.pub.output_message := output_message;
  err.pub.format_message := format_message;
  err.pub.reset_error_mgr := reset_error_mgr;
  {fields}
  err.pub.trace_level := 0;         {default := no tracing}
  err.pub.num_warnings := 0;        {no warnings emitted yet}
  err.pub.msg_code := 0;            {may be useful as a flag for "no error"}
  {message table(s)}
  err.pub.jpeg_message_table := nil;    {we don't want to use a static table}
  err.pub.last_jpeg_message := pred(JMSG_LASTMSGCODE);
  err.pub.addon_message_table := nil;
  err.pub.first_addon_message := JMSG_NOMESSAGE;   {for safety}
  err.pub.last_addon_message := JMSG_NOMESSAGE;
  {return result}
  jpeg_my_error := @err;
end;

{ ------------------------------------------------------------------------ }
{   load JPEG stream and save as BITMAP stream                             }
{   for reference: DJPEG.PAS in PASJPG10 library                           }
{ ------------------------------------------------------------------------ }

//==============================================================================
//
// LoadJPEG
//
//==============================================================================
procedure LoadJPEG(const infile, outfile: TDStream; inmemory: boolean;
                   {decompression parameters:}
                   numcolors: integer = 0);
var
  cinfo: jpeg_decompress_struct;
  err: my_error_mgr;
  dest: bmp_dest_ptr;
  num_scanlines: JDIMENSION;
begin
  {initialize the JPEG decompression object with default error handling.}
  cinfo.err := jpeg_my_error(err);
  jpeg_create_decompress(@cinfo);
  try
    {specify the source of the compressed data}
    jpeg_stream_src(@cinfo, infile);
    {obtain image info from header, set default decompression parameters}
    jpeg_read_header(@cinfo, TRUE);
    {set parameters for decompression}
    if numcolors <> 0 then
    begin
      cinfo.desired_number_of_colors := numcolors;
      cinfo.quantize_colors := True;
    end;
    {...}
    {prepare for decompression, initialize internal state}
    dest := jinit_write_bmp(@cinfo, outfile, inmemory);
    jpeg_start_decompress(@cinfo);
    {process data}
    write_bmp_header(@cinfo, dest);
    while (cinfo.output_scanline < cinfo.output_height) do
    begin
      num_scanlines :=
        jpeg_read_scanlines(@cinfo, dest^.buffer, dest^.buffer_height);
      write_bmp_pixelrow(@cinfo, dest, num_scanlines);
    end;
    write_bmp_image(@cinfo, dest);
    {finish}
    jpeg_finish_decompress(@cinfo);
  finally
    {destroy}
    jpeg_destroy_decompress(@cinfo);
  end;
end;

//==============================================================================
//
// TJPGTextureManager.Create
//
//==============================================================================
constructor TJPGTextureManager.Create(const ext: string);
begin
  TTextureManager.Create;
  bmpstream := TDMemoryStream.Create;
  SetFileExt(ext);
end;

//==============================================================================
//
// TJPGTextureManager.Destroy
//
//==============================================================================
destructor TJPGTextureManager.Destroy;
begin
  bmpstream.Free;
  Inherited Destroy;
end;

//==============================================================================
//
// TJPGTextureManager.LoadHeader
//
//==============================================================================
function TJPGTextureManager.LoadHeader(stream: TDStream): boolean;
begin
  bmpstream.Seek(0, sFromBeginning);
  LoadJPEG(stream, bmpstream, false);
  bmpstream.Seek(0, sFromBeginning);
  LoadHeader := Inherited LoadHeader(bmpstream);
end;

//==============================================================================
//
// TJPGTextureManager.LoadImage
//
//==============================================================================
function TJPGTextureManager.LoadImage(stream: TDStream): boolean;
begin
  result := Inherited LoadImage(bmpstream);
  if result then
    FBitmap.Mirror;
end;

end.

