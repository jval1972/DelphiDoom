unit voxels;

interface

const
  FLG_SKIPX0 = 1;
  FLG_SKIPX1 = 2;
  FLG_SKIPY0 = 4;
  FLG_SKIPY1 = 8;
  FLG_SKIPZ0 = 16;
  FLG_SKIPZ1 = 32;

const
  MAXVOXELSIZE = 256;

type
  voxelitem_t = LongWord;
  voxelbuffer_t = array[0..MAXVOXELSIZE - 1, 0..MAXVOXELSIZE - 1, 0..MAXVOXELSIZE - 1] of voxelitem_t;
  voxelbuffer_p = ^voxelbuffer_t;

type
  voxelrenderflags_t = packed array[0..MAXVOXELSIZE - 1, 0..MAXVOXELSIZE - 1, 0..MAXVOXELSIZE - 1] of byte;
  voxelrenderflags_p = ^voxelrenderflags_t;

implementation

end.
 