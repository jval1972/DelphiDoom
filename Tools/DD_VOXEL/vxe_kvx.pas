unit vxe_kvx;

interface


const
  MAXKVXSIZE = 256;

type
  kvxbuffer_t = array[0..MAXKVXSIZE - 1, 0..MAXKVXSIZE - 1, 0..MAXKVXSIZE - 1] of word;
  kvxbuffer_p = ^kvxbuffer_t;

type
  kvxslab_t = record
	  ztop: byte;		// starting z coordinate of top of slab
	  zleng: byte;  // # of bytes in the color array - slab height
	  backfacecull: byte;	// low 6 bits tell which of 6 faces are exposed
	  col: array[0..255] of byte;// color data from top to bottom
  end;
  kvxslab_p = ^kvxslab_t;

  TSmallintArray = packed array[0..$7FFF] of Smallint;
  PSmallintArray = ^TSmallintArray;

  TSmallIntPArray = packed array[0..$7FFF] of PSmallIntArray;
  PSmallIntPArray = ^TSmallIntPArray;


implementation

end.
 