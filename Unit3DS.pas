{-----------------------------------------------------------------------------
 Unit Name: Unit3DS
 Author:    Nitrogen
 Purpose:   Loads 3DS files.
 History:   27 Aug 02 - First Implementation.
-----------------------------------------------------------------------------}

unit Unit3DS;

interface

Uses Windows, Classes, dglOpenGL, Sysutils;

Const
     ID_HEADER = $4D4D;
     ID_OBJECTINFO = $3D3D;
     ID_VERSION = $0002;
     ID_SCALE = $0100;
     ID_MESH_VERSION = $3D3E;
     ID_OBJECT = $4000;
     ID_OBJECT_MESH = $4100;		   // This lets us know that we are reading a new object
     ID_OBJECT_VERTICES = $4110;	   // The objects vertices
     ID_OBJECT_FACES = $4120;		   // The objects faces
     ID_OBJECT_MATERIAL = $4130;	   // This is found if the object has a material, either texture map or color
     ID_OBJECT_UV = $4140;		   // The UV texture coordinates
     ID_KEYFRAME = $B000;
     ID_MESH_MATRIX = $4160;
     ID_LIGHT = $4600;
     ID_CAMERA = $4700;
     ID_MATERIAL = $AFFF;


Type
     Vertex = array[0..2] of single;
     FaceData = array[0..3] of word;

     Coord = record
                  U, V: single;
             end;

     PFile3DS = ^File3DS;
     PChunk = ^Chunk;
     Chunk = Object
              ID: word;
              Length: longword;
              Start:  longword; //Start of chunk in file

              SubChunks: array of PChunk;

              {$Ifdef Debug}
                Parent: PChunk;
              {$endif}
              Data: PFile3DS;  //Pointer back to the File3DS structure..

              Function NewSubchunk: integer;
              Procedure Read(var F: TFileStream);
              Destructor Destroy;
             end;



     File3ds = object
                NumVerts: cardinal;
                NumFaces: cardinal;
                NumCoords: cardinal;

                V: array of vertex;
                N: array of vertex;
                F: array of FaceData;
                U: array of Coord;

                C: PChunk; //For Loading Purposes..
                LastVCount: cardinal;
                LastFCount, LastUCount: cardinal;

                Procedure Draw;
                Procedure CalcNormals;
                Function Load3ds(Filename: string): boolean;
                Procedure Clear;
               end;

{$ifdef Debug}
Var
    Log: array of string;
{$endif}

implementation

Procedure SkipChunk(Var F: TFilestream; Len: longword);
begin
  F.Seek(Len-6, soFromCurrent);
end;


Destructor Chunk.Destroy;
Var I: integer;
begin
   for I := 0 to high(SubChunks) do
   if SubChunks[I] <> nil then
   begin
    SubChunks[I]^.Destroy;
    Dispose(SubChunks[I]);
   end;
end;

Function Chunk.NewSubchunk: integer;
begin
  setlength(SubChunks, High(SubChunks)+2);
  New(subchunks[High(SubChunks)]);

  SubChunks[High(SubChunks)].Data := Self.Data;

  Result := High(SubChunks);

end;


Procedure Chunk.Read(Var F: TFileStream);
Var S: String;
    Ch: char;
    I: integer;
    Version: Cardinal;
    Temp: single;
    W: Word;
begin
    Start := F.Position;

     f.ReadBuffer(ID, 2);
     f.ReadBuffer(Length, 4);


     Case ID of
       ID_VERSION:
       begin
          F.ReadBuffer(Version, 4);
          if Version > 3 then
            messagebox(0,'This file is from a later version of 3DS. It may not load correctly.', 'Incorrect Version', MB_OK or MB_ICONWARNING);

       end;

       ID_OBJECT_VERTICES:
       begin

          Data^.LastVCount := Data^.NumVerts;
          f.ReadBuffer(W, 2);
          Data^.NumVerts := W + Data^.LastVCount;

          SetLength(Data^.V, Data^.Numverts);
          For I := Data^.Lastvcount to Data^.NumVerts-1 do
          begin
            F.ReadBuffer(Data^.V[I], Sizeof(Vertex));
            //Swap Z and Y for OGL style orientation...
            Temp := Data^.V[I][1];
            Data^.V[I][1] := Data^.V[I][2];
            Data^.V[I][2] := -Temp;
          end;
       end;


       ID_OBJECT_UV:
       begin

          Data^.LastUCount := Data^.NumCoords;
          f.ReadBuffer(W, 2);
          Data^.NumCoords := W + Data^.LastUCount;

          SetLength(Data^.U, Data^.NumCoords);
          For I := Data^.LastUcount to Data^.NumCoords-1 do
            F.ReadBuffer(Data^.U[I], Sizeof(Coord));
       end;


       ID_OBJECT_FACES:
       begin

          Data^.LastFCount := Data^.NumFaces;
          f.ReadBuffer(W, 2);
          Data^.NumFaces := W + Data^.LastFCount;

          SetLength(Data^.F, Data^.NumFaces);
          For I := Data^.LastFcount to Data^.NumFaces-1 do
          begin
            F.ReadBuffer(Data^.F[I], Sizeof(FaceData));

            //We are merging all the objects into one vertex list...
            Data^.F[I][0] := Data^.F[I][0] + Data^.LastVCount;
            Data^.F[I][1] := Data^.F[I][1] + Data^.LastVCount;
            Data^.F[I][2] := Data^.F[I][2] + Data^.LastVCount;


          end;
       end;



       ID_OBJECTINFO, ID_HEADER, ID_OBJECT_MESH:
         begin
           Repeat
             SubChunks[NewSubChunk]^.read(F);
           until F.Position >= (Start+Length);
         end;

       ID_OBJECT:
         begin

           Repeat
            F.ReadBuffer(Ch, 1);//Name of Object;
            S := S + Ch;
           until Ch = #0;

           Repeat
             SubChunks[NewSubChunk]^.read(F);
           until F.Position >= (Start+Length);
         end;

       else
         SkipChunk(F, Length);


     end;



end;

Procedure File3dS.Draw;
Var I, J: integer;
    HasCoords: boolean;
begin
  if NumCoords > 0 then HasCoords := True else HasCoords := False;
  For I := 0 to NumFaces-1 do
  begin
   glBegin(GL_TRIANGLE_STRIP);
    For J := 0 to 2 do
    begin

       if HasCoords then glTexCoord2f(U[F[I][J]].U, U[F[I][J]].V);
       glNormal3fv(@N[F[I][J]]);
       glVertex3fv(@V[F[I][J]]);

    end;
   glEnd;
  end;


end;


function GetNormal(V1, V2, V3: array of Single): Vertex;
Var Res: Vertex;
    l: single;
Begin
//  Mirror(v2, TRUE, TRUE, TRUE);
 V2[0] := -V2[0];
 V2[1] := -V2[1];
 V2[2] := -V2[2];

//  Translate(v1, v2);
 V1[0] := V1[0] + V2[0];
 V1[1] := V1[1] + V2[1];
 V1[2] := V1[2] + V2[2];

//  Translate(v3, v2);
 V3[0] := V3[0] + V2[0];
 V3[1] := V3[1] + V2[1];
 V3[2] := V3[2] + V2[2];

 //  Result := CrossProduct(v1, v3);
 Res[0] := v1[1] * v3[2] - v3[1] * v1[2];
 Res[1] := v3[0] * v1[2] - v1[0] * v3[2];
 Res[2] := v1[0] * v3[1] - v3[0] * v1[1];

 //  Normalize(Result);
  L := sqrt(Res[0]*Res[0] + Res[1]*Res[1] + Res[2]*Res[2]);
  if l <> 0 then
  begin
      Res[0] := Res[0] / l;
      res[1] := Res[1] / l;
      Res[2] := Res[2] / l;
  end;
  Result := Res;
end;


Procedure file3DS.CalcNormals;
Var I, J: integer;
    Normal: vertex;
    L : single;
    FaceNormals: array of vertex;
    Count: integer;
begin
   setlength(N, high(V)+1);
   setlength(FaceNormals, high(F)+1);

   for I := 0 to numFaces-1 do
   begin

     Normal := GetNormal(V[F[I][0]],V[F[I][1]],V[F[I][2]]);
     FaceNormals[I] := Normal;

   end;


   For I := 0 to NumVerts-1 do
   begin
      ZeroMemory(@Normal, sizeof(Normal));
      Count := 0;

      for J := 0 to NumFaces-1 do
      begin
          if (F[J][0] = I) or (F[J][1] = I) or (F[J][2] = I) then
          begin
             Normal[0] := Normal[0] + FaceNormals[J][0];
             Normal[1] := Normal[1] + FaceNormals[J][1];
             Normal[2] := Normal[2] + FaceNormals[J][2];
             Inc(Count);
          end;
      end;

      N[I][0] := Normal[0] / Count;
      N[I][1] := Normal[1] / Count;
      N[I][2] := Normal[2] / Count;

      L := sqrt(N[I][0]*N[I][0] + N[I][1]*N[I][1] + N[I][2]*N[I][2]);
      if L <> 0 then
      begin
          N[I][0] := N[I][0] / l;
          N[I][1] := N[I][1] / l;
          N[I][2] := N[I][2] / l;
      end;

   end;


end;

Procedure file3ds.Clear;
begin
    Setlength(V, 0);
    Setlength(F, 0);
    Setlength(U, 0);
    SetLength(N, 0);
    NumVerts := 0;
    NumFaces := 0;
    NumCoords := 0;
end;

function file3ds.Load3DS(Filename: string): boolean;
var Fl: TFilestream;

begin
   try
    Result := True;
    Clear;
    Fl := Tfilestream.Create(Filename, fmOpenRead);

    New(C);
    C^.Data := @Self;
    C^.Read(Fl);
   except
    Messagebox(0, pchar('Error While loading '+filename), '3DS Loading Error', MB_OK or MB_ICONERROR);
   end;


    Fl.free;
    C^.Destroy;
    Dispose(C);
    CalcNormals;

end;

end.

