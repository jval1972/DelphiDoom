unit ddmd2util;

interface

procedure DDMD2_Init;

procedure DDMD2_Done;

implementation

uses
  deh_main;
  
procedure DDMD2_Init;
begin
  DEH_Init;
end;

procedure DDMD2_Done;
begin
  DEH_ShutDown;
end;

end.
