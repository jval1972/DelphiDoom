unit ddmd2main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, dglOpenGL,
  ExtCtrls, StdCtrls, CheckLst, ComCtrls;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel1: TPanel;
    CheckListBox1: TCheckListBox;
    Label1: TLabel;
    Label2: TLabel;
    CheckListBox2: TCheckListBox;
    Label3: TLabel;
    CheckListBox3: TCheckListBox;
    Label4: TLabel;
    RadiusEdit: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    rc : HGLRC;    // Rendering Context
    dc  : HDC;     // Device Context
    ElapsedTime, AppStart, LastTime : DWord;  // Timing variables
    procedure glDraw;
    procedure Idle(Sender: TObject; var Done: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  deh_main,
  ddmd2util;

{------------------------------------------------------------------}
{  Function to draw the actual scene                               }
{------------------------------------------------------------------}
procedure TForm1.glDraw;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);    // Clear The Screen And The Depth Buffer
  glLoadIdentity;                                       // Reset The View

  glTranslatef(0, 0, -4);

  glRotatef(ElapsedTime/10, 0, 1, 0);

  glBegin(GL_TRIANGLES);
    glColor3f(1, 0, 0);  glVertex3f(-1, -1, 0);
    glColor3f(0, 1, 0);  glVertex3f( 1, -1, 0);
    glColor3f(0, 0, 1);  glVertex3f( 0,  1, 0);
  glEnd;
end;


{------------------------------------------------------------------}
{  Initialise OpenGL                                               }
{------------------------------------------------------------------}
procedure glInit;
begin
  glClearColor(0.0, 0.0, 0.0, 0.0); 	   // Black Background
  glShadeModel(GL_SMOOTH);                 // Enables Smooth Color Shading
  glClearDepth(1.0);                       // Depth Buffer Setup
  glEnable(GL_DEPTH_TEST);                 // Enable Depth Buffer
  glDepthFunc(GL_LESS);		           // The Type Of Depth Test To Do

  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);   //Realy Nice perspective calculations
end;


{------------------------------------------------------------------}
{  Create the form and initialist openGL                           }
{------------------------------------------------------------------}
procedure TForm1.FormCreate(Sender: TObject);
var
  pfd: TPIXELFORMATDESCRIPTOR;
  pf: Integer;
  i: integer;
begin
  DDMD2_Init;
  for i := 0 to mobj_flags.Count - 1 do
    CheckListBox1.Items.Add(mobj_flags[i]);
  for i := 0 to mobj_flags_ex.Count - 1 do
    CheckListBox2.Items.Add(mobj_flags_ex[i]);
  for i := 0 to mobj_flags2_ex.Count - 1 do
    CheckListBox3.Items.Add(mobj_flags2_ex[i]);

  InitOpenGL;
  ReadExtensions;
  ReadImplementationProperties;

  // OpenGL initialisieren
  dc := GetDC(Panel1.Handle);

  // PixelFormat
  pfd.nSize := sizeof(pfd);
  pfd.nVersion := 1;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER or 0;
  pfd.iPixelType := PFD_TYPE_RGBA;      // PFD_TYPE_RGBA or PFD_TYPEINDEX
  pfd.cColorBits := 32;

  pf := ChoosePixelFormat(dc, @pfd);   // Returns format that most closely matches above pixel format
  SetPixelFormat(dc, pf, @pfd);

  rc := wglCreateContext(dc);    // Rendering Context = window-glCreateContext
  wglMakeCurrent(dc, rc);        // Make the DC (Form1) the rendering Context

  // Initialist GL environment variables
  glInit;
  Panel1Resize(sender);    // sets up the perspective
  AppStart := GetTickCount;

  // when the app has spare time, render the GL scene
  Application.OnIdle := Idle;
end;


{------------------------------------------------------------------}
{  Release rendering context when form gets detroyed               }
{------------------------------------------------------------------}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  wglMakeCurrent(0,0);
  wglDeleteContext(rc);
  DDMD2_Done;
end;


{------------------------------------------------------------------}
{  Application onIdle event                                        }
{------------------------------------------------------------------}
procedure TForm1.Idle(Sender: TObject; var Done: Boolean);
begin
  Done := FALSE;

  LastTime := ElapsedTime;
  ElapsedTime := GetTickCount - AppStart;      // Calculate Elapsed Time
  ElapsedTime := (LastTime + ElapsedTime) DIV 2; // Average it out for smoother movement

  glDraw;                         // Draw the scene
  SwapBuffers(DC);                  // Display the scene
end;


{------------------------------------------------------------------}
{  If the panel resizes, reset the GL scene                        }
{------------------------------------------------------------------}
procedure TForm1.Panel1Resize(Sender: TObject);
begin
  glViewport(0, 0, Panel1.Width, Panel1.Height);    // Set the viewport for the OpenGL window
  glMatrixMode(GL_PROJECTION);                      // Change Matrix Mode to Projection
  glLoadIdentity;                                   // Reset View
  gluPerspective(45.0, Panel1.Width/Panel1.Height, 1.0, 500.0);  // Do the perspective calculations. Last value = max clipping depth

  glMatrixMode(GL_MODELVIEW);         // Return to the modelview matrix
end;


{------------------------------------------------------------------}
{  Monitors all keypress events for the app                        }
{------------------------------------------------------------------}
procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    Close;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;

end.

