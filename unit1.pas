unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , Graphics
  , Dialogs
  , ExtCtrls
  , Unit2
  , unit_virtstringtree
  , laz.VirtualTrees
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    LazVirtualStringTree1: TLazVirtualStringTree;
    Panel1: TPanel;
    Splitter1: TSplitter;
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
var
  tmpFrm: TForm2 = nil;
begin
  tmpFrm:= TForm2.Create(Self);
  with tmpFrm do
  begin
    Parent:= Panel1;
    BorderStyle:= bsNone;
    Align:= alClient;
    ShowInTaskBar:= stNever;
    Show;
  end;
end;
end.

