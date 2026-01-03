unit Unit2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
  , SysUtils
  , Forms
  , Controls
  , Graphics
  , Dialogs
  , ComCtrls
  , StdCtrls
  , unit_virtstringtree
  ;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Memo1: TMemo;
    PageControl1: TPageControl;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    tsOne: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FchildVST: TVirtStringTreeHelper;
    FtmpMS: TMemoryStream;
  public
    property childVST: TVirtStringTreeHelper read FchildVST;
    property tmpMS: TMemoryStream read FtmpMS;
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  FchildVST:= TVirtStringTreeHelper.Create;
  FtmpMS:= TMemoryStream.Create;
  FtmpMS.Clear;


end;

procedure TForm2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FtmpMS.Free;
  FreeAndNil(FchildVST);
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  //FtmpVST.SaveToStream(FtmpMS);
end;

end.

