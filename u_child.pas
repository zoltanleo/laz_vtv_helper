unit u_child;

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
  , ActnList
  , unit_virtstringtree
  , laz.VirtualTrees
  ;

type

  { TfrmChild }

  TfrmChild = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    tsOne_1: TTabSheet;
    tsOne_2: TTabSheet;
    tsTwo_1: TTabSheet;
    tsTwo_2: TTabSheet;
    tsOne: TTabSheet;
    tsTwo: TTabSheet;
    tsThree: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FActList: TActionList;
    FchildVST: TLazVirtualStringTree;
    FtmpMS: TMemoryStream;
    procedure FillActionList;
    procedure CreateTree;
  public
    property childVST: TLazVirtualStringTree read FchildVST;
    property tmpMS: TMemoryStream read FtmpMS;
    property ActList: TActionList read FActList;
    procedure SaveTreeToStream;
  end;

var
  frmChild: TfrmChild;

implementation

{$R *.lfm}

{ TfrmChild }

procedure TfrmChild.FormCreate(Sender: TObject);
begin
  FchildVST:= TLazVirtualStringTree.Create(Self);
  TVirtStringTreeHelper.InitializeTree(FChildVST); // устанавливаем NodeDataSize
  FActList:= TActionList.Create(Self);

  FtmpMS:= TMemoryStream.Create;
  FtmpMS.Clear;

  FillActionList;
  CreateTree;
end;

procedure TfrmChild.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfrmChild.FormDestroy(Sender: TObject);
begin
  FtmpMS.Free;
  FActList.Free;
  FchildVST.Free;
end;

procedure TfrmChild.FormShow(Sender: TObject);
begin
  //FtmpVST.SaveToStream(FtmpMS);
end;

procedure TfrmChild.FillActionList;
begin
  with TAction.Create(FActList) do
  begin
    Name := 'ActOneRoot';
    Caption := 'Action One Root';
    ActionList := FActList;
  end;

  with TAction.Create(FActList) do
  begin
    Name := 'ActOneChild_1';
    Caption := 'Action One Child 1';
    ActionList := FActList;
  end;

  with TAction.Create(FActList) do
  begin
    Name := 'ActOneChild_2';
    Caption := 'Action One Child 2';
    ActionList := FActList;
  end;

  with TAction.Create(FActList) do
  begin
    Name := 'ActTwoRoot';
    Caption := 'Action Two Root';
    ActionList := FActList;
  end;

  with TAction.Create(FActList) do
  begin
    Name := 'ActTwoChild_1';
    Caption := 'Action Two Child 1';
    ActionList := FActList;
  end;

  with TAction.Create(FActList) do
  begin
    Name := 'ActTwoChild_2';
    Caption := 'Action Two Child 2';
    ActionList := FActList;
  end;

  with TAction.Create(FActList) do
  begin
    Name := 'ActThreeRoot';
    Caption := 'Action Three Root';
    ActionList := FActList;
  end;
end;

procedure TfrmChild.CreateTree;
var
  RootNode: PVirtualNode = nil;
  ChildNode: PVirtualNode = nil;
begin
  // Создаем узлы с помощью хелпера
  RootNode := TVirtStringTreeHelper.AddNode(FChildVST, nil, 'ActOneRoot', 'Node One', 'tsOne');
  ChildNode := TVirtStringTreeHelper.AddNode(FChildVST, RootNode, 'ActOneChild_1', 'Node One Child 1', 'tsOne_1');
  ChildNode := TVirtStringTreeHelper.AddNode(FChildVST, RootNode, 'ActOneChild_2', 'Node One Child 2', 'tsOne_1');

  RootNode := TVirtStringTreeHelper.AddNode(FChildVST, nil, 'ActTwoRoot', 'Node Two', 'tsTwo');
  ChildNode := TVirtStringTreeHelper.AddNode(FChildVST, RootNode, 'ActTwoChild_1', 'Node Two Child 1', 'tsTwo_1');
  ChildNode := TVirtStringTreeHelper.AddNode(FChildVST, RootNode, 'ActTwoChild_2', 'Node Two Child 2', 'tsTwo_2');

  RootNode := TVirtStringTreeHelper.AddNode(FChildVST, nil, 'ActThreeRoot', 'Node Three', 'tsThree');
end;

procedure TfrmChild.SaveTreeToStream;
begin
  FtmpMS.Clear;
  TVirtStringTreeHelper.SaveTreeToStream(FChildVST, FtmpMS);
end;

end.

