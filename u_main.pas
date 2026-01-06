unit u_main;

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
  , u_child
  , unit_virtstringtree
  , laz.VirtualTrees
  ;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    vstMain: TLazVirtualStringTree;
    Panel1: TPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FchildFrm: TfrmChild;
  public
    procedure vstMainNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure LoadTreeFromChild;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormShow(Sender: TObject);
begin
  FchildFrm:= TfrmChild.Create(Self);
  with FchildFrm do
  begin
    Parent:= Panel1;
    BorderStyle:= bsNone;
    Align:= alClient;
    ShowInTaskBar:= stNever;
    FchildFrm.SaveTreeToStream;// Сохраняем дерево из дочернего модуля
    Show;
  end;


  // Загружаем в основное дерево
  LoadTreeFromChild;
end;

procedure TfrmMain.vstMainNodeClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  Node: PVirtualNode = nil;
  Data: PMyRecord = nil;
  Act: TBasicAction = nil;
begin
  Node := TBaseVirtualTree(Sender).GetFirstSelected;
  if Assigned(Node) then
  begin
    Data := TBaseVirtualTree(Sender).GetNodeData(Node);

    // Ищем действие в ActList дочернего модуля
    Act := FchildFrm.ActList.FindComponent(Data^.ActionName) as TBasicAction;

    if Assigned(Act) then
    begin
      ShowMessage('Executing Action: ' + Act.Name);
      Act.Execute;
    end
    else
    begin
      ShowMessage('Action not found: ' + Data^.ActionName);
    end;
  end;
end;

procedure TfrmMain.LoadTreeFromChild;
begin
  FchildFrm.tmpMS.Position := 0;
  vstMain.Clear;
  TVirtStringTreeHelper.LoadTreeFromStream(vstMain, FchildFrm.tmpMS);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Настройка дерева
  vstMain.NodeDataSize := SizeOf(TMyRecord);
  vstMain.OnNodeClick := @vstMainNodeClick;
end;

end.

