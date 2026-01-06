### USER
Имеется три модуля проекта

unit unit_virtstringtree;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, laz.VirtualTrees, LCLIntf, LCLType;

type

  PMyRecord = ^TMyRecord;
  TMyRecord = record
    ID: Integer;          // ID узла дерева
    ParentID: Integer;    // содержит для child-узла ID root-узла (для root-узла равен -1)
    ActionName: String;   // ссылка-имя на Action в произвольном ActList
    Caption: String;      // заголовок узла
    tsName: String;       // имя вкладки PageControl
  end;

  // Вспомогательный класс для доступа к защищенным полям
  TBaseVirtualTreeAccess = class(TBaseVirtualTree)
  end;

  { TVirtStringTreeHelper }

  TVirtStringTreeHelper = class
  private
    class procedure WriteNodeData(VTree: TBaseVirtualTree; Node: PVirtualNode; Stream: TStream);
    class procedure ReadNodeData(VTree: TBaseVirtualTree; Node: PVirtualNode; Stream: TStream);
    class function GetNodeCount(VTree: TBaseVirtualTree): SizeInt;
  public
    class procedure SaveTreeToStream(VTree: TBaseVirtualTree; Stream: TStream);
    class procedure LoadTreeFromStream(VTree: TBaseVirtualTree; Stream: TStream);
    class function AddNode(VTree: TBaseVirtualTree; ParentNode: PVirtualNode;
      const AActionName, ACaption, AtsName: String): PVirtualNode;
    class procedure PrepareForSave(VTree: TBaseVirtualTree); // вызвать перед SaveToStream
    class procedure InitializeTree(VTree: TBaseVirtualTree); // устанавливает NodeDataSize
  end;

implementation


{ TVirtStringTreeHelper }

class procedure TVirtStringTreeHelper.WriteNodeData(VTree: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
var
  Data: PMyRecord = nil;
  Len: SizeInt = 0;
  EncodedActionName: UTF8String = '';
  EncodedCaption: UTF8String = '';
  EncodedtsName: UTF8String = '';

begin
  Data:= VTree.GetNodeData(Node);

  // Записываем данные в UTF-8 для обеспечения совместимости
  EncodedActionName := UTF8String(Data^.ActionName);
  EncodedCaption := UTF8String(Data^.Caption);
  EncodedtsName := UTF8String(Data^.tsName);

  Stream.Write(Data^.ID, SizeOf(Data^.ID));
  Stream.Write(Data^.ParentID, SizeOf(Data^.ParentID));

  // Записываем строки как UTF-8
  Len:= Length(EncodedActionName);
  Stream.Write(Len, SizeOf(Len));

  if (Len > 0) then Stream.Write(EncodedActionName[1], Len);

  Len := Length(EncodedCaption);
  Stream.Write(Len, SizeOf(Len));

  if (Len > 0) then Stream.Write(EncodedCaption[1], Len);

  Len := Length(EncodedtsName);
  Stream.Write(Len, SizeOf(Len));

  if (Len > 0) then Stream.Write(EncodedtsName[1], Len);
end;

class procedure TVirtStringTreeHelper.ReadNodeData(VTree: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
var
  Data: PMyRecord = nil;
  Len: SizeInt = 0;
  TempUTF8: UTF8String = '';
  NextID: SizeInt = 0;
begin
  Data:= VTree.GetNodeData(Node);

  Stream.Read(Data^.ID, SizeOf(Data^.ID));
  Stream.Read(Data^.ParentID, SizeOf(Data^.ParentID));

  // Читаем ActionName
  Stream.Read(Len, SizeOf(Len));
  SetLength(TempUTF8, Len);

  if (Len > 0) then Stream.Read(TempUTF8[1], Len);
  Data^.ActionName := String(TempUTF8);

  // Читаем Caption
  Stream.Read(Len, SizeOf(Len));
  SetLength(TempUTF8, Len);

  if (Len > 0) then Stream.Read(TempUTF8[1], Len);
  Data^.Caption := String(TempUTF8);

  // Читаем tsName
  Stream.Read(Len, SizeOf(Len));
  SetLength(TempUTF8, Len);

  if (Len > 0) then Stream.Read(TempUTF8[1], Len);
  Data^.tsName := String(TempUTF8);

  // Обновляем NextID для корректной генерации новых ID
  NextID := VTree.Tag; // используем Tag для хранения NextID

  if (Data^.ID >= NextID) then NextID := Data^.ID + 1;
  VTree.Tag := NextID;
end;

class function TVirtStringTreeHelper.GetNodeCount(VTree: TBaseVirtualTree
  ): SizeInt;
var
  Node: PVirtualNode = nil;
begin
  Result := 0;
  Node := VTree.GetFirst();
  while Assigned(Node) do
  begin
    Inc(Result);
    Node := VTree.GetNext(Node);
  end;
end;

class procedure TVirtStringTreeHelper.SaveTreeToStream(VTree: TBaseVirtualTree;
  Stream: TStream);
var
  Node: PVirtualNode = nil;
  //NodeCount: SizeInt = 0;
begin
  PrepareForSave(VTree);

  // Подсчитываем количество узлов
  //NodeCount := GetNodeCount(VTree);
  //Stream.Write(NodeCount, SizeOf(NodeCount));
  Stream.Write(GetNodeCount(VTree), SizeOf(GetNodeCount(VTree)));

  // Рекурсивно сохраняем все узлы
  Node := VTree.GetFirst();

  while Assigned(Node) do
  begin
    WriteNodeData(VTree, Node, Stream);
    Node := VTree.GetNext(Node);
  end;
end;

class procedure TVirtStringTreeHelper.LoadTreeFromStream(
  VTree: TBaseVirtualTree; Stream: TStream);
var
  NodeCount: SizeInt = 0;
  i: SizeInt = 0;
  Node: PVirtualNode = nil;
begin
  VTree.Clear;
  VTree.Tag := 1; // сбросим NextID

  // Читаем количество узлов
  Stream.Read(NodeCount, SizeOf(NodeCount));

  // Создаем и загружаем узлы
  for i := 0 to Pred(NodeCount) do
  begin
    Node := VTree.AddChild(nil); // добавляем как дочерний узел корня
    ReadNodeData(VTree, Node, Stream);
  end;
end;

class function TVirtStringTreeHelper.AddNode(VTree: TBaseVirtualTree;
  ParentNode: PVirtualNode; const AActionName, ACaption, AtsName: String
  ): PVirtualNode;
var
  Data: PMyRecord = nil;
  ParentID: SizeInt = 0;
  NextID: SizeInt = 0;
begin
  Result := VTree.AddChild(ParentNode);
  Data := VTree.GetNodeData(Result);

  if not Assigned(ParentNode)
     then ParentID := -1
     else ParentID := PMyRecord(VTree.GetNodeData(ParentNode))^.ID;

  NextID := VTree.Tag; // используем Tag для хранения NextID
  Data^.ID := NextID;
  Inc(NextID);
  VTree.Tag := NextID;

  Data^.ParentID := ParentID;
  Data^.ActionName := AActionName;
  Data^.Caption := ACaption;
  Data^.tsName := AtsName;
end;

class procedure TVirtStringTreeHelper.PrepareForSave(VTree: TBaseVirtualTree);
begin
  VTree.Tag := 1; // сбросить ID перед сохранением
end;

class procedure TVirtStringTreeHelper.InitializeTree(VTree: TBaseVirtualTree);
begin
  // Используем вспомогательный класс для доступа к защищенному свойству
  TBaseVirtualTreeAccess(VTree).NodeDataSize := SizeOf(TMyRecord);
end;

end.


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


Внеси изменения в код модулей таким образом, чтобы в vstMain отображалась структура дерева, создаваемого в процедуре TfrmMain.LoadTreeFromChild

### ASSISTANT
Вот обновленный код всех трех модулей с необходимыми изменениями для корректного отображения структуры дерева в `vstMain`.

**unit unit_virtstringtree;**

```pascal
unit unit_virtstringtree;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, laz.VirtualTrees, LCLIntf, LCLType;

type

  PMyRecord = ^TMyRecord;
  TMyRecord = record
    ID: Integer;          // ID узла дерева
    ParentID: Integer;    // содержит для child-узла ID root-узла (для root-узла равен -1)
    ActionName: String;   // ссылка-имя на Action в произвольном ActList
    Caption: String;      // заголовок узла
    tsName: String;       // имя вкладки PageControl
  end;

  // Вспомогательный класс для доступа к защищенным полям
  TBaseVirtualTreeAccess = class(TBaseVirtualTree)
  end;

  { TVirtStringTreeHelper }

  TVirtStringTreeHelper = class
  private
    class procedure WriteNodeData(VTree: TBaseVirtualTree; Node: PVirtualNode; Stream: TStream);
    class procedure ReadNodeData(VTree: TBaseVirtualTree; Node: PVirtualNode; Stream: TStream);
    class function GetNodeCount(VTree: TBaseVirtualTree): SizeInt;
  public
    class procedure SaveTreeToStream(VTree: TBaseVirtualTree; Stream: TStream);
    class procedure LoadTreeFromStream(VTree: TBaseVirtualTree; Stream: TStream);
    class function AddNode(VTree: TBaseVirtualTree; ParentNode: PVirtualNode;
      const AActionName, ACaption, AtsName: String): PVirtualNode;
    class procedure PrepareForSave(VTree: TBaseVirtualTree); // вызвать перед SaveToStream
    class procedure InitializeTree(VTree: TBaseVirtualTree); // устанавливает NodeDataSize
  end;

implementation


{ TVirtStringTreeHelper }

class procedure TVirtStringTreeHelper.WriteNodeData(VTree: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
var
  Data: PMyRecord = nil;
  Len: SizeInt = 0;
  EncodedActionName: UTF8String = '';
  EncodedCaption: UTF8String = '';
  EncodedtsName: UTF8String = '';

begin
  Data:= VTree.GetNodeData(Node);

  // Записываем данные в UTF-8 для обеспечения совместимости
  EncodedActionName := UTF8String(Data^.ActionName);
  EncodedCaption := UTF8String(Data^.Caption);
  EncodedtsName := UTF8String(Data^.tsName);

  Stream.Write(Data^.ID, SizeOf(Data^.ID));
  Stream.Write(Data^.ParentID, SizeOf(Data^.ParentID));

  // Записываем строки как UTF-8
  Len:= Length(EncodedActionName);
  Stream.Write(Len, SizeOf(Len));

  if (Len > 0) then Stream.Write(EncodedActionName[1], Len);

  Len := Length(EncodedCaption);
  Stream.Write(Len, SizeOf(Len));

  if (Len > 0) then Stream.Write(EncodedCaption[1], Len);

  Len := Length(EncodedtsName);
  Stream.Write(Len, SizeOf(Len));

  if (Len > 0) then Stream.Write(EncodedtsName[1], Len);
end;

class procedure TVirtStringTreeHelper.ReadNodeData(VTree: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
var
  Data: PMyRecord = nil;
  Len: SizeInt = 0;
  TempUTF8: UTF8String = '';
  NextID: SizeInt = 0;
begin
  Data:= VTree.GetNodeData(Node);

  Stream.Read(Data^.ID, SizeOf(Data^.ID));
  Stream.Read(Data^.ParentID, SizeOf(Data^.ParentID));

  // Читаем ActionName
  Stream.Read(Len, SizeOf(Len));
  SetLength(TempUTF8, Len);

  if (Len > 0) then Stream.Read(TempUTF8[1], Len);
  Data^.ActionName := String(TempUTF8);

  // Читаем Caption
  Stream.Read(Len, SizeOf(Len));
  SetLength(TempUTF8, Len);

  if (Len > 0) then Stream.Read(TempUTF8[1], Len);
  Data^.Caption := String(TempUTF8);

  // Читаем tsName
  Stream.Read(Len, SizeOf(Len));
  SetLength(TempUTF8, Len);

  if (Len > 0) then Stream.Read(TempUTF8[1], Len);
  Data^.tsName := String(TempUTF8);

  // Обновляем NextID для корректной генерации новых ID
  NextID := VTree.Tag; // используем Tag для хранения NextID

  if (Data^.ID >= NextID) then NextID := Data^.ID + 1;
  VTree.Tag := NextID;
end;

class function TVirtStringTreeHelper.GetNodeCount(VTree: TBaseVirtualTree
  ): SizeInt;
var
  Node: PVirtualNode = nil;
begin
  Result := 0;
  Node := VTree.GetFirst();
  while Assigned(Node) do
  begin
    Inc(Result);
    Node := VTree.GetNext(Node);
  end;
end;

class procedure TVirtStringTreeHelper.SaveTreeToStream(VTree: TBaseVirtualTree;
  Stream: TStream);
var
  Node: PVirtualNode = nil;
begin
  PrepareForSave(VTree);

  // Подсчитываем количество узлов
  Stream.Write(GetNodeCount(VTree), SizeOf(GetNodeCount(VTree)));

  // Рекурсивно сохраняем все узлы
  Node := VTree.GetFirst();

  while Assigned(Node) do
  begin
    WriteNodeData(VTree, Node, Stream);
    Node := VTree.GetNext(Node);
  end;
end;

class procedure TVirtStringTreeHelper.LoadTreeFromStream(
  VTree: TBaseVirtualTree; Stream: TStream);
var
  NodeCount: SizeInt = 0;
  i: SizeInt = 0;
  Node: PVirtualNode = nil;
  AllNodes: array of record
    Node: PVirtualNode;
    Data: TMyRecord;
  end;
  NodeMap: TFPHashMap; // Карта ID -> Node
  j: SizeInt = 0;
  ParentNode: PVirtualNode = nil;
  ParentID: Integer = 0;
begin
  VTree.Clear;
  VTree.Tag := 1; // сбросим NextID

  // Читаем количество узлов
  Stream.Read(NodeCount, SizeOf(NodeCount));

  if NodeCount <= 0 then Exit;

  SetLength(AllNodes, NodeCount);
  NodeMap := TFPHashMap.Create;

  // Создаем все узлы и читаем их данные
  for i := 0 to Pred(NodeCount) do
  begin
    AllNodes[i].Node := VTree.AddChild(nil);
    ReadNodeData(VTree, AllNodes[i].Node, Stream);
    AllNodes[i].Data := PMyRecord(VTree.GetNodeData(AllNodes[i].Node))^;
    NodeMap.Add(IntToStr(AllNodes[i].Data.ID), AllNodes[i].Node);
  end;

  // Восстанавливаем структуру дерева: устанавливаем родителей
  for j := 0 to High(AllNodes) do
  begin
    if AllNodes[j].Data.ParentID = -1 then
    begin
      // Это корневой узел, оставляем как есть
      Continue;
    end
    else
    begin
      ParentID := AllNodes[j].Data.ParentID;
      ParentNode := PVirtualNode(NodeMap.Find(IntToStr(ParentID)));
      if Assigned(ParentNode) then
      begin
        VTree.MoveTo(AllNodes[j].Node, ParentNode, amAddChildLast, False);
      end;
    end;
  end;

  NodeMap.Free;
end;

class function TVirtStringTreeHelper.AddNode(VTree: TBaseVirtualTree;
  ParentNode: PVirtualNode; const AActionName, ACaption, AtsName: String
  ): PVirtualNode;
var
  Data: PMyRecord = nil;
  ParentID: SizeInt = 0;
  NextID: SizeInt = 0;
begin
  Result := VTree.AddChild(ParentNode);
  Data := VTree.GetNodeData(Result);

  if not Assigned(ParentNode)
     then ParentID := -1
     else ParentID := PMyRecord(VTree.GetNodeData(ParentNode))^.ID;

  NextID := VTree.Tag; // используем Tag для хранения NextID
  Data^.ID := NextID;
  Inc(NextID);
  VTree.Tag := NextID;

  Data^.ParentID := ParentID;
  Data^.ActionName := AActionName;
  Data^.Caption := ACaption;
  Data^.tsName := AtsName;
end;

class procedure TVirtStringTreeHelper.PrepareForSave(VTree: TBaseVirtualTree);
begin
  VTree.Tag := 1; // сбросить ID перед сохранением
end;

class procedure TVirtStringTreeHelper.InitializeTree(VTree: TBaseVirtualTree);
begin
  // Используем вспомогательный класс для доступа к защищенному свойству
  TBaseVirtualTreeAccess(VTree).NodeDataSize := SizeOf(TMyRecord);
end;

end.
```

**unit u_child;**

```pascal
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
```

**unit u_main;**

```pascal
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
  vstMain.Repaint; // Обновляем отображение
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Настройка дерева
  TVirtStringTreeHelper.InitializeTree(vstMain); // Используем хелпер для инициализации
  vstMain.OnNodeClick := @vstMainNodeClick;
end;

end.
```

**Ключевые изменения:**

1.  **`unit_virtstringtree`:**
    *   В процедуре `LoadTreeFromStream` добавлена логика для восстановления иерархии узлов. Теперь она читает все узлы, сохраняет их в промежуточный массив, а затем использует `MoveTo` для установки правильных родительско-дочерних связей на основе `ParentID`.
    *   Используется `TFPHashMap` для эффективного поиска узла по его `ID`.
2.  **`unit u_main`:**
    *   В `FormCreate` теперь используется `TVirtStringTreeHelper.InitializeTree(vstMain)` вместо прямого доступа к `NodeDataSize`, что соответствует принципам инкапсуляции.
    *   В `LoadTreeFromChild` добавлен вызов `vstMain.Repaint` для принудительного обновления отображения дерева после загрузки.