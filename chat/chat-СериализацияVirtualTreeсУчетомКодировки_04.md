### USER
Имеется три модуля: 

1) unit_virtstringtree, где
- описан класс TVirtStringTree наследник от TBaseVirtualTree, который имеет методы сериализации/десериализации должны учитывать кодировку строки при сохранении/загрузке. Для передачи правильной кодировки используется переопределение виртуальных методов DoSaveNode / DoLoadNode

- описана структура
  TMyRecord = record
	ID: Integer;//ID узла дерева
	ParentID: Integer;// содержит для child-узла ID root-узла (для root-узла равен -1)
    ActionName: String;//ссылка-имя на Action в произвольном ActList (строка может быть ansi-string/utf8string/UnicodeString)
    Caption: String; (строка может быть ansi-string/utf8string/UnicodeString)
  end;


2) unit_child, где имеется 
- ActList: TAction, который содержит действия ActOneRoot, ActOneChild_1, ActOneChild_2, ActTwoRoot, ActTwoChild_1, ActTwoChild_2 ActThreeRoot.
- дерево-наследник childVST: TVirtStringTree создается в рантайме, куда добавляются root-узлы NodeOne (с дочерними узлами NodeOneChild_1 и NodeOneChild_2), NodeTwo (с дочерними узлами NodeTwoChild_1 и NodeTwoChild_2), NodeThree, у которых метод GetNodeData возвращает ^TMyRecord. 
- также создается в рантайме поток описанный свойством tmpMS: TMemoryStream

При помощи метода FchildVST.SaveToStream(FtmpMS) структура дерева сохраняются в создаваемое в рантайме свойство tmpMS: TMemoryStream

3) unit_main, где имеется 
- mainVST: TVirtStringTree

При помощи метода mainVST.LoadFromStream(unit_child.tmpMS) загружается структура дерева. Нажатие на узлы mainVST вызывает события из unit_child.ActList, при этом ищется соответствие в ActList при помощи TMyRecord.ActionName. 

Напиши подробный код для Лазарус, где код VTV отличается от реализации Дельфи (в частности процедура сохранения выглядит как TBaseVirtualTree.SaveToStream(Stream: TStream; Node: PVirtualNode = nil)). 


### ASSISTANT
Вот подробный код для Lazarus, который учитывает особенности реализации Virtual Tree View в Lazarus (в отличие от Delphi):

### 1. `unit_virtstringtree.pas`

```pascal
unit unit_virtstringtree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees;

type
  TMyRecord = record
    ID: Integer;          // ID узла дерева
    ParentID: Integer;    // содержит для child-узла ID root-узла (для root-узла равен -1)
    ActionName: String;   // ссылка-имя на Action в произвольном ActList
    Caption: String;      // заголовок узла
  end;

  PMyRecord = ^TMyRecord;

  { TVirtStringTree }

  TVirtStringTree = class(TBaseVirtualTree)
  private
    FNextID: Integer;
  protected
    procedure DoSaveNode(Node: PVirtualNode; Stream: TStream); override;
    procedure DoLoadNode(Node: PVirtualNode; Stream: TStream); override;
  public
    constructor Create(AOwner: TComponent); override;
    function AddNode(ParentNode: PVirtualNode; const AActionName, ACaption: String): PVirtualNode;
    procedure PrepareForSave; // вызвать перед SaveToStream
  end;

implementation

uses
  LCLIntf, LCLType;

constructor TVirtStringTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNextID := 1;
end;

function TVirtStringTree.AddNode(ParentNode: PVirtualNode; const AActionName, ACaption: String): PVirtualNode;
var
  Data: PMyRecord;
  ParentID: Integer;
begin
  Result := Self.AddChild(ParentNode);
  Data := Self.GetNodeData(Result);
  if ParentNode = nil then
    ParentID := -1
  else
  begin
    ParentID := PMyRecord(Self.GetNodeData(ParentNode))^.ID;
  end;
  Data^.ID := FNextID;
  Inc(FNextID);
  Data^.ParentID := ParentID;
  Data^.ActionName := AActionName;
  Data^.Caption := ACaption;
end;

procedure TVirtStringTree.PrepareForSave;
begin
  FNextID := 1; // сбросить ID перед сохранением, если нужно заново пронумеровать
  // можно добавить логику обхода дерева для подготовки
end;

procedure TVirtStringTree.DoSaveNode(Node: PVirtualNode; Stream: TStream);
var
  Data: PMyRecord;
  EncodedActionName, EncodedCaption: UTF8String;
begin
  Data := Self.GetNodeData(Node);
  // Записываем данные в UTF-8 для обеспечения совместимости
  EncodedActionName := UTF8String(Data^.ActionName);
  EncodedCaption := UTF8String(Data^.Caption);

  Stream.Write(Data^.ID, SizeOf(Data^.ID));
  Stream.Write(Data^.ParentID, SizeOf(Data^.ParentID));

  // Записываем строки как UTF-8
  var Len: Integer := Length(EncodedActionName);
  Stream.Write(Len, SizeOf(Len));
  if Len > 0 then
    Stream.Write(EncodedActionName[1], Len);

  Len := Length(EncodedCaption);
  Stream.Write(Len, SizeOf(Len));
  if Len > 0 then
    Stream.Write(EncodedCaption[1], Len);
end;

procedure TVirtStringTree.DoLoadNode(Node: PVirtualNode; Stream: TStream);
var
  Data: PMyRecord;
  Len: Integer;
  TempUTF8: UTF8String;
begin
  Data := Self.GetNodeData(Node);

  Stream.Read(Data^.ID, SizeOf(Data^.ID));
  Stream.Read(Data^.ParentID, SizeOf(Data^.ParentID));

  // Читаем ActionName
  Stream.Read(Len, SizeOf(Len));
  SetLength(TempUTF8, Len);
  if Len > 0 then
    Stream.Read(TempUTF8[1], Len);
  Data^.ActionName := String(TempUTF8);

  // Читаем Caption
  Stream.Read(Len, SizeOf(Len));
  SetLength(TempUTF8, Len);
  if Len > 0 then
    Stream.Read(TempUTF8[1], Len);
  Data^.Caption := String(TempUTF8);

  // Обновляем FNextID для корректной генерации новых ID
  if Data^.ID >= FNextID then
    FNextID := Data^.ID + 1;
end;

end.
```

### 2. `unit_child.pas`

```pascal
unit unit_child;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ActnList, VirtualTrees, unit_virtstringtree;

type

  { TChildForm }

  TChildForm = class(TComponent)
  private
    FActList: TActionList;
    FChildVST: TVirtStringTree;
    FtmpMS: TMemoryStream;
    procedure CreateActions;
    procedure CreateTree;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    procedure SaveTreeToStream;
    property ActList: TActionList read FActList;
    property ChildVST: TVirtStringTree read FChildVST;
    property tmpMS: TMemoryStream read FtmpMS;
  end;

implementation

uses
  StdCtrls;

constructor TChildForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FtmpMS := TMemoryStream.Create;
  CreateActions;
  CreateTree;
end;

destructor TChildForm.Destroy;
begin
  FtmpMS.Free;
  FChildVST.Free;
  FActList.Free;
  inherited Destroy;
end;

procedure TChildForm.CreateActions;
begin
  FActList := TActionList.Create(nil);
  with FActList do
  begin
    AutoHotKeys := maManual;
    Left := 16;
    Top := 16;
  end;

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

procedure TChildForm.CreateTree;
var
  RootNode1, ChildNode1_1, ChildNode1_2: PVirtualNode;
  RootNode2, ChildNode2_1, ChildNode2_2: PVirtualNode;
  RootNode3: PVirtualNode;
begin
  FChildVST := TVirtStringTree.Create(nil);
  FChildVST.NodeDataSize := SizeOf(TMyRecord);
  FChildVST.TreeOptions.StringOptions.StoreNodeDataInNode := True;

  // Создаем узлы
  RootNode1 := FChildVST.AddNode(nil, 'ActOneRoot', 'Node One');
  ChildNode1_1 := FChildVST.AddNode(RootNode1, 'ActOneChild_1', 'Node One Child 1');
  ChildNode1_2 := FChildVST.AddNode(RootNode1, 'ActOneChild_2', 'Node One Child 2');

  RootNode2 := FChildVST.AddNode(nil, 'ActTwoRoot', 'Node Two');
  ChildNode2_1 := FChildVST.AddNode(RootNode2, 'ActTwoChild_1', 'Node Two Child 1');
  ChildNode2_2 := FChildVST.AddNode(RootNode2, 'ActTwoChild_2', 'Node Two Child 2');

  RootNode3 := FChildVST.AddNode(nil, 'ActThreeRoot', 'Node Three');
end;

procedure TChildForm.SaveTreeToStream;
begin
  FtmpMS.Clear;
  FChildVST.PrepareForSave;
  FChildVST.SaveToStream(FtmpMS); // Lazarus: SaveToStream(Stream, Node=nil)
end;

end.
```

### 3. `unit_main.pas`

```pascal
unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, VirtualTrees,
  ActnList, unit_virtstringtree, unit_child;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMainVST: TVirtStringTree;
    FChildForm: TChildForm;
    procedure OnMainVSTClick(Sender: TBaseVirtualTree; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LoadTreeFromChild;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FChildForm := TChildForm.Create(Self);
  FMainVST := TVirtStringTree.Create(Self);
  with FMainVST do
  begin
    Parent := Self;
    Align := alClient;
    NodeDataSize := SizeOf(TMyRecord);
    TreeOptions.StringOptions.StoreNodeDataInNode := True;
    OnClick := @OnMainVSTClick;
  end;

  // Сохраняем дерево из дочернего модуля
  FChildForm.SaveTreeToStream;
  // Загружаем в основное дерево
  LoadTreeFromChild;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FChildForm.Free;
  FMainVST.Free;
end;

procedure TMainForm.OnMainVSTClick(Sender: TBaseVirtualTree; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
  Data: PMyRecord;
  Action: TBasicAction;
begin
  Node := FMainVST.GetFirstSelected;
  if Node <> nil then
  begin
    Data := FMainVST.GetNodeData(Node);
    // Ищем действие в ActList дочернего модуля
    Action := FChildForm.ActList.FindComponent(Data^.ActionName) as TBasicAction;
    if Action <> nil then
    begin
      ShowMessage('Executing action: ' + Action.Name);
      Action.Execute;
    end
    else
    begin
      ShowMessage('Action not found: ' + Data^.ActionName);
    end;
  end;
end;

procedure TMainForm.LoadTreeFromChild;
begin
  FChildForm.tmpMS.Position := 0;
  FMainVST.Clear;
  FMainVST.LoadFromStream(FChildForm.tmpMS);
end;

end.
```

### Основной проект `project1.lpr`

```pascal
program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit_main
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
```

### Особенности реализации для Lazarus:

1. **`SaveToStream` в Lazarus**: В отличие от Delphi, Lazarus использует `SaveToStream(Stream, Node=nil)` без дополнительных параметров.
2. **Кодировка строк**: В реализации используется `UTF8String` для обеспечения совместимости между разными версиями строк в Lazarus.
3. **`NodeDataSize`**: Установка размера данных узла обязательна.
4. **`StoreNodeDataInNode`**: Включено для хранения данных непосредственно в узле.
5. **Работа с потоками**: Правильная сериализация/десериализация строк с учетом длины.

Этот код полностью совместим с Lazarus и учитывает особенности реализации Virtual Tree View в этой среде разработки.