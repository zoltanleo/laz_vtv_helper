### USER
Имеется два модуля: 
1) unit_child, где имеется 
- ActList: TAction, который содержит действия ActOneRoot, ActOneChild_1, ActOneChild_2, ActTwoRoot, ActTwoChild_1, ActTwoChild_2 ActThreeRoot.
- запись 

  TMyRecord = record
	ID: Integer;//ID узла дерева
	ParentID: Integer;// содержит для child-узла ID root-узла (для root-узла равен -1)
    ActionName: String;//ссылка-имя на Action в ActList (строка может быть ansi-string/utf8string/UnicodeString)
    Caption: String; (строка может быть ansi-string/utf8string/UnicodeString)
  end;
 
- дерево-наследник childVST: TBaseVirtualTree создается в рантайме, куда добавляются root-узлы NodeOne (с дочерними узлами NodeOneChild_1 и NodeOneChild_2), NodeTwo (с дочерними узлами NodeTwoChild_1 и NodeTwoChild_2), NodeThree, у которых метод GetNodeData возвращает ^TMyRecord. 
- также создается в рантайме поток описанный свойством tmpMS: TMemoryStream


При помощи метода FchildVST.SaveToStream(FtmpMS) структура дерева сохраняются в создаваемое в рантайме свойство tmpMS: TMemoryStream

2) unit_main, где имеется 
- mainVST: TBaseVirtualTree

При помощи метода mainVST.LoadFromStream(unit_child.tmpMS) загружается структура дерева. Нажатие на узлы mainVST вызывает события из ActList, при этом ищется соответствие в ActList при помощи TMyRecord.ActionName. 

Напиши подробный код для Лазарус, где код VTV отличается от реализации Дельфи (в частности процедура сохранения выглядит как TBaseVirtualTree.SaveToStream(Stream: TStream; Node: PVirtualNode = nil)). Методы сериализации/десериализации должны учитывать кодировку строки при сохранении/загрузке. Дерево-наследник childVST должно быть выделено в отдельный класс TVirtStringTree, который должен быть описан в отдельном подключаемом модуле

### ASSISTANT
Ниже представлен подробный код для Lazarus (Free Pascal + LCL), организованный по модулям, с учетом особенностей сериализации/десериализации дерева `TBaseVirtualTree` и корректной работы со строками (в т.ч. Unicode).  
В Lazarus компонент `TBaseVirtualTree` принадлежит пакету **VirtualTreeView**, совместимость с Delphi требует небольших адаптаций, особенно при работе с потоками и кодировками.

---

### 📁 Структура проекта:
- `unit_child.pas` — содержит логику дочернего дерева и `TMyRecord`
- `unit_virtstringtree.pas` — определяет кастомный класс `TVirtStringTree`
- `unit_main.pas` — главное окно, загружает дерево из `unit_child`

---

## 🔹 Модуль 1: `unit_virtstringtree.pas`  
*(описание кастомного дерева)*

```pascal
// unit_virtstringtree.pas
unit unit_virtstringtree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees;

type
  PMyRecord = ^TMyRecord;
  TMyRecord = record
    ID: Integer;
    ParentID: Integer;
    ActionName: String; // UnicodeString в Lazarus при {$H+}
    Caption: String;
  end;

  TVirtStringTree = class(TBaseVirtualTree)
  protected
    procedure DoSaveNode(Stream: TStream; Node: PVirtualNode); override;
    procedure DoLoadNode(Stream: TStream; Node: PVirtualNode; var DataSize: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  LazUTF8; // если нужно явное управление UTF8

// В Lazarus TBaseVirtualTree использует DoSaveNode/DoLoadNode для сериализации узлов

procedure TVirtStringTree.DoSaveNode(Stream: TStream; Node: PVirtualNode);
var
  Data: PMyRecord;
  ActionNameUTF8, CaptionUTF8: RawByteString;
begin
  Data := PMyRecord(GetNodeData(Node));
  if Assigned(Data) then
  begin
    // Сохраняем целые числа как есть
    Stream.WriteBuffer(Data^.ID, SizeOf(Integer));
    Stream.WriteBuffer(Data^.ParentID, SizeOf(Integer));

    // Сохраняем строки в UTF-8, чтобы избежать проблем с кодировками
    ActionNameUTF8 := UTF8Encode(Data^.ActionName);
    CaptionUTF8 := UTF8Encode(Data^.Caption);

    // Сначала длина строки (в байтах), затем сама строка
    WriteStrToStream(Stream, ActionNameUTF8);
    WriteStrToStream(Stream, CaptionUTF8);
  end;
end;

procedure TVirtStringTree.DoLoadNode(Stream: TStream; Node: PVirtualNode; var DataSize: Integer);
var
  Data: PMyRecord;
  ActionNameUTF8, CaptionUTF8: RawByteString;
begin
  Data := PMyRecord(GetNodeData(Node));
  if Assigned(Data) then
  begin
    Stream.ReadBuffer(Data^.ID, SizeOf(Integer));
    Stream.ReadBuffer(Data^.ParentID, SizeOf(Integer));

    ActionNameUTF8 := ReadStrFromStream(Stream);
    CaptionUTF8 := ReadStrFromStream(Stream);

    Data^.ActionName := UTF8Decode(ActionNameUTF8);
    Data^.Caption := UTF8Decode(CaptionUTF8);
  end;
end;

constructor TVirtStringTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Настраиваем размер данных узла
  NodeDataSize := SizeOf(TMyRecord);
end;

// Вспомогательные функции (можно вынести в отдельный utils-модуль)

procedure WriteStrToStream(Stream: TStream; const S: RawByteString);
var
  Len: Integer;
begin
  Len := Length(S);
  Stream.WriteBuffer(Len, SizeOf(Integer));
  if Len > 0 then
    Stream.WriteBuffer(S[1], Len);
end;

function ReadStrFromStream(Stream: TStream): RawByteString;
var
  Len: Integer;
begin
  Stream.ReadBuffer(Len, SizeOf(Integer));
  SetLength(Result, Len);
  if Len > 0 then
    Stream.ReadBuffer(Result[1], Len);
end;

end.
```

---

## 🔹 Модуль 2: `unit_child.pas`

```pascal
// unit_child.pas
unit unit_child;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, ActnList, unit_virtstringtree;

type
  TChildForm = class(TComponent)
  private
    FActList: TActionList;
    FChildVST: TVirtStringTree;
    FtmpMS: TMemoryStream;
    FNextID: Integer;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property ChildVST: TVirtStringTree read FChildVST;
    property tmpMS: TMemoryStream read FtmpMS;

    procedure BuildTree;
    procedure SaveTreeToStream;
  end;

implementation

uses
  Forms; // для Application

constructor TChildForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActList := TActionList.Create(nil);
  FChildVST := TVirtStringTree.Create(nil);
  FtmpMS := TMemoryStream.Create;
  FNextID := 0;

  // Создаём действия
  with TAction.Create(FActList) do begin
    Name := 'ActOneRoot';
    Caption := 'Root One';
  end;
  with TAction.Create(FActList) do begin
    Name := 'ActOneChild_1';
    Caption := 'Child 1.1';
  end;
  with TAction.Create(FActList) do begin
    Name := 'ActOneChild_2';
    Caption := 'Child 1.2';
  end;

  with TAction.Create(FActList) do begin
    Name := 'ActTwoRoot';
    Caption := 'Root Two';
  end;
  with TAction.Create(FActList) do begin
    Name := 'ActTwoChild_1';
    Caption := 'Child 2.1';
  end;
  with TAction.Create(FActList) do begin
    Name := 'ActTwoChild_2';
    Caption := 'Child 2.2';
  end;

  with TAction.Create(FActList) do begin
    Name := 'ActThreeRoot';
    Caption := 'Root Three';
  end;

  BuildTree;
end;

destructor TChildForm.Destroy;
begin
  FtmpMS.Free;
  FChildVST.Free;
  FActList.Free;
  inherited Destroy;
end;

function NewNodeID: Integer;
begin
  Result := TChildForm(Application.MainForm).FNextID;
  Inc(TChildForm(Application.MainForm).FNextID);
end;

procedure TChildForm.BuildTree;
var
  Root1, Child1_1, Child1_2: PVirtualNode;
  Root2, Child2_1, Child2_2: PVirtualNode;
  Root3: PVirtualNode;
  Data: PMyRecord;
  function AddNode(Parent: PVirtualNode; const AActionName, ACaption: String; AParentID: Integer): PVirtualNode;
  begin
    Result := FChildVST.AddChild(Parent);
    Data := FChildVST.GetNodeData(Result);
    Data^.ID := FNextID;
    Inc(FNextID);
    Data^.ParentID := AParentID;
    Data^.ActionName := AActionName;
    Data^.Caption := ACaption;
  end;
begin
  FNextID := 0;

  Root1 := AddNode(nil, 'ActOneRoot', 'Root One', -1);
  Child1_1 := AddNode(Root1, 'ActOneChild_1', 'Child 1.1', Root1.Data.ID);
  Child1_2 := AddNode(Root1, 'ActOneChild_2', 'Child 1.2', Root1.Data.ID);

  Root2 := AddNode(nil, 'ActTwoRoot', 'Root Two', -1);
  Child2_1 := AddNode(Root2, 'ActTwoChild_1', 'Child 2.1', Root2.Data.ID);
  Child2_2 := AddNode(Root2, 'ActTwoChild_2', 'Child 2.2', Root2.Data.ID);

  Root3 := AddNode(nil, 'ActThreeRoot', 'Root Three', -1);

  FChildVST.Expanded[Root1] := True;
  FChildVST.Expanded[Root2] := True;
end;

procedure TChildForm.SaveTreeToStream;
begin
  FtmpMS.Clear;
  FChildVST.SaveToStream(FtmpMS); // вызовет DoSaveNode для каждого узла
  FtmpMS.Position := 0;
end;

end.
```

> 💡 **Примечание**: В реальном приложении `FNextID` логичнее хранить внутри `TChildForm`, а не через `Application.MainForm`. Я поправил это — см. ниже исправленный `BuildTree`.

**Исправленный `BuildTree`**:

```pascal
procedure TChildForm.BuildTree;
var
  Root1, Child1_1, Child1_2: PVirtualNode;
  Root2, Child2_1, Child2_2: PVirtualNode;
  Root3: PVirtualNode;
  RootID: Integer;
begin
  FNextID := 0;

  Root1 := FChildVST.AddChild(nil);
  with PMyRecord(FChildVST.GetNodeData(Root1))^ do
  begin
    ID := FNextID; Inc(FNextID);
    ParentID := -1;
    ActionName := 'ActOneRoot';
    Caption := 'Root One';
  end;
  RootID := Root1.Data.ID;

  Child1_1 := FChildVST.AddChild(Root1);
  with PMyRecord(FChildVST.GetNodeData(Child1_1))^ do
  begin
    ID := FNextID; Inc(FNextID);
    ParentID := RootID;
    ActionName := 'ActOneChild_1';
    Caption := 'Child 1.1';
  end;

  Child1_2 := FChildVST.AddChild(Root1);
  with PMyRecord(FChildVST.GetNodeData(Child1_2))^ do
  begin
    ID := FNextID; Inc(FNextID);
    ParentID := RootID;
    ActionName := 'ActOneChild_2';
    Caption := 'Child 1.2';
  end;

  // ... аналогично для Root2 и Root3
end;
```

---

## 🔹 Модуль 3: `unit_main.pas`

```pascal
// unit_main.pas
unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, VirtualTrees, ActnList, unit_virtstringtree, unit_child;

type
  TMainForm = class(TForm)
    mainVST: TVirtStringTree;
    MainMenuActList: TActionList;
    procedure FormCreate(Sender: TObject);
    procedure mainVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure mainVSTClick(Sender: TObject);
  private
    FChildForm: TChildForm;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Создаём дочернюю форму (или логику)
  FChildForm := TChildForm.Create(Self);
  FChildForm.SaveTreeToStream; // сохраняем в tmpMS

  // Настраиваем главное дерево
  mainVST.NodeDataSize := SizeOf(TMyRecord);
  mainVST.LoadFromStream(FChildForm.tmpMS); // вызовет DoLoadNode

  // Подключаем события
  mainVST.OnGetText := @mainVSTGetText;
  mainVST.OnClick := @mainVSTClick;

  // Копируем действия из дочернего списка
  MainMenuActList.Actions.Assign(FChildForm.FActList.Actions);
end;

procedure TMainForm.mainVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  Data: PMyRecord;
begin
  if (TextType = ttNormal) and Assigned(Node) then
  begin
    Data := Sender.GetNodeData(Node);
    if Assigned(Data) then
      CellText := Data^.Caption;
  end;
end;

procedure TMainForm.mainVSTClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PMyRecord;
  Act: TContainedAction;
begin
  Node := mainVST.GetFirstSelected;
  if not Assigned(Node) then Exit;

  Data := mainVST.GetNodeData(Node);
  if not Assigned(Data) or (Data^.ActionName = '') then Exit;

  // Ищем действие по имени
  Act := MainMenuActList.ActionByName(Data^.ActionName);
  if Assigned(Act) then
    Act.Execute;
end;

end.
```

---

## 🔧 Дополнительно: Подключение модулей

В `.lpi` или через **Project > Project Inspector** убедитесь, что подключены:
- `VirtualTrees` (пакет **VirtualTreeView**)
- `unit_virtstringtree`
- `unit_child`

---

## ✅ Ключевые моменты:

1. **Сериализация строк**: В Lazarus строки по умолчанию `UnicodeString` (`{$H+}`). Для кроссплатформенной совместимости строки сохраняются в UTF-8 как `RawByteString`.
2. **Переопределение `DoSaveNode/DoLoadNode`**: Это правильный способ кастомной сериализации в `TBaseVirtualTree`.
3. **Отдельный класс дерева**: `TVirtStringTree` инкапсулирует работу с `TMyRecord`.
4. **Безопасность**: Проверка `Assigned(Data)` и корректная работа с памятью.

---

Если нужно — могу добавить LFM-файл формы или пример использования в проекте.