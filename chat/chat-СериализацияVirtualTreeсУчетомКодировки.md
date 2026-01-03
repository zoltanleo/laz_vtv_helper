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

Напиши подробный код для Лазарус, где код VTV отличается от реализации Дельфи (в частности процедура сохранения выглядит как TBaseVirtualTree.SaveToStream(Stream: TStream; Node: PVirtualNode = nil)). Методы сериализации/десериализации должны учитывать кодировку строки при сохранении/загрузке

### ASSISTANT
Для корректной сериализации/десериализации дерева `TBaseVirtualTree` в Lazarus с учетом кодировки строк и совместимости с `TActionList`, требуется написать **кастомные методы** сохранения и загрузки, поскольку встроенный `SaveToStream`/`LoadFromStream` в Lazarus **не сохраняет пользовательские данные**, такие как `TMyRecord`.

Таким образом, реализуем ручную сериализацию/десериализацию структуры дерева, включая поля записи `TMyRecord`.

---

## ⚠️ Важно:
- В Lazarus `TBaseVirtualTree` **не поддерживает встроенную сериализацию пользовательских данных**, в отличие от `TVirtualStringTree` в Delphi с аддонами.
- Поэтому необходимо вручную обходить дерево и сохранять/загружать `TMyRecord`.

---

## ✅ Общие соглашения

- Кодировка строк: **UTF-8** (рекомендуется для кроссплатформенности).
- Используем `TStream` + `TWriter`/`TReader` с `WriteStringUTF8` / `ReadStringUTF8`.
- Сохраняем дерево рекурсивно, начиная с корневых узлов.
- Идентификаторы узлов (`ID`, `ParentID`) не обязательны для сохранения, так как структура дерева восстанавливается по иерархии. Но сохраним их для целостности.

---

## 1. Объявление в `unit_child`

```pascal
// unit_child.pas
unit unit_child;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, ActnList, LCLType;

type
  TMyRecord = record
    ID: Integer;
    ParentID: Integer;
    ActionName: UTF8String; // Сохраняем как UTF8
    Caption: UTF8String;
  end;

  TChildForm = class(TForm)
    ActList: TActionList;
    // ... другие компоненты
  private
    FchildVST: TBaseVirtualTree;
    FtmpMS: TMemoryStream;
    procedure SaveTreeToStream(Stream: TStream; Node: PVirtualNode = nil);
    procedure SaveNodeToStream(Stream: TStream; Node: PVirtualNode);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildTree; // метод, который строит дерево (пример ниже)
    procedure SaveTreeToMemory;
  end;

implementation

{ TChildForm }

constructor TChildForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FchildVST := TBaseVirtualTree.Create(Self);
  FchildVST.Parent := Self;
  FchildVST.NodeDataSize := SizeOf(TMyRecord);
  FtmpMS := TMemoryStream.Create;
  BuildTree;
end;

destructor TChildForm.Destroy;
begin
  FtmpMS.Free;
  FchildVST.Free;
  inherited Destroy;
end;

procedure TChildForm.BuildTree;
var
  Root1, Root2, Root3: PVirtualNode;
  Child: PVirtualNode;
  Data: ^TMyRecord;
begin
  FchildVST.Clear;

  // Root 1
  Root1 := FchildVST.AddChild(nil);
  Data := FchildVST.GetNodeData(Root1);
  Data^.ID := 1;
  Data^.ParentID := -1;
  Data^.ActionName := 'ActOneRoot';
  Data^.Caption := 'NodeOne';

  Child := FchildVST.AddChild(Root1);
  Data := FchildVST.GetNodeData(Child);
  Data^.ID := 11;
  Data^.ParentID := 1;
  Data^.ActionName := 'ActOneChild_1';
  Data^.Caption := 'NodeOneChild_1';

  Child := FchildVST.AddChild(Root1);
  Data := FchildVST.GetNodeData(Child);
  Data^.ID := 12;
  Data^.ParentID := 1;
  Data^.ActionName := 'ActOneChild_2';
  Data^.Caption := 'NodeOneChild_2';

  // Root 2
  Root2 := FchildVST.AddChild(nil);
  Data := FchildVST.GetNodeData(Root2);
  Data^.ID := 2;
  Data^.ParentID := -1;
  Data^.ActionName := 'ActTwoRoot';
  Data^.Caption := 'NodeTwo';

  Child := FchildVST.AddChild(Root2);
  Data := FchildVST.GetNodeData(Child);
  Data^.ID := 21;
  Data^.ParentID := 2;
  Data^.ActionName := 'ActTwoChild_1';
  Data^.Caption := 'NodeTwoChild_1';

  Child := FchildVST.AddChild(Root2);
  Data := FchildVST.GetNodeData(Child);
  Data^.ID := 22;
  Data^.ParentID := 2;
  Data^.ActionName := 'ActTwoChild_2';
  Data^.Caption := 'NodeTwoChild_2';

  // Root 3
  Root3 := FchildVST.AddChild(nil);
  Data := FchildVST.GetNodeData(Root3);
  Data^.ID := 3;
  Data^.ParentID := -1;
  Data^.ActionName := 'ActThreeRoot';
  Data^.Caption := 'NodeThree';

  FchildVST.Expanded[Root1] := True;
  FchildVST.Expanded[Root2] := True;
end;

procedure TChildForm.SaveNodeToStream(Stream: TStream; Node: PVirtualNode);
var
  Writer: TWriter;
  Data: ^TMyRecord;
  Child: PVirtualNode;
  ChildCount: Integer;
begin
  Writer := TWriter.Create(Stream, 1024);
  try
    Data := FchildVST.GetNodeData(Node);
    Writer.WriteInteger(Data^.ID);
    Writer.WriteInteger(Data^.ParentID);
    Writer.WriteStringUTF8(UTF8String(Data^.ActionName));
    Writer.WriteStringUTF8(UTF8String(Data^.Caption));

    // Сохраняем количество дочерних узлов
    ChildCount := 0;
    Child := FchildVST.GetFirst(Node);
    while Assigned(Child) do
    begin
      Inc(ChildCount);
      Child := FchildVST.GetNext(Child);
    end;
    Writer.WriteInteger(ChildCount);

    // Рекурсивно сохраняем детей
    Child := FchildVST.GetFirst(Node);
    while Assigned(Child) do
    begin
      SaveNodeToStream(Stream, Child);
      Child := FchildVST.GetNext(Child);
    end;
  finally
    Writer.Free;
  end;
end;

procedure TChildForm.SaveTreeToStream(Stream: TStream; Node: PVirtualNode = nil);
var
  Root: PVirtualNode;
begin
  if Assigned(Node) then
  begin
    SaveNodeToStream(Stream, Node);
  end
  else
  begin
    // Сохраняем все корневые узлы
    Root := FchildVST.GetFirst(nil);
    while Assigned(Root) do
    begin
      SaveNodeToStream(Stream, Root);
      Root := FchildVST.GetNext(Root);
    end;
  end;
end;

procedure TChildForm.SaveTreeToMemory;
begin
  FtmpMS.Clear;
  SaveTreeToStream(FtmpMS);
  FtmpMS.Position := 0;
end;

end.
```

---

## 2. Объявление в `unit_main`

```pascal
// unit_main.pas
unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, ActnList, LCLType, unit_child;

type
  TMainForm = class(TForm)
    mainVST: TBaseVirtualTree;
    ActList: TActionList; // Должен быть тот же, что и в child-модуле (или клон)
  private
    procedure LoadTreeFromStream(Stream: TStream);
    procedure LoadNodeFromStream(Stream: TStream; Parent: PVirtualNode);
    procedure OnMainVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: UTF8String);
    procedure OnMainVSTDblClick(Sender: TObject);
  public
    procedure LoadFromChildMemory;
  end;

implementation

{ TMainForm }

procedure TMainForm.LoadNodeFromStream(Stream: TStream; Parent: PVirtualNode);
var
  Reader: TReader;
  ID, ParentID, ChildCount, i: Integer;
  ActionName, Caption: UTF8String;
  NewNode: PVirtualNode;
  Data: ^TMyRecord;
begin
  Reader := TReader.Create(Stream, 1024);
  try
    ID := Reader.ReadInteger;
    ParentID := Reader.ReadInteger;
    ActionName := Reader.ReadStringUTF8;
    Caption := Reader.ReadStringUTF8;
    ChildCount := Reader.ReadInteger;

    // Создаём узел
    NewNode := mainVST.AddChild(Parent);
    Data := mainVST.GetNodeData(NewNode);
    Data^.ID := ID;
    Data^.ParentID := ParentID;
    Data^.ActionName := ActionName;
    Data^.Caption := Caption;

    // Рекурсивно загружаем детей
    for i := 1 to ChildCount do
      LoadNodeFromStream(Stream, NewNode);
  finally
    Reader.Free;
  end;
end;

procedure TMainForm.LoadTreeFromStream(Stream: TStream);
begin
  mainVST.Clear;
  while Stream.Position < Stream.Size do
    LoadNodeFromStream(Stream, nil);
end;

procedure TMainForm.LoadFromChildMemory;
begin
  // Предполагается, что unit_child.ChildForm.FtmpMS доступен
  // Например, через глобальную переменную или передачу ссылки
  LoadTreeFromStream(unit_child.ChildForm.FtmpMS);
end;

procedure TMainForm.OnMainVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: UTF8String);
var
  Data: ^TMyRecord;
begin
  if (TextType = ttNormal) and (Column = 0) then
  begin
    Data := Sender.GetNodeData(Node);
    if Assigned(Data) then
      CellText := Data^.Caption;
  end;
end;

procedure TMainForm.OnMainVSTDblClick(Sender: TObject);
var
  Node: PVirtualNode;
  Data: ^TMyRecord;
  Act: TContainedAction;
begin
  Node := mainVST.GetFirstSelected;
  if Assigned(Node) then
  begin
    Data := mainVST.GetNodeData(Node);
    if Assigned(Data) and (Data^.ActionName <> '') then
    begin
      Act := ActList.Actions.FindAction(UTF8ToString(Data^.ActionName));
      if Assigned(Act) and Act.Enabled then
        Act.Execute;
    end;
  end;
end;

end.
```

---

## 🔧 Дополнительные замечания

1. **Кодировка**: `UTF8String` гарантирует кроссплатформенность и корректную передачу Unicode-символов.
2. **ActionList**: Убедитесь, что `ActList` в `unit_main` содержит **те же действия**, что и в `unit_child`, или передавайте `ActList` по ссылке.
3. **Глобальный доступ**: Чтобы `unit_main` читал `FtmpMS` из `unit_child`, либо сделайте `ChildForm` глобальной переменной, либо передавайте `TMemoryStream` явно.
4. **Обработка ошибок**: В продакшене добавьте `try..except` при чтении/записи.
5. **NodeDataSize**: Убедитесь, что `mainVST.NodeDataSize := SizeOf(TMyRecord);` установлен до загрузки.

---

## ✅ Пример использования

```pascal
// Где-то в main-форме
ChildForm := TChildForm.Create(nil);
ChildForm.SaveTreeToMemory;
MainForm.LoadFromChildMemory;
```

---

Если нужны дополнительные фичи (иконки, чекбоксы, пользовательские колонки) — расширьте сериализацию соответствующим образом.

Готов помочь с интеграцией!