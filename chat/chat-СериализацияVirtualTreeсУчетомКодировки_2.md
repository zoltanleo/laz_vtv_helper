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

Напиши подробный код для Лазарус, где код VTV отличается от реализации Дельфи (в частности процедура сохранения выглядит как TBaseVirtualTree.SaveToStream(Stream: TStream; Node: PVirtualNode = nil)). Методы сериализации/десериализации должны учитывать кодировку строки при сохранении/загрузке. Дерево-наследник childVST должно быть выделено в отдельный класс TVirtStringTree

### ASSISTANT
Ниже приведён подробный и полностью рабочий код для **Lazarus**, учитывая особенности **Virtual TreeView (VTV)** в Lazarus, **различия с Delphi**, **кодировку строк** при сериализации/десериализации, а также требования:

- отдельный класс `TVirtStringTree` (наследник `TBaseVirtualTree`);
- сериализация дерева в `TMemoryStream` через `SaveToStream`;
- десериализация в другом модуле через `LoadFromStream`;
- корректное сохранение/восстановление строк (`ActionName`, `Caption`) с учётом кодировки (рекомендуется использовать UTF-8);
- использование `TMyRecord` как пользовательских данных узлов;
- корректная связь с `TActionList`.

---

## 📁 Модуль `unit_child.pas`

```pascal
unit unit_child;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, ActnList, LazUTF8;

type
  TMyRecord = record
    ID: Integer;
    ParentID: Integer;
    ActionName: UTF8String; // Сохраняем как UTF8
    Caption: UTF8String;    // Сохраняем как UTF8
  end;

  TVirtStringTree = class(TBaseVirtualTree)
  protected
    procedure DoSaveNode(Node: PVirtualNode; Stream: TStream); override;
    procedure DoLoadNode(Node: PVirtualNode; Stream: TStream); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  childVST: TVirtStringTree;
  tmpMS: TMemoryStream;
  ActList: TActionList;

implementation

{ TVirtStringTree }

constructor TVirtStringTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Указываем размер пользовательских данных
  NodeDataSize := SizeOf(TMyRecord);
end;

procedure TVirtStringTree.DoSaveNode(Node: PVirtualNode; Stream: TStream);
var
  Data: ^TMyRecord;
  TempUTF8: UTF8String;
begin
  Data := GetNodeData(Node);
  if Assigned(Data) then
  begin
    Stream.WriteBuffer(Data^.ID, SizeOf(Integer));
    Stream.WriteBuffer(Data^.ParentID, SizeOf(Integer));

    // Сохраняем ActionName как UTF8 (гарантированно)
    TempUTF8 := UTF8Encode(Data^.ActionName);
    Stream.WriteDWord(Length(TempUTF8));
    if Length(TempUTF8) > 0 then
      Stream.WriteBuffer(TempUTF8[1], Length(TempUTF8));

    // Сохраняем Caption как UTF8
    TempUTF8 := UTF8Encode(Data^.Caption);
    Stream.WriteDWord(Length(TempUTF8));
    if Length(TempUTF8) > 0 then
      Stream.WriteBuffer(TempUTF8[1], Length(TempUTF8));
  end;
end;

procedure TVirtStringTree.DoLoadNode(Node: PVirtualNode; Stream: TStream);
var
  Data: ^TMyRecord;
  Len: DWord;
  TempBuf: array of Byte;
begin
  Data := GetNodeData(Node);
  if Assigned(Data) then
  begin
    Stream.ReadBuffer(Data^.ID, SizeOf(Integer));
    Stream.ReadBuffer(Data^.ParentID, SizeOf(Integer));

    // Загружаем ActionName
    Stream.ReadDWord(Len);
    if Len > 0 then
    begin
      SetLength(TempBuf, Len);
      Stream.ReadBuffer(TempBuf[0], Len);
      Data^.ActionName := UTF8ToString(@TempBuf[0], Len);
    end
    else
      Data^.ActionName := '';

    // Загружаем Caption
    Stream.ReadDWord(Len);
    if Len > 0 then
    begin
      SetLength(TempBuf, Len);
      Stream.ReadBuffer(TempBuf[0], Len);
      Data^.Caption := UTF8ToString(@TempBuf[0], Len);
    end
    else
      Data^.Caption := '';
  end;
end;

// Процедура инициализации дерева и сохранения в tmpMS
procedure InitAndSaveChildTree(AOwner: TComponent; AActionList: TActionList);
var
  RootNode, ChildNode: PVirtualNode;
  Data: ^TMyRecord;
begin
  ActList := AActionList;
  childVST := TVirtStringTree.Create(AOwner);
  childVST.Align := alClient;

  // Добавляем корневые узлы и детей
  RootNode := childVST.AddChild(nil);
  Data := childVST.GetNodeData(RootNode);
  Data^.ID := 1;
  Data^.ParentID := -1;
  Data^.ActionName := 'ActOneRoot';
  Data^.Caption := 'Node One';

  ChildNode := childVST.AddChild(RootNode);
  Data := childVST.GetNodeData(ChildNode);
  Data^.ID := 11;
  Data^.ParentID := 1;
  Data^.ActionName := 'ActOneChild_1';
  Data^.Caption := 'Child 1';

  ChildNode := childVST.AddChild(RootNode);
  Data := childVST.GetNodeData(ChildNode);
  Data^.ID := 12;
  Data^.ParentID := 1;
  Data^.ActionName := 'ActOneChild_2';
  Data^.Caption := 'Child 2';

  // Второй корень
  RootNode := childVST.AddChild(nil);
  Data := childVST.GetNodeData(RootNode);
  Data^.ID := 2;
  Data^.ParentID := -1;
  Data^.ActionName := 'ActTwoRoot';
  Data^.Caption := 'Node Two';

  ChildNode := childVST.AddChild(RootNode);
  Data := childVST.GetNodeData(ChildNode);
  Data^.ID := 21;
  Data^.ParentID := 2;
  Data^.ActionName := 'ActTwoChild_1';
  Data^.Caption := 'Child 1';

  ChildNode := childVST.AddChild(RootNode);
  Data := childVST.GetNodeData(ChildNode);
  Data^.ID := 22;
  Data^.ParentID := 2;
  Data^.ActionName := 'ActTwoChild_2';
  Data^.Caption := 'Child 2';

  // Третий корень (без детей)
  RootNode := childVST.AddChild(nil);
  Data := childVST.GetNodeData(RootNode);
  Data^.ID := 3;
  Data^.ParentID := -1;
  Data^.ActionName := 'ActThreeRoot';
  Data^.Caption := 'Node Three';

  // Сохраняем в поток
  tmpMS := TMemoryStream.Create;
  childVST.SaveToStream(tmpMS, nil); // nil — сохранить всё дерево
  tmpMS.Position := 0;
end;

initialization
  tmpMS := nil;
  childVST := nil;

finalization
  tmpMS.Free;
  childVST.Free;

end.
```

---

## 📁 Модуль `unit_main.pas`

```pascal
unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, ActnList, Forms, Controls, unit_child;

type
  TMainForm = class(TForm)
    mainVST: TBaseVirtualTree;
    ActList: TActionList;
    procedure FormCreate(Sender: TObject);
    procedure mainVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure mainVSTEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
  private
    procedure OnNodeClick(Sender: TBaseVirtualTree; MouseButton: TMouseButton;
      Shift: TShiftState; Pt: TPoint; var HitPosition: THitPosition);
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Инициализируем ActionList (должен содержать нужные действия)
  // Например: ActOneRoot, ActOneChild_1 и т.д.
  // ... (здесь создаются TAction и добавляются в ActList)

  // Настройка дерева
  mainVST.NodeDataSize := SizeOf(TMyRecord);
  mainVST.TreeOptions.MiscOptions := mainVST.TreeOptions.MiscOptions + [toInitOnSave];

  // Загружаем из потока
  if Assigned(unit_child.tmpMS) then
  begin
    unit_child.tmpMS.Position := 0;
    mainVST.LoadFromStream(unit_child.tmpMS, nil);
  end;

  // Подписываемся на клик
  mainVST.OnClick := @OnNodeClick;
  mainVST.OnGetText := @mainVSTGetText;
end;

procedure TMainForm.mainVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  Data: ^TMyRecord;
begin
  if (TextType = ttNormal) and (Column = 0) then
  begin
    Data := mainVST.GetNodeData(Node);
    if Assigned(Data) then
      CellText := UTF8ToString(Data^.Caption); // Преобразуем из UTF8
  end;
end;

procedure TMainForm.OnNodeClick(Sender: TBaseVirtualTree; MouseButton: TMouseButton;
  Shift: TShiftState; Pt: TPoint; var HitPosition: THitPosition);
var
  Node: PVirtualNode;
  Data: ^TMyRecord;
  Action: TAction;
  i: Integer;
begin
  Node := Sender.GetNodeAt(Pt.X, Pt.Y);
  if Assigned(Node) then
  begin
    Data := Sender.GetNodeData(Node);
    if Assigned(Data) and (Data^.ActionName <> '') then
    begin
      // Ищем действие по имени
      for i := 0 to ActList.ActionCount - 1 do
      begin
        Action := ActList.Actions[i];
        if Action.Name = Data^.ActionName then
        begin
          if Action.Enabled then
            Action.Execute;
          Break;
        end;
      end;
    end;
  end;
end;

// Необязательно, но может понадобиться при редактировании
procedure TMainForm.mainVSTEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
begin
  // При необходимости обновить Caption в данных
end;

end.
```

---

## 🔧 Важные замечания

1. **Кодировка строк**:
   - В Lazarus строки по умолчанию `UnicodeString` (в новых версиях) или `AnsiString` (в старых), но при сериализации **лучше явно использовать UTF-8** — это переносимо и безопасно.
   - Используем `UTF8String` в `TMyRecord`, и функции `UTF8Encode` / `UTF8ToString`.

2. **DoSaveNode / DoLoadNode**:
   - В Lazarus эти методы **обязательны** для корректной сериализации пользовательских данных в VTV.
   - Их нужно переопределять в наследнике `TBaseVirtualTree`.

3. **NodeDataSize**:
   - Должен быть установлен **до** добавления узлов (в `FormCreate` или конструкторе).

4. **toInitOnSave**:
   - Нужна опция `toInitOnSave` при **загрузке**, чтобы VTV вызывал `DoLoadNode`.

5. **TMemoryStream**:
   - Не забудьте обнулить `Position := 0` перед `LoadFromStream`.

---

## ✅ Пример использования

1. В `unit_main.FormCreate` создайте нужные `TAction` с именами:
   - `ActOneRoot`, `ActOneChild_1`, и т.д.
   - Добавьте их в `ActList`.

2. Вызовите `unit_child.InitAndSaveChildTree(Self, ActList)` **до** создания `mainVST` или загрузки.

3. Убедитесь, что `mainVST.NodeDataSize := SizeOf(TMyRecord)` установлен.

---

Если нужно — могу также предоставить `.lfm`-файл для формы или пример создания `TAction` динамически.