unit unit_virtstringtree;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, laz.VirtualTrees, LCLIntf, LCLType;

type
  PMyRecord = ^TMyRecord;
  TMyRecord = record
    ID: Integer; // ID узла дерева
    ParentID: Integer; // содержит для child-узла ID root-узла (для root-узла равен -1)
    ActionName: String; // ссылка-имя на Action в произвольном ActList
    Caption: String; // заголовок узла
    tsName: String; // имя вкладки PageControl
  end;

  TNodeRec = record
    Node: PVirtualNode;
    Data: TMyRecord;
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
    class procedure InitializeTree(VTree: TBaseVirtualTree); // устанавливает NodeDataSize
    class function AddNode(VTree: TBaseVirtualTree; ParentNode: PVirtualNode;
      const AActionName, ACaption, AtsName: String): PVirtualNode;
    class procedure PrepareForSave(VTree: TBaseVirtualTree); // вызвать перед SaveToStream
    // Модифицированная сигнатура для SaveTreeToStream: добавляем параметр RootNode
    class procedure SaveTreeToStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream; RootNode: PVirtualNode = nil);
    // Модифицированная сигнатура для LoadTreeFromStream: добавляем параметр ParentNode
    class procedure LoadTreeFromStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream; ParentNode: PVirtualNode = nil);
  end;

implementation

{ TVirtStringTreeHelper }

class procedure TVirtStringTreeHelper.WriteNodeData(VTree: TBaseVirtualTree; Node: PVirtualNode; Stream: TStream);
var
  Data: PMyRecord = nil;
  Len: SizeInt = 0;
  EncodedActionName: UTF8String = '';
  EncodedCaption: UTF8String = '';
  EncodedtsName: UTF8String = '';
begin
  Data := VTree.GetNodeData(Node);

  // Записываем данные в UTF-8 для обеспечения совместимости
  EncodedActionName := UTF8String(Data^.ActionName);
  EncodedCaption := UTF8String(Data^.Caption);
  EncodedtsName := UTF8String(Data^.tsName);

  Stream.Write(Data^.ID, SizeOf(Data^.ID));
  Stream.Write(Data^.ParentID, SizeOf(Data^.ParentID));

  // Записываем строки как UTF-8
  Len := Length(EncodedActionName);
  Stream.Write(Len, SizeOf(Len));
  if (Len > 0) then Stream.Write(EncodedActionName[1], Len);

  Len := Length(EncodedCaption);
  Stream.Write(Len, SizeOf(Len));
  if (Len > 0) then Stream.Write(EncodedCaption[1], Len);

  Len := Length(EncodedtsName);
  Stream.Write(Len, SizeOf(Len));
  if (Len > 0) then Stream.Write(EncodedtsName[1], Len);
end;

class procedure TVirtStringTreeHelper.ReadNodeData(VTree: TBaseVirtualTree; Node: PVirtualNode; Stream: TStream);
var
  Data: PMyRecord = nil;
  Len: SizeInt = 0;
  TempUTF8: UTF8String = '';
  NextID: SizeInt = 0;
begin
  Data := VTree.GetNodeData(Node);

  Stream.Read(Data^.ID, SizeOf(Data^.ID));
  Stream.Read(Data^.ParentID, SizeOf(Data^.ParentID));

  // Читаем ActionName
  Stream.Read(Len, SizeOf(Len));
  if (Len > 0) then
  begin
    SetLength(TempUTF8, Len);
    Stream.Read(TempUTF8[1], Len);
    Data^.ActionName := String(TempUTF8);
  end
  else
    Data^.ActionName := '';

  // Читаем Caption
  Stream.Read(Len, SizeOf(Len));
  if (Len > 0) then
  begin
    SetLength(TempUTF8, Len);
    Stream.Read(TempUTF8[1], Len);
    Data^.Caption := String(TempUTF8);
  end
  else
    Data^.Caption := '';

  // Читаем tsName
  Stream.Read(Len, SizeOf(Len));
  if (Len > 0) then
  begin
    SetLength(TempUTF8, Len);
    Stream.Read(TempUTF8[1], Len);
    Data^.tsName := String(TempUTF8);
  end
  else
    Data^.tsName := '';

  // Обновляем Tag для следующего ID
  NextID := VTree.Tag;
  if (Data^.ID >= NextID) then NextID := Data^.ID + 1;
  VTree.Tag := NextID;
end;

class function TVirtStringTreeHelper.GetNodeCount(VTree: TBaseVirtualTree): SizeInt;
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

// --- МОДИФИЦИРОВАННАЯ ПРОЦЕДУРА SaveTreeToStream ---
// Заменяем вызов AStream.WriteInteger(ChildCount) на AStream.Write(ChildCount, SizeOf(ChildCount))
class procedure TVirtStringTreeHelper.SaveTreeToStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream; RootNode: PVirtualNode);
var
  Node: PVirtualNode;
  ChildNode: PVirtualNode;
  NodeData: PMyRecord;
  // Добавляем переменные для подсчета количества детей
  ChildCount: Integer;
begin
  // Если RootNode не задан, сохраняем всё дерево (все корневые узлы)
  if not Assigned(RootNode) then
  begin
    Node := ATree.GetFirst();
    while Assigned(Node) do
    begin
      // Рекурсивно сохраняем этот корневой узел и его поддерево
      SaveTreeToStream(ATree, AStream, Node);
      Node := ATree.GetNextSibling(Node);
    end;
  end
  else // Если RootNode задан, сохраняем его и его поддерево
  begin
    Node := RootNode;
    WriteNodeData(ATree, Node, AStream); // Используем вспомогательную процедуру

    // Подсчитываем количество детей
    ChildCount := 0;
    ChildNode := Node.FirstChild;
    while Assigned(ChildNode) do
    begin
      Inc(ChildCount);
      ChildNode := ChildNode.NextSibling;
    end;

    // --- ИСПРАВЛЕНИЕ: Заменяем AStream.WriteInteger(ChildCount) ---
    AStream.Write(ChildCount, SizeOf(ChildCount));

    // Рекурсивно сохраняем детей
    ChildNode := Node.FirstChild;
    while Assigned(ChildNode) do
    begin
      // Рекурсивный вызов для сохранения поддерева ребенка
      SaveTreeToStream(ATree, AStream, ChildNode);
      ChildNode := ChildNode.NextSibling;
    end;
  end;
end;

// --- МОДИФИЦИРОВАННАЯ ПРОЦЕДУРА LoadTreeFromStream ---
// Заменяем вызов AStream.ReadInteger(ChildCount) на AStream.Read(ChildCount, SizeOf(ChildCount))
class procedure TVirtStringTreeHelper.LoadTreeFromStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream; ParentNode: PVirtualNode);
var
  NewNode: PVirtualNode;
  NodeData: PMyRecord;
  NodeUserData, NodeCaption, NodeActionName: String;
  Len: Integer;
  ChildCount: Integer;
  I: Integer;
begin
  // Проверяем, есть ли еще данные в потоке
  if AStream.Position >= AStream.Size then
    Exit;

  // --- ИСПРАВЛЕНИЕ: Читаем ID ---
  AStream.Read(Len, SizeOf(Len)); // Это будет ID
  // Пропускаем чтение строк, так как теперь используем ReadNodeData
  // ... (старый код чтения строк удаляется)

  // Возвращаем указатель на начало данных узла (ID)
  AStream.Seek(-SizeOf(Len), soFromCurrent);

  // Создаем новый узел
  NewNode := ATree.AddChild(ParentNode); // Используем ParentNode, переданный в параметре
  // Читаем и устанавливаем данные узла
  ReadNodeData(ATree, NewNode, AStream); // Используем вспомогательную процедуру

  // --- ИСПРАВЛЕНИЕ: Заменяем AStream.ReadInteger(ChildCount) ---
  AStream.Read(ChildCount, SizeOf(ChildCount));

  // Рекурсивно загружаем детей
  for I := 0 to ChildCount - 1 do
  begin
    // Рекурсивный вызов для загрузки поддерева, передаем только что созданный NewNode как Parent
    LoadTreeFromStream(ATree, AStream, NewNode);
  end;
end;

class function TVirtStringTreeHelper.AddNode(VTree: TBaseVirtualTree;
  ParentNode: PVirtualNode; const AActionName, ACaption, AtsName: String): PVirtualNode;
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