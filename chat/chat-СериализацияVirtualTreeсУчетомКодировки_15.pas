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

  // Вспомогательный класс для доступа к защищенным полям
  TBaseVirtualTreeAccess = class(TBaseVirtualTree)
  end;

  { TVirtStringTreeHelper }

  TVirtStringTreeHelper = class
  private
    // --- НОВАЯ ВСПОМОГАТЕЛЬНАЯ ПРОЦЕДУРА ---
    // Вспомогательная процедура для рекурсивного подсчета узлов и их детей
    class procedure CountNodesAndChildren(VTree: TBaseVirtualTree; Node: PVirtualNode; var Count: SizeInt);
    // --- КОНЕЦ НОВОЙ ВСПОМОГАТЕЛЬНОЙ ПРОЦЕДУРЫ ---
    class procedure WriteNodeData(VTree: TBaseVirtualTree; Node: PVirtualNode; Stream: TStream);
    class procedure ReadNodeData(VTree: TBaseVirtualTree; Node: PVirtualNode; Stream: TStream);
    class function GetNodeCount(VTree: TBaseVirtualTree): SizeInt;
  public
    // --- ИЗМЕНЕНАЯ ПРОЦЕДУРА ---
    class procedure SaveTreeToStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream; RootNode: PVirtualNode = nil);
    // --- КОНЕЦ ИЗМЕНЕНИЯ ---
    // --- ИЗМЕНЕНАЯ ПРОЦЕДУРА ---
    class procedure LoadTreeFromStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream; ParentNode: PVirtualNode = nil);
    // --- КОНЕЦ ИЗМЕНЕНИЯ ---
    class function AddNode(VTree: TBaseVirtualTree; ParentNode: PVirtualNode;
      const AActionName, ACaption, AtsName: String): PVirtualNode;
    class procedure PrepareForSave(VTree: TBaseVirtualTree); // вызвать перед SaveToStream
    class procedure InitializeTree(VTree: TBaseVirtualTree); // устанавливает NodeDataSize
    //class procedure LoadTreeFromStreamWithStructure(VTree: TBaseVirtualTree; Stream: TStream);
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

class procedure TVirtStringTreeHelper.ReadNodeData(VTree: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
var
  Data: PMyRecord = nil;
  Len: LongInt = 0;
  TempUTF8: UTF8String = '';
  NextID: SizeInt = 0;
begin
  Data := VTree.GetNodeData(Node);

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

  // Обновляем Tag для отслеживания максимального ID при загрузке
  NextID := VTree.Tag;
  if Data^.ID >= NextID then
    VTree.Tag := Data^.ID + 1; // Устанавливаем Tag на следующий возможный ID
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

// --- НОВАЯ ВСПОМОГАТЕЛЬНАЯ ПРОЦЕДУРА ---
class procedure TVirtStringTreeHelper.CountNodesAndChildren(VTree: TBaseVirtualTree; Node: PVirtualNode; var Count: SizeInt);
var
  ChildNode: PVirtualNode;
begin
  if not Assigned(Node) then Exit;
  Inc(Count); // Считаем текущий узел
  // Рекурсивно считаем детей
  ChildNode := Node^.FirstChild;
  while Assigned(ChildNode) do
  begin
    CountNodesAndChildren(VTree, ChildNode, Count);
    ChildNode := ChildNode^.NextSibling;
  end;
end;
// --- КОНЕЦ НОВОЙ ВСПОМОГАТЕЛЬНОЙ ПРОЦЕДУРЫ ---

// --- ИЗМЕНЕНАЯ ПРОЦЕДУРА SaveTreeToStream ---
// Сохраняет дерево, начиная с RootNode (или все корневые узлы, если RootNode = nil).
// Сохраняет структуру: узел -> ChildCount -> дети узла и т.д.
class procedure TVirtStringTreeHelper.SaveTreeToStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream; RootNode: PVirtualNode);
var
  Node: PVirtualNode;
  ChildNode: PVirtualNode;
  NodeData: PMyRecord;
  // --- ДОБАВЛЕНО: переменная для подсчета количества детей ---
  ChildCount: Integer;
  // --- КОНЕЦ ДОБАВЛЕНИЯ ---
  StartNode: PVirtualNode; // Для определения начального узла обхода
  // --- ДОБАВЛЕНО: переменная для подсчета общего количества узлов ---
  TotalNodeCount: SizeInt;
  // --- КОНЕЦ ДОБАВЛЕНИЯ ---
begin
  // Если RootNode не задан, сохраняем все корневые узлы
  if not Assigned(RootNode) then
    Node := ATree.GetFirstNodeOnLevel(0) // Получаем первый корневой узел
  else
    Node := RootNode; // Иначе начинаем с указанного узла

  StartNode := Node;

  // --- ДОБАВЛЕНО: запись общего количества узлов в начало потока ---
  TotalNodeCount := 0;
  if Assigned(RootNode) then
    CountNodesAndChildren(ATree, RootNode, TotalNodeCount) // Подсчет для поддерева
  else
  begin
    Node := ATree.GetFirstNodeOnLevel(0); // Сброс для подсчета всех корневых
    while Assigned(Node) do
    begin
      CountNodesAndChildren(ATree, Node, TotalNodeCount); // Подсчет для каждого корня
      Node := Node^.NextSibling; // Переход к следующему корню
    end;
  end;
  AStream.Write(TotalNodeCount, SizeOf(TotalNodeCount));
  // --- КОНЕЦ ДОБАВЛЕНИЯ ---

  Node := StartNode; // Восстанавливаем начальный узел для сохранения

  while Assigned(Node) do
  begin
    // --- ИЗМЕНЕНО: теперь сохраняем данные узла перед ChildCount ---
    WriteNodeData(ATree, Node, AStream);

    // Подсчитываем количество детей для текущего узла
    ChildCount := 0;
    ChildNode := Node^.FirstChild; // Начинаем с первого ребенка
    while Assigned(ChildNode) do
    begin
      Inc(ChildCount);
      ChildNode := ChildNode^.NextSibling; // Переходим к следующему ребенку
    end;

    // Записываем количество детей
    AStream.Write(ChildCount, SizeOf(ChildCount));

    // Рекурсивно сохраняем детей
    ChildNode := Node^.FirstChild; // Начинаем с первого ребенка
    while Assigned(ChildNode) do
    begin
      // Рекурсивный вызов для сохранения поддерева ребенка
      SaveTreeToStream(ATree, AStream, ChildNode); // Передаем ChildNode как RootNode для поддерева
      ChildNode := ChildNode^.NextSibling; // Переходим к следующему ребенку
    end;

    // --- ИЗМЕНЕНО: Переход к следующему узлу на том же уровне, если RootNode не задан ---
    if not Assigned(RootNode) then
      Node := Node^.NextSibling // Обходим корневые узлы, если RootNode = nil
    else
      Break; // Если RootNode задан, обрабатываем только его поддерево
  end;
end;
// --- КОНЕЦ ИЗМЕНЕНИЯ ---

// --- ИЗМЕНЕНАЯ ПРОЦЕДУРА LoadTreeFromStream ---
// Загружает дерево из потока в ATree. Если ParentNode = nil, очищает ATree и загружает все корневые узлы.
// Если ParentNode задан, загружает один узел и его детей как поддерево к ParentNode.
class procedure TVirtStringTreeHelper.LoadTreeFromStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream; ParentNode: PVirtualNode);
var
  NewNode: PVirtualNode;
  NodeData: PMyRecord;
  // --- ДОБАВЛЕНО: переменная для хранения количества детей ---
  ChildCount: Integer;
  // --- КОНЕЦ ДОБАВЛЕНИЯ ---
  i: Integer; // --- ДОБАВЛЕНО: Для цикла загрузки детей ---
  CurrentParent: PVirtualNode; // --- ДОБАВЛЕНО: Для ясности ---
  // --- ДОБАВЛЕНО: переменные для чтения общего количества узлов ---
  TotalNodeCount: SizeInt;
  LoadedNodeCount: SizeInt;
  // --- КОНЕЦ ДОБАВЛЕНИЯ ---
begin
  // --- ИЗМЕНЕНО: Проверка на пустой поток и чтение TotalNodeCount ---
  if AStream.Size = 0 then Exit; // Нечего загружать

  // Если ParentNode = nil, очищаем дерево и начинаем загрузку с корня
  if not Assigned(ParentNode) then
  begin
    ATree.Clear; // Очищаем дерево перед загрузкой
    ATree.Tag := 1; // Сбрасываем ID для новой загрузки, если используется такая логика в других частях
    // Читаем общее количество узлов
    if AStream.Position + SizeOf(TotalNodeCount) > AStream.Size then Exit;
    AStream.Read(TotalNodeCount, SizeOf(TotalNodeCount));
    LoadedNodeCount := 0;

    // Цикл для загрузки всех корневых узлов и их поддеревьев
    while (AStream.Position < AStream.Size) and (LoadedNodeCount < TotalNodeCount) do
    begin
        // Создаем корневой узел (Parent = nil)
        NewNode := ATree.AddChild(nil);
        ReadNodeData(ATree, NewNode, AStream);
        Inc(LoadedNodeCount);

        // Читаем количество детей для этого корневого узла
        if AStream.Position + SizeOf(ChildCount) > AStream.Size then Exit; // Недостаточно данных для ChildCount
        AStream.Read(ChildCount, SizeOf(ChildCount));

        // Цикл для загрузки ChildCount детей
        for i := 0 to pred(ChildCount) do
        begin
            // Рекурсивный вызов для загрузки поддерева, передаем только что созданный NewNode как Parent
            LoadTreeFromStream(ATree, AStream, NewNode); // Передаем NewNode как Parent для его детей
            Inc(LoadedNodeCount);
        end;
    end;
  end
  else // Если ParentNode задан, загружаем один узел как ребенка ParentNode
  begin
    // Создаем новый узел как дочерний для ParentNode
    NewNode := ATree.AddChild(ParentNode);
    // Читаем данные для этого узла
    ReadNodeData(ATree, NewNode, AStream);
    Inc(LoadedNodeCount);

    // Читаем количество детей для только что созданного узла
    if AStream.Position + SizeOf(ChildCount) > AStream.Size then Exit; // Недостаточно данных для ChildCount
    AStream.Read(ChildCount, SizeOf(ChildCount));

    // Цикл для загрузки ChildCount детей
    for i := 0 to pred(ChildCount) do
    begin
        // Рекурсивный вызов для загрузки поддерева, передаем NewNode как Parent
        LoadTreeFromStream(ATree, AStream, NewNode); // NewNode становится Parent для следующего уровня
        Inc(LoadedNodeCount);
    end;
  end;
end;
// --- КОНЕЦ ИЗМЕНЕНИЯ ---

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
