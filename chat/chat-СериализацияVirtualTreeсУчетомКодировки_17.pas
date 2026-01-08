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

// --- ИЗМЕНЕНАЯ ПРОЦЕДУРА SaveTreeToStream ---
// Сохраняет дерево, начиная с RootNode (или все корневые узлы, если RootNode = nil).
// Сохраняет структуру: узел -> ChildCount -> дети узла и т.д.
class procedure TVirtStringTreeHelper.SaveTreeToStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream; RootNode: PVirtualNode);
var
  Node: PVirtualNode;
  ChildNode: PVirtualNode;
  NodeData: PMyRecord;
  ChildCount: Cardinal; // --- ИЗМЕНЕНО: Теперь используем Cardinal как тип ChildCount ---
  StartNode: PVirtualNode; // Для определения начального узла обхода
  TotalNodeCount: SizeInt; // --- ВВЕДЕНО: Для подсчета общего количества узлов ---
  LoadedNodeCount: SizeInt; // --- ВВЕДЕНО: Для отслеживания загруженных узлов ---
  // --- ДОБАВЛЕНО: переменные для обхода поддерева ---
  CheckNode: PVirtualNode; // Вместо scoped-переменной
  IsInSubtree: Boolean; // Вместо scoped-переменной
  CheckNodeParent: PVirtualNode; // Вместо scoped-переменной
  // --- КОНЕЦ ДОБАВЛЕНИЯ ---
begin
  // --- ВВЕДЕНО: Подсчет общего количества узлов ---
  TotalNodeCount := 0;
  if Assigned(RootNode) then
  begin
    // Подсчет для поддерева начиная с RootNode
    Node := RootNode;
    while Assigned(Node) do
    begin
      Inc(TotalNodeCount);
      // Используем GetNext с ConsiderChildrenAbove = True для обхода поддерева
      Node := ATree.GetNext(Node, True);
      // Прерываем, если вышли за пределы поддерева RootNode
      if Assigned(Node) and (Node^.Parent <> RootNode) and (Node <> RootNode) then
      begin
        // Проверяем, не является ли текущий узел потомком другого узла вне поддерева RootNode
        // Более надежный способ - проверить путь от узла к корню
        CheckNode := Node^.Parent; // Используем объявленную переменную
        IsInSubtree := False; // Используем объявленную переменную
        while Assigned(CheckNode) and (CheckNode <> ATree.RootNode) do
        begin
          if CheckNode = RootNode then
          begin
            IsInSubtree := True;
            Break;
          end;
          CheckNode := CheckNode^.Parent;
        end;
        if not IsInSubtree then Break;
      end;
    end;
  end
  else
  begin
    // Подсчет для всего дерева
    Node := ATree.GetFirst();
    while Assigned(Node) do
    begin
      Inc(TotalNodeCount);
      Node := ATree.GetNext(Node);
    end;
  end;

  // Записываем общее количество узлов
  AStream.Write(TotalNodeCount, SizeOf(TotalNodeCount));
  LoadedNodeCount := 0; // Сбрасываем счетчик
  // --- КОНЕЦ ВВЕДЕНИЯ ---

  // Если RootNode не задан, сохраняем все корневые узлы
  if not Assigned(RootNode) then
    Node := ATree.GetFirst() // Начинаем с первого узла дерева
  else
    Node := RootNode; // Иначе начинаем с указанного узла

  StartNode := Node;

  Node := StartNode; // Восстанавливаем начальный узел для сохранения

  while Assigned(Node) do
  begin
    // --- ИЗМЕНЕНО: теперь сохраняем данные узла перед ChildCount ---
    WriteNodeData(ATree, Node, AStream);
    Inc(LoadedNodeCount); // Увеличиваем счетчик

    // --- ИЗМЕНЕНО: Используем TVirtualNode.ChildCount для подсчета детей ---
    ChildCount := Node.ChildCount;

    // Записываем количество детей
    AStream.Write(ChildCount, SizeOf(ChildCount));

    // --- ИЗМЕНЕНО: Используем прямой доступ к первому ребенку ---
    ChildNode := Node.FirstChild; // Используем прямой доступ к первому ребенку

    // Рекурсивно сохраняем детей
    while Assigned(ChildNode) do
    begin
      // Рекурсивный вызов для сохранения поддерева ребенка
      SaveTreeToStream(ATree, AStream, ChildNode); // Передаем ChildNode как RootNode для поддерева
      Inc(LoadedNodeCount); // Увеличиваем счетчик
      // Переходим к следующему ребенку
      ChildNode := ChildNode.NextSibling;
    end;

    // --- ИЗМЕНЕНО: Переход к следующему узлу на том же уровне, если RootNode не задан ---
    // Используем GetNext с ConsiderChildrenAbove = False для обхода на одном уровне
    if not Assigned(RootNode) then
      Node := ATree.GetNext(Node, False) // Обходим узлы на корневом уровне и ниже, не заходя в детей
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
  ChildCount: Cardinal; // --- ИЗМЕНЕНО: Теперь используем Cardinal как тип ChildCount ---
  i: Integer; // --- ВВЕДЕНО: Для цикла загрузки детей ---
  CurrentParent: PVirtualNode; // --- ВВЕДЕНО: Для ясности ---
  TotalNodeCount: SizeInt; // --- ВВЕДЕНО: Для чтения общего количества узлов ---
  LoadedNodeCount: SizeInt; // --- ВВЕДЕНО: Для отслеживания загруженных узлов ---
begin
  // --- ИЗМЕНЕНО: Проверка на пустой поток и чтение TotalNodeCount ---
  if AStream.Size = 0 then Exit; // Нечего загружать

  // Читаем общее количество узлов
  if AStream.Position + SizeOf(TotalNodeCount) > AStream.Size then Exit;
  AStream.Read(TotalNodeCount, SizeOf(TotalNodeCount));
  LoadedNodeCount := 0; // Сбрасываем счетчик

  // Если ParentNode = nil, очищаем дерево и начинаем загрузку с корня
  if not Assigned(ParentNode) then
  begin
    ATree.Clear; // Очищаем дерево перед загрузкой
    ATree.Tag := 1; // Сбрасываем ID для новой загрузки, если используется такая логика в других частях

    // Цикл для загрузки TotalNodeCount узлов
    // Этот цикл должен обрабатывать как корневые узлы, так и их поддеревья
    // Логика: читаем узел, ChildCount, затем рекурсивно загружаем ChildCount детей
    // Проблема: как отличить корневой узел от дочернего при чтении?
    // Решение: SaveTreeToStream записывает узел, затем ChildCount, затем детей этого узла.
    // LoadTreeFromStream (рекурсивно) читает узел, ChildCount, создает детей, вызывая себя для каждого ребенка.
    // При вызове с ParentNode = nil, мы должны читать корневые узлы, пока не загрузим TotalNodeCount.
    // Для этого основной цикл должен вызывать LoadTreeFromStream с ParentNode = nil,
    // но LoadTreeFromStream должен отличать вызов для корневого узла от вызова для дочернего.
    // В текущей реализации, если ParentNode = nil, он создает корневой узел AddChild(nil).
    // Если ParentNode <> nil, он создает дочерний AddChild(ParentNode).
    // Это работает, если мы вызываем LoadTreeFromStream для каждого корневого узла отдельно.
    // Но нам нужно вызвать его один раз, и он должен сам обработать все корневые узлы.
    // Значит, основной цикл здесь должен читать корневые узлы и вызывать LoadTreeFromStream для их детей.
    // А сам LoadTreeFromStream (когда ему передают ParentNode) будет читать только один узел и его детей.

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
        for i := 0 to pred(Integer(ChildCount)) do // Приводим Cardinal к Integer для цикла
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
    for i := 0 to pred(Integer(ChildCount)) do // Приводим Cardinal к Integer для цикла
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