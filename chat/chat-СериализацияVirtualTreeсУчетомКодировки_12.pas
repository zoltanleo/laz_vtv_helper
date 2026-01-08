// unit unit_virtstringtree;

// ... (other code remains the same)

  TVirtStringTreeHelper = class
  public
    class procedure InitializeTree(ATree: TLazVirtualStringTree);
    class procedure AddNode(ATree: TLazVirtualStringTree; AParentNode: PVirtualNode;
      const AUserData, ACaption, AActionName: string);
    // Удаляем объявление устаревшей процедуры
    // class procedure LoadTreeFromStreamWithStructure(ATree: TLazVirtualStringTree; AStream: TMemoryStream);
    // Модифицированная сигнатура для SaveTreeToStream: добавляем параметр RootNode
    class procedure SaveTreeToStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream; RootNode: PVirtualNode = nil);
    // Модифицированная сигнатура для LoadTreeFromStream: добавляем параметр ParentNode
    class procedure LoadTreeFromStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream; ParentNode: PVirtualNode = nil);
  end;

// ... (other code remains the same)

implementation

// ... (other code remains the same)

class procedure TVirtStringTreeHelper.SaveTreeToStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream; RootNode: PVirtualNode);
var
  Node: PVirtualNode;
  ChildNode: PVirtualNode;
  NodeData: PMyRecord;
  // Добавляем переменные для подсчета количества детей
  ChildCount: Integer;
  NodeUserData, NodeCaption, NodeActionName: String;
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
    NodeData := ATree.GetNodeData(Node);
    if Assigned(NodeData) then
    begin
      NodeUserData := NodeData.UserData;
      NodeCaption := NodeData.Caption;
      NodeActionName := NodeData.ActionName;
    end
    else
    begin
      // Обработка случая, если NodeData не назначено
      NodeUserData := '';
      NodeCaption := '';
      NodeActionName := '';
    end;

    // Записываем данные текущего узла
    AStream.WriteBuffer(Pointer(NodeUserData)^, Length(NodeUserData));
    AStream.WriteWord(0); // Разделитель
    AStream.WriteBuffer(Pointer(NodeCaption)^, Length(NodeCaption));
    AStream.WriteWord(0); // Разделитель
    AStream.WriteBuffer(Pointer(NodeActionName)^, Length(NodeActionName));
    AStream.WriteWord(0); // Разделитель

    // Подсчитываем количество детей
    ChildCount := 0;
    ChildNode := Node.FirstChild;
    while Assigned(ChildNode) do
    begin
      Inc(ChildCount);
      ChildNode := ChildNode.NextSibling;
    end;

    // Записываем количество детей
    AStream.WriteInteger(ChildCount);

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

class procedure TVirtStringTreeHelper.LoadTreeFromStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream; ParentNode: PVirtualNode);
var
  NewNode: PVirtualNode;
  NodeData: PMyRecord;
  NodeUserData, NodeCaption, NodeActionName: String;
  Len: Integer;
  ChildCount: Integer;
  I: Integer;
  // Используем TNodeArray для хранения созданных узлов, если нужно
  // TNodeArray = array of PVirtualNode; (уже определен в laz.VirtualTrees)
  // В данном случае мы будем создавать узлы по одному и сразу обрабатывать их детей
begin
  // Проверяем, есть ли еще данные в потоке
  if AStream.Position >= AStream.Size then
    Exit;

  // Читаем UserData
  Len := 0;
  while (AStream.Position < AStream.Size) and (AStream.ReadByte <> 0) do
  begin
    AStream.Seek(-1, soFromCurrent);
    SetLength(NodeUserData, Len + 1);
    AStream.ReadBuffer(NodeUserData[Len + 1], 1);
    Inc(Len);
  end;
  SetLength(NodeUserData, Len); // Устанавливаем реальную длину

  // Проверяем снова, чтобы избежать ошибок при поврежденном потоке
  if AStream.Position >= AStream.Size then
    Exit;

  // Читаем Caption
  Len := 0;
  while (AStream.Position < AStream.Size) and (AStream.ReadByte <> 0) do
  begin
    AStream.Seek(-1, soFromCurrent);
    SetLength(NodeCaption, Len + 1);
    AStream.ReadBuffer(NodeCaption[Len + 1], 1);
    Inc(Len);
  end;
  SetLength(NodeCaption, Len);

  if AStream.Position >= AStream.Size then
    Exit;

  // Читаем ActionName
  Len := 0;
  while (AStream.Position < AStream.Size) and (AStream.ReadByte <> 0) do
  begin
    AStream.Seek(-1, soFromCurrent);
    SetLength(NodeActionName, Len + 1);
    AStream.ReadBuffer(NodeActionName[Len + 1], 1);
    Inc(Len);
  end;
  SetLength(NodeActionName, Len);

  if AStream.Position >= AStream.Size then
    Exit;

  // Читаем количество детей
  AStream.ReadInteger(ChildCount);

  // Создаем новый узел
  NewNode := ATree.AddChild(ParentNode); // Используем ParentNode, переданный в параметре
  NodeData := ATree.GetNodeData(NewNode);
  if Assigned(NodeData) then
  begin
    NodeData.UserData := NodeUserData;
    NodeData.Caption := NodeCaption;
    NodeData.ActionName := NodeActionName;
  end;

  // Рекурсивно загружаем детей
  for I := 0 to ChildCount - 1 do
  begin
    // Рекурсивный вызов для загрузки поддерева, передаем только что созданный NewNode как Parent
    LoadTreeFromStream(ATree, AStream, NewNode);
  end;
end;

// ... (other code remains the same)