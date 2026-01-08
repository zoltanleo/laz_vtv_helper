// --- ИЗМЕНЕНАЯ ПРОЦЕДУРА SaveTreeToStream ---
// Сохраняет дерево, начиная с RootNode (или все корневые узлы, если RootNode = nil).
// Сохраняет структуру: TotalNodeCount, узел -> ChildCount -> дети узла и т.д.
class procedure TVirtStringTreeHelper.SaveTreeToStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream; RootNode: PVirtualNode);
var
  Node: PVirtualNode;
  ChildNode: PVirtualNode;
  NodeData: PMyRecord;
  ChildCount: Cardinal; // Используем Cardinal как тип ChildCount
  StartNode: PVirtualNode; // Для определения начального узла обхода
  TotalNodeCount: SizeInt; // Для подсчета общего количества узлов
  LoadedNodeCount: SizeInt; // Для отслеживания загруженных узлов
  // --- ДОБАВЛЕНО: переменные для обхода поддерева ---
  CheckNode: PVirtualNode; // Вместо scoped-переменной
  IsInSubtree: Boolean; // Вместо scoped-переменной
  // --- КОНЕЦ ДОБАВЛЕНИЯ ---
  // --- ДОБАВЛЕНО: переменные для обхода корневых узлов ---
  RootIter: PVirtualNode; // Итератор для корневых узлов
  SubtreeRoot: PVirtualNode; // Текущий корневой узел поддерева при обходе от RootNode
  CurrentLevel: Cardinal; // Для проверки уровня узла
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
    RootIter := ATree.GetFirst(True); // Получаем первый узел дерева
    while Assigned(RootIter) do
    begin
      CurrentLevel := ATree.GetNodeLevel(RootIter); // Проверяем уровень
      if CurrentLevel = 0 then // Это корневой узел
      begin
        // Для каждого корневого узла подсчитываем его поддерево
        SubtreeRoot := RootIter;
        Node := SubtreeRoot;
        while Assigned(Node) do
        begin
          Inc(TotalNodeCount);
          Node := ATree.GetNext(Node, True); // Обходим поддерево
          // Прерываем, если вышли за пределы поддерева SubtreeRoot
          if Assigned(Node) and (Node^.Parent <> SubtreeRoot) and (Node <> SubtreeRoot) then
          begin
            CheckNode := Node^.Parent;
            IsInSubtree := False;
            while Assigned(CheckNode) and (CheckNode <> ATree.RootNode) do
            begin
              if CheckNode = SubtreeRoot then
              begin
                IsInSubtree := True;
                Break;
              end;
              CheckNode := CheckNode^.Parent;
            end;
            if not IsInSubtree then Break; // Вышли за пределы поддерева корневого узла
          end;
        end;
      end;
      RootIter := ATree.GetNext(RootIter, True); // Переходим к следующему узлу
      // Прерываем, если следующий узел не корневой (уровень > 0)
      if Assigned(RootIter) and (ATree.GetNodeLevel(RootIter) > 0) then Break;
    end;
  end;

  // Записываем общее количество узлов
  AStream.Write(TotalNodeCount, SizeOf(TotalNodeCount));
  LoadedNodeCount := 0; // Сбрасываем счетчик
  // --- КОНЕЦ ВВЕДЕНИЯ ---

  // Если RootNode не задан, сохраняем все корневые узлы
  if not Assigned(RootNode) then
  begin
    RootIter := ATree.GetFirst(True); // Начинаем с первого узла дерева
    while Assigned(RootIter) do
    begin
      CurrentLevel := ATree.GetNodeLevel(RootIter); // Проверяем уровень
      if CurrentLevel = 0 then // Это корневой узел
      begin
        Node := RootIter;
        // Обрабатываем поддерево RootIter
        SubtreeRoot := Node;
        while Assigned(Node) do
        begin
          // --- теперь сохраняем данные узла перед ChildCount ---
          WriteNodeData(ATree, Node, AStream);
          Inc(LoadedNodeCount); // Увеличиваем счетчик

          // --- Используем TVirtualNode.ChildCount для подсчета детей ---
          ChildCount := Node.ChildCount;

          // Записываем количество детей
          AStream.Write(ChildCount, SizeOf(ChildCount));

          // --- Используем прямой доступ к первому ребенку ---
          ChildNode := Node.FirstChild; // Используем прямой доступ к первому ребенку

          // Рекурсивно сохраняем детей
          while Assigned(ChildNode) do
          begin
            // Рекурсивный вызов для сохранения поддерева ребенка
            // Передаем ChildNode как RootNode для поддерева, но с ограничением по подсчету
            // Чтобы не пересчитывать TotalNodeCount снова, передаем nil или флаг.
            // Лучше изменить логику подсчета, чтобы он происходил до записи.
            // Текущая реализация уже подсчитала TotalNodeCount.
            SaveTreeToStream(ATree, AStream, ChildNode); // Передаем ChildNode как RootNode для поддерева
            Inc(LoadedNodeCount); // Увеличиваем счетчик
            // Переходим к следующему ребенку
            ChildNode := ChildNode.NextSibling;
          end;

          // Переходим к следующему узлу в поддереве SubtreeRoot
          Node := ATree.GetNext(Node, True);
          // Прерываем, если вышли за пределы поддерева SubtreeRoot
          if Assigned(Node) and (Node^.Parent <> SubtreeRoot) and (Node <> SubtreeRoot) then
          begin
            CheckNode := Node^.Parent;
            IsInSubtree := False;
            while Assigned(CheckNode) and (CheckNode <> ATree.RootNode) do
            begin
              if CheckNode = SubtreeRoot then
              begin
                IsInSubtree := True;
                Break;
              end;
              CheckNode := CheckNode^.Parent;
            end;
            if not IsInSubtree then Break; // Вышли за пределы поддерева корневого узла
          end;
        end;
      end;
      RootIter := ATree.GetNext(RootIter, True); // Переходим к следующему узлу
      // Прерываем, если следующий узел не корневой (уровень > 0)
      if Assigned(RootIter) and (ATree.GetNodeLevel(RootIter) > 0) then Break;
    end;
  end
  else // Если RootNode задан, сохраняем его поддерево
  begin
    Node := RootNode;
    SubtreeRoot := Node;
    while Assigned(Node) do
    begin
      // --- теперь сохраняем данные узла перед ChildCount ---
      WriteNodeData(ATree, Node, AStream);
      Inc(LoadedNodeCount); // Увеличиваем счетчик

      // --- Используем TVirtualNode.ChildCount для подсчета детей ---
      ChildCount := Node.ChildCount;

      // Записываем количество детей
      AStream.Write(ChildCount, SizeOf(ChildCount));

      // --- Используем прямой доступ к первому ребенку ---
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

      // Переходим к следующему узлу в поддереве SubtreeRoot
      Node := ATree.GetNext(Node, True);
      // Прерываем, если вышли за пределы поддерева SubtreeRoot
      if Assigned(Node) and (Node^.Parent <> SubtreeRoot) and (Node <> SubtreeRoot) then
      begin
        CheckNode := Node^.Parent;
        IsInSubtree := False;
        while Assigned(CheckNode) and (CheckNode <> ATree.RootNode) do
        begin
          if CheckNode = SubtreeRoot then
          begin
            IsInSubtree := True;
            Break;
          end;
          CheckNode := CheckNode^.Parent;
        end;
        if not IsInSubtree then Break; // Вышли за пределы поддерева RootNode
      end;
    end;
  end;
end;
// --- КОНЕЦ ИЗМЕНЕНИЯ ---

// --- ИЗМЕНЕНАЯ ПРОЦЕДУРА LoadTreeFromStream ---
// Загружает дерево из потока в ATree. Если ParentNode = nil, очищает ATree и загружает все корневые узлы.
// Если ParentNode задан, загружает один узел и его детей как поддерево к ParentNode.
// Ожидает формат: TotalNodeCount, [узел, ChildCount, (дети узла)], ...
class procedure TVirtStringTreeHelper.LoadTreeFromStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream; ParentNode: PVirtualNode);
var
  NewNode: PVirtualNode;
  NodeData: PMyRecord;
  ChildCount: Cardinal; // Теперь используем Cardinal как тип ChildCount
  i: Integer; // Для цикла загрузки детей
  CurrentParent: PVirtualNode; // Для ясности
  TotalNodeCount: SizeInt; // Для чтения общего количества узлов
  LoadedNodeCount: SizeInt; // Для отслеживания загруженных узлов
  // --- ДОБАВЛЕНО: переменные для контроля количества узлов ---
  CurrentLoadCount: SizeInt; // Счетчик узлов, загруженных в рамках текущего вызова
  TargetLoadCount: SizeInt; // Целевое количество узлов для загрузки в рамках текущего вызова
  // --- КОНЕЦ ДОБАВЛЕНИЯ ---
begin
  // --- Проверка на пустой поток и чтение TotalNodeCount ---
  if AStream.Size = 0 then Exit; // Нечего загружать

  // Читаем общее количество узлов только если ParentNode = nil
  if not Assigned(ParentNode) then
  begin
      if AStream.Position + SizeOf(TotalNodeCount) > AStream.Size then Exit;
      AStream.Read(TotalNodeCount, SizeOf(TotalNodeCount));
      LoadedNodeCount := 0; // Сбрасываем общий счетчик
  end
  else
  begin
      // Если ParentNode задан, мы в рекурсивном вызове.
      // TotalNodeCount уже был прочитан ранее.
      // Предполагаем, что TotalNodeCount доступен из внешнего контекста.
      // Для простоты, будем использовать общий LoadedNodeCount.
      // Лучше передавать TotalNodeCount как параметр, но это усложнит интерфейс.
      // Альтернатива: использовать AStream.Size как ориентир, но это менее надежно.
      // Попробуем использовать общий LoadedNodeCount и TotalNodeCount из ATree.Tag.
      // Но Tag используется для ID. Не будем его перегружать.
      // Вместо этого, будем читать узлы, пока не встретим конец потока или пока не загрузим ChildCount детей.
      // TotalNodeCount не читаем повторно.
      TotalNodeCount := -1; // Индикатор, что это рекурсивный вызов
  end;

  // Если ParentNode = nil, очищаем дерево и начинаем загрузку с корня
  if not Assigned(ParentNode) then
  begin
    ATree.Clear; // Очищаем дерево перед загрузкой
    ATree.Tag := 1; // Сбрасываем ID для новой загрузки

    // Цикл для загрузки TotalNodeCount узлов как корневые
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
            // Inc(LoadedNodeCount) уже происходит внутри рекурсивного вызова
        end;
    end;
  end
  else // Если ParentNode задан, загружаем один узел как ребенка ParentNode
  begin
    // Проверяем, не превысили ли мы TotalNodeCount. Это сложно сделать без передачи TotalNodeCount.
    // Будем полагаться на структуру потока: узел, ChildCount, дети.
    // Создаем новый узел как дочерний для ParentNode
    NewNode := ATree.AddChild(ParentNode);
    // Читаем данные для этого узла
    ReadNodeData(ATree, NewNode, AStream);
    Inc(LoadedNodeCount); // Увеличиваем общий счетчик

    // Читаем количество детей для только что созданного узла
    if AStream.Position + SizeOf(ChildCount) > AStream.Size then Exit; // Недостаточно данных для ChildCount
    AStream.Read(ChildCount, SizeOf(ChildCount));

    // Цикл для загрузки ChildCount детей
    for i := 0 to pred(Integer(ChildCount)) do // Приводим Cardinal к Integer для цикла
    begin
        // Рекурсивный вызов для загрузки поддерева, передаем NewNode как Parent
        LoadTreeFromStream(ATree, AStream, NewNode); // NewNode становится Parent для следующего уровня
        // Inc(LoadedNodeCount) уже происходит внутри рекурсивного вызова
    end;
  end;
end;
// --- КОНЕЦ ИЗМЕНЕНИЯ ---