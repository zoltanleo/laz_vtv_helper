unit unit_virtstringtree;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, laz.VirtualTrees, LCLIntf, LCLType;

type

  PMyRecord = ^TMyRecord;
  TMyRecord = record
    ID: SizeInt;          // ID узла дерева
    ParentID: SizeInt;    // содержит для child-узла ID root-узла (для root-узла равен -1)
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
    class procedure WriteNodeData(aTree: TBaseVirtualTree; aNode: PVirtualNode; Stream: TStream);
    class procedure ReadNodeData(aTree: TBaseVirtualTree; aNode: PVirtualNode; Stream: TStream);
    //class function GetNodeCount(aTree: TBaseVirtualTree): SizeInt;
  public
    class procedure SaveTreeToStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream);
    class procedure LoadTreeFromStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream; ParentNode: PVirtualNode = nil);
    class function AddNode(aTree: TBaseVirtualTree; aNode: PVirtualNode; const AActionName, ACaption, AtsName: String): PVirtualNode;
    class procedure InitializeTree(aTree: TBaseVirtualTree); // устанавливает NodeDataSize
  end;

implementation

{ TVirtStringTreeHelper }

class procedure TVirtStringTreeHelper.WriteNodeData(aTree: TBaseVirtualTree; aNode: PVirtualNode; Stream: TStream);
var
  Data: PMyRecord = nil;
  Len: SizeInt = 0;
  EncodedActionName: UTF8String = '';
  EncodedCaption: UTF8String = '';
  EncodedtsName: UTF8String = '';
begin
  Data:= aTree.GetNodeData(aNode);

  // Записываем данные в UTF-8 для обеспечения совместимости
  EncodedActionName := UTF8String(Data^.ActionName);
  EncodedCaption := UTF8String(Data^.Caption);
  EncodedtsName := UTF8String(Data^.tsName);

  // Подсчитываем общую длину байтов данных этого конкретного узла (не всего дерева)
  len:= SizeOf(Data^.ID)
        + SizeOf(Data^.ParentID)
        + SizeOf(Length(EncodedActionName))
        + SizeOf(Length(EncodedCaption))
        + SizeOf(Length(EncodedtsName));

  Stream.Write(len, SizeOf(len));// и пишем ее в поток

  // Записываем числовые значения ID и ParentID в поток.
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

class procedure TVirtStringTreeHelper.ReadNodeData(aTree: TBaseVirtualTree; aNode: PVirtualNode; Stream: TStream);
var
  Data: PMyRecord = nil;
  Len: SizeInt = 0;
  TempUTF8: UTF8String = '';
begin
  Data:= aTree.GetNodeData(aNode);

  Stream.Read(Data^.ID, SizeOf(Data^.ID));
  Stream.Read(Data^.ParentID, SizeOf(Data^.ParentID));

  // Читаем ActionName
  Stream.Read(Len, SizeOf(Len));
  SetLength(TempUTF8, Len);

  if (Len > 0) then
  begin
    Stream.Read(TempUTF8[1], Len);
    Data^.ActionName := String(TempUTF8);
  end else Data^.ActionName := '';

  // Читаем Caption
  Stream.Read(Len, SizeOf(Len));
  SetLength(TempUTF8, Len);

  if (Len > 0) then
  begin
    Stream.Read(TempUTF8[1], Len);
    Data^.Caption := String(TempUTF8)
  end else Data^.Caption := '';

  // Читаем tsName
  Stream.Read(Len, SizeOf(Len));
  SetLength(TempUTF8, Len);

  if (Len > 0) then
  begin
    Stream.Read(TempUTF8[1], Len);
    Data^.tsName := String(TempUTF8);
  end else Data^.tsName:= '';
end;

//class function TVirtStringTreeHelper.GetNodeCount(aTree: TBaseVirtualTree): SizeInt;
//var
//  Node: PVirtualNode = nil;
//begin
//  Result := 0;
//  Node := aTree.GetFirst();
//  while Assigned(Node) do
//  begin
//    Inc(Result);
//    Node := aTree.GetNext(Node);
//  end;
//end;

class procedure TVirtStringTreeHelper.SaveTreeToStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream);
var
  Node: PVirtualNode = nil;
  NodeArr: TNodeArray = nil;
  i: SizeInt = 0;

  // Функция для рекурсивного добавления узлов в массив
  procedure AddNodesToArray(ANode: PVirtualNode; var ANodeArray: TNodeArray);
  var
     ChildNode: PVirtualNode = nil;
  begin
    // Добавляем текущий узел в массив
    SetLength(ANodeArray, Length(ANodeArray) + 1);
    ANodeArray[High(ANodeArray)] := ANode;

    // Проходим по дочерним узлам
    ChildNode := ANode^.FirstChild;
    while Assigned(ChildNode) do
    begin
      AddNodesToArray(ChildNode, ANodeArray); // Рекурсивно добавляем потомков
      ChildNode := ChildNode^.NextSibling;
    end;
  end;

begin
  // Проходим по корневым узлам
  Node:= ATree.GetFirst;
  if not Assigned(Node) then Exit;

  AStream.Clear;

  while Assigned(Node) do
  begin
    AddNodesToArray(Node, NodeArr);
    Node:= Node^.NextSibling;
  end;

  //записываем в поток последовательно все данные узлов по порядку
  for i := 0 to High(NodeArr) do WriteNodeData(ATree, NodeArr[i], AStream);
end;

class procedure TVirtStringTreeHelper.LoadTreeFromStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream; ParentNode: PVirtualNode);
//var
//  NewNode: PVirtualNode;
//  NodeData: PMyRecord;
//  ChildCount: Cardinal; // --- ИЗМЕНЕНО: Теперь используем Cardinal как тип ChildCount ---
//  i: Integer; // --- ВВЕДЕНО: Для цикла загрузки детей ---
//  CurrentParent: PVirtualNode; // --- ВВЕДЕНО: Для ясности ---
//  TotalNodeCount: SizeInt; // --- ВВЕДЕНО: Для чтения общего количества узлов ---
//  LoadedNodeCount: SizeInt; // --- ВВЕДЕНО: Для отслеживания загруженных узлов ---
begin
  //// --- ИЗМЕНЕНО: Проверка на пустой поток и чтение TotalNodeCount ---
  //if AStream.Size = 0 then Exit; // Нечего загружать
  //
  //// Читаем общее количество узлов
  //if AStream.Position + SizeOf(TotalNodeCount) > AStream.Size then Exit;
  //AStream.Read(TotalNodeCount, SizeOf(TotalNodeCount));
  //LoadedNodeCount := 0; // Сбрасываем счетчик
  //
  //// Если ParentNode = nil, очищаем дерево и начинаем загрузку с корня
  //if not Assigned(ParentNode) then
  //begin
  //  ATree.Clear; // Очищаем дерево перед загрузкой
  //  ATree.Tag := 1; // Сбрасываем ID для новой загрузки, если используется такая логика в других частях
  //
  //  // Цикл для загрузки TotalNodeCount узлов
  //  // Этот цикл должен обрабатывать как корневые узлы, так и их поддеревья
  //  // Логика: читаем узел, ChildCount, затем рекурсивно загружаем ChildCount детей
  //  // Проблема: как отличить корневой узел от дочернего при чтении?
  //  // Решение: SaveTreeToStream записывает узел, затем ChildCount, затем детей этого узла.
  //  // LoadTreeFromStream (рекурсивно) читает узел, ChildCount, создает детей, вызывая себя для каждого ребенка.
  //  // При вызове с ParentNode = nil, мы должны читать корневые узлы, пока не загрузим TotalNodeCount.
  //  // Для этого основной цикл должен вызывать LoadTreeFromStream с ParentNode = nil,
  //  // но LoadTreeFromStream должен отличать вызов для корневого узла от вызова для дочернего.
  //  // В текущей реализации, если ParentNode = nil, он создает корневой узел AddChild(nil).
  //  // Если ParentNode <> nil, он создает дочерний AddChild(ParentNode).
  //  // Это работает, если мы вызываем LoadTreeFromStream для каждого корневого узла отдельно.
  //  // Но нам нужно вызвать его один раз, и он должен сам обработать все корневые узлы.
  //  // Значит, основной цикл здесь должен читать корневые узлы и вызывать LoadTreeFromStream для их детей.
  //  // А сам LoadTreeFromStream (когда ему передают ParentNode) будет читать только один узел и его детей.
  //
  //  // Цикл для загрузки всех корневых узлов и их поддеревьев
  //  while (AStream.Position < AStream.Size) and (LoadedNodeCount < TotalNodeCount) do
  //  begin
  //      // Создаем корневой узел (Parent = nil)
  //      NewNode := ATree.AddChild(nil);
  //      ReadNodeData(ATree, NewNode, AStream);
  //      Inc(LoadedNodeCount);
  //
  //      // Читаем количество детей для этого корневого узла
  //      if AStream.Position + SizeOf(ChildCount) > AStream.Size then Exit; // Недостаточно данных для ChildCount
  //      AStream.Read(ChildCount, SizeOf(ChildCount));
  //
  //      // Цикл для загрузки ChildCount детей
  //      for i := 0 to pred(Integer(ChildCount)) do // Приводим Cardinal к Integer для цикла
  //      begin
  //          // Рекурсивный вызов для загрузки поддерева, передаем только что созданный NewNode как Parent
  //          LoadTreeFromStream(ATree, AStream, NewNode); // Передаем NewNode как Parent для его детей
  //          Inc(LoadedNodeCount);
  //      end;
  //  end;
  //end
  //else // Если ParentNode задан, загружаем один узел как ребенка ParentNode
  //begin
  //  // Создаем новый узел как дочерний для ParentNode
  //  NewNode := ATree.AddChild(ParentNode);
  //  // Читаем данные для этого узла
  //  ReadNodeData(ATree, NewNode, AStream);
  //  Inc(LoadedNodeCount);
  //
  //  // Читаем количество детей для только что созданного узла
  //  if AStream.Position + SizeOf(ChildCount) > AStream.Size then Exit; // Недостаточно данных для ChildCount
  //  AStream.Read(ChildCount, SizeOf(ChildCount));
  //
  //  // Цикл для загрузки ChildCount детей
  //  for i := 0 to pred(Integer(ChildCount)) do // Приводим Cardinal к Integer для цикла
  //  begin
  //      // Рекурсивный вызов для загрузки поддерева, передаем NewNode как Parent
  //      LoadTreeFromStream(ATree, AStream, NewNode); // NewNode становится Parent для следующего уровня
  //      Inc(LoadedNodeCount);
  //  end;
  //end;
end;

class function TVirtStringTreeHelper.AddNode(aTree: TBaseVirtualTree;
  aNode: PVirtualNode; const AActionName, ACaption, AtsName: String): PVirtualNode;
var
  Data: PMyRecord = nil;
  ParentID: SizeInt = 0;
begin
  Result := aTree.AddChild(aNode);
  Data := aTree.GetNodeData(Result);

  if not Assigned(aNode)
     then ParentID := -1
     else ParentID := PMyRecord(aTree.GetNodeData(Result))^.ID;

  Data^.ID := aTree.AbsoluteIndex(Result);
  Data^.ParentID := ParentID;
  Data^.ActionName := AActionName;
  Data^.Caption := ACaption;
  Data^.tsName := AtsName;
end;

class procedure TVirtStringTreeHelper.InitializeTree(aTree: TBaseVirtualTree);
begin
  // Используем вспомогательный класс для доступа к защищенному свойству
  TBaseVirtualTreeAccess(aTree).NodeDataSize := SizeOf(TMyRecord);
end;


end.

