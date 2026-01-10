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
  public
    class procedure SaveTreeToStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream);
    class procedure LoadTreeFromStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream);
    class function AddNode(aTree: TBaseVirtualTree; aNode: PVirtualNode; const AActionName, ACaption, AtsName: String): PVirtualNode;
    class procedure InitializeTree(aTree: TBaseVirtualTree); // устанавливает NodeDataSize
  end;

implementation

{ TVirtStringTreeHelper }

class procedure TVirtStringTreeHelper.WriteNodeData(aTree: TBaseVirtualTree; aNode: PVirtualNode; Stream: TStream);
var
  Data: PMyRecord = nil;
  Len: Int64 = 0;
  EncodedActionName: UTF8String = '';
  EncodedCaption: UTF8String = '';
  EncodedtsName: UTF8String = '';
  tmpStream: TMemoryStream = nil;//поток для сериализации этого узла
begin
  tmpStream:= TMemoryStream.Create;

  try
    Data:= aTree.GetNodeData(aNode);

    // Записываем данные в UTF-8 для обеспечения совместимости
    EncodedActionName := UTF8String(Data^.ActionName);
    EncodedCaption := UTF8String(Data^.Caption);
    EncodedtsName := UTF8String(Data^.tsName);

    tmpStream.Clear;

    // Сериализуем числовые значения ID и ParentID в поток.
    tmpStream.Write(Data^.ID, SizeOf(int64));
    tmpStream.Write(Data^.ParentID, SizeOf(Int64));

    // Сериализуем строки как UTF-8
    Len:= Length(EncodedActionName);
    tmpStream.Write(Len, SizeOf(Len));

    if (Len > 0) then tmpStream.Write(EncodedActionName[1], Len);

    Len := Length(EncodedCaption);
    tmpStream.Write(Len, SizeOf(Len));

    if (Len > 0) then tmpStream.Write(EncodedCaption[1], Len);

    Len := Length(EncodedtsName);
    tmpStream.Write(Len, SizeOf(Len));

    if (Len > 0) then tmpStream.Write(EncodedtsName[1], Len);


    // Подсчитываем общую длину байтов данных в временном потоке и пишем её в основной поток
    // в
    //Stream.Write(TmpStream.Size, SizeOf(SizeInt)); // SizeInt - 4 байта(32-бит)/8 байт(64-бит)
    Len:= TmpStream.Size;
    Stream.Write(Len, SizeOf(Len)); // Int64 - 8 байт(32-бит)/8 байт(64-бит)

    tmpStream.Seek(0,soBeginning);//сдвигаемся на начало данных
    Stream.CopyFrom(tmpStream,tmpStream.Size);//пишем все сериализованные данные узла
  finally
    tmpStream.Free;
  end;
end;

class procedure TVirtStringTreeHelper.ReadNodeData(aTree: TBaseVirtualTree; aNode: PVirtualNode; Stream: TStream);
var
  Data: PMyRecord = nil;
  Len: Int64 = 0;
  TempUTF8: UTF8String = '';
begin
  Data:= aTree.GetNodeData(aNode);

  Stream.Read(Data^.ID, SizeOf(Int64));
  Stream.Read(Data^.ParentID, SizeOf(Int64));

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

  //сериализуем в поток данные узлов по порядку из расположения
  for i := 0 to High(NodeArr) do
  begin
    AStream.Seek(0, soEnd);//сдвигаем позицию в конец потока
    WriteNodeData(ATree, NodeArr[i], AStream);//сериализуем данные узла в него
  end;
end;

class procedure TVirtStringTreeHelper.LoadTreeFromStream(ATree: TLazVirtualStringTree; AStream: TMemoryStream);
var
  Node: PVirtualNode = nil;
  NodeArr: TNodeArray = nil;
  Len: int64 = 0;
  DestMS: TMemoryStream = nil;
  Buff: array of Byte;
  i: SizeInt = 0;
  Data: PMyRecord = Nil;
begin
  if (AStream.Size = 0) then Exit;

  DestMS:= TMemoryStream.Create;
  try
    //ATree.BeginUpdate;
    try
      ATree.Clear;
      AStream.Position:= 0;

      while (AStream.Position < AStream.Size) do //пока не достигли конца AStream
      begin
        Node:= ATree.AddChild(nil);//создаем узел

        SetLength(NodeArr, Length(NodeArr) + 1);//увеличиваем размер массива узлов
        NodeArr[High(NodeArr)]:= Node;// Добавляем текущий узел в массив

        AStream.Read(Len,SizeOf(Int64));//читаем, сколько байт прочитать для очередного узла
        SetLength(Buff,Len);
        AStream.Read(Buff,Len);
        //AStream.CopyFrom(DestMS,Len);//читаем все байты во временный поток
        DestMS.Clear;
        DestMS.Write(Buff,Length(Buff));
        DestMS.Seek(0,soBeginning);
        ReadNodeData(ATree,Node,DestMS);//десериализуем данные

        Data:= nil;
        Data:= ATree.GetNodeData(Node);
        //ATree.Clear;
      end;


      if (Length(NodeArr) > 0) then
      begin
        for i := 0 to High(NodeArr) do
        begin

        end;
      end;

    finally
      //ATree.EndUpdate;
    end;
  finally
    DestMS.Free;
  end;

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

