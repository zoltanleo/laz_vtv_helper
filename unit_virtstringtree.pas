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

  TRecArr = array of TMyRecord;

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
  DestMS: TMemoryStream = nil;
  Buff: array of Byte;
  Node: PVirtualNode = nil;
  Len: int64 = 0;
  Data: PMyRecord = Nil;
  RecArrSrc: TRecArr;
  RecArrDest: TRecArr;

  //возвращает кол-во элементов с ParentID = ChildID во входном массиве InRecArr,
  //при их наличии заполняет ими выходной массив OutRecArr
  function GetChildRecords(ChildID: SizeInt; InRecArr: TRecArr; out OutRecArr: TRecArr):SizeInt;
  var
    idx: SizeInt  = 0;
  begin
    Result:= 0;

    for idx := 0 to High(InRecArr) do
      if (InRecArr[idx].ParentID = ChildID) then Inc(Result);

    if (Result = 0) then Exit;

    SetLength(OutRecArr,0);//инициализируем выходной буфер-массив

    for idx := 0 to High(InRecArr) do
      if (InRecArr[idx].ParentID = ChildID) then
      begin
        SetLength(OutRecArr,Length(OutRecArr) + 1);
        OutRecArr[High(OutRecArr)]:= InRecArr[idx];
      end;
  end;

  //добавляет в дерево aTree узлы одного aParentID, если ParentNode определен,
  //то узлы будут дочерними, иначе - корневыми
  procedure AddNodeFromArray(aParentID: SizeInt; ParentNode: PVirtualNode = nil);
  var
    _Node: PVirtualNode = nil;
    _Data: PMyRecord = nil;
    _RecArr: TRecArr;
    j: SizeInt = 0;
  begin
    if (GetChildRecords(aParentID, RecArrSrc,_RecArr) = 0) then Exit;

    for j := 0 to High(_RecArr) do
    begin
      _Node:= aTree.AddChild(ParentNode);
      _Data:= aTree.GetNodeData(_Node);
      _Data^:= _RecArr[j];
    end;

    if Assigned(ParentNode)
      then _Node:= ParentNode^.FirstChild
      else _Node:= aTree.GetFirst;

    while Assigned(_Node) do
    begin
      _Data:= aTree.GetNodeData(_Node);
      AddNodeFromArray(_Data^.ID, _Node);//добавляем вложенные узлы
      _Node:= _Node^.NextSibling;
    end;
  end;
begin
  if (AStream.Size = 0) then Exit;

  DestMS:= TMemoryStream.Create;
  try
    ATree.BeginUpdate;
    try
      ATree.Clear;
      AStream.Position:= 0;
      SetLength(RecArrSrc,0);

      while (AStream.Position < AStream.Size) do //пока не достигли конца AStream
      begin
        ATree.Clear;
        Node:= ATree.AddChild(nil);//создаем узел

        AStream.Read(Len,SizeOf(Int64));//читаем, сколько байт прочитать для очередного узла

        {обходной путь вместо AStream.CopyFrom
         SetLength(Buff,Len);//задаем размер буфера
         AStream.Read(Buff,Len);//заполняем буфер
        }

        DestMS.Clear;

        {обходной путь вместо AStream.CopyFrom
         DestMS.Write(Buff,Length(Buff));//пишем во временный поток
        }
        AStream.CopyFrom(DestMS,Len);

        DestMS.Seek(0,soBeginning);
        ReadNodeData(ATree,Node,DestMS);//десериализуем данные

        //получаем десериализованные данные узла
        Data:= ATree.GetNodeData(Node);

        SetLength(RecArrSrc,Length(RecArrSrc) + 1);//увеличиваем размер массива
        RecArrSrc[High(RecArrSrc)]:= Data^;// добавляем текущую запись в массив
      end;

      //получаем ParentID первого root-узла
      Node:= ATree.GetFirst;
      Data:= ATree.GetNodeData(Node);

      //перестраиваем дерево
      if (GetChildRecords(Data^.ParentID,RecArrSrc,RecArrDest) = 0) then Exit;

      ATree.Clear;
      AddNodeFromArray(Data^.ParentID);//заполняем дерева
    finally
      ATree.EndUpdate;
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

  if Assigned(aNode) then
  begin
    Data:= aTree.GetNodeData(aNode);
    ParentID := Data^.ID;
  end else ParentID := -1;

  Data:= aTree.GetNodeData(Result);

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

