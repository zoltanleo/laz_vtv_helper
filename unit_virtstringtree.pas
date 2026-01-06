unit unit_virtstringtree;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, laz.VirtualTrees, LCLIntf, LCLType;

type

  PMyRecord = ^TMyRecord;
  TMyRecord = record
    ID: Integer;          // ID узла дерева
    ParentID: Integer;    // содержит для child-узла ID root-узла (для root-узла равен -1)
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
    class procedure WriteNodeData(VTree: TBaseVirtualTree; Node: PVirtualNode; Stream: TStream);
    class procedure ReadNodeData(VTree: TBaseVirtualTree; Node: PVirtualNode; Stream: TStream);
    class function GetNodeCount(VTree: TBaseVirtualTree): SizeInt;
  public
    class procedure SaveTreeToStream(VTree: TBaseVirtualTree; Stream: TStream);
    class procedure LoadTreeFromStream(VTree: TBaseVirtualTree; Stream: TStream);
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
  Data:= VTree.GetNodeData(Node);

  // Записываем данные в UTF-8 для обеспечения совместимости
  EncodedActionName := UTF8String(Data^.ActionName);
  EncodedCaption := UTF8String(Data^.Caption);
  EncodedtsName := UTF8String(Data^.tsName);

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

class procedure TVirtStringTreeHelper.ReadNodeData(VTree: TBaseVirtualTree;
  Node: PVirtualNode; Stream: TStream);
var
  Data: PMyRecord = nil;
  Len: SizeInt = 0;
  TempUTF8: UTF8String = '';
  NextID: SizeInt = 0;
begin
  Data:= VTree.GetNodeData(Node);

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

  // Обновляем NextID для корректной генерации новых ID
  NextID := VTree.Tag; // используем Tag для хранения NextID

  if (Data^.ID >= NextID) then NextID := Data^.ID + 1;
  VTree.Tag := NextID;
end;

class function TVirtStringTreeHelper.GetNodeCount(VTree: TBaseVirtualTree
  ): SizeInt;
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

class procedure TVirtStringTreeHelper.SaveTreeToStream(VTree: TBaseVirtualTree;
  Stream: TStream);
var
  Node: PVirtualNode = nil;
  //NodeCount: SizeInt = 0;
begin
  PrepareForSave(VTree);

  // Подсчитываем количество узлов
  //NodeCount := GetNodeCount(VTree);
  //Stream.Write(NodeCount, SizeOf(NodeCount));
  Stream.Write(GetNodeCount(VTree), SizeOf(GetNodeCount(VTree)));

  // Рекурсивно сохраняем все узлы
  Node := VTree.GetFirst();

  while Assigned(Node) do
  begin
    WriteNodeData(VTree, Node, Stream);
    Node := VTree.GetNext(Node);
  end;
end;

class procedure TVirtStringTreeHelper.LoadTreeFromStream(
  VTree: TBaseVirtualTree; Stream: TStream);
var
  NodeCount: SizeInt = 0;
  i: SizeInt = 0;
  Node: PVirtualNode = nil;
begin
  VTree.Clear;
  VTree.Tag := 1; // сбросим NextID

  // Читаем количество узлов
  Stream.Read(NodeCount, SizeOf(NodeCount));

  // Создаем и загружаем узлы
  for i := 0 to Pred(NodeCount) do
  begin
    Node := VTree.AddChild(nil); // добавляем как дочерний узел корня
    ReadNodeData(VTree, Node, Stream);
  end;
end;

class function TVirtStringTreeHelper.AddNode(VTree: TBaseVirtualTree;
  ParentNode: PVirtualNode; const AActionName, ACaption, AtsName: String
  ): PVirtualNode;
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

