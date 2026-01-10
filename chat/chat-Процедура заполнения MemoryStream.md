### USER
Имеется массив из 10 целых чисел с произвольным значением от 10 до 50. Каждое число характеризует длину массива произвольных байтов. Необходимо последовательно записать в поток ms:TmemoryStream длину массива байтов + сам массив произвольных байтов для каждого числа массива целых чисел. 
Напиши код процедуры для fpc/lazarus

### ASSISTANT
Вот пример кода процедуры на **Free Pascal (FPC) / Lazarus**, которая:

- Создаёт массив из 10 случайных целых чисел от 10 до 50.
- Для каждого числа:
  - Генерирует массив байтов указанной длины со случайными значениями.
  - Записывает в `TMemoryStream` сначала длину массива (как `Integer`), затем сами байты.

```pascal
uses
  Classes, SysUtils;

procedure FillStreamWithByteArrays(ms: TMemoryStream);
var
  i, LengthValue, j: Integer;
  ByteArray: array of Byte;
begin
  Randomize; // Инициализация генератора случайных чисел

  // Массив из 10 случайных длин от 10 до 50
  for i := 0 to 9 do
  begin
    LengthValue := Random(41) + 10; // от 10 до 50 включительно

    // Создаём динамический массив байтов нужной длины
    SetLength(ByteArray, LengthValue);

    // Заполняем его случайными байтами
    for j := 0 to LengthValue - 1 do
      ByteArray[j] := Random(256); // от 0 до 255

    // Записываем длину массива как Integer (4 байта)
    ms.WriteBuffer(LengthValue, SizeOf(Integer));

    // Записываем сам массив байтов
    if LengthValue > 0 then
      ms.WriteBuffer(ByteArray[0], LengthValue);
  end;
end;

// Пример использования
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    FillStreamWithByteArrays(ms);
    WriteLn('Данные записаны в поток. Размер: ', ms.Size);
  finally
    ms.Free;
  end;
end.
```

### Объяснение:
- `Random(41) + 10` — генерирует число от 10 до 50.
- `SetLength(ByteArray, LengthValue)` — создаёт динамический массив байтов заданной длины.
- `ms.WriteBuffer` используется для записи как длины (`Integer`, 4 байта), так и самих байтов.
- `WriteBuffer` безопасен и не зависит от endianness (в отличие от `Write`).

Можешь использовать этот код в своём проекте Lazarus/FPC.

### USER
Создай пример чтения из потока ms: TmemoryStream, созданного в процедуре FillStreamWithByteArrays(ms: TMemoryStream), длины массива байтов (в переменную LenBuff, которая будет записываться в динамический массив целых чисел ДутИгааФкк) и самого массива байтов (Buff), заданного в переменной LenBuff (Buff также будет записываться, но в динамический массив BuffArr массива байтов)
Напиши код процедуры для fpc/lazarus

### ASSISTANT
Вот пример кода процедуры на **Free Pascal (FPC) / Lazarus**, который читает из потока `TMemoryStream`, созданного в предыдущей процедуре:

- Читает длину массива байтов (`LenBuff`) и сохраняет в динамический массив `ДутИгааФкк` (целые числа).
- Читает сам массив байтов (`Buff`) и сохраняет в динамический массив `BuffArr` (массив массивов байтов).

```pascal
uses
  Classes, SysUtils;

type
  // Тип для динамического массива целых чисел
  TIntArray = array of Integer;
  // Тип для массива массивов байтов
  TByteArrayArray = array of array of Byte;

procedure ReadFromStream(ms: TMemoryStream; out ДутИгааФкк: TIntArray; out BuffArr: TByteArrayArray);
var
  i, LenBuff: Integer;
  Buff: array of Byte;
begin
  ms.Position := 0; // Убедимся, что начинаем с начала потока

  // Предполагаем, что в потоке 10 записей (по количеству элементов в исходном массиве)
  SetLength(ДутИгааФкк, 10);
  SetLength(BuffArr, 10);

  for i := 0 to 9 do
  begin
    // Читаем длину массива байтов (4 байта)
    ms.ReadBuffer(LenBuff, SizeOf(Integer));
    ДутИгааФкк[i] := LenBuff;

    // Создаём буфер нужной длины
    SetLength(Buff, LenBuff);

    // Читаем сам массив байтов
    if LenBuff > 0 then
      ms.ReadBuffer(Buff[0], LenBuff);

    // Сохраняем прочитанный массив байтов
    BuffArr[i] := Buff;
  end;
end;

// Пример использования
var
  ms: TMemoryStream;
  ДутИгааФкк: TIntArray;
  BuffArr: TByteArrayArray;
  i, j: Integer;
begin
  // Предположим, что ms уже заполнен с помощью FillStreamWithByteArrays
  ms := TMemoryStream.Create;
  try
    FillStreamWithByteArrays(ms); // используем процедуру из предыдущего ответа

    ReadFromStream(ms, ДутИгааФкк, BuffArr);

    // Проверка: выводим длины и первые несколько байтов каждого массива
    for i := 0 to High(ДутИгааФкк) do
    begin
      WriteLn('Запись ', i, ': длина = ', ДутИгааФкк[i]);
      Write('Первые 5 байтов: ');
      for j := 0 to Min(4, High(BuffArr[i])) do
        Write(BuffArr[i][j], ' ');
      WriteLn;
    end;
  finally
    ms.Free;
  end;
end.
```

### Объяснение:
- `ms.Position := 0` — устанавливает указатель в начало потока.
- `ReadBuffer` — читает фиксированное количество байтов из потока.
- `SetLength` — выделяет память под динамические массивы.
- `ДутИгааФкк` — массив длин (целые числа).
- `BuffArr` — массив массивов байтов, соответствующих этим длинам.

> ⚠️ Обратите внимание: чтобы знать, сколько всего записей в потоке, в реальных приложениях может потребоваться записывать общее количество перед основными данными. В данном случае мы полагаемся на то, что их 10.

### USER
Создай пример чтения из потока ms: TmemoryStream, созданного в процедуре FillStreamWithByteArrays(ms: TMemoryStream), длины массива байтов (в переменную LenBuff, которая будет записываться в динамический массив целых чисел LenBuffArr) и самого массива байтов (Buff), заданного в переменной LenBuff (Buff также будет записываться, но в динамический массив BuffArr массива байтов)
Напиши код процедуры для fpc/lazaruss

### ASSISTANT
Вот правильный код процедуры на **Free Pascal (FPC) / Lazarus**, которая читает из потока `TMemoryStream`, созданного в процедуре `FillStreamWithByteArrays`, следующие данные:

- Длину массива байтов → записывает в динамический массив `LenBuffArr` (массив целых чисел).
- Сам массив байтов → записывает в динамический массив `BuffArr` (массив массивов байтов).

```pascal
uses
  Classes, SysUtils;

type
  TIntArray = array of Integer;
  TByteArrayArray = array of array of Byte;

procedure ReadFromStream(ms: TMemoryStream; out LenBuffArr: TIntArray; out BuffArr: TByteArrayArray);
var
  i, LenBuff: Integer;
  Buff: array of Byte;
begin
  ms.Position := 0; // Устанавливаем позицию в начало потока

  // Предполагаем, что в потоке 10 записей, как в FillStreamWithByteArrays
  SetLength(LenBuffArr, 10);
  SetLength(BuffArr, 10);

  for i := 0 to 9 do
  begin
    // Читаем длину массива байтов (4 байта)
    ms.ReadBuffer(LenBuff, SizeOf(Integer));
    LenBuffArr[i] := LenBuff;

    // Создаём буфер нужной длины
    SetLength(Buff, LenBuff);

    // Читаем сам массив байтов
    if LenBuff > 0 then
      ms.ReadBuffer(Buff[0], LenBuff);

    // Сохраняем прочитанный массив байтов
    BuffArr[i] := Buff;
  end;
end;

// Пример использования
var
  ms: TMemoryStream;
  LenBuffArr: TIntArray;
  BuffArr: TByteArrayArray;
  i, j: Integer;
begin
  // Предположим, что ms уже заполнен с помощью FillStreamWithByteArrays
  ms := TMemoryStream.Create;
  try
    FillStreamWithByteArrays(ms); // используем процедуру из предыдущего ответа

    ReadFromStream(ms, LenBuffArr, BuffArr);

    // Проверка: выводим длины и первые несколько байтов каждого массива
    for i := 0 to High(LenBuffArr) do
    begin
      WriteLn('Запись ', i, ': длина = ', LenBuffArr[i]);
      Write('Первые 3 байта: ');
      for j := 0 to Min(2, High(BuffArr[i])) do
        Write(BuffArr[i][j], ' ');
      WriteLn;
    end;
  finally
    ms.Free;
  end;
end.
```

### Пояснение:
- `LenBuffArr`: динамический массив целых чисел, содержащий длины байтовых массивов.
- `BuffArr`: динамический массив массивов байтов, каждый из которых соответствует длине из `LenBuffArr`.
- `ms.ReadBuffer`: используется для чтения из потока.
- `ms.Position := 0`: устанавливает указатель в начало потока для корректного чтения.

### USER
Модифицируй ReadFromStream с условием, что мы не знаем заранее, сколько в потоке записей, но точно знаем, что  первые SizeOf(Integer) байтов - это длина первого первого массива байтов

### ASSISTANT
Вот модифицированная версия процедуры `ReadFromStream`, которая **не знает заранее количество записей**, а читает их до тех пор, пока не достигнет конца потока (`ms.Size`), при этом **первые 4 байта** всегда содержат длину первого массива байтов:

```pascal
uses
  Classes, SysUtils;

type
  TIntArray = array of Integer;
  TByteArrayArray = array of array of Byte;

procedure ReadFromStream(ms: TMemoryStream; out LenBuffArr: TIntArray; out BuffArr: TByteArrayArray);
var
  LenBuff: Integer;
  Buff: array of Byte;
  Count: Integer;
begin
  ms.Position := 0; // Устанавливаем позицию в начало потока

  Count := 0;
  SetLength(LenBuffArr, 0);
  SetLength(BuffArr, 0);

  while ms.Position < ms.Size do
  begin
    // Читаем длину следующего массива байтов (4 байта)
    if ms.Read(LenBuff, SizeOf(Integer)) <> SizeOf(Integer) then
      Break; // Если не удалось прочитать 4 байта — выходим

    // Увеличиваем размер массивов на 1
    SetLength(LenBuffArr, Count + 1);
    SetLength(BuffArr, Count + 1);

    // Сохраняем длину
    LenBuffArr[Count] := LenBuff;

    // Создаём буфер нужной длины
    SetLength(Buff, LenBuff);

    // Читаем сам массив байтов
    if LenBuff > 0 then
    begin
      if ms.Read(Buff[0], LenBuff) <> LenBuff then
        Break; // Если не удалось прочитать нужное количество байтов — выходим
    end;

    // Сохраняем прочитанный массив
    BuffArr[Count] := Buff;

    Inc(Count);
  end;
end;

// Пример использования
var
  ms: TMemoryStream;
  LenBuffArr: TIntArray;
  BuffArr: TByteArrayArray;
  i, j: Integer;
begin
  ms := TMemoryStream.Create;
  try
    FillStreamWithByteArrays(ms); // используем процедуру из предыдущего примера

    ReadFromStream(ms, LenBuffArr, BuffArr);

    WriteLn('Прочитано записей: ', Length(LenBuffArr));
    for i := 0 to High(LenBuffArr) do
    begin
      WriteLn('Запись ', i, ': длина = ', LenBuffArr[i]);
      Write('Первые 3 байта: ');
      for j := 0 to Min(2, High(BuffArr[i])) do
        Write(BuffArr[i][j], ' ');
      WriteLn;
    end;
  finally
    ms.Free;
  end;
end.
```

### Изменения:
- Теперь процедура **не зависит от заранее известного количества записей**.
- Цикл `while ms.Position < ms.Size` продолжается, пока в потоке есть данные.
- Используется `ms.Read`, чтобы проверить, удалось ли прочитать нужное количество байтов (вдруг поток оборвался).