### USER

имеется три константы
s1 = 'ЖЖЖ'
s2 = 123
s3 = 'ффф'

и поток в памяти 
ms: TMemoryStream

для fpc/lazarus напиши процедуру записи и чтения этих констант в поток ms с разделителем (например, нулевой байт) между ними 

### ASSISTANT

Вот процедуры для записи и чтения констант в/из TMemoryStream с разделителем:

```pascal
uses Classes;

const
  s1 = 'ЖЖЖ';
  s2 = 123;
  s3 = 'ффф';

// Процедура записи констант в поток с разделителями
procedure WriteToStream(ms: TMemoryStream);
var
  s1_utf8, s3_utf8: UTF8String;
  separator: Byte;
begin
  ms.Clear; // Очищаем поток перед записью

  // Преобразуем строки в UTF-8
  s1_utf8 := UTF8String(s1);
  s3_utf8 := UTF8String(s3);

  separator := 0;

  // Записываем первую строку
  if Length(s1_utf8) > 0 then
    ms.WriteBuffer(s1_utf8[1], Length(s1_utf8));
  // Записываем разделитель
  ms.WriteBuffer(separator, SizeOf(Byte));

  // Записываем число как строку в формате UTF-8
  var s2_str: UTF8String := UTF8String(IntToStr(s2));
  if Length(s2_str) > 0 then
    ms.WriteBuffer(s2_str[1], Length(s2_str));
  // Записываем разделитель
  ms.WriteBuffer(separator, SizeOf(Byte));

  // Записываем третью строку
  if Length(s3_utf8) > 0 then
    ms.WriteBuffer(s3_utf8[1], Length(s3_utf8));
  // Записываем завершающий разделитель (по желанию)
  ms.WriteBuffer(separator, SizeOf(Byte));
end;

// Процедура чтения констант из потока
procedure ReadFromStream(ms: TMemoryStream; out rs1, rs3: string; out rs2: Integer);
var
  buffer: array of Byte;
  data: UTF8String;
  parts: TStringList;
  i: Integer;
begin
  // Читаем все содержимое потока
  SetLength(buffer, ms.Size);
  ms.Position := 0;
  ms.ReadBuffer(buffer[0], ms.Size);

  // Преобразуем в UTF-8 строку
  if ms.Size > 0 then
    data := UTF8String(Copy(PChar(@buffer[0]), 1, ms.Size))
  else
    data := '';

  // Разбиваем по нулевым байтам
  parts := TStringList.Create;
  try
    i := 1;
    while i <= Length(data) do
    begin
      var start_pos := i;
      while (i <= Length(data)) and (data[i] <> #0) do
        Inc(i);

      parts.Add(Copy(data, start_pos, i - start_pos));

      if i <= Length(data) then
        Inc(i); // Пропускаем нулевой байт
    end;

    // Извлекаем значения
    if parts.Count >= 3 then
    begin
      rs1 := String(parts[0]);
      rs2 := StrToIntDef(string(parts[1]), 0);
      rs3 := String(parts[2]);
    end
    else
    begin
      rs1 := '';
      rs2 := 0;
      rs3 := '';
    end;
  finally
    parts.Free;
  end;
end;

// Пример использования:
{
var
  ms: TMemoryStream;
  read_s1, read_s3: string;
  read_s2: Integer;
begin
  ms := TMemoryStream.Create;
  try
    WriteToStream(ms);

    ms.Position := 0;
    ReadFromStream(ms, read_s1, read_s3, read_s2);

    // Проверка: read_s1 = 'ЖЖЖ', read_s2 = 123, read_s3 = 'ффф'
  finally
    ms.Free;
  end;
end;
}
```

Этот код корректно обрабатывает юникодные строки и использует нулевые байты в качестве разделителей. При записи числа оно преобразуется в строку, а при чтении обратно парсится в целое число.

### USER

модифицируй код процедур так, чтобы константы записывались последовательно без преобразования числа в строку

### ASSISTANT

```pascal
uses Classes;

const
  s1 = 'ЖЖЖ';
  s2 = 123;
  s3 = 'ффф';

// Процедура записи констант в поток с разделителями
procedure WriteToStream(ms: TMemoryStream);
var
  s1_utf8, s3_utf8: UTF8String;
  separator: Byte;
begin
  ms.Clear; // Очищаем поток перед записью

  // Преобразуем строки в UTF-8
  s1_utf8 := UTF8String(s1);
  s3_utf8 := UTF8String(s3);

  separator := 0;

  // Записываем первую строку
  if Length(s1_utf8) > 0 then
    ms.WriteBuffer(s1_utf8[1], Length(s1_utf8));
  // Записываем разделитель
  ms.WriteBuffer(separator, SizeOf(Byte));

  // Записываем число как число (4 байта)
  ms.WriteBuffer(s2, SizeOf(Integer));
  // Записываем разделитель
  ms.WriteBuffer(separator, SizeOf(Byte));

  // Записываем третью строку
  if Length(s3_utf8) > 0 then
    ms.WriteBuffer(s3_utf8[1], Length(s3_utf8));
  // Записываем завершающий разделитель (по желанию)
  ms.WriteBuffer(separator, SizeOf(Byte));
end;

// Процедура чтения констант из потока
procedure ReadFromStream(ms: TMemoryStream; out rs1, rs3: string; out rs2: Integer);
var
  buffer: array of Byte;
  pos: Integer;
  temp_str: UTF8String;
  separator_found: Boolean;
begin
  SetLength(buffer, ms.Size);
  ms.Position := 0;
  ms.ReadBuffer(buffer[0], ms.Size);

  pos := 0;

  // Читаем первую строку до разделителя
  temp_str := '';
  while (pos < ms.Size) and (buffer[pos] <> 0) do
  begin
    temp_str := temp_str + AnsiChar(buffer[pos]);
    Inc(pos);
  end;
  rs1 := String(temp_str);
  Inc(pos); // пропускаем разделитель

  // Читаем число (4 байта)
  if (pos + SizeOf(Integer) - 1) < ms.Size then
  begin
    rs2 := PInteger(@buffer[pos])^;
    Inc(pos, SizeOf(Integer));
  end
  else
    rs2 := 0;

  Inc(pos); // пропускаем разделитель

  // Читаем третью строку до разделителя
  temp_str := '';
  while (pos < ms.Size) and (buffer[pos] <> 0) do
  begin
    temp_str := temp_str + AnsiChar(buffer[pos]);
    Inc(pos);
  end;
  rs3 := String(temp_str);
end;
```

### USER

модифицируй предыдущий пример так, чтобы сначала в поток ms записывалась длина константы + сепаратор, а из потока данные читались с учетом записанной длины

### ASSISTANT

```pascal
uses Classes;

const
  s1 = 'ЖЖЖ';
  s2 = 123;
  s3 = 'ффф';

// Процедура записи констант в поток с указанием длин
procedure WriteToStream(ms: TMemoryStream);
var
  s1_utf8, s3_utf8: UTF8String;
  str_len: Integer;
  separator: Byte;
begin
  ms.Clear; // Очищаем поток перед записью

  // Преобразуем строки в UTF-8
  s1_utf8 := UTF8String(s1);
  s3_utf8 := UTF8String(s3);

  separator := 0;

  // Записываем длину первой строки
  str_len := Length(s1_utf8);
  ms.WriteBuffer(str_len, SizeOf(Integer));
  // Записываем саму строку
  if str_len > 0 then
    ms.WriteBuffer(s1_utf8[1], str_len);
  // Записываем разделитель
  ms.WriteBuffer(separator, SizeOf(Byte));

  // Записываем длину числа (всегда 4 байта для Integer)
  str_len := SizeOf(Integer);
  ms.WriteBuffer(str_len, SizeOf(Integer));
  // Записываем само число
  ms.WriteBuffer(s2, SizeOf(Integer));
  // Записываем разделитель
  ms.WriteBuffer(separator, SizeOf(Byte));

  // Записываем длину третьей строки
  str_len := Length(s3_utf8);
  ms.WriteBuffer(str_len, SizeOf(Integer));
  // Записываем саму строку
  if str_len > 0 then
    ms.WriteBuffer(s3_utf8[1], str_len);
  // Записываем завершающий разделитель (по желанию)
  ms.WriteBuffer(separator, SizeOf(Byte));
end;

// Процедура чтения констант из потока
procedure ReadFromStream(ms: TMemoryStream; out rs1, rs3: string; out rs2: Integer);
var
  str_len: Integer;
  buffer: array of Byte;
  pos: Integer;
  temp_str: UTF8String;
  separator: Byte;
begin
  SetLength(buffer, ms.Size);
  ms.Position := 0;
  ms.ReadBuffer(buffer[0], ms.Size);

  pos := 0;

  // Читаем длину первой строки
  if (pos + SizeOf(Integer) - 1) < ms.Size then
  begin
    str_len := PInteger(@buffer[pos])^;
    Inc(pos, SizeOf(Integer));

    // Читаем строку указанной длины
    if str_len > 0 then
    begin
      temp_str := '';
      var end_pos := pos + str_len - 1;
      if end_pos < ms.Size then
      begin
        for var i := pos to end_pos do
          temp_str := temp_str + AnsiChar(buffer[i]);
        pos := end_pos + 1;
      end;
    end
    else
      temp_str := '';
  end
  else
    temp_str := '';

  rs1 := String(temp_str);

  // Пропускаем разделитель
  if (pos < ms.Size) and (buffer[pos] = 0) then
    Inc(pos);

  // Читаем длину числа
  if (pos + SizeOf(Integer) - 1) < ms.Size then
  begin
    str_len := PInteger(@buffer[pos])^;
    Inc(pos, SizeOf(Integer));

    // Читаем само число
    if (str_len = SizeOf(Integer)) and ((pos + SizeOf(Integer) - 1) < ms.Size) then
    begin
      rs2 := PInteger(@buffer[pos])^;
      Inc(pos, SizeOf(Integer));
    end
    else
      rs2 := 0;
  end
  else
    rs2 := 0;

  // Пропускаем разделитель
  if (pos < ms.Size) and (buffer[pos] = 0) then
    Inc(pos);

  // Читаем длину третьей строки
  if (pos + SizeOf(Integer) - 1) < ms.Size then
  begin
    str_len := PInteger(@buffer[pos])^;
    Inc(pos, SizeOf(Integer));

    // Читаем строку указанной длины
    if str_len > 0 then
    begin
      temp_str := '';
      var end_pos := pos + str_len - 1;
      if end_pos < ms.Size then
      begin
        for var i := pos to end_pos do
          temp_str := temp_str + AnsiChar(buffer[i]);
        pos := end_pos + 1;
      end;
    end
    else
      temp_str := '';
  end
  else
    temp_str := '';

  rs3 := String(temp_str);
end;
```