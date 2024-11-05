# Лабораторная работа №2 (OpenAddress HashMap Dict)

`Шевченко Дарья Павловна  369053`

---

## Требования

1. Функции:
    - добавление и удаление элементов;
    - фильтрация;
    - отображение (map);
    - свертки (левая и правая);
    - структура должна быть [моноидом](https://ru.m.wikipedia.org/wiki/Моноид).
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.

---

## Ключевые элементы реализации

### Добавление элемента

```fsharp
let rec add (key: 'Key) (value: 'Value) (map: OpenAddressHashMap<'Key, 'Value>) : OpenAddressHashMap<'Key, 'Value> =
    if float map.Size / float map.Capacity = 1 then
        let newCapacity = map.Capacity * 2
        let newTable = Array.create newCapacity None

        let newMap =
            { Capacity = newCapacity
              Table = newTable
              Size = map.Size }

        add key value newMap
    else
        match findSlot key map.Capacity map.Table (hash map.Capacity key) 0 with
        | Some index ->
            let newTable = Array.copy map.Table
            newTable.[index] <- Some(key, value)

            { map with
                Table = newTable
                Size = map.Size + 1 }
        | None -> map
```

### Удаление элемента

```fsharp
let remove (key: 'Key) (map: OpenAddressHashMap<'Key, 'Value>) : OpenAddressHashMap<'Key, 'Value> =
    match findSlot key map.Capacity map.Table (hash map.Capacity key) 0 with
    | Some index ->
        let newTable = Array.copy map.Table
        newTable.[index] <- None

        { map with
            Table = newTable
            Size = map.Size - 1 }
    | None -> map
```

### Получение значения по ключу

```fsharp
let getValue (key: 'Key) (map: OpenAddressHashMap<'Key, 'Value>) : 'Value option =
    match findSlot key map.Capacity map.Table (hash map.Capacity key) 0 with
    | Some index ->
        match map.Table.[index] with
        | Some (_, v) -> Some v
        | None -> None
    | None -> None
```

### Фильтрация

```fsharp
let filter
    (predicate: ('Key * 'Value) -> bool)
    (map: OpenAddressHashMap<'Key, 'Value>)
    : OpenAddressHashMap<'Key, 'Value> =
    let filteredItems =
        Array.fold
            (fun acc el ->
                match el with
                | Some (k, v) when predicate (k, v) -> (k, v) :: acc
                | _ -> acc)
            []
            map.Table

    let newDict =
        { Capacity = map.Capacity
          Table = Array.create map.Capacity None
          Size = 0 }

    List.fold (fun acc (k, v) -> add k v acc) newDict filteredItems

```

### Отображение

```fsharp
let map
    (mapper: ('Key * 'Value) -> ('Key * 'Value))
    (map: OpenAddressHashMap<'Key, 'Value>)
    : OpenAddressHashMap<'Key, 'Value> =
    let newTable = Array.create map.Capacity None

    let updatedTable =
        Array.fold
            (fun (table: ('Key * 'Value) option array) el ->
                match el with
                | Some (k, v) ->
                    let (newK, newV) = mapper (k, v)

                    match findSlot newK map.Capacity table (hash map.Capacity newK) 0 with
                    | Some index ->
                        table.[index] <- Some(newK, newV)
                        table
                    | None -> table
                | None -> table)
            newTable
            map.Table

    { map with Table = updatedTable }
```

### Свертки (левая и правая)

```fsharp
let foldL
    (folder: 'State -> ('Key * 'Value) -> 'State)
    (state: 'State)
    (map: OpenAddressHashMap<'Key, 'Value>)
    : 'State =
    Array.fold
        (fun acc el ->
            match el with
            | Some (k, v) -> folder acc (k, v)
            | None -> acc)
        state
        map.Table
```

```fsharp
let foldR
    (folder: ('Key * 'Value) -> 'State -> 'State)
    (state: 'State)
    (map: OpenAddressHashMap<'Key, 'Value>)
    : 'State =
    Array.foldBack
        (fun el acc ->
            match el with
            | Some (k, v) -> folder (k, v) acc
            | None -> acc)
        map.Table
        state
```

### Структура должна быть моноидом

Нейтральный элемент

```fsharp
let createEmpty (capacity: int) : OpenAddressHashMap<'Key, 'Value> =
    { Capacity = capacity
      Table = Array.create capacity None
      Size = 0 }


```

Бинарная операция (слияние)

Операция слияния (merge) реализована в методе Merge, который принимает другую хэш-таблицу и возвращает новую хэш-таблицу, содержащую все пары ключ-значение из обеих таблиц. Если ключи одинаковые, то значения складываются.

```fsharp
let merge
    (dict1: OpenAddressHashMap<'Key, 'Value>)
    (dict2: OpenAddressHashMap<'Key, 'Value>)
    : OpenAddressHashMap<'Key, 'Value> =
    let newCapacity = max dict1.Capacity dict2.Capacity
    let newTable = Array.create newCapacity None

    let addAll (table: ('Key * 'Value) option array) (map: OpenAddressHashMap<'Key, 'Value>) =
        Array.fold
            (fun updatedTable el ->
                match el with
                | Some (k, v) ->
                    match findSlot k newCapacity updatedTable (hash newCapacity k) 0 with
                    | Some index ->
                        updatedTable.[index] <- Some(k, v)
                        updatedTable
                    | None -> updatedTable
                | None -> updatedTable)
            table
            map.Table

    let mergedTable = addAll (addAll newTable dict1) dict2

    { Capacity = newCapacity
      Table = mergedTable
      Size = dict1.Size + dict2.Size }

```

## Тестирование

NUnit - Unit тесты

FsCheck - property-based тесты

### Результаты тестов

| Тестовый случай                                                            | Результат | Время выполнения |
|---------------------------------------------------------------------------|-----------|------------------|
| Add and GetValue should return correct values                              | Passed    | 9 ms             |
| Filter should return dictionary with only matching elements                | Passed    | 1 ms             |
| FoldL should correctly accumulate state                                     | Passed    | < 1 ms           |
| FoldR should correctly accumulate state                                     | Passed    | < 1 ms           |
| Map should transform all elements                                           | Passed    | < 1 ms           |
| Merge should combine two dictionaries                                        | Passed    | < 1 ms           |
| Merge with empty dictionary should return original dictionary               | Passed    | < 1 ms           |
| Remove should delete element and GetValue should return None               | Passed    | < 1 ms           |
| Check insert                                                               | Passed    | 55 ms            |
| Check insert string                                                        | Passed    | 27 ms            |
| Merge is associative                                                      | Passed    | 17 ms            |
| Monoid - Merge with empty returns original dict                          | Passed    | 5 ms             |

### Итоговый отчет

**Общее количество тестов:** 12  
**Успешно пройдено:** 12  
**Общее время выполнения:** 0.5023 секунд  

## Выводы

Я узнала, что такое открытая адресация, реализовала один из вариантов открытой адресации - Linear Probing. Было достаточно легко понять, как это сделать. Столкнулась с проблемой, когда переменные могут быть разных типов, поэтому узнала, что такое box и unbox, чтобы обрабатывать разные типы.  
