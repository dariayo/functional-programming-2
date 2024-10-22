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
member this.Add(key, value) =
        match findSlotForInsert key with
        | Some index ->
            match table.[index] with
            | Some _ ->
                table.[index] <- Some(key, value)
            | None ->
                table.[index] <- Some(key, value)
                size <- size + 1

            this
        | None -> failwith "Dictionary is full"
```

### Удаление элемента

```fsharp
member this.Remove(key) =
        match findSlot key (hash key) 0 with
        | Some index ->
            match table.[index] with
            | Some _ ->
                table.[index] <- None
                size <- size - 1
                this
            | None -> this
        | None -> this
```

### Получение значения по ключу

```fsharp
member this.GetValue(key) =
        match findSlot key (hash key) 0 with
        | Some index ->
            match table.[index] with
            | Some (_, v) -> Some v
            | None -> None
        | None -> None
```

### Фильтрация

```fsharp
member this.Filter(predicate) =
    let filteredItems =
        Array.fold
            (fun acc el ->
                match el with
                | Some (k, v) when predicate (k, v) -> (k, v) :: acc
                | _ -> acc)
            []
            table

    let newDict = OpenAddressHashMap<'Key, 'Value>(capacity)
    List.fold (fun (dict: OpenAddressHashMap<'Key, 'Value>) (k, v) -> dict.Add(k, v)) newDict filteredItems

```

### Отображение

```fsharp
member this.Map(mapper) =
        let newTable =
            Array.map
                (fun el ->
                    match el with
                    | Some (k, v) -> Some(mapper (k, v))
                    | None -> None)
                table

        let newDict = OpenAddressHashMap<'Key, 'Value>(capacity)

        Array.iter
            (function
            | Some (k, v) -> newDict.Add(k, v) |> ignore
            | None -> ())
            newTable

        newDict
```

### Свертки (левая и правая)

```fsharp
member this.FoldL (folder: 'State -> ('Key * 'Value) -> 'State) (state: 'State) =
        let folderFn acc el =
            match el with
            | Some (k, v) -> folder acc (k, v)
            | None -> acc

        Array.fold folderFn state table
```

```fsharp
member this.FoldR (folder: ('Key * 'Value) -> 'State -> 'State) (state: 'State) =
        let folderFn el acc =
            match el with
            | Some (k, v) -> folder (k, v) acc
            | None -> acc

        Array.foldBack folderFn table state
```

### Структура должна быть моноидом

Нейтральный элемент

```fsharp
static member Empty() = OpenAddressHashMap<'Key, 'Value>(0)
```

Бинарная операция (слияние)

Операция слияния (merge) реализована в методе Merge, который принимает другую хэш-таблицу и возвращает новую хэш-таблицу, содержащую все пары ключ-значение из обеих таблиц. Если ключи одинаковые, то значения складываются.

```fsharp
member this.Merge(other: OpenAddressHashMap<'Key, 'Value>) =
    let resultDict = OpenAddressHashMap<'Key, 'Value>(max capacity other.Capacity)
    let mergeFunc v1 v2 =
        match box v1, box v2 with
        | (:? string as s1), (:? string as s2) -> unbox (s1 + s2)  
        | (:? int as i1), (:? int as i2) -> unbox (i1 + i2)        
        | _ -> v1 
    Array.iter
        (function
        | Some (k, v) -> resultDict.Add(k, v) |> ignore
        | None -> ())
        table

    Array.iter
        (function
        | Some (k, v) ->
            match resultDict.GetValue(k) with
            | Some existingValue ->
                let newValue = mergeFunc existingValue v
                resultDict.Add(k, newValue) |> ignore
            | None ->
                resultDict.Add(k, v) |> ignore
        | None -> ())
        other.Table
    resultDict
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
