module OpenAddressHashMap

type OpenAddressHashMap<'Key, 'Value when 'Key: comparison>
    (
        capacity: int,
        table: ('Key * 'Value) option array,
        size: int
    ) =
    let hash (key: 'Key) : int =
        (hash key) % capacity
        |> (fun x -> if x < 0 then x + capacity else x)

    let rec findSlot (key: 'Key) index probeCount =
        if probeCount >= capacity then
            None
        else
            match table.[index] with
            | Some (k, _) when k = key -> Some index
            | None -> Some index
            | _ -> findSlot key ((index + 1) % capacity) (probeCount + 1)

    let findSlotForInsert (key: 'Key) = findSlot key (hash key) 0

    let updateTable (table: ('Key * 'Value) option array) index key value =
        let newTable = Array.copy table
        newTable.[index] <- Some(key, value)
        newTable

    member this.Add(key: 'Key, value: 'Value) =
        if float size / float capacity = 1 then
            let newCapacity = capacity * 2
            let newTable = Array.create newCapacity None
            let newMap = OpenAddressHashMap<'Key, 'Value>(newCapacity, newTable, size)
            newMap.Add(key, value)
        else
            match findSlotForInsert key with
            | Some index ->
                let updatedTable = updateTable table index key value
                OpenAddressHashMap<'Key, 'Value>(capacity, updatedTable, size + 1)
            | None -> this

    member this.Remove(key: 'Key) =
        match findSlot key (hash key) 0 with
        | Some index ->
            let newTable = Array.copy table
            newTable.[index] <- None
            OpenAddressHashMap<'Key, 'Value>(capacity, newTable, size - 1)
        | None -> this

    member this.GetValue(key: 'Key) =
        match findSlot key (hash key) 0 with
        | Some index ->
            match table.[index] with
            | Some (_, v) -> Some v
            | None -> None
        | None -> None

    member this.Filter(predicate: ('Key * 'Value) -> bool) =
        let filteredItems =
            Array.fold
                (fun acc el ->
                    match el with
                    | Some (k, v) when predicate (k, v) -> (k, v) :: acc
                    | _ -> acc)
                []
                table

        let newDict =
            OpenAddressHashMap<'Key, 'Value>(capacity, Array.create capacity None, 0)

        List.fold (fun (dict: OpenAddressHashMap<'Key, 'Value>) (k, v) -> dict.Add(k, v)) newDict filteredItems

    member this.Map(mapper: ('Key * 'Value) -> ('Key * 'Value)) =
        let newTable = Array.create capacity None
        let mutable newSize = 0

        Array.iter
            (function
            | Some (k, v) ->
                let (newK, newV) = mapper (k, v)
                let index = findSlotForInsert newK

                if index.IsSome then
                    newTable.[index.Value] <- Some(newK, newV)
                    newSize <- newSize + 1
            | None -> ())
            table

        OpenAddressHashMap<'Key, 'Value>(capacity, newTable, newSize)


    member this.FoldL (folder: 'State -> ('Key * 'Value) -> 'State) (state: 'State) =
        let folderFn acc el =
            match el with
            | Some (k, v) -> folder acc (k, v)
            | None -> acc

        Array.fold folderFn state table

    member this.FoldR (folder: ('Key * 'Value) -> 'State -> 'State) (state: 'State) =
        let folderFn el acc =
            match el with
            | Some (k, v) -> folder (k, v) acc
            | None -> acc

        Array.foldBack folderFn table state

    member this.Merge(other: OpenAddressHashMap<'Key, 'Value>) =
        let newCapacity = max this.Capacity other.Capacity
        let newTable = Array.create newCapacity None
        let mutable newSize = 0

        let mergeFunc v1 v2 =
            match box v1, box v2 with
            | (:? int as i1), (:? int as i2) -> unbox (i1 + i2)
            | (:? string as s1), (:? string as s2) -> unbox (s1 + s2)
            | _ -> v1

        Array.iter
            (function
            | Some (k, v) ->
                let index = findSlotForInsert k

                if index.IsSome then
                    newTable.[index.Value] <- Some(k, v)
                    newSize <- newSize + 1
            | None -> ())
            this.Table

        Array.iter
            (function
            | Some (k, v) ->
                match this.GetValue(k) with
                | Some existingValue ->
                    let mergedValue = mergeFunc existingValue v
                    let index = findSlotForInsert k

                    if index.IsSome then
                        newTable.[index.Value] <- Some(k, mergedValue)
                | None ->
                    let index = findSlotForInsert k

                    if index.IsSome then
                        newTable.[index.Value] <- Some(k, v)
                        newSize <- newSize + 1
            | None -> ())
            other.Table

        OpenAddressHashMap<'Key, 'Value>(newCapacity, newTable, newSize)


    static member CreateEmpty capacity =
        OpenAddressHashMap<'Key, 'Value>(capacity, Array.create capacity None, 0)

    member this.Capacity = capacity
    member this.Size = size
    member this.Table = table
