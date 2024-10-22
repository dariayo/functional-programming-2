module OpenAddressHashMap

type OpenAddressHashMap<'Key, 'Value when 'Key: comparison>(capacity: int) =
    let mutable table =
        if capacity > 0 then
            Array.create capacity (None: ('Key * 'Value) option)
        else
            [||]

    let mutable size = 0

    let hash key =
        abs (hash key) % (if capacity > 0 then capacity else 1)

    let rec findSlot key index probeCount =
        if probeCount >= capacity then
            None
        else
            match table.[index] with
            | Some (k, _) when k = key -> Some index
            | None -> Some index
            | _ -> findSlot key ((index + 1) % capacity) (probeCount + 1)

    let findSlotForInsert key = findSlot key (hash key) 0
    
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

    member this.GetValue(key) =
        match findSlot key (hash key) 0 with
        | Some index ->
            match table.[index] with
            | Some (_, v) -> Some v
            | None -> None
        | None -> None

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

    static member Empty() = OpenAddressHashMap<'Key, 'Value>(0)

    member this.Capacity = capacity
    member this.Size = size 
    member this.Table = table
