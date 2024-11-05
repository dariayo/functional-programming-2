module OpenAddressHashMap

type OpenAddressHashMap<'Key, 'Value when 'Key: comparison> =
    { Capacity: int
      Table: ('Key * 'Value) option array
      Size: int }

let hash (capacity: int) (key: 'Key) : int =
    (hash key) % capacity
    |> (fun x -> if x < 0 then x + capacity else x)

let rec findSlot
    (key: 'Key)
    (capacity: int)
    (table: ('Key * 'Value) option array)
    (index: int)
    (probeCount: int)
    : int option =
    if probeCount >= capacity then
        None
    else
        match table.[index] with
        | Some (k, _) when k = key -> Some index
        | None -> Some index
        | _ -> findSlot key capacity table ((index + 1) % capacity) (probeCount + 1)

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

let remove (key: 'Key) (map: OpenAddressHashMap<'Key, 'Value>) : OpenAddressHashMap<'Key, 'Value> =
    match findSlot key map.Capacity map.Table (hash map.Capacity key) 0 with
    | Some index ->
        let newTable = Array.copy map.Table
        newTable.[index] <- None

        { map with
            Table = newTable
            Size = map.Size - 1 }
    | None -> map

let getValue (key: 'Key) (map: OpenAddressHashMap<'Key, 'Value>) : 'Value option =
    match findSlot key map.Capacity map.Table (hash map.Capacity key) 0 with
    | Some index ->
        match map.Table.[index] with
        | Some (_, v) -> Some v
        | None -> None
    | None -> None

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

let createEmpty (capacity: int) : OpenAddressHashMap<'Key, 'Value> =
    { Capacity = capacity
      Table = Array.create capacity None
      Size = 0 }
