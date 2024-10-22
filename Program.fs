open System
open OpenAddressHashMap

[<EntryPoint>]
let main argv =
    let hashMap = OpenAddressHashMap<int, string>(5)

    let hashMap = hashMap.Add(1, "one")
    let hashMap = hashMap.Add(2, "two")
    let hashMap = hashMap.Add(3, "three")

    match hashMap.GetValue(1) with
    | Some value -> printfn "Key 1: %s" value
    | None -> printfn "Key 1 not found"

    match hashMap.GetValue(4) with
    | Some value -> printfn "Key 4: %s" value
    | None -> printfn "Key 4 not found"

    let hashMap = hashMap.Remove(3)

    match hashMap.GetValue(2) with
    | Some value -> printfn "Key 2: %s" value
    | None -> printfn "Key 2 not found (as expected after removal)"

    let otherDict = OpenAddressHashMap<int, string>(5)
                     .Add(2, "two")
                     .Add(4, "four")

    let mergedDict = hashMap.Merge(otherDict)

    printfn "Merged dictionary contents:"
    for k in [1; 2; 3; 4] do
        match mergedDict.GetValue(k) with
        | Some value -> printfn "Key %d: %s" k value
        | None -> printfn "Key %d not found" k

    let filteredDict = mergedDict.Filter(fun (k, _) -> k % 2 = 0)

    printfn "Filtered dictionary (only even keys):"
    for k in [1; 2; 3; 4] do
        match filteredDict.GetValue(k) with
        | Some value -> printfn "Key %d: %s" k value
        | None -> printfn "Key %d not found" k

    let mappedDict = mergedDict.Map(fun (k, v) -> (k, v.ToUpper()))

    printfn "Mapped dictionary (values in uppercase):"
    for k in [1; 2; 3; 4] do
        match mappedDict.GetValue(k) with
        | Some value -> printfn "Key %d: %s" k value
        | None -> printfn "Key %d not found" k

    let sumKeys = mergedDict.FoldL(fun acc (k, _) -> acc + k) 0
    printfn "Sum of keys in merged dictionary: %d" sumKeys

    let concatValues = mergedDict.FoldR(fun (k, v) acc -> v + " " + acc) ""
    printfn "Concatenated values in merged dictionary: %s" (concatValues.Trim())

    0
