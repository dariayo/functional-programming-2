open System
open OpenAddressHashMap

[<EntryPoint>]
let main argv =
    let hashMap = createEmpty 5

    let hashMap = add 1 "one" hashMap
    let hashMap = add 2 "two" hashMap
    let hashMap =  add 3 "three" hashMap

    match getValue 1 hashMap with
    | Some value -> printfn "Key 1: %s" value
    | None -> printfn "Key 1 not found"

    match getValue 4 hashMap with
    | Some value -> printfn "Key 4: %s" value
    | None -> printfn "Key 4 not found"

    let hashMap = remove 2 hashMap

    match getValue 2 hashMap with
    | Some value -> printfn "Key 2: %s" value
    | None -> printfn "Key 2 not found (as expected after removal)"

    let otherDict = createEmpty 5 
    let otherDict = add 3 "three" otherDict
    let otherDict = add 4 "four" otherDict

    let mergedDict = merge hashMap otherDict


    printfn "Merged dictionary contents:"

    for k in [ 1; 2; 3; 4 ] do
        match getValue k mergedDict with
        | Some value -> printfn "Key %d" k
        | None -> ()

    let filteredDict = filter (fun (k, _) -> k % 2 = 0) otherDict

    printfn "Filtered dictionary (only even keys):"

    for k in [ 1; 2; 3; 4 ] do
        match getValue k filteredDict with
        | Some value -> printfn "Key %d: %s" k value
        | None -> ()

    let mappedDict = map (fun (k, (v: string)) -> (k, v.ToUpper())) otherDict

    printfn "Mapped dictionary (values in uppercase):"

    for k in [ 1; 2; 3; 4 ] do
        match getValue k mappedDict with
        | Some value -> printfn "Key %d: %s" k value
        | None -> ()

    let sumKeys =  foldL (fun acc (k, _) -> acc + k) 0 otherDict
    printfn "Sum of keys in merged dictionary: %d" sumKeys

    let concatValues = foldR (fun (k, v) acc -> acc + " " + v) "" mergedDict
    printfn "Concatenated values in merged dictionary: %s" (concatValues.Trim())

    0
