module OpenAddressHashMapPropertyTests

open NUnit.Framework
open FsCheck
open OpenAddressHashMap
open System.IO

[<TestFixture>]
type OpenAddressHashMapPropertyTests() =
    let rec fillDict (dict: OpenAddressHashMap<int, int>) (values: int list) =
        match values with
        | [] -> dict
        | head :: tail -> fillDict (dict.Add(head, head)) tail

    let equalDicts (dict1: OpenAddressHashMap<int, int>) (dict2: OpenAddressHashMap<int, int>) =
        let allKeys1 = [for i in 0 .. dict1.Capacity - 1 do yield dict1.GetValue(i)]
        let allKeys2 = [for i in 0 .. dict2.Capacity - 1 do yield dict2.GetValue(i)]
        
        allKeys1 = allKeys2

    [<Test>]
    member this.``Monoid - Merge with empty returns original dict``() =
        let prop (numbers: int list) =
            let dict = OpenAddressHashMap<int, int>(List.length numbers * 2)
            let filledDict = fillDict dict numbers
            let emptyDict = OpenAddressHashMap<int, int>.Empty()
            
            let merged1 = filledDict.Merge(emptyDict)  
            let merged2 = emptyDict.Merge(filledDict)  

            (filledDict.Size = merged1.Size) &&
            (filledDict.Capacity = merged1.Capacity) &&
            (equalDicts filledDict merged1) &&
            (filledDict.Size = merged2.Size) &&
            (filledDict.Capacity = merged2.Capacity) &&
            (equalDicts filledDict merged2)

        Check.QuickThrowOnFailure prop

    [<Test>]
    member this.``Check insert``() =
        let prop (values: int list) =
            let dict = OpenAddressHashMap<int, int>(List.length values * 2) 
            let mutable currentDict = dict

            List.forall (fun value ->
                currentDict <- currentDict.Add(value, value)
                currentDict.GetValue(value) = Some value
            ) values

        Check.QuickThrowOnFailure prop

    [<Test>]
    member this.``Check insert string``() =
        let prop (pairs: (int * string) list) =
            let dict = OpenAddressHashMap<int, string>(List.length pairs * 2) 
            let mutable currentDict = dict

            List.forall (fun (key, value) ->
                currentDict <- currentDict.Add(key, value)
                currentDict.GetValue(key) = Some value
            ) pairs

        Check.QuickThrowOnFailure prop

    [<Test>]
    member this.``Merge is associative``() =
        let prop (values1: int list, values2: int list) =
            let dict1 = OpenAddressHashMap<int, int>((List.length values1 + List.length values2) * 2) 
            let dict2 = OpenAddressHashMap<int, int>((List.length values1 + List.length values2) * 2)

            let filledDict1 = fillDict dict1 values1
            let filledDict2 = fillDict dict2 values2

            let merged1 = filledDict1.Merge(filledDict2)
            let merged2 = filledDict2.Merge(filledDict1)

            equalDicts merged1 merged2

        Check.QuickThrowOnFailure prop

    [<OneTimeSetUp>]
    member this.SetUp() =
        let sw = new StringWriter()
        System.Console.SetOut(sw)
