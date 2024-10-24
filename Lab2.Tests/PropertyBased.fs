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
        let allKeys1 =
            [ for i in 0 .. dict1.Capacity - 1 do
                  match dict1.Table.[i] with
                  | Some (k, _) -> yield k
                  | None -> () ]

        let allKeys2 =
            [ for i in 0 .. dict2.Capacity - 1 do
                  match dict2.Table.[i] with
                  | Some (k, _) -> yield k
                  | None -> () ]

        if Set.ofList allKeys1 <> Set.ofList allKeys2 then
            false
        else
            List.forall (fun key -> dict1.GetValue(key) = dict2.GetValue(key)) allKeys1


    let genDict =
        gen {
            let! keyValuePairs = Arb.generate<list<int * int>>

            let dict = OpenAddressHashMap<int, int>.CreateEmpty (List.length keyValuePairs * 2)

            let filledDict =
                List.fold (fun (acc: OpenAddressHashMap<int, int>) (k, v) -> acc.Add(k, v)) dict keyValuePairs

            return filledDict
        }

    let dictArb = Arb.fromGen genDict

    [<Test>]
    member this.``Monoid - Merge with empty returns original dict``() =
        let prop (dict: OpenAddressHashMap<int, int>) =
            let emptyDict = OpenAddressHashMap<int, int>.CreateEmpty (0)
            let mergedDict = dict.Merge(emptyDict)
            equalDicts dict mergedDict

        Prop.forAll dictArb prop
        |> Check.QuickThrowOnFailure

    [<Test>]
    member this.``Check insert``() =
        let prop (values: int list) =
            let dict = OpenAddressHashMap<int, int>.CreateEmpty (List.length values * 2)
            let mutable currentDict = dict

            List.forall
                (fun value ->
                    currentDict <- currentDict.Add(value, value)
                    currentDict.GetValue(value) = Some value)
                values

        Check.QuickThrowOnFailure prop

    [<Test>]
    member this.``Check insert string``() =
        let prop (pairs: (int * string) list) =
            let dict = OpenAddressHashMap<int, string>.CreateEmpty (List.length pairs * 2)
            let mutable currentDict = dict

            List.forall
                (fun (key, value) ->
                    currentDict <- currentDict.Add(key, value)
                    currentDict.GetValue(key) = Some value)
                pairs

        Check.QuickThrowOnFailure prop

    [<Test>]
    member this.``Merge is associative``() =
        let prop (values1: int list, values2: int list, values3: int list) =
            let dict1 = OpenAddressHashMap<int, int>.CreateEmpty (100)
            let dict2 = OpenAddressHashMap<int, int>.CreateEmpty (100)
            let dict3 = OpenAddressHashMap<int, int>.CreateEmpty (100)

            let filledDict1 = fillDict dict1 values1
            let filledDict2 = fillDict dict2 values2
            let filledDict3 = fillDict dict3 values3

            let mergedLeft = filledDict1.Merge(filledDict2).Merge(filledDict3)
            let mergedRight = filledDict1.Merge(filledDict2.Merge(filledDict3))

            equalDicts mergedLeft mergedRight

        Check.QuickThrowOnFailure prop

    [<OneTimeSetUp>]
    member this.SetUp() =
        let sw = new StringWriter()
        System.Console.SetOut(sw)
