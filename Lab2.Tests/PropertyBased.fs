module OpenAddressHashMapPropertyTests

open NUnit.Framework
open FsCheck
open OpenAddressHashMap
open System.IO

[<TestFixture>]
type OpenAddressHashMapPropertyTests() =

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
        Gen.sized (fun size ->
            gen {
                let! keyValuePairs = Gen.listOfLength size (Arb.generate<int * int>)
                let dict = OpenAddressHashMap<int, int>.CreateEmpty (size * 2)

                let filledDict =
                    List.fold (fun (acc: OpenAddressHashMap<int, int>) (k, v) -> acc.Add(k, v)) dict keyValuePairs

                return filledDict
            })

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
        let prop
            (
                dict1: OpenAddressHashMap<int, int>,
                dict2: OpenAddressHashMap<int, int>,
                dict3: OpenAddressHashMap<int, int>
            ) =
            let mergedLeft = dict1.Merge(dict2).Merge(dict3)
            let mergedRight = dict1.Merge(dict2.Merge(dict3))
            equalDicts mergedLeft mergedRight

        let tripleArb = Arb.fromGen (Gen.zip3 genDict genDict genDict)

        Prop.forAll tripleArb prop
        |> Check.QuickThrowOnFailure

    [<OneTimeSetUp>]
    member this.SetUp() =
        let sw = new StringWriter()
        System.Console.SetOut(sw)
