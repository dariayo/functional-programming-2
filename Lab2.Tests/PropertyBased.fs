module OpenAddressHashMapPropertyTests

open NUnit.Framework
open FsCheck
open OpenAddressHashMap
open System.IO

[<TestFixture>]
type OpenAddressHashMapPropertyTests() =

    let equalDicts (dict1: OpenAddressHashMap<int, int>) (dict2: OpenAddressHashMap<int, int>) =
        if dict1.Size <> dict2.Size then
            false
        else
            let rec compareSlots index =
                if index >= dict1.Capacity then
                    true 
                else
                    match dict1.Table.[index] with
                    | Some (k, v) ->
                        match getValue k dict2 with
                        | Some v2 when v = v2 -> compareSlots (index + 1) 
                        | _ -> false 
                    | None -> compareSlots (index + 1) 

            compareSlots 0


    let genDict =
        Gen.sized (fun size ->
            gen {
                let! keyValuePairs = Gen.listOfLength size (Arb.generate<int * int>)
                let dict = createEmpty (size * 2)

                let filledDict =
                    List.fold (fun (acc: OpenAddressHashMap<int, int>) (k, v) -> add k v acc) dict keyValuePairs

                return filledDict
            })

    let dictArb = Arb.fromGen genDict

    [<Test>]
    member this.``Monoid - Merge with empty returns original dict``() =
        let prop (dict: OpenAddressHashMap<int, int>) =
            let emptyDict = createEmpty 0
            let mergedDict = merge dict emptyDict
            equalDicts dict mergedDict

        Prop.forAll dictArb prop
        |> Check.QuickThrowOnFailure

    [<Test>]
    member this.``Check insert``() =
        let prop (values: int list) =
            let dict = createEmpty (List.length values * 2) 
            let mutable currentDict = dict

            List.forall
                (fun value ->
                    currentDict <- add value value currentDict
                    getValue value currentDict = Some value)
                values

        Check.QuickThrowOnFailure prop

    [<Test>]
    member this.``Check insert string``() =
        let prop (pairs: (int * string) list) =
            let dict = createEmpty (List.length pairs * 2)
            let mutable currentDict = dict

            List.forall
                (fun (key, value) ->
                    currentDict <- add key value currentDict
                    getValue key currentDict = Some value)
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
            let mergedLeft = merge (merge dict1 dict2 ) dict3
            let mergedRight = merge dict1 (merge dict2 dict3) 
            equalDicts mergedLeft mergedRight

        let tripleArb = Arb.fromGen (Gen.zip3 genDict genDict genDict)

        Prop.forAll tripleArb prop
        |> Check.QuickThrowOnFailure

    [<OneTimeSetUp>]
    member this.SetUp() =
        let sw = new StringWriter()
        System.Console.SetOut(sw)
