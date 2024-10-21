module Lab2.Tests

open NUnit.Framework
open OpenAddressHashMap

[<TestFixture>]
type OpenAddressHashMapTests() =

    [<Test>]
    member this.``Add and TryGetValue should return correct values``() =
        let dict = OpenAddressHashMap<int, string>(5)
        let dict = dict.Add(1, "one").Add(2, "two")

        Assert.AreEqual(Some "one", dict.TryGetValue(1))
        Assert.AreEqual(Some "two", dict.TryGetValue(2))
        Assert.AreEqual(None, dict.TryGetValue(3))

    [<Test>]
    member this.``Remove should delete element and TryGetValue should return None``() =
        let dict = OpenAddressHashMap<int, string>(5)
        let dict = dict.Add(1, "one").Add(2, "two")
        let dict = dict.Remove(1)

        Assert.AreEqual(None, dict.TryGetValue(1))
        Assert.AreEqual(Some "two", dict.TryGetValue(2))

    [<Test>]
    member this.``Merge should combine two dictionaries``() =
        let dict1 =
            OpenAddressHashMap<int, string>(5)
                .Add(1, "one")
                .Add(2, "two")

        let dict2 =
            OpenAddressHashMap<int, string>(5)
                .Add(2, "два")
                .Add(3, "three")

        let merged = dict1.Merge(dict2)

        Assert.AreEqual(Some "one", merged.TryGetValue(1))
        Assert.AreEqual(Some "два", merged.TryGetValue(2)) 
        Assert.AreEqual(Some "three", merged.TryGetValue(3))

    [<Test>]
    member this.``Merge with empty dictionary should return original dictionary``() =
        let dict1 =
            OpenAddressHashMap<int, string>(5)
                .Add(1, "one")
                .Add(2, "two")

        let emptyDict = OpenAddressHashMap<int, string>.Empty ()

        let result = dict1.Merge(emptyDict)

        Assert.AreEqual(dict1.TryGetValue(1), result.TryGetValue(1))
        Assert.AreEqual(dict1.TryGetValue(2), result.TryGetValue(2))
        Assert.AreEqual(dict1.Size, result.Size)

    [<Test>]
    member this.``Filter should return dictionary with only matching elements``() =
        let dict =
            OpenAddressHashMap<int, string>(5)
                .Add(1, "one")
                .Add(2, "two")
                .Add(3, "three")

        let filtered = dict.Filter(fun (k, _) -> k % 2 = 0) 

        Assert.AreEqual(None, filtered.TryGetValue(1))
        Assert.AreEqual(Some "two", filtered.TryGetValue(2))
        Assert.AreEqual(None, filtered.TryGetValue(3))

    [<Test>]
    member this.``Map should transform all elements``() =
        let dict =
            OpenAddressHashMap<int, string>(5)
                .Add(1, "one")
                .Add(2, "two")
                .Add(3, "three")

        let mapped = dict.Map(fun (k, v) -> (k, v.ToUpper())) 

        Assert.AreEqual(Some "ONE", mapped.TryGetValue(1))
        Assert.AreEqual(Some "TWO", mapped.TryGetValue(2))
        Assert.AreEqual(Some "THREE", mapped.TryGetValue(3))

    [<Test>]
    member this.``FoldL should correctly accumulate state``() =
        let dict =
            OpenAddressHashMap<int, string>(5)
                .Add(1, "one")
                .Add(2, "two")
                .Add(3, "three")

        let sumKeys = dict.FoldL (fun acc (k, _) -> acc + k) 0
        Assert.AreEqual(6, sumKeys) // 1 + 2 + 3

    [<Test>]
    member this.``FoldR should correctly accumulate state``() =
        let dict =
            OpenAddressHashMap<int, string>(5)
                .Add(1, "one")
                .Add(2, "two")
                .Add(3, "three")

        let concatValues = dict.FoldR (fun (_, v) acc -> v + " " + acc) ""
        Assert.AreEqual("one two three", concatValues.Trim())