module Lab2.Tests

open NUnit.Framework
open OpenAddressHashMap

[<TestFixture>]
type OpenAddressHashMapTests() =

    [<Test>]
    member this.``Add and GetValue should return correct values``() =
        let dict = createEmpty (5)
        let dict = add 1 "one" dict
        let dict = add 2 "two" dict

        Assert.AreEqual(Some "one", getValue 1 dict)
        Assert.AreEqual(Some "two",  getValue 2 dict)
        Assert.AreEqual(None,  getValue 3 dict)

    [<Test>]
    member this.``Remove should delete element and GetValue should return None``() =
        let dict = createEmpty (5)
        let dict = add 1 "one" dict
        let dict = add 2 "two" dict
        let dict = remove 1 dict

        Assert.AreEqual(None, getValue 1 dict)
        Assert.AreEqual(Some "two", getValue 2 dict)

    [<Test>]
    member this.``Merge should combine two dictionaries``() =
        let dict1 = createEmpty 5 
        let dict1 = add 1 "one" dict1
        let dict1 = add 2 "two" dict1

        let dict2 = createEmpty 5
        let dict2 = add 2 "two" dict2
        let dict2 = add 3 "three" dict2

        let merged = merge dict1 dict2

        Assert.AreEqual(Some "one", getValue 1 merged)
        Assert.AreEqual(Some "two", getValue 2 merged)
        Assert.AreEqual(Some "three", getValue 3 merged)

    [<Test>]
    member this.``Merge with empty dictionary should return original dictionary``() =
        let dict1 = createEmpty 5 
        let dict1 = add 1 "one" dict1
        let dict1 = add 2 "two" dict1

        let emptyDict= createEmpty 0

        let result = merge dict1 emptyDict

        Assert.AreEqual(getValue 1 dict1, getValue 1 result)
        Assert.AreEqual(getValue 2 dict1, getValue 2 result)
        Assert.AreEqual(dict1.Size, result.Size)

    [<Test>]
    member this.``Filter should return dictionary with only matching elements``() =
        let dict = createEmpty 5 
        let dict = add 1 "one" dict
        let dict = add 2 "two" dict
        let dict = add 3 "three" dict

        let filtered = filter (fun (k, _) -> k % 2 = 0) dict

        Assert.AreEqual(None, getValue 1 filtered)
        Assert.AreEqual(Some "two", getValue 2 filtered)
        Assert.AreEqual(None, getValue 3 filtered)

    [<Test>]
    member this.``Map should transform all elements``() =
        let dict = createEmpty 5 
        let dict = add 1 "one" dict
        let dict = add 2 "two" dict
        let dict = add 3 "three" dict

        let mapped = map (fun (k, (v: string)) -> (k, v.ToUpper())) dict

        Assert.AreEqual(Some "ONE", getValue 1 mapped)
        Assert.AreEqual(Some "TWO", getValue 2 mapped)
        Assert.AreEqual(Some "THREE", getValue 3 mapped)

    [<Test>]
    member this.``FoldL should correctly accumulate state``() =
        let dict = createEmpty 5 
        let dict = add 1 "one" dict
        let dict = add 2 "two" dict
        let dict = add 3 "three" dict

        let sumKeys = foldL (fun acc (k, _) -> acc + k) 0 dict
        Assert.AreEqual(6, sumKeys)

    [<Test>]
    member this.``FoldR should correctly accumulate state``() =
        let dict = createEmpty 5 
        let dict = add 1 "one" dict
        let dict = add 2 "two" dict
        let dict = add 3 "three" dict

        let concatValues = foldR (fun (_, v) acc -> acc + " " + v) "" dict
        Assert.AreEqual("three two one", concatValues.Trim())
