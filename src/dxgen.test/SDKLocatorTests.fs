module SDKLocatorTests

open NUnit.Framework
open FsUnit

open SDKLocator

[<Test>]
let ``findSDKRootDirectory should return a valid path that exists`` () =
    findSDKRootDirectory ()
    |> System.IO.Directory.Exists
    |> should equal true