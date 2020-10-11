module FSharp.CommandLine.ExtensionTests

open NUnit.Framework
open FsUnit
open InternalExtensions

[<SetUp>]
let Setup () =
    ()

[<Test>]
let compileFunc_success () =
    let funcHelper = FuncHelper.compileFunc <@ fun x y -> x + y @>
    let result  = funcHelper 10 10
    result |> should equal 20

[<Test>]
let getFirstArgumentName_success () =
    let argument = FuncHelper.getFirstArgumentName <@ fun x y -> x + y @>
    argument |> should equal (Some ["x"])