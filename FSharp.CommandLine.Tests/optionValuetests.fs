module FSharp.CommandLine.OptionValueTests

open FsUnit
open OptionValues
open Microsoft.FSharp.Core
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

[<Test>]
let construct_success_one () =
    let value = {
        format = "Test %d"
        paramNames = Option.None
        handler = id
       }
    let (f, rest) = ValueFormat<_, _, _, _, _, _>.construct value
    f "Test 1" |> should equal (Some 1)
    rest |> should equal ["T";"e";"s";"t";" "; "<INT_VALUE>"]

[<Test>]
let construct_fail_unmatched_param () =
    let value = {
        format = "Test %d %s"
        paramNames = Option.None
        handler = id
       }
    let (f, rest) = ValueFormat<_, _, _, _, _, _>.construct value
    printf "%A" rest
    f "Test 1" |> should equal (None)
    rest |> should equal ["T";"e";"s";"t";" "; "<INT_VALUE>"; " "; "<STRING_VALUE>"]

[<Test>]
let construct_fail_extra_param () =
    let value = {
        format = "Test %d"
        paramNames = Option.None
        handler = id
       }
    let (f, rest) = ValueFormat<_, _, _, _, _, _>.construct value
    f "Test 1 3" |> should equal (None)
    rest |> should equal ["T";"e";"s";"t";" "; "<INT_VALUE>"]

[<Test>]
let construct_success_param_name () =
    let value = {
        format = "Test %d"
        paramNames = Option.None
        handler = id
       }
    let (f, rest) = ValueFormat<_, _, _, _, _, _>.construct value
    f "Test 1 3" |> should equal (None)
    rest |> should equal ["T";"e";"s";"t";" "; "<INT_VALUE>"]

[<Test>]
let construct_success_param_name_test () =
    let value = {
        format = "Test %d"
        paramNames = Option.Some ["Test"]
        handler = id
       }
    let (f, rest) = ValueFormat<_, _, _, _, _, _>.construct value
    f "Test 1 3" |> should equal (None)
    rest |> should equal ["T";"e";"s";"t";" "; "<Test>"]

[<Test>]
let construct_success_two_param_name_test () =
    let value = {
        format = "Test %d %d"
        paramNames = Option.Some ["Test"; "Test2"]
        handler = id
       }
    let (f, rest) = ValueFormat<_, _, _, _, _, _>.construct value
    f "Test 1 3" |> should equal (Some (1, 3))
    rest |> should equal ["T";"e";"s";"t";" "; "<Test>"; " "; "<Test2>"]

[<Test>]
let construct_handler_success () =
    let value = {
        format = "Test %d %d"
        paramNames = Option.Some ["Test"; "Test2"]
        handler = fun (a, b) -> (a + 2, b + 3)
       }
    let (f, rest) = ValueFormat<_, _, _, _, _, _>.construct value
    f "Test 1 3" |> should equal (Some (3, 6))
    rest |> should equal ["T";"e";"s";"t";" "; "<Test>"; " "; "<Test2>"]

[<Test>]
let construct_handler_failure () =
    let value = {
        format = "Test %d %d"
        paramNames = Option.Some ["Test"; "Test2"]
        handler = fun (a, b) -> (a + 2, b + 3)
       }
    let (f, rest) = ValueFormat<_, _, _, _, _, _>.construct value
    f "Test 1 3" |> should not' (equal (Some (4, 5)))
    rest |> should equal ["T";"e";"s";"t";" "; "<Test>"; " "; "<Test2>"]

let mapper_maps_success () =
    let value = {
        format = "Test %d %d"
        paramNames = Option.Some ["Test"; "Test2"]
        handler = fun (a, b) -> (a + 2, b + 3)
       }
    let newValue = value.map <| fun (a,b) -> (a + 1, b + 1)
    let (f, rest) = ValueFormat<_, _, _, _, _, _>.construct value
    f "Test 1 3" |> should equal (Some (4, 7))

let mapper_const_success () =
    let value = {
        format = "Test %d %d"
        paramNames = Option.Some ["Test"; "Test2"]
        handler = fun (a, b) -> (a + 2, b + 3)
       }
    let newValue = value.asConst (3,4)
    let (f, rest) = ValueFormat<_, _, _, _, _, _>.construct value
    f "Test 1 3" |> should equal (Some (3, 4))