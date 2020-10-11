module FSharp.CommandLine.AbstractionTests

open NUnit.Framework
open FsUnit
open FSharp.CommandLine.Internals.Abstraction
open FSharp.CommandLine.Internals.Abstraction.StateConfig

[<SetUp>]
let Setup () =
    ()

[<Test>]
let returnValue_func_Success () =
    let stateConfig = returnValue 0
    let result = stateConfig.func "test"
    result |> should equal (0, "test")

[<Test>]
let returnValue_config_Success () =
    let stateConfig = returnValue 0
    let result = stateConfig.config "test"
    result |> should equal "test"

[<Test>]
let returnWith_func_Success () =
    let stateConfig = returnWith <| fun _ -> 0
    let result = stateConfig.func "test"
    result |> should equal (0, "test")

[<Test>]
let returnWith_config_Success () =
    let stateConfig = returnWith <| fun _ -> 0
    let result = stateConfig.config "test"
    result |> should equal "test"

let flip f a b = f b a

type Config = {
    onOffSwitch : bool
    counter : int
}

let turnOn config = { config with onOffSwitch = true }
let setConfigCounter num config = { config with counter = num }

let defaultConfig = {onOffSwitch = false; counter = 0}
let defaultStateConfig = {
    config = id
    func = fun args -> (args, args)
}

let defaultStateFunc = 
    (fun (x: string) -> 
        let possibleString = Seq.tryHead x |> Option.map string
        {
            config = id
            func = fun (args : string) -> 
                match args |> Seq.toList with
                      [] -> ("", "")
                    | x :: []-> (string(x), "")
                    | x :: xs -> (string(x), System.String.Concat(xs))
        }
    )

[<Test>]
let scbind_success () =
    let newStateConfig = scbind 
                            defaultStateFunc
                            (turnOn >> setConfigCounter 4)
                            defaultStateConfig
    newStateConfig.func "test" |> should equal ("t", "est")
    let newConfig = { onOffSwitch = true; counter = 4;}
    newStateConfig.config defaultConfig |> should equal newConfig

[<Test>]
let bind_success () =
    let newstateConfig = bind defaultStateFunc defaultStateConfig
    newstateConfig.func "test" |> should equal ("t", "est")

[<Test>]
let mapConfig_success () =
    let newstateConfig = mapConfig turnOn defaultStateConfig
    newstateConfig.config defaultConfig |> should equal {onOffSwitch = true; counter = 0}

[<Test>]
let map_success () =
    let newStateConfig = map (fun _ -> "t") defaultStateConfig
    newStateConfig.func "test" |> should equal ("t", "test")

[<Test>]
let zip_success () =
    let newStateConfig = zip defaultStateConfig defaultStateConfig (fun _ _-> "zip success")
    newStateConfig.func "test" |> should equal ("zip success", "test")

[<Test>]
let combine_success () =
    let newStateConfig = combine defaultStateConfig defaultStateConfig
    newStateConfig.func "test" |> should equal ("test", "test")
