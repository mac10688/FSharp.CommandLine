module FSharp.CommandLine.BasicTypeTests

open NUnit.Framework
open BasicTypes
open FsUnit

[<SetUp>]
let Setup () =
    ()

[<Test>]
let helpFor_willFail () =
    (fun () -> helpText { for x in [1..10] -> seq { HelpUsage } } |> ignore) |> should (throwWithMessage "Not supported") typeof<System.Exception>

[<Test>]
let helpYield_isEmpty () =
    let helper = helpText {
        yield HelpUsage 
    }

    helper |> should equal Seq.empty

[<Test>]
let defaultUsage_success () =
    let helper = helpText {
        defaultUsage
    }

    helper |> should equal (seq { HelpUsage } )

[<Test>]
let customUsage_success () =
    let helper = helpText {
        customUsage ["test"]
    }

    helper |> should equal (seq {HelpUsageCustomArgs ["test"]})

[<Test>]
let text_success () =
    let helper = helpText {
        text "test"
    }

    helper |> should equal (seq { HelpRawString "test" })

[<Test>]
let allSubcommands_success () =
    let helper = helpText {
        allSubcommands
    }

    helper |> should equal (seq { HelpAllSubcommands })

[<Test>]
let specificSubcommands_success () =
    let helper = helpText {
        specificSubcommands ["test"]
    }

    helper |> should equal (seq { HelpSpecificSubcommands ["test"] })

[<Test>]
let allOptions_success () =
    let helper = helpText {
        allOptions
    }

    helper |> should equal (seq { HelpAllOptions })

[<Test>]
let specificOptions_success () =
    let helper = helpText {
        specificOptions ["test"]
    }

    helper |> should equal (seq { HelpSpecificOptions ["test"] })

[<Test>]
let section_success () =
    let helpSection = seq { HelpAllOptions }
    let helper = helpText {
        section "sectionName" helpSection
    }

    helper |> should equal <| seq { HelpSection("sectionName", helpSection) }

[<Test>]
let conditionalSection_true_success () =
    let helpSection = seq { HelpAllOptions }
    let cond = fun () -> true
    let helper = helpText {
        conditionalSection "sectionName" cond helpSection
    }

    helper |> should equal <| seq { HelpSection("sectionName", helpSection) }

[<Test>]
let conditionalSection_false_success () =
    let helpSection = seq { HelpAllOptions }
    let cond = fun () -> false
    let helper = helpText {
        conditionalSection "sectionName" cond helpSection
    }

    helper |> should equal <| Seq.empty

[<Test>]
let commandOptionSummary_nameRepresentations_success () =
    let x = {
        names = ["Test"]
        description = "Some description"
        isFlag = false
        paramNames = [["paramName"]]
        isMatch = fun x -> Option.None
        genSuggestions = fun x -> []
    }
    
    x.NameRepresentations |> should equal ["--Test"; "/Test"]

[<Test>]
let commandOptionSummary_nameRepresentations_two_success () =
    let x = {
        names = ["Test1"; "Test2"]
        description = "Some description"
        isFlag = false
        paramNames = [["paramName1"; "paramName2"]; ["paramName3"; "paramName4"]]
        isMatch = fun x -> Option.None
        genSuggestions = fun x -> []
    }
    
    x.NameRepresentations |> should equal ["--Test1"; "/Test1"; "--Test2"; "/Test2"]

[<Test>]
let commandOptionSummary_Print_success () =
    let x = {
        names = ["Test"]
        description = "Some description"
        isFlag = false
        paramNames = [["paramName"]]
        isMatch = fun x -> Option.None
        genSuggestions = fun x -> []
    }
    
    x.Print () |> should equal ("--Test=paramName", "Some description")

[<Test>]
let commandOptionSummary_print_two_success () =
    let x = {
        names = ["Test1"; "Test2"]
        description = "Some description"
        isFlag = false
        paramNames = [["paramName1"; "paramName2"]; ["paramName3"; "paramName4"]]
        isMatch = fun x -> Option.None
        genSuggestions = fun x -> []
    }
    
    x.Print () |> should equal ("--Test1, --Test2={paramName3paramName4|paramName1paramName2}", "Some description")