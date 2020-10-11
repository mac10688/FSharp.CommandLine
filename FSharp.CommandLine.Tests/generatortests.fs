module FSharp.CommandLine.GeneratorTests

open NUnit.Framework
open FsUnit
open FSharp.CommandLine.Generators
open FSharp.CommandLine.BasicTypes
open FSharp.CommandLine.Internals.Abstraction

[<SetUp>]
let Setup () =
    ()

[<Test>]
let interperate_helpUsage_empty_config () =
    let commandFun = fun _ -> seq { HelpUsage }
    let cmd = StateConfig.returnValue 5
    let x = Generators.Help.interpret commandFun cmd
    x |> should equal (seq { "usage:  " })

[<Test>]
let interpret_helpUsage_displayName_paramNames () =
    let commandFun = fun _ -> seq { HelpUsage }

    let commandSummary  = {
        name = "Name"
        displayName = Some "DisplayName"
        description = "Test description"
        paramNames = Some ["Test"; "Params"]
        help = None
        genSuggestions = fun _ -> []
    }

    let commandInfo = {
        summary = commandSummary
        options = []
        subcommands = []
    }

    let cmd = {
        config = fun _ -> commandInfo
        func = fun args -> (5, args)
    }

    let x = Generators.Help.interpret commandFun cmd
    x |> should equal (seq { "usage: DisplayName Test Params" })

[<Test>]
let interpret_helpUsage_displayName_noParamNames_noCommands_noCommandOptionSummaries () =
    let commandFun = fun _ -> seq { HelpUsage }

    let commandSummary  = {
        name = "Name"
        displayName = Some "DisplayName"
        description = "Test description"
        paramNames = None
        help = None
        genSuggestions = fun _ -> []
    }

    let commandInfo = {
        summary = commandSummary
        options = []
        subcommands = []
    }

    let cmd = {
        config = fun _ -> commandInfo
        func = fun args -> (5, args)
    }

    let x = Generators.Help.interpret commandFun cmd
    x |> should equal (seq { "usage: DisplayName " })

[<Test>]
let interpret_helpUsage_displayName_noParamNames_noCommands_commandOptionSummaries () =
    let commandFun = fun _ -> seq { HelpUsage }

    let commandSummary  = {
        name = "Name"
        displayName = Some "DisplayName"
        description = "Test description"
        paramNames = None
        help = None
        genSuggestions = fun _ -> []
    }

    let commandOption = {
        names = ["commandOptionName1"]
        description = "description"
        isFlag = false
        paramNames = [["paramNames1"]]
        isMatch = fun _ -> Some ["Matches"]
        genSuggestions = fun _ -> []
    }

    let commandInfo = {
        summary = commandSummary
        options = [commandOption]
        subcommands = []
    }

    let cmd = {
        config = fun _ -> commandInfo
        func = fun args -> (5, args)
    }

    let x = Generators.Help.interpret commandFun cmd
    x |> should equal (seq { "usage: DisplayName [options]" })

[<Test>]
let interpret_helpUsage_displayName_noParamNames_commands_noCommandsOptionSummaries () =
    let commandFun = fun _ -> seq { HelpUsage }

    let commandSummary  = {
        name = "Name"
        displayName = Some "DisplayName"
        description = "Test description"
        paramNames = None
        help = None
        genSuggestions = fun _ -> []
    }

    let subcmd = {
        config = fun _ -> {
            summary = {
                name = "SubName"
                displayName = Some "SubDisplayName"
                description = "Sub Test description"
                paramNames = Some ["SubTest"; "SubParams"]
                help = None
                genSuggestions = fun _ -> []
            }
            options = []
            subcommands = []
        }
        func = fun args -> (0, args)
    }

    let commandInfo = {
        summary = commandSummary
        options = []
        subcommands = [subcmd]
    }

    let cmd = {
        config = fun _ -> commandInfo
        func = fun args -> (5, args)
    }

    let x = Generators.Help.interpret commandFun cmd
    x |> should equal (seq { "usage: DisplayName <command>" })

[<Test>]
let interpret_helpUsage_displayName_noParamNames_commands_commandOptionSummaries () =
    let commandFun = fun _ -> seq { HelpUsage }

    let commandSummary  = {
        name = "Name"
        displayName = Some "DisplayName"
        description = "Test description"
        paramNames = None
        help = None
        genSuggestions = fun _ -> []
    }

    let commandOption = {
        names = ["commandOptionName1"]
        description = "description"
        isFlag = false
        paramNames = [["paramNames1"]]
        isMatch = fun _ -> Some ["Matches"]
        genSuggestions = fun _ -> []
    }

    let subcmd = {
        config = fun _ -> {
            summary = {
                name = "SubName"
                displayName = Some "SubDisplayName"
                description = "Sub Test description"
                paramNames = Some ["SubTest"; "SubParams"]
                help = None
                genSuggestions = fun _ -> []
            }
            options = []
            subcommands = []
        }
        func = fun args -> (0, args)
    }

    let commandInfo = {
        summary = commandSummary
        options = [commandOption]
        subcommands = [subcmd]
    }

    let cmd = {
        config = fun _ -> commandInfo
        func = fun args -> (5, args)
    }

    let x = Generators.Help.interpret commandFun cmd
    x |> should equal (seq { "usage: DisplayName [options] <command>" })

[<Test>]
let interpret_helpCustomArgs_displayName () =
    let commandFun = fun _ -> seq { HelpUsageCustomArgs ["Custom"; "Args"]}

    let commandSummary  = {
        name = "Name"
        displayName = Some "DisplayName"
        description = "Test description"
        paramNames = None
        help = None
        genSuggestions = fun _ -> []
    }

    let commandInfo = {
        summary = commandSummary
        options = []
        subcommands = []
    }

    let cmd = {
        config = fun _ -> commandInfo
        func = fun args -> (5, args)
    }

    let x = Generators.Help.interpret commandFun cmd

    x |> should equal (seq {"usage: DisplayName Custom Args"})
    

[<Test>]
let interpret_helpRawString () =
    let commandFun = fun _ -> seq { HelpRawString "Just A Raw String"}

    let commandSummary  = {
        name = "Name"
        displayName = Some "DisplayName"
        description = "Test description"
        paramNames = None
        help = None
        genSuggestions = fun _ -> []
    }

    let commandInfo = {
        summary = commandSummary
        options = []
        subcommands = []
    }

    let cmd = {
        config = fun _ -> commandInfo
        func = fun args -> (5, args)
    }

    let x = Generators.Help.interpret commandFun cmd

    x |> should equal (seq {"Just A Raw String"})


[<Test>]
let interpret_helpAllSubcommands () =
    let commandFun = fun _ -> seq { HelpAllSubcommands }

    let commandSummary  = {
        name = "Name"
        displayName = Some "DisplayName"
        description = "Test description"
        paramNames = None
        help = None
        genSuggestions = fun _ -> []
    }

    let commandInfo = {
        summary = commandSummary
        options = []
        subcommands = [{
            config = fun _ -> {
                summary = {
                    name = "SubName1"
                    displayName = Some "SubDisplayName1"
                    description = "Sub Test description 1"
                    paramNames = Some ["SubTest1"; "SubParams1"]
                    help = None
                    genSuggestions = fun _ -> []
                }
                options = []
                subcommands = []
            }
            func = fun args -> (0, args)
        };
        {
            config = fun _ -> {
                summary = {
                    name = "SubName2"
                    displayName = Some "SubDisplayName2"
                    description = "Sub Test description 2"
                    paramNames = Some ["SubTest2"; "SubParams2"]
                    help = None
                    genSuggestions = fun _ -> []
                }
                options = []
                subcommands = []
            }
            func = fun args -> (0, args)
        }
        ]
    }

    let cmd = {
        config = fun _ -> commandInfo
        func = fun args -> (5, args)
    }

    let x = Generators.Help.interpret commandFun cmd
    x |> should equal (seq { "SubName1 SubTest1 SubParams1      Sub Test description 1"; "SubName2 SubTest2 SubParams2      Sub Test description 2"})

[<Test>]
let interpret_helpSpecificSubcommands () =
    let commandFun = fun _ -> seq { HelpSpecificSubcommands ["SubName1"] }

    let commandSummary  = {
        name = "Name"
        displayName = Some "DisplayName"
        description = "Test description"
        paramNames = None
        help = None
        genSuggestions = fun _ -> []
    }

    let commandInfo = {
        summary = commandSummary
        options = []
        subcommands = [{
            config = fun _ -> {
                summary = {
                    name = "SubName1"
                    displayName = Some "SubDisplayName1"
                    description = "Sub Test description 1"
                    paramNames = Some ["SubTest1"; "SubParams1"]
                    help = None
                    genSuggestions = fun _ -> []
                }
                options = []
                subcommands = []
            }
            func = fun args -> (0, args)
        };
        {
            config = fun _ -> {
                summary = {
                    name = "SubName2"
                    displayName = Some "SubDisplayName2"
                    description = "Sub Test description 2"
                    paramNames = Some ["SubTest2"; "SubParams2"]
                    help = None
                    genSuggestions = fun _ -> []
                }
                options = []
                subcommands = []
            }
            func = fun args -> (0, args)
        }
        ]
    }

    let cmd = {
        config = fun _ -> commandInfo
        func = fun args -> (5, args)
    }

    let x = Generators.Help.interpret commandFun cmd
    x |> should equal (seq { "SubName1 SubTest1 SubParams1      Sub Test description 1"})

[<Test>]
let interpret_helpAllOptions () =
    let commandFun = fun _ -> seq { HelpAllOptions }
    
    let commandSummary  = {
        name = "Name"
        displayName = Some "DisplayName"
        description = "Test description"
        paramNames = None
        help = None
        genSuggestions = fun _ -> []
    }
    
    let commandOption1 = {
        names = ["commandOptionName1"]
        description = "description"
        isFlag = false
        paramNames = [["paramNames1"]]
        isMatch = fun _ -> Some ["Matches"]
        genSuggestions = fun _ -> []
    }

    let commandOption2 = {
        names = ["commandOptionName2"]
        description = "description"
        isFlag = false
        paramNames = [["paramNames2"]]
        isMatch = fun _ -> Some ["Matches"]
        genSuggestions = fun _ -> []
    }
    
    let commandInfo = {
        summary = commandSummary
        options = [commandOption1; commandOption2]
        subcommands = []
    }
    
    let cmd = {
        config = fun _ -> commandInfo
        func = fun args -> (5, args)
    }
    
    let x = Generators.Help.interpret commandFun cmd
    x |> should equal (seq { "--commandOptionName1=paramNames1  description"; "--commandOptionName2=paramNames2  description" })

[<Test>]
let interpret_helpSpecificOptions () =
    let commandFun = fun _ -> seq { HelpSpecificOptions ["commandOptionName1"] }
    
    let commandSummary  = {
        name = "Name"
        displayName = Some "DisplayName"
        description = "Test description"
        paramNames = None
        help = None
        genSuggestions = fun _ -> []
    }
    
    let commandOption1 = {
        names = ["commandOptionName1"]
        description = "description"
        isFlag = false
        paramNames = [["paramNames1"]]
        isMatch = fun _ -> Some ["Matches"]
        genSuggestions = fun _ -> []
    }

    let commandOption2 = {
        names = ["commandOptionName2"]
        description = "description"
        isFlag = false
        paramNames = [["paramNames2"]]
        isMatch = fun _ -> Some ["Matches"]
        genSuggestions = fun _ -> []
    }
    
    let commandInfo = {
        summary = commandSummary
        options = [commandOption1; commandOption2]
        subcommands = []
    }
    
    let cmd = {
        config = fun _ -> commandInfo
        func = fun args -> (5, args)
    }
    
    let x = Generators.Help.interpret commandFun cmd
    x |> should equal (seq { "--commandOptionName1=paramNames1  description"; })

[<Test>]
let interpret_helpSection () =
    let commandFun = fun _ -> seq { HelpSection ("Test Name1", seq { HelpUsage; HelpRawString "Rawstring"}) }
    
    let commandSummary  = {
        name = "Name"
        displayName = Some "DisplayName"
        description = "Test description"
        paramNames = None
        help = None
        genSuggestions = fun _ -> []
    }

    let commandInfo = {
        summary = commandSummary
        options = []
        subcommands = []
    }

    let cmd = {
        config = fun _ -> commandInfo
        func = fun args -> (5, args)
    }

    let x = Generators.Help.interpret commandFun cmd
    x |> should equal (seq { "Test Name1:"; "  usage: DisplayName "; "  Rawstring"})

[<Test>]
let interpret_helpEmptyLine () =
    let commandFun = fun _ -> seq { HelpEmptyLine }
    
    let commandSummary  = {
        name = "Name"
        displayName = Some "DisplayName"
        description = "Test description"
        paramNames = None
        help = None
        genSuggestions = fun _ -> []
    }

    let commandInfo = {
        summary = commandSummary
        options = []
        subcommands = []
    }

    let cmd = {
        config = fun _ -> commandInfo
        func = fun args -> (5, args)
    }
    
    let x = Generators.Help.interpret commandFun cmd
    x |> should equal (seq { ""})