open System
open Akkling

type ReaderMsg =
    | Start
    | InputError of string
    | Read
    | Continue  of string

type WriterMsg =
    | InputSuccess of string
    | InputError of string

type ValidationMsg =
    | Success of string
    | ValidationError of string

let system = System.create "system" <| Configuration.defaultConfig()

let doPrintInstructions () =
    Console.WriteLine("Write whatever you want into the console!");
    Console.WriteLine("Some entries will pass validation, and some won't...\n\n");
    Console.WriteLine("Type 'exit' to quit this application at any time.\n");

let isValid (str:string) = str.Length % 2 = 0 

let rec handleWriterMsg (msg:WriterMsg) =
    match msg with
    |  InputError err -> 
        Console.ForegroundColor <- ConsoleColor.Red 
        Console.WriteLine(err)
    | InputSuccess text ->        
        Console.ForegroundColor <- ConsoleColor.Green 
        Console.WriteLine(text)
    Console.ResetColor()
    become handleWriterMsg

let rec handleValidationMsg consoleWriter (context:Actor<string>) (msg:string) =
    if String.IsNullOrEmpty msg then consoleWriter <! InputError "null"
    else if isValid msg then
        consoleWriter <! InputSuccess "Input is valid"
    else consoleWriter <! InputError "Invalid: input had odd number of characters."
    context.Sender() <! Continue msg
    become <| handleValidationMsg consoleWriter context


let rec handleReaderMsg consoleWriter inputValidator (context:Actor<ReaderMsg>) (msg:ReaderMsg) =
    match msg with
    | Start -> 
        doPrintInstructions ()
        context.Self <! Read
    | ReaderMsg.InputError err -> 
        consoleWriter <! InputError err
        context.Self <! Read
    | Read -> 
        let read = Console.ReadLine()
        if not (String.IsNullOrEmpty(read)) && String.Equals(read, "exit", StringComparison.OrdinalIgnoreCase) then
            system.Terminate () |> ignore
        inputValidator <! read
    | Continue read ->
        consoleWriter <! InputSuccess read
        context.Self <! Read

    become (handleReaderMsg consoleWriter inputValidator context)


let consoleWriter = spawn system "consoleWriter" <| props(actorOf handleWriterMsg)
let inputValidator = spawn system "inputValidator" <| props(actorOf2 (handleValidationMsg consoleWriter))
let consoleReader = spawn system "consoleReader" <| props(actorOf2 (handleReaderMsg consoleWriter inputValidator))


[<EntryPoint>]
let main argv =

    consoleReader <! Start
    system.WhenTerminated.Wait ()
    //Console.ReadLine () |> ignore
    0 // return an integer exit code
