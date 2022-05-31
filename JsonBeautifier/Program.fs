open System
open System.Collections.Generic
open System.IO
open System.Text
open FParsec

module JSonParser =
    type Json = 
        | JNull
        | JBool of bool
        | JNumber of float
        | JString of string
        | JList of Json list
        | JObject of IDictionary<string, Json>

    let stringLiteral =
        let normalChar = manySatisfy (fun c -> c <> '\\' && c <> '"')
        let escapedChar = pstring "\\" >>. (anyOf "\\\"nrt" |>> function | '\\' -> "\\\\" | '\"' -> "\\\"" | 'n' -> "\n" | 'r' -> "\r" | 't' -> "\t" | c -> string c)
        between (pstring "\"") (pstring "\"") (stringsSepBy normalChar escapedChar)

    let manyContained popen pclose psep p = between popen pclose <| sepBy p psep
        
    let json, jsonRef = createParserForwardedToRef()

    let jnull = stringReturn "null" JNull .>> spaces

    let jbool = 
        let jtrue = stringReturn "true" <| JBool true .>> spaces
        let jfalse = stringReturn "false" <| JBool false .>> spaces
        jtrue <|> jfalse

    let jnumber = pfloat .>> spaces |>> JNumber

    let jstring = stringLiteral .>> spaces |>> JString

    let jliteral = choice [jnull; jbool; jnumber; jstring]

    let jlist =
        json
        |> manyContained (skipChar '[' .>> spaces) (skipChar ']' .>> spaces) (skipChar ',' .>> spaces)
        |>> JList

    let jobject =
        let jproperty = stringLiteral .>> spaces .>> skipChar ':' .>> spaces .>>. json .>> spaces
        jproperty
        |> manyContained (skipChar '{' .>> spaces) (skipChar '}' .>> spaces) (skipChar ',' .>> spaces)
        |>> Seq.map (fun j -> fst j, snd j)
        |>> dict
        |>> JObject

    do jsonRef.Value <- choice [jliteral; jlist; jobject]


open JSonParser

[<EntryPoint>]
let main _ =
    let beautify jdata =        
        let result = new StringBuilder()
        let mutable indent = 0
        let inline add str = str |> string |> result.Append |> ignore
        let addIndent () = String.replicate indent " " |> add

        let toList dic = dic |> Seq.map (fun (KeyValue(k, v)) -> (k, v)) |> List.ofSeq

        let rec inner src = 
            match src with
            | JNull -> add "null"
            | JBool b -> add b
            | JNumber n -> add n
            | JString s -> "\"" + s + "\"" |> add
            | JList lst ->
                add "[\n"
                indent <- indent + 2
                for object in lst do
                    addIndent ()
                    inner object
                    if object <> List.last lst then add ","
                    add "\n"
                indent <- indent - 2
                addIndent ()
                add "]"
            | JObject ob ->
                add "{\n"
                indent <- indent + 2
                let dictList = toList ob
                for (key, value) in dictList do
                    addIndent ()
                    "\"" + key + "\": " |> add
                    inner value
                    if (key, value) <> List.last dictList then add ","
                    add "\n"
                indent <- indent - 2
                addIndent ()
                add "}"

        jdata |> inner
        result |> string

    use stream = new StreamReader(Console.OpenStandardInput())
    let text = stream.ReadToEnd()

    match run json text with
    | Success (res, _, _) -> res |> beautify |> printfn "%s"
    | Failure (err, _, _) -> printfn "Parsing failed: %s" err
    0