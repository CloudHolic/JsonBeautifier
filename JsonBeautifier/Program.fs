open System
open System.Collections.Generic
open System.IO
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
        let mutable indent = 0

        let rec inner src = 
            match src with
            | JNull -> "null"
            | JBool b -> string b |> fun s -> s.ToLower()
            | JNumber n -> string n
            | JString s -> "\"" + s + "\""
            | JList lst ->
                indent <- indent + 2
                lst
                |> List.map (fun x -> 
                    "\n" + String.replicate indent " " + inner x)
                |> fun l -> (",", l) |> String.Join
                |> fun s -> 
                    indent <- indent - 2
                    "[" + s + "\n" + String.replicate indent " " + "]"
            | JObject dict ->
                indent <- indent + 2
                dict
                |> Seq.map (fun (KeyValue(k, v)) ->
                    "\n" + String.replicate indent " " + "\"" + k + "\": " + inner v)
                |> fun l -> (",", l) |> String.Join
                |> fun s ->
                    indent <- indent - 2
                    "{" + s + "\n" + String.replicate indent " " + "}"

        jdata |> inner

    use stream = new StreamReader(Console.OpenStandardInput())
    let text = stream.ReadToEnd()

    match run json text with
    | Success (res, _, _) -> res |> beautify |> printfn "%s"
    | Failure (err, _, _) -> printfn "Parsing failed: %s" err
    0