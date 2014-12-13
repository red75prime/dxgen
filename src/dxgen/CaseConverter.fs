module CaseConverter

open System
open System.Text.RegularExpressions
open FSharpx

type private PascalCaseMatcher = Regex< @"^(?<Identifier>[A-Z0-9][a-z0-9]*)+" >
type private EnumCaseMatcher = Regex< @"((?<Identifier>[A-Z0-9]+)_?)+" >
  
type public SourceCase =
    | Pascal = 0
    | Enum = 1
    
let private getIdentifierParts sourceCase ident =
    let matchGroup = 
        match sourceCase with
        | SourceCase.Pascal -> PascalCaseMatcher().Match(ident).Identifier
        | SourceCase.Enum -> EnumCaseMatcher().Match(ident).Identifier
        | _ -> failwith "Unexpected identifier case."

    matchGroup.Captures |> Linq.Enumerable.OfType |> Seq.map(fun (c: Capture) -> c.Value)

let public toSnakeCase sourceCase ident =
    ident
    |> getIdentifierParts sourceCase
    |> Seq.map (fun part -> part.ToLower())
    |> (fun parts -> String.Join("_", parts))

let public toPascalCase sourceCase ident =
    ident
    |> getIdentifierParts sourceCase
    |> Seq.map (fun part -> System.Globalization.CultureInfo.InvariantCulture.TextInfo.ToTitleCase(part.ToLower())) 
    |> (fun parts -> String.Join("", parts))
                        