module CaseConverter

open System
open System.Text.RegularExpressions
open FSharpx

type private PascalCaseMatcher = Regex< @"^(?!.*_)(?<Identifier>[A-Z0-9][a-z0-9]*)+" >
type private EnumCaseMatcher = Regex< @"^(?![A-Z0-9]*[a-z])((?<Identifier>[A-Z0-9]+)_?)+" >
    
let private getIdentifierParts ident  =
    let matchGroup = 
        match ident with
        | _ when PascalCaseMatcher.IsMatch(ident) -> PascalCaseMatcher().Match(ident).Identifier
        | _ when EnumCaseMatcher.IsMatch(ident) -> EnumCaseMatcher().Match(ident).Identifier
        | _ -> failwith "Unexpected identifier case."

    matchGroup.Captures |> Linq.Enumerable.OfType |> Seq.map(fun (c: Capture) -> c.Value)

let public toSnakeCase ident =
    ident
    |> getIdentifierParts
    |> Seq.map (fun part -> part.ToLower())
    |> (fun parts -> String.Join("_", parts))

let public toPascalCase ident =
    ident
    |> getIdentifierParts
    |> Seq.map (fun part -> System.Globalization.CultureInfo.InvariantCulture.TextInfo.ToTitleCase(part.ToLower())) 
    |> (fun parts -> String.Join("", parts))
                        