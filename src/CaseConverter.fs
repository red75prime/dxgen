module CaseConverter

open System
open System.Text.RegularExpressions

let private pascalCaseMatcher = new Regex(@"^([A-Z0-9][a-z0-9]*)+")
let private enumCaseMatcher = new Regex(@"(?:([A-Z0-9]+)_?)+")
  
type public SourceCase =
    | Pascal = 0
    | Enum = 1
    
let private getIdentifierParts sourceCase ident =
    let matchGroup = 
        match sourceCase with
        | SourceCase.Pascal -> pascalCaseMatcher.Match(ident).Groups.[1]
        | SourceCase.Enum -> enumCaseMatcher.Match(ident).Groups.[1]
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
                        
/// Warning. It changes some acronyms to title case.
let public toSnake (ident:string)=
  // Replace some acronyms for better results
  let ident2=ident.Replace("CPU","Cpu").Replace("GPU","Gpu").Replace("OnID","OnId").Replace("IASet","IaSet")
                  .Replace("OMSet","OmSet").Replace("RSSet","RsSet").Replace("SOSet","SoSet").Replace("MemoryDC","MemoryDc")
                  .Replace("LOGFONT","Logfont")
  let ident1 = if ident2.EndsWith("DC") then ident2.Substring(0, ident2.Length-1)+"c" else ident2
  let mtch=pascalCaseMatcher.Match(ident1) 
  if mtch.Success then
    String.Join("_", mtch.Groups.[1].Captures |> Linq.Enumerable.OfType |> Seq.map (fun (c:Capture) -> c.Value.ToLowerInvariant()))
  else
    ident
