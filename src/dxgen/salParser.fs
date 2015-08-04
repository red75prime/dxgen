//Parses SAL annotations (InOut, OutWrites(p) and such) 
module salparser

open cdesc
open parserm
open FSharp.Reflection

// FParsec is too heavy-weight for such simple task
// parserm - monadic parser is easier to use. Downside: it is not scalable (slower, combinatoric explosion in manyMulti and other parts).
// (I found parserm.fs in my projects' folder. It is likely that I wrote it.
//  Ah, yes, indeed. http://pastebin.com/cUzjJ6Rb )

let ibChars=
  parser{
    let! c=item
    if c<>"(" && c<>")" then
      return c
  }

let chr ch=
  Parser(
    fun (s:System.String) ->
      if s.StartsWith(ch) then
        seq{yield (ch,s.Substring(ch.Length))}
      else
        Seq.empty
  )

let str str v=
  Parser(
    fun (s:System.String) ->
      if s.StartsWith(str) then
        seq{yield (v,s.Substring(str.Length))}
      else
        Seq.empty
  )

let manyS p=
  parser {
    let! (r : string list) = manyMulti p
    let r'=System.String.Join("",r)
    return r'
  }

let (>>+) p1 p2=
  parser {
    let! m1=p1
    let! m2=p2
    return System.String.Join("",[m1;"|";m2])
  }

// anyexcP ::= any character except '(' and ')'
// pmatch ::= {anyexcP} | {anyexcP} '(' pmatch ')' pmatch
let rec pmatch=
  parser{
    let! v=manyS ibChars
    let! p1=chr "("
    let! i=pmatch
    let! p2=chr ")"     
    let! e=pmatch
    return System.String.Join("",[v;p1;i;p2;e])
  } ++ parser {
    let! v'=manyMulti ibChars
    return System.String.Join("",v')
  }

let salParser : Parser<CParamAnnotation, string>=
  let ucs=Reflection.FSharpType.GetUnionCases(typeof<CParamAnnotation>)
  let parsers=ref []
  for v in ucs do
    let curParser : Parser<CParamAnnotation, string>=
      match v.GetFields() with
      |[||] -> str v.Name (unbox <| FSharpValue.MakeUnion(v,[| |]))
      |[|_|] -> parser {
         let! _=chr v.Name
         let! _=chr "("
         let! (p:string) = pmatch
         let! _=chr ")"
         return (unbox <| FSharpValue.MakeUnion(v,[|box p|],false))
        }
      |[|_;_|] -> parser {
         let! _=chr v.Name
         let! _=chr "("
         let! p1=pmatch
         let! _=chr ","
         let! p2=pmatch
         let! _=chr ")"
         return (unbox <| FSharpValue.MakeUnion(v,[|box p1; box p2|],false))
        }
      |_ -> raise <| new System.Exception "Unsupported"
    parsers := curParser :: !parsers
  choiceMulti !parsers

let pr=parse salParser "InReadsBytesOpt(d)"

// Result of parsing of ill-formed annotation is undefined.
let parseSAL (str:System.String)=
  match parse salParser str |> Seq.filter (snd >> ((=)"")) |> Seq.map fst |> Seq.tryHead with
  |Some(a) -> a
  |_ -> 
    printfn "Cannot parse SAL %s" str
    NoAnnotation
  
