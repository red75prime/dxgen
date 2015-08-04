module parserm

// based on "Monadic Parser Combinators" by Graham Hutton and Erik Meijer (1996)

type Parser<'a,'s>=Parser of ('s -> seq<'a*'s>)

let parse (Parser p) cs=p cs

let zero=Parser (fun _->Seq.empty)

let nullparser=Parser (fun cs-> seq{ yield ((),cs);} )

// Returns all possible matches of p|q
// Beware! Combinatoric explosion.
let (++) p q=Parser (fun cs->seq{yield! parse p cs; yield! parse q cs})

// Returns first match of p|q
let (+++) p q=Parser (fun cs->Seq.truncate 1 (seq{yield! parse p cs; yield! parse q cs}))

let returnp v=Parser (fun cs->seq{yield (v,cs)})

type public ParserMonadBuilder()=
  member public x.Return(v)=returnp v
  member public x.Bind(p,f)=Parser (fun cs->seq{for (a,cs') in parse p cs do yield! parse (f a) cs'})
  member public x.Zero()=zero
  member public x.Delay(f)=Parser (fun cs -> parse (f()) cs)

let public parser=new ParserMonadBuilder()

let item=
  Parser (fun (cs:System.String)->
        match cs with
        |"" -> Seq.empty
        |_ -> Seq.singleton (cs.Substring(0,1),cs.Substring(1))
      )

let sat p=
  parser{
    let! c=item
    if p c then
      return c
  }

let choiceMulti ps=
  Parser
    (fun cs ->
      ps 
        |> Seq.collect 
            (fun p -> 
              let pr=parse p cs
              pr)
      )

let rec many p=
  (many1 p) +++ (returnp [])
and many1 p=
  parser{
    let! a=p
    let! as'=many p
    return (a::as')
  }

let maybe p=
  parser{
    let! a=p
    return Some a
  } +++
  parser{
    return None
  }

let (>>>) p1 p2=
  parser {
    let! v1=p1
    let! v2=p2
    return List.concat [v1;v2]
  }

let rec manyMulti p=
  (many1Multi p) ++ (returnp [])
and many1Multi p=
  parser{
    let! a=p
    let! as'=manyMulti p
    return (a::as')
  }
