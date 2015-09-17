module utils

// Returns pairs of sequense elements, except for last element
let seqPairwise (s:seq<'t>)=
  if Seq.isEmpty s then
    Seq.empty
  else
    let s1=s |> Seq.map Some
    let s2= // shifted sequence
      seq {
        yield! s |> Seq.skip 1 |> Seq.map Some
        yield None
      }
    Seq.zip s1 s2 
      |> Seq.map 
        (function 
          |(Some(a),Some(b)) -> [a;b]
          |(Some(a),None) -> [a]
          |_ -> raise <| new System.Exception("Unreachable"))
  
