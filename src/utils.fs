module utils

// Returns pairs of sequense elements, except for last element
let seqPairwise (s:seq<'t>)=
    seq {
        let prev = ref None
        for item in s do
            match !prev with
            |None -> prev := Some(item)
            |Some(prev_item) ->
                yield [prev_item; item]
                prev:= Some(item)
        match !prev with
        |None -> ()
        |Some(item) -> yield [item]
    }
  
let seqAppendDelim (delim: string) sq =
    sq |> seqPairwise |> Seq.map (function |[s; _] -> s + delim |[s] -> s |_ -> "")

let seqToLines maxLineLen (delim: string) (indent:string) sq =
    let curLine = ref ""
    seq {
        for item in sq do
            let line = if System.String.IsNullOrEmpty !curLine 
                            then item 
                            else !curLine + delim + item
            if line.Length > maxLineLen then
                yield !curLine
                curLine := indent + item
            else
                curLine := line
        yield !curLine
    }
