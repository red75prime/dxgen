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

let deleteLastChar (s: string) =
    s.Substring(0, s.Length - 1)

let formatUse maxLineLen modul (imports: string seq) =
    if Seq.length imports = 1 then
        sprintf "use %s::%s;" modul (Seq.exactlyOne imports)
    else
        let ret = sprintf "use %s::{%s};" modul (System.String.Join(", ", imports))
        if ret.Length < maxLineLen then
            ret
        else
            let sb = new System.Text.StringBuilder();
            let apl s = sb.AppendLine(s) |> ignore
            apl <| sprintf "use %s::{" modul
            let indent = "    "
            let delim = ", "
            let line = ref indent
            for import in imports do
                if (!line+import+delim).Length < maxLineLen then
                    line := !line + import + delim
                else 
                    apl <| deleteLastChar !line
                    line := indent + import + delim
            if !line <> indent then
                apl <| deleteLastChar !line
            sb.Append("};") |> ignore
            sb.ToString()

let coloredText texCol f =
    let oldColor = System.Console.ForegroundColor
    try
        System.Console.ForegroundColor <- texCol
        f()
    finally
        System.Console.ForegroundColor <- oldColor
