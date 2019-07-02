module Utils

    let getKeyAndRemaining (t : string) =
        let splitterIndex = t |> Seq.findIndex (fun c -> c = ',')
        let key = t.Substring(0, splitterIndex).Trim().TrimStart('\"').TrimEnd('\"')
        let remaining = t.Substring(splitterIndex + 1).Trim()
        key, remaining

    let getKeyAndValue (t : string) =
        let key, remaining = getKeyAndRemaining t
        key, remaining.TrimStart('\"').TrimEnd('\"')
       
    let getIntArray (s : string) =
        let content = s.TrimStart('[').TrimEnd(']')

        if content = "" then
            Array.empty<int>
        else
            content.Split(',')
                |> Seq.map (int)
                |> Seq.toArray
