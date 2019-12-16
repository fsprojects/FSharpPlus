namespace DocsTool

module Uri =
    open System
    let simpleCombine (slug : string) (baseUri : Uri) =
        sprintf "%s/%s" (baseUri.AbsoluteUri.TrimEnd('/')) (slug.TrimStart('/'))

    let create (url : string) =
        match Uri.TryCreate(url, UriKind.Absolute) with
        | (true, v) -> v
        | _ -> failwithf "Bad url %s" url
