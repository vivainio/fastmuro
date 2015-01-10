
open System.Net
open System.IO
open System.Xml.Linq
open System.Xml.XPath
open HtmlAgilityPack
open System
open System.Diagnostics
open System.Text.RegularExpressions


let pr o = 
    printfn "%A" 0
    o
    
let fetch(url:string) = 
    let req = WebRequest.Create url
    let resp = req.GetResponse()
    use sr = new StreamReader(resp.GetResponseStream())
    let cont = sr.ReadToEnd()
    use wr = new StreamWriter("C:/t/dump.html",false)
    wr.Write(cont)


    cont


let fetchAsync url = 
    async {
        let req = WebRequest.Create(Uri url)
        use! resp = req.AsyncGetResponse()
        use stream = resp.GetResponseStream()
        use reader = new IO.StreamReader(stream)
        let html = reader.ReadToEnd()
        return html
    }

(*
	<div class="listBlock main">

		<div class="titleText">
			

			<h3 class="title">
				
				
				
				<a href="threads/ddr2-p%C3%B6yt%C3%A4koneen-laatumuistit-2-x-2-x-2-gb-lis%C3%A4tty-tavaraa-haukipudas.1183891/"
					title=""
					class="PreviewTooltip"
					data-previewUrl="threads/ddr2-p%C3%B6yt%C3%A4koneen-laatumuistit-2-x-2-x-2-gb-lis%C3%A4tty-tavaraa-haukipudas.1183891/preview">DDR2 Pöytäkoneen Laatumuistit 2 x (2 x 2 GB) + Lisätty tavaraa. * Haukipudas *</a>
					


*)
   

[<AutoOpen>]
module FsExtensions =
 
    type XPathNodeIterator with
        /// Converts an XPathNodeIterator into a seq{XPathNavigator}.
        member this.AsEnumerable =
            this |> Seq.unfold (fun it -> match it.MoveNext() with
                                          | false -> None 
                                          | true -> Some(it.Current, it))

 
type OfferH = { t: string; url: string}
type PostBody = { body: string }

type OfferFull = { offer: OfferH; body: PostBody}


let parseForum(s:string, rootUrl) = 
    let doc = new HtmlDocument()
    doc.LoadHtml(s)
    let nav = doc.CreateNavigator()
    let ns = nav.Select("""//h3/a""")
    let offers = [
            for n in ns.AsEnumerable do 
            yield { url = rootUrl + n.GetAttribute("href",""); t = n.ToString()} ]

    offers

let parseThread(s: string) = 
    let doc = new HtmlDocument()
    doc.LoadHtml(s)
    let nav = doc.CreateNavigator()
    let ns = nav.Select("""//article""")
    ns.MoveNext() |> ignore

    printfn "%A articles, first= %A" ns.Count ns.Current
    { PostBody.body = (ns.Current.TypedValue :?> string).Trim()}
 

let headTemplate = """<h2><a href="{0}">{1}</a></h2>"""

let highlight (s:string) (tag:string) (pat:string)  =
    let replacer (m:Match) =
        sprintf "<%s>%s</%s>" tag m.Value tag
        
    Regex.Replace(s, pat, replacer)


let patList = [
    "GTX\s?\d+";
    "[i|I]ntel"
]

let highlightBody b = 
    patList 
    |> Seq.fold (fun acc elem -> highlight acc "b" elem) b


type Reporter(fname:string) =
    let wr = new StreamWriter(fname,false)
    member t.Offer(o) =
        let out = String.Format(headTemplate, o.offer.url, o.offer.t);
        wr.Write(out)
        t.Post(o.body)
    member t.Close() =
        wr.WriteLine("</body></html>")
        wr.Close() 

    member t.Post(b:PostBody) =

        let ``convert to html`` (text:string) = 
            text.Split [|'\n'|]
            |> String.concat "<br>"
            |> highlightBody 

     
        let wrapTag tag s = sprintf "<%s>%s</%s>" tag s tag    
        
        ``convert to html`` b.body
        |> wrapTag "p"
        |> wr.Write

        
    member t.Header() = 
        wr.Write """
        <!DOCTYPE html>
        <head><meta charset="UTF-8"></head>
        <html><body>
        """

 
 type Message = 
    Job of OfferH * AsyncReplyChannel<OfferFull>      

let crawlActor (inbox: MailboxProcessor<Message>) =
    let rec loop() = async {
        
        let! msg = inbox.Receive()
        match msg with 
        | Job(offer, replyChannel) ->
            let! cont = fetchAsync offer.url
            let full = { offer = offer; body = {body = cont} }

            printf "Reply for %A" offer.url
            replyChannel.Reply full
        
        
        return! loop()
    }
    
    loop()

let fetchAndParse offer = 
    async {
        let! cont = fetchAsync offer.url
        let parsed = parseThread cont
        let full = { offer = offer; body = parsed }
        return full
    }
    

let forums = [
    "komponentit.119", "Komponentit";
    "kannettavat-tietokoneet.121", "Kannettavat";
    "mobiililaitteet-ja-tarvikkeet.142", "Mobiili"
    "tietokoneet-ja-konepaketit.171","Tietokoneet"
    ]
   
let fetchForum slug outputFile =
    let rootUrl = sprintf "http://murobbs.muropaketti.com/forums/%s" slug 

    let cont = fetch rootUrl
    let pd = parseForum(cont, "http://murobbs.muropaketti.com/")
    //printfn "%A" pd
    let report = Reporter(outputFile)
    report.Header()
    let todo = pd
    //mailbox.Post pd.Head

    let results = Async.Parallel [for o in todo -> fetchAndParse o]

    let offers = Async.RunSynchronously results
    offers |>
    Seq.iter (fun offer -> report.Offer offer)

    report.Close()
    
let fetchAllForums targetdir =
    for slug, name in forums do
        let outputFile = sprintf "%s/%s.html" targetdir name

        fetchForum slug outputFile 
        printfn "%A" slug

let test1(args:string[]) =
    
    let hld = highlight "foo bar baz bar quux" "bar" "b"
    printfn "%A" hld
    let hg = highlightBody "a b GTX680 c Intel d e" 
    printfn "%A" hg
    1

[<EntryPoint>]
let main(args:string[]) =
    fetchAllForums args.[0]
    0   



