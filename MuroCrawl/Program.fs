
open System.Net
open System.IO
open System.Xml.Linq
open System.Xml.XPath
open HtmlAgilityPack
open System
open System.Diagnostics
open System.Text.RegularExpressions
open System.Text

let pr o = 
    printfn "%A" 0
    o
    
let fetch(url:string, encoding: Encoding) = 
    let req = WebRequest.Create url
    let resp = req.GetResponse()
    use sr = new StreamReader(resp.GetResponseStream(), encoding)
    let cont = sr.ReadToEnd()
    use wr = new StreamWriter("C:/t/dump.html",false)
    wr.Write(cont)


    cont


let fetchAsync url (encoding:Encoding) = 
    async {
        let req = WebRequest.Create(Uri url)
        use! resp = req.AsyncGetResponse()
        use stream = resp.GetResponseStream()
        use reader = new IO.StreamReader(stream, encoding)
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


let fetchAndParse offer = 
    async {
        let! cont = fetchAsync offer.url Encoding.UTF8
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
   
let reportTo offers outputfile =
    let r = Reporter(outputfile)
    r.Header()
    offers
    |> Seq.iter (fun offer -> r.Offer offer)
    r.Close()

let fetchForum slug outputFile =
    let rootUrl = sprintf "http://murobbs.muropaketti.com/forums/%s" slug 

    let cont = fetch(rootUrl,Encoding.UTF8)
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

let extract (s:string) (sel:string) = 
    let doc = new HtmlDocument()
    doc.LoadHtml(s)
    let nav = doc.CreateNavigator()
    let ns = nav.Select(sel)
    ns.AsEnumerable
    
    
type Tori() = 
    member x.parseThread cont =
        let node = 
            extract cont """//div[contains(@class,"body")]"""
            |> Seq.head 
  
        
        let iso = (node.TypedValue :?> string).Trim()
        let enc = Encoding.GetEncoding("iso-8859-1")

        
              
        { PostBody.body =  iso }

    member x.fetchAndParse offer = 
        
        async {
            let enc = Encoding.GetEncoding("iso-8859-1")
            let! cont = fetchAsync offer.url enc
            let parsed = x.parseThread cont
            let full = { offer = offer; body = parsed }
            return full
        }
    

    member x.fetchTori(outputdir) =
        let cont = fetch("http://www.tori.fi/satakunta/tietotekniikka?ca=10&w=1&cg=5030&st=s&st=k&st=u&st=h&st=g", 
                    Encoding.Default)
        //*[@id="item_17363666"]/div[3]/a
        //$x('//div[@class="desc"]/a')
        let iter = extract cont """//div[@class="desc"]/a"""
        let todo = [
            for n in iter do 
            yield { url = n.GetAttribute("href",""); t = n.ToString()} ]

        let results = Async.Parallel [for o in todo -> x.fetchAndParse o]

        let offers = Async.RunSynchronously results

        sprintf "%s/ToriAtk.html" outputdir
        |> reportTo offers 
            
        printfn "%A" offers

[<EntryPoint>]
let main(args:string[]) =
    let tori = new Tori()
    tori.fetchTori args.[0]
    
    //fetchAllForums args.[0]
    0   



