open System
open System.IO

// Define the 2 types of Document
// using a Discriminated Union.
type DocType =
    | Ham
    | Spam

// Parse a string into either case of Document,
// throwing an exception in unknown cases.
let parseDoc (label:string) =
    match label with
    | "ham"  -> Ham
    | "spam" -> Spam
    | _      -> failwith "Unknown label"

// Break a line of text into a tuple, where
// the first part is the label, the second the SMS.
// Each line is split by Tabs ('\t'),
// the first part parsed as either ham or spam,
// and the rest of the string is the SMS itself.
let parseLine (line:string) =
    let split = line.Split('\t')
    (split.[0] |> parseDoc, split.[1])

// The original can be found in the
// University of California Irvine
// Machine Learning Repository:
// http://archive.ics.uci.edu/ml/datasets/SMS+Spam+Collection
// Path to the dataset:
let path = Path.Combine(__SOURCE_DIRECTORY__, "../Data/SMSSpamCollection")

// Read the file into an array of strings,
// and parse each of them into a tuple
// (Ham or Spam, SMS).
let dataset =
    File.ReadAllLines path
    |> Array.map parseLine

open System.Text.RegularExpressions

let matchWords = Regex(@"\w+")

let wordTokenizer (text:string) =
    text.ToLowerInvariant()
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

#load "NaiveBayes.fs"
open NaiveBayes.Classifier

let validation, training = dataset.[..999], dataset.[1000..]

// Super simple model: predict based on one single token, "txt"
let txtClassifier = train training wordTokenizer (["txt"] |> set)

validation
|> Seq.averageBy (fun (l,o) ->
    if l = txtClassifier o then 1. else 0.)
|> printfn "Based on 'txt', correctly classified: %.3f"

// Even simpler model: always predict the most frequent class,
// in that case, Ham
validation
|> Seq.averageBy (fun (l,o) ->
    if l = Ham then 1. else 0.)
|> printfn "Aways Ham, correctly classified: %.3f"

// Use every single token
let tokenized =
    training
    |> Seq.map (fun (lbl,sms) -> lbl,wordTokenizer sms)

let allTokens =
    training
    |> Seq.map snd
    |> vocabulary wordTokenizer

let fullClassifier = train training wordTokenizer allTokens
validation
|> Seq.averageBy (fun (l,o) ->
    if l = fullClassifier o then 1. else 0.)
|> printfn "Based on all tokens, correctly classified: %.3f"

// generic model evaluation function
let evaluate (tokenizer:Tokenizer) (tokens:Token Set) =
    let model = train training tokenizer tokens
    validation
    |> Seq.averageBy (fun (l,o) ->
        if l = model o then 1. else 0.)
    |> printfn "Correctly classified: %.3f"

evaluate wordTokenizer allTokens

// does casing matter?
let casedTokenizer (text:string) =
    text
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

let casedTokens =
    training
    |> Seq.map snd
    |> vocabulary casedTokenizer

evaluate casedTokenizer casedTokens

// most frequent tokens

let top n (tokenizer:Tokenizer) (docs:string []) =
    let tokenized = docs |> Array.map tokenizer
    let tokens = tokenized |> Set.unionMany
    tokens
    |> Seq.sortBy (fun t -> - countIn tokenized t)
    |> Seq.take n
    |> Set.ofSeq

let ham,spam =
    let rawHam,rawSpam =
        training
        |> Array.partition (fun (lbl,_) -> lbl=Ham)
    rawHam |> Array.map snd,
    rawSpam |> Array.map snd

let hamCount = ham |> vocabulary casedTokenizer |> Set.count
let spamCount = spam |> vocabulary casedTokenizer |> Set.count

let topHam = ham |> top (hamCount / 10) casedTokenizer
let topSpam = spam |> top (spamCount / 10) casedTokenizer

let topTokens = Set.union topHam topSpam


evaluate casedTokenizer topTokens


ham |> top 20 casedTokenizer |> Seq.iter (printfn "%s")
spam |> top 20 casedTokenizer |> Seq.iter (printfn "%s")


// identifying most specific tokens

let commonTokens = Set.intersect topHam topSpam
let specificTokens = Set.difference topTokens commonTokens

evaluate casedTokenizer specificTokens


// taking a look at exceptional tokens

let rareTokens n (tokenizer:Tokenizer) (docs:string []) =
    let tokenized = docs |> Array.map tokenizer
    let tokens = tokenized |> Set.unionMany
    tokens
    |> Seq.sortBy (fun t -> countIn tokenized t)
    |> Seq.take n
    |> Set.ofSeq

let rareHam = ham |> rareTokens 50 casedTokenizer |> Seq.iter (printfn "%s")
let rareSpam = spam |> rareTokens 50 casedTokenizer |> Seq.iter (printfn "%s")


// making tokens out of phone and sms numbers

let phoneWords = Regex(@"0[7-9]\d{9}")
let phone (text:string) =
    match (phoneWords.IsMatch text) with
    | true -> "__PHONE__"
    | false -> text

let txtCode = Regex(@"\b\d{5}\b")
let txt (text:string) =
    match (txtCode.IsMatch text) with
    | true -> "__TXT__"
    | false -> text

let smartTokenizer = casedTokenizer >> Set.map phone >> Set.map txt

smartTokenizer "hello World, call 08123456789 or txt 12345"

let smartTokens =
    specificTokens
    |> Set.add "__TXT__"
    |> Set.add "__PHONE__"

evaluate smartTokenizer smartTokens


// tip: analyzing message length

let lengthAnalysis len =

    let long (msg:string) = msg.Length > len

    let ham,spam =
        dataset
        |> Array.partition (fun (docType,_) -> docType = Ham)
    let spamAndLongCount =
        spam
        |> Array.filter (fun (_,sms) -> long sms)
        |> Array.length
    let longCount =
        dataset
        |> Array.filter (fun (_,sms) -> long sms)
        |> Array.length

    let pSpam = (float spam.Length) / (float dataset.Length)
    let pLongIfSpam =
        float spamAndLongCount / float spam.Length
    let pLong =
        float longCount /
        float (dataset.Length)

    let pSpamIfLong = pLongIfSpam * pSpam / pLong
    pSpamIfLong

for l in 10 .. 10 .. 130 do
    printfn "P(Spam if Length > %i) = %.4f" l (lengthAnalysis l)


// analyzing errors

let bestClassifier = train training smartTokenizer smartTokens

validation
|> Seq.filter (fun (docType,_) -> docType = Ham)
|> Seq.averageBy (fun (docType,sms) ->
    if docType = bestClassifier sms
    then 1.0
    else 0.0)
|> printfn "Properly classified Ham: %.5f"

validation
|> Seq.filter (fun (docType,_) -> docType = Spam)
|> Seq.averageBy (fun (docType,sms) ->
    if docType = bestClassifier sms
    then 1.0
    else 0.0)
|> printfn "Properly classified Spam: %.5f"
