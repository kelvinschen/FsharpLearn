
let readText (filename : string) = 
  let line = 
   try
      let reader = System.IO.File.OpenText filename
      reader.ReadToEnd ()
   with
      _ -> "" // The file cannot be read, so we return an empty string
  line

let convertText (src : string) = 
  let toLowerCase (src : string) = 
    String.map (fun (sc : char) -> if List.exists (fun (c : char) -> c = sc) ['A'..'Z'] then
                                      sc+char(32) else sc)   src
  let removeChars (str : string) =
    let removeChar (str : string) (c : char) =
      str.Replace( c |> string , "")
    let chars = seq {for i in 0..127 do if i < 97 || i > 123 then yield char(i) }
    Seq.fold removeChar str chars
  src |> toLowerCase |> removeChars

//Reverse the order of a list of any type
let rec reverse (lst : 'a list) : 'a list =
  match lst with
    elm :: rest -> (reverse rest) @ [elm]
    | [] -> []

/// Calculate the cumulative sum of a list of integers from the first to the last element. First element is the first number in the original list, last element is the sum of all integers in the original list.
let cumSum (lst : int list) : int list =
  /// Prepend the sum of the first element in acc and elm to acc.
  let updateCumSum (acc : int list) (elm : int) : int list =
    match acc with
      [] -> [elm]
      | a -> a.Head + elm :: a

  List.fold updateCumSum [] lst |> reverse

/// Given a monotonic function and an index into its value set, find the corresponding value on its definition set.
let reverseLookup (monotonic : 'a list) (v : 'a) : int =
    try
      List.findIndex (fun w -> w > v) monotonic
    with
      _ -> monotonic.Length - 1

/// Generate a random character according to a histogram.
let rnd = System.Random() // A global object, if included in randomChar then seed point only changes slowly giving strongly correlated results in time
let randomChar (hist : int list) : char =
  let cumHist = cumSum hist
  let v = rnd.Next(cumHist.[cumHist.Length-1])
  let i = reverseLookup cumHist v
  'a' + char i

/// Generate a string of random characters each distributed according to a histogram.
let randomString (hist : int list) (len : int) : string = 
  /// Append random characters to a string, each distributed according to a histogram.
  let rec appendRandomString (hist : int list) (len : int) (src : string) : string = 
    if len = 0 then
      src
    else
      src + string (randomChar hist) |> appendRandomString hist (len-1)

  appendRandomString hist len ""
      
/// Generate a histogram of the characters 'a'..'z' in a given string.
let histogram (src : string) : int list =
  let chars = seq {'a'..'z'}
  Seq.map (fun c -> Seq.fold (fun acc elem -> if elem = c  then acc+1 else acc) 0 src) chars |> List.ofSeq
  
let rawtext = readText "littleClausAndBigClaus.txt"
let text = convertText rawtext
let hist = histogram text
let alphabet = List.init hist.Length (fun i -> 'a' + char i)
printfn "A histogram:\n %A" (List.zip alphabet hist)

let ranStr = randomString hist 16316
//printfn "A random string: %s" ranStr
let newHist = histogram ranStr
printfn "Resulting histogram:\n %A" (List.zip alphabet newHist)

