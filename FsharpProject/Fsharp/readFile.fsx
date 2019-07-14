
open System
open System.Text

let readText (filename : string) = 
  let line = 
   try
      let reader = System.IO.File.OpenText filename
      reader.ReadToEnd ()
   with
      _ -> "" // The file cannot be read, so we return an empty string
  line


// let convertText (src : string) = 
//   match src with 

//Meaningless Iter function
// let rec stringIter (f : ( char -> unit)) (str : string) =
//    match str with
//    | "" -> ""
//    | null -> null
//    | _ -> f str.[0] 
//           stringIter f str.[1..(str.Length - 1)]

//let testfunc a = printfn "%A" a
////////////

//Meaningless Map function
// let rec stringMap (f : char -> char) (str : string) = 
//     match str with
//     | "" -> ""
//     | null -> null
//     | _ -> let res = System.Text.StringBuilder str.Length
//            stringIter (fun c -> res.Append(f c) |> ignore) str
//            res.ToString()

/////////////////

//7.2 String convert

//     convertText: lambda implement
// let convertText (src : string) =
//   src |> (fun (src : string) -> String.map (fun (sc : char) -> 
//                     if List.exists (fun (c : char) -> c = sc) ['A'..'Z'] then
//                         sc+char(32) else sc)  src ) |> (fun (str:string)  ->
//                                            let chars = seq {for i in 0..127 do if i < 97 || i > 123 then yield char(i)}
//                                            Seq.fold (fun (str : string) (c : char) ->str.Replace( c |> string , "")  ) str chars)

let convertText (src : string) = 
  let toLowerCase (src : string) = 
    String.map (fun (sc : char) -> if List.exists (fun (c : char) -> c = sc) ['A'..'Z'] then
                                      sc+char(32) else sc)   src
  let removeChars (str : string) =
    let removeChar (str : string) (c : char) =
      str.Replace( c |> string , "")
    let chars = seq {for i in 0..127 do if i < 97 || i > 122 then yield char(i) }
    Seq.fold removeChar str chars
  src |> toLowerCase |> removeChars

//7.3 histogram
let histogram (src : string) =
  let chars = seq {'a'..'z'}
  Seq.map (fun c -> Seq.fold (fun acc elem -> if elem = c  then acc+1 else acc) 0 src) chars |> List.ofSeq
   


//7.5 occurrences of pairs letter
let cooccurrence (src : string) = 
  let srcList = Seq.toList src
  // write a function that split srcArray into pairs (actually, List.chunkBySize is ok) 
  let rec split lst = 
    match lst with
    | [] -> []
    | head::tail -> if tail.IsEmpty then [] else (head,tail.Head)::(split tail)

  let splitedList = split srcList

  let rec getAlphabetPair m n = 
    if m < 123 then (if n<123 then [(char(m),char(n))]@(getAlphabetPair m (n+1)) else [(char(m+1),char(97))]@(getAlphabetPair (m+1) 98)) else []
  //this ugly function is used to generate all alphabet pairs

  //delete extra ('{','a') in the list
  let ll = List.filter (fun (a,b) -> a<>char(123)) (getAlphabetPair 97 97) //97 is the decimal number of 'a' in ASCII and 122 is 'z'
  let finalRes = List.map (fun (a,b) -> List.fold (fun acc (m,n) -> if m=a&&n=b then acc+1 else acc) 0 splitedList) ll
  List.chunkBySize 26 finalRes //return a list of list


  
//7.6 
let fstOrderMarkovModel (cooc : int list list) (len : int) =









    