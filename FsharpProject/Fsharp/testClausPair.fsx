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
    let chars = seq {for i in 0..127 do if i < 97 || i > 122 then yield char(i) }
    Seq.fold removeChar str chars
  src |> toLowerCase |> removeChars

let rec reverse (lst : 'a list) : 'a list =
  match lst with
    elm :: rest -> (reverse rest) @ [elm]
    | [] -> []


/// Calculate the cumulative sum of a list of integers from the first to the last element. First element is the first number in the original list, last element is the sum of all integers in the original list.
/// 对列表遍历，第n个元素是该元素与它前面的所有元素的和
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
      List.findIndex (fun w -> w > v) monotonic //遍历列表，找出第一个大于v的元素并返回索引,注意index从0开始
    with
      _ -> monotonic.Length - 1  // 如果整个列表不存在大于v的元素，返回最后一个元素的索引

///根据概率生成字符
/// Generate a random character according to a histogram.
let rnd = System.Random() // A global object, if included in randomChar then seed point only changes slowly giving strongly correlated results in time
let randomChar (hist : int list) : char =
  let cumHist = cumSum hist //先生成一个列表，该列表任一元素是其与其前面所有元素的和
  let v = rnd.Next(cumHist.[cumHist.Length-1]) //生成的随机数v，且v的最大值不应超过经过cumSum后的列表的最大值
  let i = reverseLookup cumHist v  //遍历列表，找出列表中第一个大于随机数v的元素并返回其索引
  'a' + char i  //以'a'为基准，加上i即可得到生成的随机数

/// Generate a string of random characters each distributed according to a histogram.
/// 生成指定长度的字符串，该字符串的字符直方图应该接近但不等于源直方图
let randomString (hist : int list) (len : int) : string = 
  /// Append random characters to a string, each distributed according to a histogram.
  let rec appendRandomString (hist : int list) (len : int) (src : string) : string = 
    if len = 0 then
      src
    else
      src + string (randomChar hist) |> appendRandomString hist (len-1)

  appendRandomString hist len ""


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

// this function uses histogram generated by cooccurrence function, and generate a random string with given length
let randomCharPair (hist : int list list) (len : int) : string =
    let outterHist =
        List.fold (fun acc elem -> List.sum elem :: acc ) [] hist
    let cumOutterHist = cumSum outterHist
    let outterV = rnd.Next(cumOutterHist.[cumOutterHist.Length-1])
    let outterI = reverseLookup cumOutterHist outterV
    let firstChar = 'a' + char(outterI)    
    let rec generateStr c len = 
        if len = 0 then "" else
            let innerHist = hist.[int(c)-97]
            let cumInnerHist = cumSum innerHist
            if List.sum cumInnerHist = 0 then 
                let secondChar =char((rnd.Next())%26+97)
                string secondChar + generateStr secondChar (len-1)
            else
                let innerV = rnd.Next(cumInnerHist.[cumInnerHist.Length-1])
                let innerI = reverseLookup cumInnerHist innerV
                let secondChar = 'a' + char(innerI)
                string secondChar + generateStr secondChar (len-1)
    generateStr firstChar len


let hist = cooccurrence (convertText (readText "littleClausAndBigClaus.txt"))
let generateStr = randomCharPair hist 16316
let generatedHist = cooccurrence generateStr
printfn "%A" (List.map2 (fun elem1 elem2 -> List.zip elem1 elem2) hist generatedHist)