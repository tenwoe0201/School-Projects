(*function from public-class-repo written by Eric Van Wyk*)
let read_file (filename:string) : string list =
  let rec read_lines channel sofar =
    try 
      let ln = input_line channel
      in read_lines channel (ln :: sofar)
    with
    | End_of_file -> sofar
    | e -> raise e
  in
  try 
    let channel = open_in filename
    in 
    let lines_in_reverse = read_lines channel []
    in List.rev lines_in_reverse
  with
  | e -> raise e

let rec explode : string -> char list = function
  | "" -> []
  | s  -> String.get s 0 :: explode (String.sub s 1 ((String.length s) - 1))

let rec implode : char list -> string = function
  | []    -> ""
  | c::cs -> String.make 1 c ^ implode cs

let d1 = "words-small.txt"
let d2 = "words-google-10000.txt"
let d3 = "words-corncob.txt"

let add_qu (word_string : string) : string = word_string ^ "qu"

let sort (char_list: char list) : char list = List.sort (fun a b -> if a > b then 1 else -1) char_list

let rec check_include (word_string : string) (string_words: string list) : bool =
  match string_words with
  | [] -> false
  | x::xs -> if sort (explode (add_qu word_string)) = sort (explode x) then true else check_include word_string xs

let rec find_word (word_string : string) (string_words: string list) : string =
  match string_words with
  | [] -> failwith "this should not happen"
  | x::xs -> if sort (explode (add_qu word_string)) = sort (explode x) then x else find_word word_string xs

let qu_quiz (file_name: string) : (string * string) list =
  let string_words = List.map String.lowercase_ascii (read_file file_name)
  in
  let rec return_list (string_word: string list) : (string * string) list =
    match string_word with
    | [] -> []
    | x::xs -> if check_include x string_words then (x,(find_word x string_words))::(return_list xs) else (return_list xs)
  in
  return_list string_words



	
