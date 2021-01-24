(* This file contains a few helper functions and type declarations
   that are to be used in Homework 2. *)

(* Place part 1 functions 'take', 'drop', 'length', 'rev',
   'is_elem_by', 'is_elem', 'dedup', and 'split_by' here. *)
let length (lst: 'a list) : int = List.fold_left (fun a b -> a + 1) 0 lst
let rec take n l = match l with
  | [] -> [] 
  | x::xs -> if n > 0 then x::take (n-1) xs else []

let rec drop n l = match l with
  | [] -> [] 
  | x::xs -> if n > 0 then drop (n-1) xs else l
let rev lst = List.fold_left (fun a b -> b::a) [] lst
let is_elem_by (f: 'a -> 'b -> bool) (x: 'b) (lst: 'a list) : bool =
  List.fold_left (fun a b -> if a then a else (f b x)) false lst
let is_elem (x: 'a) (lst: 'a list) : bool = is_elem_by (=) x lst
let dedup lst = List.fold_right (fun a b -> if is_elem a b then b else a::b) lst []
let split_by (f: 'a -> 'b -> bool) (lst: 'a list) (sep_lst : 'b list) = 
  let (a,b) = List.fold_right (fun a (x,y) -> if is_elem_by f a sep_lst then ([], x::y) else (a::x, y)) lst ([],[])
  in (a::b)
let nth lst (idx: int) = 
  let (x, l) = List.fold_left (fun (z,y) b -> if z = idx then (z + 1, b@y) else (z + 1, y)) (0, []) lst
  in
  l
type word = char list
type line = word list
let remove_empty_list (lst: char list list) : char list list = 
  List.filter (fun a -> a <> []) lst
let convert_to_non_blank_lines_of_words (lst: char list) : char list list list =
  let break_into_words (lst: char list) : char list list = 
    split_by (=) lst [','; '.'; '!';'?';';';':';'-';' ']
  in 
  let break_into_lines (lst: char list) : char list list = 
    split_by (=) lst ['\n']
  in
  let list_of_lines = remove_empty_list (break_into_lines lst)
  in
  List.map remove_empty_list (List.map break_into_words list_of_lines)
(* Some functions for reading files. *)
let read_file (filename:string) : char list option =
  let rec read_chars channel sofar =
    try 
      let ch = input_char channel
      in read_chars channel (ch :: sofar)
    with
    | _ -> sofar
  in
  try 
    let channel = open_in filename
    in 
    let chars_in_reverse = read_chars channel []
    in Some (rev chars_in_reverse)
  with
    _ -> None
type result = OK 
	    | FileNotFound of string
	    | IncorrectNumLines of int 
	    | IncorrectLines of (int * int) list
	    | IncorrectLastStanza
let check_two_equal (x: char list list) (y: char list list) = x = y
let check_last_two_lines (last_two: char list list) (first_four: char list list) : bool =
  let sorted_x = List.sort (fun a b -> if a = b then 0 else if a > b then 1 else -1) last_two
  in
  let sorted_y = List.sort (fun a b -> if a = b then 0 else if a > b then 1 else -1) first_four
  in
  (sorted_x = sorted_y)
let check_incorrect_lines (lst: char list list list) : bool = 
  (check_two_equal (nth lst 0) (nth lst 1)) && (check_two_equal (nth lst 2) (nth lst 3)) && 
  (check_two_equal (nth lst 6) (nth lst 7)) && (check_two_equal (nth lst 8) (nth lst 9)) &&
  (check_two_equal (nth lst 12) (nth lst 13)) && (check_two_equal (nth lst 14) (nth lst 15)) &&
  (check_last_two_lines ((nth lst 4)@(nth lst 5)) ((nth lst 0)@(nth lst 2))) && 
  (check_last_two_lines ((nth lst 10)@(nth lst 11)) ((nth lst 6)@(nth lst 8)))&&
  (check_last_two_lines ((nth lst 16)@(nth lst 17)) ((nth lst 12)@(nth lst 14)))
let return_incorrect_lines (lst : char list list list) : (int*int) list = 
  let y = []
  in
  let helper x = 
    let x' = if (check_two_equal (nth lst 0) (nth lst 1)) = false then ((1,2)::x) else x in
    let x'' = if ((check_two_equal (nth lst 2) (nth lst 3))) = false then ((3,4)::x') else x' in
    let x''' = if (check_two_equal (nth lst 6) (nth lst 7)) = false then ((7,8)::x'') else x'' in
    let x'''' = if (check_two_equal (nth lst 8) (nth lst 9)) = false then ((9,10)::x''') else x''' in
    let x''''' = if (check_two_equal (nth lst 12) (nth lst 13)) = false then ((13,14)::x'''') else x'''' in
    let x'''''' = if (check_two_equal (nth lst 14) (nth lst 15)) = false then ((15,16)::x''''') else x''''' in
    let x''''''' = if ((check_last_two_lines ((nth lst 4)@(nth lst 5))
                  ((nth lst 0)@(nth lst 2))) = false) && (is_elem (1,2) x'''''') = false &&
                  (is_elem (3,4) x'''''') = false
                  then ((5,6)::x'''''') else x'''''' in
    let x'''''''' = if ((check_last_two_lines ((nth lst 10)@(nth lst 11))
                    ((nth lst 6)@(nth lst 8))) = false) && (is_elem (7,8) x''''''') = false &&
                    (is_elem (9,10) x''''''') = false  
                    then ((11,12)::x''''''') else x''''''' in
    let x''''''''' = if ((check_last_two_lines ((nth lst 16)@(nth lst 17))
                    ((nth lst 12)@(nth lst 14))) = false) && (is_elem (13,14) x'''''''') = false &&
                    (is_elem (15,16) x'''''''') = false
                    then ((17,18)::x'''''''') else x'''''''' in x'''''''''
  in
  rev (helper y)
let incorrect_last_stanza (lst: char list list list) : bool =
  let (x,l) = List.fold_left (fun (y,z) b -> if y > 17 then (y + 1, z) else (y + 1, z@(nth lst y))) (0, []) lst
  in
  let l_dedup = dedup l
  in
  let m_dedup = dedup ((nth lst 18)@(nth lst 19)@(nth lst 20)@(nth lst 21)@(nth lst 22)@(nth lst 23))
  in
  let filter1 = List.filter (fun a -> is_elem a l_dedup) m_dedup
  in
  let filter2 = List.filter (fun a -> is_elem a m_dedup) l_dedup
  in
  (length filter1 = length m_dedup) && (length filter2 = length l_dedup)
let paradelle (fname: string) : result = 
  let charlst = read_file fname
  in
  match charlst with
  | None -> FileNotFound fname
  | Some lst ->  
          let lower_case_list_1 = List.map Char.lowercase_ascii lst
          in
          let lower_case_list = convert_to_non_blank_lines_of_words lower_case_list_1
          in
          if length lower_case_list <> 24 
          then IncorrectNumLines (length lower_case_list)
          else if (check_incorrect_lines lower_case_list) = false 
          then IncorrectLines (return_incorrect_lines lower_case_list)
          else if (incorrect_last_stanza lower_case_list) = false
          then IncorrectLastStanza
          else OK
  

