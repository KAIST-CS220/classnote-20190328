/// Python
// class ConsCell:
//     def __init__(hd, tl):
//         self.head = hd
//         self.tail = tl
//
//     def cons(elt):
//         ConsCell(elt, self)
//
//     def iter():
//
//
// /// C
// struct ConsCell {
//    int head;
//    struct ConsCell* tail;
// }

type IntList =
  | Nil
  | Cons of int * IntList

// [ 1; 2 ; 3 ]
let lst =
  Cons (1, Cons (2, Cons (3, Nil)))

let empty = Nil
let cons elt lst = Cons (elt, lst)
let car = function
  | Nil -> failwith "Impossible!!!"
  | Cons (hd, _) -> hd
let cdr = function
  | Nil -> failwith "Impossible!!!"
  | Cons (_, tl) -> tl

// [ 1; 2 ; 3 ]
// ====          1 :: 2 :: 3 :: []
let lst2 =
  // Cons (1, Cons (2, Cons (3, Nil)))
  cons 1 (cons 2 (cons 3 empty))

let (++) elt lst = cons elt lst

let lst3 = (1 ++ (2 ++ (3 ++ empty)))

let (^+^) elt lst = cons elt lst

let lst4 = 1 ^+^ 2 ^+^ 3 ^+^ empty

type List<'a> =
  | Empty
  | ConsCell of 'a * 'a List

let empty2 = Empty
let cons2 elt lst = ConsCell (elt, lst)
let car2 lst =
  match lst with
  | Empty -> failwith "NO!"
  | ConsCell (hd, _) -> hd
//

// [1 ; 2; 3 ]
let lst5 =
  ConsCell (1, Empty)

let lst6 =
  ConsCell ("abc", Empty)

let lst7 =
  ConsCell (1, ConsCell ("Abc", Empty))

let isEmpty lst = lst = Empty

let rec append l1 l2 =
  match l1 with
  | [] -> l2
  | hd :: tl -> hd :: (append tl l2)

// let rec append acc l1 l2 =
//   match l1 with
//   | [] -> acc @
//   | hd :: tl -> append ()

[<EntryPoint>]
let main argv =
    0 // return an integer exit code
