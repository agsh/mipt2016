http://fsharp.org/use/linux/
2 + 2 // val it : int = 4
6 / 5 // val it : int = 1
6 / -5 // val it : int = -1
true || false // val it : bool = true
true && false // val it : bool = false
not false // val it : bool = true
2 * 2 = 4 // val it : bool = true
2 * 2 <> 5 // val it : bool = true
2 = "два"
// https://msdn.microsoft.com/en-us/library/dd233230.aspx
123213123;;
333333333333333333333333333333333333333333333333333333333333333
333333333333333333333333333333333333333333333333333333333333333I
6. / 5. // val it : float = 1.2
42 + 3.4
let v = 42. + 3.6
1.1 + (float 2)
9. - v
max 1 2
2. + max 2.5 5.6 + min 3.4 (max 2.3 4.5)
2 - 2 - 2
// Напишите выражение, высчитывающее минимальное число среди 10, 2, 5, -1, 99
let inc x = x + 1 // val succ : int -> int
succ 1
let dec x = x - 1
let dec x = x - 1. // -.
let rec fac n = if n = 0 then 1 else n * fac (dec n)
fac 5
let square x = x * x
let squareIfLess5 x = if x < 5 then square x else x
let square (x:float) = x * x
let squareIfLess5 x = if x < 5. then square x else x
let m = "Марьванна" // val it : string = "Марьванна"
let h = "Hello"
let hm  = h + " " + m
// prfix infix
(-) 3 4
let add x y = x + y
2 |>add<| 2
let (++) x y = x + y
2 ++ 3
let lostNumbers = [4;8;15;16;23;42]
let cards = [3; 7; 12]
let mix = lostNumbers @ cards
List.concat [[1;2;3];[4;5]]
let lostNumbers = [13;666]
mix // ?
[1; "Гарри Поттер"; 16.5] // ?
[] // val it : 'a list = []
[[1; 2]; [3; 4]] // val it : int list list = [[1; 2]; [3; 4]]
[[1; 2]; [3; 4]] @ [[5; 6]] // int list list
[[]; []] // 'a list list
[1; [2; 3; 4]]
let fcards = 1 :: cards
[3] :: [1; 3; 4]
[3] :: [[5; 6]] // int list list
mix.[3] // !!
List.nth mix 3
13 :: [] // ?
List.head [1..5]
[1..2..5]
List.head []
List.head [13]
List.tail [1..5] //[2, 3, 4, 5]
List.length [2..6] // 5
List.isEmpty [1..5]
List.isEmpty []
List.empty = []
let isEmpty x = if x = List.empty then true else false // 'a list -> bool when 'a : equality
let isEmpty x = x = List.empty
let isEmpty x =
  match x with
  | [] -> true
  | _ -> false
let isEmpty = function // наперёд
  | [] -> true
  | _ -> false
let rec length xs = 
  if List.isEmpty xs
  then 0
  else 1 + length (List.tail xs)
length [1..5]
let rec length xs =
    match xs with
    | []    -> 0
    | y::ys -> 1 + length ys
Seq.take 4 [1..10]
[1, 2, 3, 4]
Seq.take 4 (Seq.initInfinite (fun i -> i + 1)) // наперёд
// take - самостоятельно
// получение элемента самостоятельно
let rec get xs n = if n = 0 then List.head xs else get (List.tail xs) (n - 1)
List.min [1..10]
