(*
Write a recursive function which verifies the balancing of parentheses
in a string, which we represent as a List[Char] not a String. For
example, the function should return true for the following strings:
(if (zero? x) max (/ 1 x))
I told him (that it’s not (yet) done). (But he wasn’t listening)
The function should return false for the following strings:
:-)
())(
The last example shows that it’s not enough to verify that a string
contains the same number of opening and closing parentheses.
balance :: String → Bool
*)

reverseAll — функция, получающая на вход
списочную структуру и обращающая все её
элементы, а также её саму.
firstElem — функция, возвращающая номер первого
вхождения заданного атома в список.

set — функция, возвращающая список из
всех атомов, содержащихся в заданном
списке. Каждый атом должен
присутствовать в результирующем списке в
единственном числе.

freq — функция, возвращающая список пар
(символ, частота). Каждая пара определяет
атом из заданного списка и частоту его
вхождения в этот список.

(+) 9 5 

let sum x y = (+) x y // (int -> int -> int)
let sum = (+)

(+) (9 5)  // ?
((+) 9) 5  // ?

let sumWith9 = (+) 9 // (int -> int)

sumWith9 5
let res = sumWith9 90 // int

let doItAgain f x = f (f x) // f:('a -> 'a) -> x:'a -> 'a

doItAgain sumWith9 3 // 21

// сами
zipWith ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
zipWith (+) [1; 2; 3; 4] [4; 3; 2; 1]  // [5, 5, 5, 5]
zipWith (+) ["CSKA"; "Zenith"] ["champion"; "champion"]

// f:('a -> 'b -> 'c) -> y:'b -> x:'a -> 'c
let flip f y x = f x y

flip zip [1;2;3;4;5] (explode "hello") // [('h',1),('e',2),('l',3),('l',4),('o',5)]

// ФВП



// f:('a -> 'b) -> _arg1:'a list -> 'b list
let rec map f = function
  | [] -> []
  | (x::xs) -> f x :: map f xs

map (fun x -> x + 3) [1;5;3;1;6] 
map (replicate 3) [3..6]
map (map (fun x -> x ** 2.)) [[1.;2.];[3.;4.;5.;6.];[7.;8.]]
map fst [(1,2);(3,5);(6,3);(2,6);(2,5)]


//f:('a -> bool) -> _arg1:'a list -> 'a list
let rec filter f = function
  | [] -> []
  | (x::xs) when f x -> x :: filter f xs
  | (x::xs) -> filter f xs

filter (fun x -> x > 3) [1;5;3;2;1;6;4;3;2;1]
filter (fun x -> x = 3) [1;2;3;4;5]
filter (fun x -> x <> []) [[1;2;3];[];[3;4;5];[2;2];[];[];[]]
filter (fun x -> List.exists (fun y -> x = y) ['a'..'z']) (explode "u LaUgH aT mE BeCaUsE I aM diFfeRent")
filter (fun x -> List.exists (fun y -> x = y) ['A'..'Z']) (explode "i lauGh At You BecAuse u r aLL the Same")

let sum x y = x + y
let sum = fun x y -> x + y

Список треугольных чисел
Список пирамидальных чисел
n-е треугольное число tn равно количеству
одинаковых монет, из которых можно построить
равносторонний треугольник, на каждой стороне
которого укладывается n монет.
n-е пирамидальное число pn равно количеству
одинаковых шаров, из которых можно построить
пирамиду с треугольным основанием, на каждой
стороне которой укладывается n шаров


(*
Сегодня речь пойдет о чрезвычайно общей и постоянно применяющейся в функциональном программировании технике - о свертках и префиксных суммах.
Мы изучим, что они собой представляют и какими бывают, увидим их многочисленные применения и поймем, зачем может быть полезно выразить алгоритм в виде свертки или префиксной сумме.
Рассмотрим вновь несколько рекурсивных функций обработки списков:
*)

let rec sum = function
  [] -> 0
  | h::t -> h + (sum t)

let rec mn = function
  [] -> 0
  | h::t -> h :: (mn t)

let rec concat = function
  [] -> []
  | h::t -> h @ (concat t)

let rec map f = function
  [] -> []
  | h::t -> (f h)::(map f t)

let rec filter f = function
  [] -> []
  | h::t when (f h) ->  h::(filter f t)
  | h::t -> filter f t

let rec any f = function
  [] -> false
  | h::t -> (f h) || (any f t)

let rec all f = function
  [] -> true
  | h::t -> (f h) && (any f t)

(*Все эти функции вычисляют формулу вида x1 # (x2 # (x3 # (... # u))
Например:
    sum:            +,      0
    min-list:       min,    0 (лучше бы -inf)
    concat:         append, ()
    map:            cons,   ()
    filter:         (\x r. if (p x) then (cons x r) else r), ()
    any:            (\x r. (p x) || r),         #f
    all:            (\x r. (p x) && r),         #t 
Формулы такого вида над списками называются "правыми свертками". 
А реализовать эту формулу можно так: *)

let rec foldr f a list =
  match list with
  [] -> a
  | h::t -> f h (foldr f a t)

let sum = foldr (+) 0
sum [1..6]

let filter f = foldr (fun x a -> if f x then x::a else a) []
filter (fun x -> x%2=0) [1..20]


// Теперь рассмотрим несколько итеративных функций:

let sum =
  let rec loop a = function
    [] -> a
    |h::t -> loop (h+a) t
  loop 0

let mn =
  let rec loop a = function
    [] -> a
    | h::t -> loop (h::a) t
  loop 0

let concat (list:'a list list) =
  let rec loop a = function
    [] -> a
    |h::t -> loop (a@h) t
  loop [] list

concat [[1..5];[6..7]]

let reverse (list:'a list) =
  let rec loop a = function
    [] -> a
    | h::t -> loop (h::a) t
  loop [] list
reverse [1..9]

(*
Все они вычисляют формулу вида
    (((u # x1) # x2) # .. ) # xn
Например:
  sum:            +,      0
    min:            min,    0 (лучше бы -inf)
    dl-concat:      dl-append, dl-empty
    reverse:        cons,   ()
    any:            (\r x. r || (p x)),         #f
    all:            (\r x. r && (p x)),         #t
  Формулы такого вида над списками называются "левыми свертками".
  Вот общая функция, вычисляющая левую свертку:
*)

let foldl f a list =
  let rec loop a = function
    [] -> a
    |h::t -> loop (f a h) t
  loop a list

let sum = foldl (+) 0

let reverse list = foldl (fun a x -> x::a) [] list
reverse [1..9]

(*
Эти две процедуры не просто реализуют один и тот же алгоритм ("свертку") двумя разными способами - это два разных алгоритма, предназначенных для разных целей.
Первый из них (левая свертка) - это итеративный алгоритм, начинающий с некоторого начального значения и модифицирующий его с помощью каждого элемента списка. В императивном языке ему соответствует цикл:
    Result res = u;
    for(Value v in list) {
        res #= v;
    }
    return res;
Именно этот распространенный паттерн абстрагируется левой сверткой.
Он основан на том, что известно правило изменения ответа при дописывании элемента в конец списка.
Например:
    При дописывании элемента в конец списка его сумма увеличивается на этот элемент
    При дописывании элемента в конец списка к его reverse приписывается этот элемент
Итеративность левой свертки основана на том, что можно итеративно представить список в виде последовательности дописываний элемента в конец, начиная с пустого списка.
*)

// Хвостовая рекурсия
let rec fac' = function
  0 -> 1
  | x -> x * fac' (x-1)
  
let rec fac'' x =
  if x <= 0I then 1I
  else x * fac'' (x-1I)

fac'' 1000000I

let fac x =
  let rec fac x acc = 
    if x <= 0I then acc
    else fac (x - 1I) (acc * x)
  fac x 1I

fac 1000000I

(*Правая же свертка - рекурсивный алгоритм, строящий составное значение из головы списка и ответа для хвоста списка.
В данном случае известно правило изменения ответа при приписывании элемента в начало списка.
Например:
    При приписывании элемента в начало списка его сумма увеличивается на этот элемент
    При приписывании элемента в начало списка списков к его конкатенации приписывается этот элемент
В общем случае простого и эффективного способа преобразования между этими двумя правилами не существует (нельзя легко понять, как изменяется результат от дописывания в конец, зная, как изменяется результат от дописывания в начало, и наоборот).*)

foldl (+) 0 [1..5]
foldl (-) 0 [1..5] // -15 = 0-1-2-3-4-5
foldr (-) 0 [1..5] // 3 = 1-(2-(3-(4-(5-0))))

List.foldBack (-) 0 [1..5]
(*
Однако в некоторых случаях результаты левой и правой свертки с одной и той же операцией и начальным значением - совпадают. В каких же?
    (((u # x1) # x2) # .. ) # xn
    x1 # (x2 # (x3 # (... # u)))
Записав эти формулы для списков из 1 или 3 элементов, получаем:
    forall x, u#x = x#u  - следовательно, во-первых, операция # должна оперировать над аргументами одинакового типа, а во-вторых, u должно коммутировать с каждым элементом этого типа.
    Отсюда НЕ следует, что u должно быть единицей для # - однако очень часто выбирают u именно таким образом. Рассмотрим именно этот случай.
    forall a b c, ((u#a)#b)#c = a#(b#(c#u)) --> (a#b)#c = a#(b#c) - т.е. операция # должна быть ассоциативна.
  Эти два условия достаточны (но, если u не единичный элемент, не необходимы) для того, чтобы результаты левой и правой свертки совпадали.
(вскоре мы увидим, что если функция выражается с *разной* операцией #, но с одинаковым значением u для левой и правой свертки, то имеет место гораздо более сильное свойство - Третья теорема о гомоморфизмах.)
 
 Помимо сверток над списками часто используются также "бегущие свертки" (scan), называемые "префиксными суммами": например, "бегущая сумма", "бегущий минимум". Их можно интерпретировать как вычсления последовательности промежуточных результатов свертки.
   *)

List.scanBack (+) [1..5] 0
List.scan (+) 0 [1..5]

(* Генерация списков *)
let list1 = [1 .. 10]       // val it : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
let list2 = [1 .. 2 .. 10]  // val it : int list = [1; 3; 5; 7; 9]
let list3 = ['a' .. 'g']    // val it : char list ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g';]

// Ключевая конструкция yield или -> для генерации списков
let list4 = [ for a in 1 .. 10 do yield (a * a) ] // val it : int list = [1; 4; 9; 16; 25; 36; 49; 64; 81; 100]

// Несколько элементов
let list5 = 
  [ for a in 1 .. 3 do
      for b in 3 .. 7 do
        yield (a, b) ]
// val it : (int * int) list = [(1, 3); (1, 4); (1, 5); (1, 6); (1, 7); (2, 3); (2, 4); (2, 5); (2, 6); (2, 7); (3, 3); (3, 4); (3, 5); (3, 6); (3, 7)]

// Синтаксический сахар для замены do-yield: ->
let list6 = 
  [for a in 1..3 do
    for b in 4..6 ->
      (a, b) ]

// генерация списка с условием
let list7 = [ for a in 1 .. 100 do 
                if a % 3 = 0 && a % 5 = 1 then yield a]
// val it : int list = [6; 21; 36; 51; 66; 81; 96]

// для любых перечислимых типов
let list8 = [for a in ['a'.. 'f'] do yield [a; a; a] ]
// val it : char list list = [['a'; 'a'; 'a']; ['b'; 'b'; 'b']; ['c'; 'c'; 'c']; ['d'; 'd'; 'd']; ['e'; 'e'; 'e']; ['f'; 'f'; 'f']]

// yield! используется для генерации одновременно нескольких элементов
let list9 = [for a in 1 .. 5 do yield! [ a .. a + 3 ] ] // val it : int list = [1; 2; 3; 4; 2; 3; 4; 5; 3; 4; 5; 6; 4; 5; 6; 7; 5; 6; 7; 8]

// для генерации списка можно использовать различные возможности языка
let list10 = 
  [
    let thisIsIt = "!"
    for a in 1 .. 5 do
      match a with
      | 3 -> yield! ["hello"; "world"; thisIsIt]
      | _ -> yield a.ToString()
  ]
// val it : string list = ["1"; "2"; "hello"; "world"; "!"; "4"; "5"]


// последовательности задаются похожим на списки образом
let seq1 = seq {1..78}
seq1 |> Seq.iter (printfn "%d")
// если нужно задать последовательность перечислением элементов, используются квадратные скобки
let seq2 = seq [1; 2; 9]
// последовательность может быть сколь угодно большой, т.к. элементы последовательности высчитываются только тогда, когда они требуются
let seq3 = seq { 1I .. 1000000000000I }
let list11 = [1I .. 1000000000000I ]

// ещё один пример
let seq4 =
    seq { for a in 1 .. 10 do
            printfn "created: %i" a
            yield a }
// val seq4 : seq<int>, 
seq4 |> Seq.take 3
(*
а вот когда нам нужен 3-ий элемент последовательности, начинаются вычисления
created: 1
created: 2
created: 3
val it : seq<int> = seq [1; 2; 3]
*)


// ещё один пример сравнения последовательностей и списков
let list12 = [for a in 5..-1..0 -> 10 / a] // ошибка деления на ноль
let seq5 = seq {for a in 5..-1..0 -> 10 / a} // ошибок нет
seq5 |> Seq.take 5 |> Seq.iter (printfn "%i") // ошибок нет
seq5 |> Seq.take 6 |> Seq.iter (printfn "%i") // 2, 2, 3, 5, 10, ошибка деления на ноль

// бесконечные последовательности
// чётные числа
let seq6 =
  let rec seq6' x = seq { yield x; yield! seq6' (x + 2) } // рекурсивное создание последовательности
  seq6' 0

Seq.take 10 seq6 // val it : seq<int> = seq [0; 2; 4; 6; ...]

// пример из самостоятельной работы
let seq7 = 
    let rec seq7' x = seq { yield! [0; x]; yield! seq7' (x + 1) }
    seq7' 1;;
Seq.take 10 seq7 // val it : seq<int> = seq [0; 1; 0; 2; ...]

let rec ones' = 1 :: ones' // бесконечный список из единиц как в Haskell'е не сделать..
let rec ones = seq {yield 1; yield! ones} // но его можно реализовать так
Seq.take 100 ones // val it : seq<int> = seq [1; 1; 1; 1; ...]
let ones'' = Seq.initInfinite (fun _ -> 1) // или так, с использованием функции initInfinite

(* 
Генерация последовательностей с использованием функции Seq.unfold
тип этой функции ('a -> ('b * 'a) option) -> 'a -> seq<'b>
первый аргумент - функция для генерации нового элемента последовательности из состояния, она должна возвращать известный нам тип option, заключающий в себе кортеж из этого самого нового элемента и нового состояния
второй аргумент - начальное состояние
*)
// создаём список из чётных чисел меньше ста
let seq8 = Seq.unfold (fun state -> if state < 100 then Some(state, state+2) else None) 2
seq8 |> Seq.iter (printf "%i ")

// генерация бесконечной последовательности чисел Фиббоначе
let fibb = Seq.unfold (fun state -> Some(fst state + snd state, (snd state, fst state + snd state))) (0,1)
Seq.take 20 fibb

let rec cc amount coins =
  if amount = 0
  then 1
  else 
    if (amount < 0 || List.length coins = 0)
    then 0
    else cc amount (List.tail coins) + cc (amount - List.head coins) coins

cc 100 (List.rev [50; 25; 10; 5; 1])

type Anniversary =
 Birthday of string * int * int * int
 | Wedding of string * string * int * int * int
 | Death of string * int * int * int

Birthday ("someone", 2014, 5, 4)
// Birthday "someone" 2012 11 7
let today = Birthday ("someone", 2014, 5, 4)
// today
// Birthday "someone" 2012 11 7


let (kurtCobain : Anniversary) = Birthday ("Kurt Cobain", 1967, 2, 20)
let (kurtWedding : Anniversary) = Wedding ("Kurt Cobain", "Courtney Love", 1990, 1 ,12)
let anniversaries = [
    kurtCobain;
    kurtWedding;
    Death ("Kurt Cobain", 1994, 4, 5)
]

let showDate (y: int) m d = y.ToString() + "." + m.ToString() + "." + d.ToString()

let showAnniversary = function
  Birthday (name, year, month, day) -> name + " born " + showDate year month day // синеньким
  | Wedding (name1, name2, year, month, day) ->
      name1 + " married " + name2 + " on " + showDate year month day
  | Death (name, year, month, day) -> name + " dead in " + showDate year month day

let who = function
 Birthday (him, _, _, _) -> him 
 | Wedding (him, _, _, _, _) -> him
 | Death (him, _, _, _) -> him

List.map who anniversaries
(*
1) Kurt Cobain born 1967-2-20
2) Kurt Cobain married Courtney Love on 1990-1-12
3) Kurt Cobain dead 1994-4-5
*)

type Point = { x  : float; y : float }
              
let a = { x = 13.22 ; y = 8.99 }
let b = { a with y = 666.13 }
let absPoint a = sqrt (a.x*a.x + a.y*a.y)

Some 1
None
Some "str" // type?
Some 42
Some 42 : [Nothing]  // ?
Just 42 : [Just "str", Nothing] // ?

type Option<'a> =
  Some of 'a
  | None

type 'a List =  // haskell!!!
  Nil
  | Cons of 'a * ('a List)

let l1 = Cons (3, (Cons (4, (Cons (5, Nil)))))

let rec apply x y = match x with
  Nil -> y
  | Cons (head, tail) -> Cons (head, apply tail y)

apply l1 (Cons (1, Nil))

type 'a Tree =
  EmptyTree
  | Node of 'a * 'a Tree * 'a Tree

let singleton x = Node (x, EmptyTree, EmptyTree)

let rec treeInsert x = function
  EmptyTree -> singleton x 
  | Node (a, left, right) -> 
    if x = a then Node (x, left, right) 
    else 
      if x < a then Node (a, (treeInsert x left), right) 
      else Node (a, left, (treeInsert x right))
// when 'a : comparsion

let list2tree list =
 let rec l2t acc = function
   [] -> acc
   | (head::tail) -> l2t (treeInsert head acc) tail
 in l2t EmptyTree list

list2tree [12; 1; 6; 4; 90; 9]

tree2list // сами

let rec tree2list = function
  EmptyTree -> []
  | Node (v, left, right) -> List.append (tree2list left) (v :: tree2list right)

let treesort x = x |> list2tree |> tree2list

treesort [12; 1; 6; 4; 90; 9]

list2tree [12, 12, 12, 13, 13, 14]
Как будет выглядеть дерево?

data Tree a = EmptyTree | Node a Int (Tree a) (Tree a) deriving (Show, Read, Eq)
singleton :: a -> Tree a
singleton x = Node x 1 EmptyTree EmptyTree
treeInsert :: (Ord a) => a -> Tree a -> Tree a 
treeInsert x EmptyTree = singleton x
treeInsert x (Node a i left right) 
 | x == a = Node x (i+1) left right 
 | x < a = Node a i (treeInsert x left) right 
 | x > a = Node a i left (treeInsert x right)
list2tree :: (Ord a) => [a] -> Tree a
list2tree = l2t EmptyTree
 where
 l2t acc [] = acc
 l2t acc (head:tail) = l2t (treeInsert head acc) tail
