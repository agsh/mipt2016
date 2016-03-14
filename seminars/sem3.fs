// 1) Целочисленный остаток от деления: rem'
// 2) Значение целочисленного деления: quot'
let sign = function
  | a when a > 0 -> 1
  | a when a = 0 -> 0
  | _ -> -1

let rem a b =
  let rec rem' a b = 
    match (a-b) with
    | c when c < 0 -> a
    | _ -> rem' (a-b) b
  sign a * sign b * rem' (abs a) (abs b)

// Можно сократить по высоте
let rem a b =
  let rec rem' a b = match (a-b) with | c when c < 0 -> a | _ -> rem' (a-b) b;
  sign a * sign b * rem' (abs a) (abs b)

let rec quot' a b =
  if a < b then 0 else 1 + quot' (a-b) b

 
//Наибольший общий делитель: gcd
   
let rec gcd a b = 
  match (a,b) with
    | (a,b) when a = b -> a
    | (a,b) when a > b -> gcd (a-b) b
    | (a,b) when b > a -> gcd a (b-a) 

let rec gcd a b = 
  match (a,b) with
    | (a,b) when a = b -> a
    | (a,b) when a > b -> gcd (a-b) b
    | _ -> gcd a (b-a)


// replicate i:int -> a:'a -> 'a list
// List.replicate

let rec replicate i a =
  match i with 
  | i when i < 0 -> []
  | _ -> a :: (replicate (i-1) a)

// zip a:'a list -> b:'b list -> ('a * 'b) list List.zip

let rec zip a b =
  match (a,b) with
  | ([],_) | (_,[]) -> []
  | (x::xs, y::ys) -> (x,y) :: zip xs ys 

Seq.zip (Seq.initInfinite (fun x->x)) (seq ["a";" b"; "c"])

// append a:'a list -> b:'a list -> 'a list @

let rec append a b =
  match a with
  | [] -> b
  | (x::xs) -> x :: (append xs b)

// factorial
let rec fac = function
  | n when n <= 0 -> 1
  | n -> (n * fac (n-1))

let rec fac = 
  let rec fac' acc = function
    | n when n <= 0 -> acc
    | n -> fac' (acc * n) (n-1)
  fac' 1

// reverse  'a list -> 'a list List.rev

let reverse<'a> = 
  let rec reverse' acc = function
    | [] -> acc
    | (x::xs) -> reverse' (x::acc) xs
  reverse' []

// fib - через аккумулятор сами

let rec private fib' a b = function
  | 0 -> a
  | n -> fib' b (a+b) (n-1)

let fib = fib' 1 1

let explode (s:string) = [for c in s -> c]
explode "sfsdaf"

(*
Функция delete :: Char -> String -> String, кото-
рая принимает на вход строку и символ и возвращает
строку, в которой удалены все вхождения символа. При-
мер: delete ’l’ "Hello world!" должно возвращать "Heoword!".
Функция substitute :: Char -> Char -> String -> String,
которая заменяет в строке указанный символ на заданный.
Пример: substitute ’e’ ’i’ "eigenvalue" возвращает
"iiginvalui" 
*)
