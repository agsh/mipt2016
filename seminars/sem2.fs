elem 4 [1..10] // true
elem 666 [1..10] // false
// написать elem
List.exists (fun x -> x = 2) [1..10]
[1..2..20]
['a'..'z']
(1,2)
("Гарри Поттер", 13, 2.5)
fst (1,2)
snd (1,2)
fst (1,2,3) // ?
let (a,b,c) = (1,2,3) //(сопоставление с образцом, 3с. ниже)
b
[("Гарри Поттер", 13, 2.5),("Колобок", 1, 55.5)]
sum для списка?
max для списка?
length для списка?
(* Опишите функцию, которая для данного числа n
создает список из всех попарных сумм чисел от
1 до n. ( Т.е. [1+1, 1+2, 1+3, ..., 1+n, 2+1, 2+2, ...,
n+n] - всего n*n элементов) *)
let sayWhat i = 
  match i with
      | 1 -> "место встречи изменить нельзя"
      | 2 -> "суббота вечер"
      | 3 -> "я ничего не знаю"
      | 4 -> "ненавижу FP, заберите меня отсюда"
sayWhat 3
sayWhat 55
let sayWhat i = 
  match i with
      | 1 -> "место встречи изменить нельзя"
      | 2 -> "суббота утро"
      | 3 -> "я ничего не знаю"
      | 4 -> "ненавижу FP, заберите меня отсюда"
      | _ -> "кто здесь?"
let sayWhat i = 
  match i with
      | 1 -> "место встречи изменить нельзя"
      | _ -> "кто здесь?"
      | 2 -> "суббота утро"
      | 3 -> "я ничего не знаю"
      | 4 -> "ненавижу FP, заберите меня отсюда"
sayWhat 4  

let opinion man = "Я, " + snd man + " считаю \"" + sayWhat (fst man) + "\""
opinion (4, "Пупкин Васёк")
let opinion (num, name) = "Я, " + name + " считаю \"" + sayWhat num + "\""    

let bmiTell weight height = 
  let c = weight / height ** 2. in
    match c with
    | a when a <= 18.5 -> "You're underweight, you emo, you!"
    | a when a <= 25.0 -> "You're supposedly normal. Pffft, I bet you're ugly"
    | a when a <= 30.0 -> "You're fat! Lose some weight, fatty!"
    | _ -> "You're a whale, congratulations!"

bmiTell 59.0 170.0

let bmiTell weight height = 
  let c = weight / height ** 2. 
  let skinny = 18.5
  let normal = 25.0
  let fat = 30.0
  match c with
    | a when a <= skinny -> "You're underweight, you emo, you!"
    | a when a <= normal -> "You're supposedly normal. Pffft, I bet you're ugly"
    | a when a <= fat -> "You're fat! Lose some weight, fatty!"
    | _ -> "You're a whale, congratulations!"

let bmiTell weight height = 
  let c = weight / height ** 2. 
  let skinny, normal, fat = 18.5, 25.0, 30.0
  match c with
    | a when a <= skinny -> "You're underweight, you emo, you!"
    | a when a <= normal -> "You're supposedly normal. Pffft, I bet you're ugly"
    | a when a <= fat -> "You're fat! Lose some weight, fatty!"
    | _ -> "You're a whale, congratulations!"
