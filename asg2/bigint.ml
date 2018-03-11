open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let length    = List.length
    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))
    
    let rec remzeros list =
       if list = []
       then []
       else let revl = reverse list in
          if (car revl) = 0
          then remzeros (reverse (cdr revl))
          else list

    let rec comp list1 list2 = 
       let len1 = length list1
       in let len2 = length list2
       in let rev1 = reverse list1
       in let rev2 = reverse list2
       in if len1 > len2 then list1
       else if len2 > len1 then list2
       else match (list1, list2) with
           | [], []          -> []
           | list1, []       -> list1
           | [], list2       -> list2
           | list1, list2    ->
             if (car (rev1)) > (car (rev2))
             then list1
             else if (car (rev1)) < (car (rev2))
             then list2
             else comp (reverse(cdr(rev1))) (reverse(cdr(rev2)))

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> failwith "Subtraction: list1 < list2"
        | car1::cdr1, [], carry  -> 
          if car1 = 0
          then 9 :: (sub' cdr1 [] 1)
          else let dif = car1 - carry*1
              in dif :: (sub' cdr1 [] 0)
        | [], list2, carry              -> failwith "Subtraction Error"
        | car1::cdr1, car2::cdr2, carry ->
          if car2 > (car1 - carry*1)
          then let dif = ((car1 + 10) - carry*1) - car2
              in dif :: (sub' cdr1 cdr2 1)
          else let dif = (car1 - carry*1) - car2
              in dif :: (sub' cdr1 cdr2 0)

    let rec mul' list1 list2 =
         if (car list2) = 1
         then list1
         else (add' list1 (mul' list1 (sub' list2 [1] 0)) 0)
  
    let rec div' list1 list2 ans =
        if (comp list1 list2) = []
        then (ans, list1)
        else (div' (sub' list1 list2 0) list2 (add' ans [1] 0))

    let rec rem' list1 list2 =
        if (comp list2 list1) = list2
        then list1
        else rem' (sub' list1 list2 0) list2

    let rec pow' list1 list2 = 
        if (car list2) = 1
        then list1
        else (mul' list1 (pow' list1 (sub' list2 [1] 0)))

    let add (Bigint (neg1, list1)) (Bigint (neg2, list2)) =
        if neg1 = neg2
        then Bigint (neg1, add' list1 list2 0)
        else if (neg1 = Pos && neg2 = Neg)
        then (
            if (comp list1 list2) = list1 
            then Bigint(neg1, sub' list1 list2 0)
            else Bigint(neg2, sub' list2 list1 0))
        else if (neg1 = Neg && neg2 = Pos)
        then (
             if (comp list1 list2) = list1
             then Bigint(neg1, sub' list1 list2 0)
             else Bigint(neg2, sub' list2 list1 0))
        else (
                if (comp list1 list2) = list1
                then Bigint(neg1, sub' list1 list2 0)
                else Bigint(neg2, sub' list2 list1 0)
             )

    let sub (Bigint (neg1, list1)) (Bigint (neg2, list2)) =
        if (neg1 = Pos && neg2 = Pos)
        then (
            if (comp list1 list2) = list1
            then Bigint (neg1, sub' list1 list2 0)
            else Bigint (Neg, sub' list2 list1 0))
        else if (neg1 = Neg && neg2 = Neg)
        then (
            if (comp list1 list2) = list1
            then Bigint(neg1, add' list1 list2 0)
            else Bigint(Pos, sub' list2 list1 0))
        else Bigint(neg1, add' list1 list2 0)

    let mul (Bigint (neg1, list1)) (Bigint (neg2, list2)) =
        if neg1 = neg2
        then Bigint (Pos, mul' list1 list2)
        else Bigint (Neg, mul' list1 list2)

    let div (Bigint (neg1, list1)) (Bigint (neg2, list2)) =
        if (car list2) <> 0 then (
            if neg1 = neg2
            then Bigint(Pos, fst(div' list1 list2 [0]))
            else Bigint(Neg, fst(div' list1 list2 [0]))
        )
        else(printf "dc: division by zero\n"; Bigint(Pos,[0]))

    let rem (Bigint (neg1, list1)) (Bigint (neg2, list2)) = 
        if (car list2) <> 0 then (
            if neg1 = neg2
            then Bigint(neg1, snd(div' list1 list2 [0]))
            else Bigint(Neg, snd(div' list1 list2 [0]))
        )
        else (printf "dc: remainder by zero\n"; Bigint(Pos,[0]))

    let pow (Bigint (neg1, list1)) (Bigint (neg2, list2)) =
        if neg2 = Neg
        then (Bigint (Pos, []))
        else if neg1 = Pos
           then (Bigint (neg1, pow' list1 list2))
           else if rem (Bigint (Pos, list2)) (Bigint (Pos, [2])) = 
                       (Bigint (Pos, [1]))
               then (Bigint (Neg, pow' list1 list2))
               else (Bigint (Pos, pow' list1 list2))

end

