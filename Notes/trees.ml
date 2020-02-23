#type 'a btree = Empty | Node of 'a * 'a btree * 'a btree;;
type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

#let rec member x btree =
   match btree with
     Empty -> false
   | Node(y, left, right) ->
       if x = y then true else
       if x < y then member x left else member x right;;
val member : 'a -> 'a btree -> bool = <fun>

#let rec insert x btree =
   match btree with
     Empty -> Node(x, Empty, Empty)
   | Node(y, left, right) ->
       if x <= y then Node(y, insert x left, right)
                 else Node(y, left, insert x right);;
val insert : 'a -> 'a btree -> 'a btree = <fun>