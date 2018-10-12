module Main exposing (..)


zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith aux lista1 lista2 = case (lista1, lista2) of 
    ([], lista2_) -> []
    (lista1_, []) -> []
    (x::xs, y::ys) -> aux x y :: zipWith aux xs ys

   

groupBy : (a -> Bool) -> List a -> (List a, List a) 
groupBy z list = (acepta z list, noAcepta z list)

acepta a list = case list of 
    [] -> []
    (x::xss) -> if a x 
                then acepta a xss 
                else x:: acepta a xss

noAcepta a list = case list of 
    [] -> []
    (b::bss) -> if a b 
                then b:: noAcepta a bss 
                else noAcepta a bss
  

bind : Maybe a -> (a -> Maybe b) -> Maybe b
bind xs b = 
  case xs of 
    Nothing -> Nothing 
    Just a -> b a