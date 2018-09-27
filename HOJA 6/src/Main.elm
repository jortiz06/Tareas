module Main exposing (..)


type Natural = Suc Natural | Cero

enteroANatural : Int -> Natural
enteroANatural i = if i == 0
    then Cero
    else Suc (enteroANatural (i - 1))


naturalAEntero : Natural -> Int
naturalAEntero n = case n of
    Cero -> 0
    Suc i -> 1 + naturalAEntero i



sumar : Natural -> Natural -> Natural
sumar a1 b1 = case (a1,b1) of
    (a1_, Cero) -> a1_
    (Cero, b1_) -> b1_
    (Suc a1_, b1_) -> Suc (sumar a1_ b1_)

multip : Natural -> Natural -> Natural
multip a1 b1 =
    case (a1,b1) of
        (a1_, Cero) -> Cero
        (Cero, b1_) -> Cero
        (Suc a1_, b1_) -> sumar b1_ (multip a1_ b1_)
