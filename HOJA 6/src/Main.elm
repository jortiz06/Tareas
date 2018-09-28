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

resta : Natural -> Natural -> Natural
resta a1 b1 = case (a1,b1) of
    (a1_, Cero) -> a1_
    (Cero, b1_) -> Cero
    (Suc a1_, Suc b1_) -> resta a1_ b1_

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

division : Natural -> Natural -> (Natural,Natural)
division a1 b1 = contador (a1)(b1)(Cero)
contador a1 b1 c1 =
    if a1== (Suc(Cero))  && b1 == (Suc(Cero)) then ((Suc c1, Cero))
    else if resta a1 b1 == Cero then (b1, c1)
    else contador (resta a1 b1) (b1) (Suc c1) 
    