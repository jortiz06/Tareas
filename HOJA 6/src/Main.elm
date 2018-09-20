module Main exposing (..)
type Natural
    = Suc Natural 
    | Cero 
    

type alias Model  = 
    { Sum 
    x: Int  
    
    }
    

Sum: Int -> Int -> Int 
Sum x y = 
    case (x, y) of 

