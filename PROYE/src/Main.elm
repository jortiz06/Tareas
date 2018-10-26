module Main exposing (main)

import Browser
import Canvas
import CanvasColor as Color exposing (Color)
import Html exposing (Html)
import Html.Attributes exposing (class, style)

-- Las coordenadas de cada una de las esquinas del
-- poligono que se dibujara


fractal = [
    (500, 300),
    (700, 700),
    (300, 700)]


koch reps = fractal

sierpinski reps = case fractal of 
    b1::b2::b3::bs -> agregar b1 b2 b3 ++ bs
    _ -> fractal


agregar (x1,y1) (x2,y2) (x3,y3) =  [(1/2*(x1 + x2), 1/2*(y1+y2)) , (1/2*(x2 + x3), 1/2*(y2 + y3)), (1/2*(x1+x3), 1/2*(y1+y3)), (1/2*(x1 + x2), 1/2*(y1+y2))] 




-- Dada una lista de coordenadas, esta funcion
-- genera los comandos necesarios para dibujar
-- las lineas que connectan dichas coordenadas
dibujar triangulo context =
    let
        acc (x,y) s = s |> Canvas.lineTo x y
    in
        case triangulo of
            (x0,y0)::xs ->
                List.foldl acc (Canvas.moveTo x0 y0 context) xs
                |> Canvas.lineTo x0 y0
            _ -> context


-- Funcion que genera el html que corresponde al
-- poligono siendo dibujado
view : Html msg
view =
    let
        width = 1000
        height = 1000
        poligono ctx = dibujar (koch 5) ctx
        frakt ctx = dibujar (sierpinski 9) ctx 
    in
        Canvas.element
            width
            height
            [ style "border" "5px solid yellow" ]
            (
                Canvas.empty
                |> Canvas.clearRect 0 0 width height
                |> poligono
                |> frakt
                |> Canvas.stroke
            )

main = view