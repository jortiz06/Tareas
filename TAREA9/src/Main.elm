import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Basics exposing (..)



main =
  Browser.sandbox { init = init , update = update, view = view }



type alias Calculadora = 
  { suma : Int -> Int -> Int
  , multi : Int -> Int -> Int
  }

calculadora : Calculadora 
calculadora = 
  { suma = (\x y -> x + y)
  , multi = (\x y -> x * y)
  }

type alias Modelo = 
  { display : String 
  , function : Int -> Int -> Int
  , saveValue : Int
  , append : Bool 
  }

init : Modelo
init =
  { display = ""
   , function = (\x y -> y)
   , saveValue = 0
   , append = True
  }

parseInt : String -> Int
parseInt input =
  Maybe.withDefault 0 (String.toInt input)

operacion : Modelo -> (Int -> Int -> Int) -> Modelo
operacion model function =
  {model 
    | function = function 
    , saveValue = (parseInt model.display)
    , append = False
  }



type Msg = N | Suma | Multi | Igual | Cero | Numero Int | Limpiar

update : Msg -> Modelo -> Modelo
update msg modelo =
  case msg of
    N ->
      modelo

    Limpiar ->
      init 
    
    Numero numero ->
      updateDisplay modelo numero 
    
    Cero -> 
      cero modelo 

    Suma ->
      operacion modelo calculadora.suma 

    Multi -> 
      operacion modelo calculadora.multi
    
    Igual -> 
      igual modelo 
  
updateDisplay : Modelo -> Int -> Modelo
updateDisplay model number = 
  if model.append then 
    {model | display = model.display ++ Debug.toString (number)}
  else 
    { model | display = number |> Debug.toString , append = True}



igual : Modelo -> Modelo 
igual modelo = 
  if modelo.append then
    {modelo 
      | display = calcular modelo 
      , saveValue = modelo.display |> parseInt
      , append = False 
    }
  else 
  {modelo | display = calcular modelo, append = False }


calcular : Modelo -> String 
calcular modelo =
  modelo.function modelo.saveValue (parseInt modelo.display) |> Debug.toString 

cero : Modelo -> Modelo 
cero modelo = 
  if String.isEmpty modelo.display || not modelo.append then
    {modelo | display = "0" , append = False}
  else 
    {modelo | display = modelo.display ++ "0"}



calculadoraButton : Msg -> String  -> Html Msg
calculadoraButton msg buttonText =
  button 
    [class "button"
    , onClick msg
    ]
    [span [] [ buttonText |> text ] ]

calculadoraButtonWide : Msg -> String  -> Html Msg
calculadoraButtonWide msg buttonText =
  button 
    [class "button wide"
    , onClick msg
    ]
    [span [] [ buttonText |> text ] ]


stylesheet : Html a 
stylesheet = 
  let 
    tag =
      "link"

    attrs =
      [attribute "Rel""stylesheet"
      , attribute "property" "stylesheet"
      , attribute "href""styles.css"
      ]

    child =
      []
  in 
    node tag attrs child 





view : Modelo -> Html Msg
view model =
  div [class "calculadora"]
      [stylesheet
      , div [class "row"]
        
          [div [class "display-text"]
            [text (model.display)]
          ]
        , div [class "buttons"]
          [calculadoraButton (Numero 7) "7"

          , calculadoraButton (Numero 8) "8"
                   
          ,calculadoraButton (Numero 9) "9"
          
          , calculadoraButton Suma "+"
          , div [] []
          , calculadoraButton (Numero 4) "4"
          , calculadoraButton (Numero 5) "5"
          , calculadoraButton (Numero 6) "6"
          , calculadoraButton Multi "X"
          , div [] []
          , calculadoraButton (Numero 1) "1"
          , calculadoraButton (Numero 2) "2"
          , calculadoraButton (Numero 3) "3"
          , calculadoraButton Cero "0"
          , div [] []
          , calculadoraButton Igual "="
          , calculadoraButtonWide Limpiar "CA" 
          ]]