import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Basics exposing (..)



main =
  Browser.sandbox { init = init , update = update, view = view }


-- MODEL

type alias Calculadora = 
  { suma : Float -> Float -> Float
  , multi : Float -> Float -> Float
  }

calculadora : Calculadora 
calculadora = 
  { suma = (\x y -> x + y)
  , multi = (\x y -> x * y)
  }

type alias Model = 
  { display : String 
  , function : Float -> Float -> Float
  , saveValue : Float
  , append : Bool 
  }

init : Model
init =
  { display = ""
   , function = (\x y -> y)
   , saveValue = 0
   , append = True
  }

parseFloat : String -> Float
parseFloat input =
  Maybe.withDefault 0 (String.toFloat input)

operacion : Model -> (Float -> Float -> Float) -> Model 
operacion model function =
  {model 
    | function = function 
    , saveValue = (parseFloat model.display)
    , append = False
  }


-- UPDATE

type Msg = None | Suma | Multi | Igual | Cero | Numero Int | Limpiar

update : Msg -> Model -> Model
update msg model =
  case msg of
    None ->
      model

    Limpiar ->
      init 
    
    Numero numero ->
      updateDisplay model numero 
    
    Cero -> 
      cero model 

    Suma ->
      operacion model calculadora.suma 

    Multi -> 
      operacion model calculadora.multi
    
    Igual -> 
      igual model 
  
updateDisplay : Model -> Int -> Model 
updateDisplay model number = 
  if model.append then 
    {model | display = model.display ++ Debug.toString (number)}
  else 
    { model | display = number |> Debug.toString , append = True}



igual : Model -> Model 
igual model = 
  if model.append then
    {model 
      | display = calcular model 
      , saveValue = model.display |> parseFloat
      , append = False 
    }
  else 
  {model | display = calcular model, append = False }


calcular : Model -> String 
calcular model =
  model.function model.saveValue (parseFloat model.display) |> Debug.toString 

cero : Model -> Model 
cero model = 
  if String.isEmpty model.display || not model.append then
    {model | display = "0" , append = False}
  else 
    {model | display = model.display ++ "0"}



-- VIEW

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





view : Model -> Html Msg
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
          , calculadoraButton (Numero 9) "9"
          
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