import Svg exposing (Svg)
import Svg.Attributes exposing (width, height, x, y, fill, stroke)
import Geometry.Svg as Svg
import Browser
import Random
import Random.List exposing (shuffle)
import Html exposing (Html, button, div, text, ol, li)
import Html.Events exposing (onClick)


main =
  Browser.element 
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }


-- MODEL

type alias Model = 
  { seats : List String
  }

seatCoords = 
  -- 1 - 4

   [ (10, 10)
   , (110, 10)
   , (10, 60)
   , (110, 60)
  -- 5 - 9
   , (230, 10)
   , (330, 10)
   , (230, 60)
   , (330, 60)
 -- 9 - 11
   , (330, 120)
   , (330, 180)
   , (230, 150)  
   ]

init : () -> (Model, Cmd Msg)
init _ =
  ( 
  { seats =
    List.sort [ "Dan"
    , "Dale"
    , "Gedion"
    , "Bryan"
    , "Logan"
    , "Denis"
    , "Jake"
    , "Joe"
    , "Scott"
    , "Reid"
    , "Justin"
    ]
  }
  , Cmd.none
  )

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


validateConstraints : List String -> List String -> Bool
validateConstraints oldOrder newOrder =
  let
      combined = List.map2 Tuple.pair oldOrder newOrder 
  in
      List.foldl 
        (\t b -> b && (Tuple.first t) /= (Tuple.second t))
        True 
        combined



-- UPDATE

type Msg
  = Randomize
  | NewOrder (List String)
  | Clear

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Randomize ->
      (model, Random.generate NewOrder (shuffle model.seats))
    NewOrder newOrder ->
      if (validateConstraints model.seats newOrder) then
        ({model | seats = newOrder}, Cmd.none)
      else 
        (model, Random.generate NewOrder (shuffle model.seats))

    Clear ->
     ({ model | seats = [] }, Cmd.none)





-- VIEW

desk : Int -> String -> (Int, Int) -> Svg Msg
desk index name coords = 
  let 
     x0 =  (10 + (100 * index))
     row = (floor (((toFloat x0) / 500)))
     x1 = x0 - (row * 500)
     y0 =  (10 + (row) * 50)
 in
    Svg.svg
                      [ x (String.fromInt (Tuple.first coords))
                      , y (String.fromInt (Tuple.second coords))
                      ]
                      [
                      Svg.text_ 
                      [ x "5"
                      , y "25"]
                      [ Svg.text name ]
                      , Svg.rect
                        [ width "90"
                        , height "50"
                        , fill "transparent"
                        , stroke "blue"
                        ]
                        []
                       ]



room : List String -> Svg Msg
room seats = Svg.g
                []
        ([ Svg.rect
          [ width "430"
          , height "240"
          , fill  "transparent"
          , stroke "black"
          ]
          [] 
          ] ++ (List.map3 desk (List.range 1 11) seats seatCoords))

view : Model -> Html Msg
view model =
  div []
    ([ button [ onClick Randomize ] [ text "Randomize" ] 
    , button [ onClick Clear ] [ text "Clear" ]
    , Svg.svg
        [ width "500"
        , height "300"]
        [room model.seats
        ]
    ])
