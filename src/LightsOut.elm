module LightsOut exposing(..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Browser

main = Browser.sandbox {init=init, update=update, view=view}

type alias Light = {x:Int, y:Int, onoff:Bool}
type alias Model = List Light
type alias Position = {x:Int, y:Int}
type Msg = Toggle Position

init:Model
init = start 5

start: Int -> Model
start l = List.map (\i -> light i l) (List.range 0 (l^2-1))
    
light: Int -> Int -> Light
light i l =
    {x= (modBy l i) + 1, y= i // l + 1, onoff=False}
       
update: Msg -> Model -> Model
update msg model =
  case msg of
    Toggle pos -> toggle model pos

toggle: Model -> Position -> Model
toggle model pos =
  List.map (\p -> if (abs(p.x-pos.x)==0 || abs(p.y-pos.y)==0)&& (abs(p.x-pos.x) <=1 && abs(p.y-pos.y) <=1) then
                      {p|onoff= not p.onoff}
                  else
                    p
            ) model


unit = 50

piece p =
      circle [cx (String.fromInt (p.x*unit))
              ,cy (String.fromInt (p.y*unit))
              ,r (String.fromInt (unit//2))
              ,fill (if p.onoff then
                      "pink"
                    else
                      "cyan"
                    )
              ,onClick (Toggle {x=p.x,y=p.y})]
              []

view: Model -> Html Msg
view model =
  svg [width "600"
      ,height "600"]
      (List.map piece model)
