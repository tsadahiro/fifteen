module Otobe exposing(..)

import Html exposing(Html)
import Svg exposing(..)
import Svg.Attributes exposing(..)
import Svg.Events exposing(..)
import Browser
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated
import Simple.Animation.Property as P

main = Browser.sandbox {init=init, update=update, view=view}

--type alias Model =  Bool --true/false
type alias Piece = {x:Int, y:Int, n:Int, prevX:Int, prevY:Int}
type alias Model = List Piece --listに加えていく
--type alias Position = {x:Int, y:Int}
type Msg = Slide Piece --入れ替える、スイッチ

init: Model
init = List.map (\i -> {x=modBy 5 i, y=i//5, n=modBy 25 (i+26), prevX=modBy 5 i, prevY=i//5})
    (List.range 0 24)

update: Msg -> Model -> Model --モデル作りかえる 大文字は型 小文字は変数
update msg model =
  case msg of
    Slide piece -> slide model piece--現在の状況のクリックされた場所をtoggleする

slide: Model -> Piece -> Model
slide model piece =
  let
      clicked = List.filter (\p -> (p.x==piece.x && p.y==piece.y)) model
      space = List.filter (\p -> (p.x-piece.x)^2 + (p.y-piece.y)^2 == 1 && p.n == 0) model
      others = List.filter (\p -> not (p.x==piece.x && p.y==piece.y) && p.n/=0) model
  in
      if (List.length space) > 0 then
          List.concat [
               List.map (\p -> {p|n=0}) clicked
              ,List.map (\p -> {p|n=piece.n, prevX=piece.x, prevY=piece.y}) space
              ,List.map (\p -> {p|prevX=p.x, prevY=p.y}) others
              ]
      else
          model


unit = 50

pieceSvg p =
  animatedG (fade {x=(toFloat (p.prevX*unit)), y=(toFloat (p.prevY*unit))}
                 {x=(toFloat (p.x*unit)), y=(toFloat (p.y*unit))})
      [][
      rect [x (String.fromInt (0 * unit))
             ,y (String.fromInt (0 * unit))
             ,width (String.fromInt unit)
             ,height (String.fromInt unit)
             ,stroke (if p.n==0 then
                        "none"
                        else
                          "black")
             ,rx (String.fromInt (unit//10))
             ,ry (String.fromInt (unit//10))
             ,fill (if p.n==0 then
                      "none"
                      else
                        "skyblue"
              )
             ,onClick (Slide p)]--circleをクリックした時msgを発生させる
             [
             ]
          ,text_ [x (String.fromInt (unit*4//10))
                 ,y (String.fromInt (unit*6//10))
                 ,stroke "white"
                 ,fill "white"
                 ]
          [text (if p.n == 0 then
                     ""
                 else
                     String.fromInt p.n)]
     ]

view: Model -> Html Msg --モデルを描画する
view model =
  svg[width "600"
      ,height "600"]
    (List.map pieceSvg model)--model野中の一つ一つを描いたものをリストとして返してくれる


fade : {x:Float, y:Float} -> {x:Float, y:Float} -> Animation
fade start stop =
    Animation.steps
        { startAt = [P.x start.x, P.y start.y]
        , options = []
        }
        [
         Animation.step 1000 [P.x stop.x, P.y stop.y]
        ]

animatedG : Animation -> List (Svg.Attribute msg) -> List (Svg msg) -> Svg msg
animatedG =
    animatedSvg Svg.g

animatedSvg =
    Simple.Animation.Animated.svg
        { class = Svg.Attributes.class
        }
        
