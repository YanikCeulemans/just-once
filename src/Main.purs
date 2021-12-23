module Main where

import Prelude

import CSS (StyleM, marginBottom, pct, px, width)
import Data.Array (cons, foldr, init)
import Data.Enum (toEnum)
import Data.Foldable (class Foldable, find)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Halogen (ClassName(..), Component)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (HTML, i, style)
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (IProp(..), classes)
import Halogen.VDom.DOM.Prop (Prop(..))
import Halogen.VDom.Driver (runUI)
import Percentage as Percentage

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


data ColumnProp
  = Spacing Int
  | ExtraClassNames (Array ClassName)


marginColumn :: Int -> StyleM Unit
marginColumn =
  toNumber >>> px >>> marginBottom


column :: forall a b c. Foldable a => a ColumnProp -> Array (HTML c b) -> HTML c b
column props children =
  HH.div [ classes appliedProps.classes ] $ appliedProps.children
  where
    appliedProps = foldr applyProps { classes: [ ClassName "column" ], children } props

    applyProps p acc = case p of
      (Spacing amount) -> acc { children = 
        fromMaybe acc.children $ init do
          child <- children
          [child, HH.div [ CSS.style $ marginColumn amount ] [] ]
        }
      (ExtraClassNames ecns) -> acc { classes = acc.classes <> ecns }


column_ :: forall w i. Array (HTML w i) -> HTML w i
column_ =
  HH.div [ classes [ ClassName "column" ] ]


header :: forall w i. HTML w i
header =
  HH.header [ classes [ ClassName "header" ] ] [ ]


progressBar :: forall a b. Percentage.Percentage -> HTML b a
progressBar p =
  HH.div [ classes [ ClassName "progress-bar" ], CSS.style do width <<< pct <<< toNumber $ Percentage.toInt p ] []

card :: forall w i. HTML w i
card =
  HH.div [ classes [ ClassName "card" ] ]
    [ column_
      [ Percentage.fromInt 66 # progressBar
      , column [ Spacing 10, ExtraClassNames [ ClassName "padding-large" ] ]
        [ HH.h3 [] [ HH.text "Google" ]
        , HH.h2 [ classes [ ClassName "fg-primary" ] ] [ HH.text "972 598" ]
        , HH.span [] [ HH.text "firsname.lastname@email.com" ]
        ]
      ]
    ]


data Action = Increment | Decrement

component :: forall a b c d. Component HTML d c b a
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = 0

  render state =
    column_
      [ header
      , column [ Spacing 10, ExtraClassNames [ ClassName "padding-regular" ] ] [ card, card ]
      -- , HH.button [ HE.onClick \_ -> Just Decrement ] [ HH.text "-" ]
      -- , HH.div_ [ HH.text $ show state ]
      -- , HH.button [ HE.onClick \_ -> Just Increment ] [ HH.text "+" ]
      ]

  handleAction = case _ of
    Increment -> H.modify_ \state -> state + 1
    Decrement -> H.modify_ \state -> state - 1