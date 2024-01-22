{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.Components.ContactsList where

import Common.Types.App
import Data.Array
import Data.Maybe
import Mobility.Prelude
import Prelude
import PrestoDOM
import Screens.Types
import Styles.Types
import Components.GenericRadioButton.Controller as GenericRadioButton
import Components.GenericRadioButton.View (radioButtonView)
import Data.String as DS
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Helpers.Utils as HU
import Language.Strings (getString)
import Language.Types (STR(..))
import Styles.Colors as Color
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle

view :: forall w. (Action -> Effect Unit) -> Array NewContacts -> PrestoDOM (Effect Unit) w
view push contacts =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ textView
        $ [ text $ getString SELECT_PREFERRED_CONTACTS
          , color Color.black700
          , margin $ MarginVertical 14 10
          ]
        <> FontStyle.subHeading1 TypoGraphy
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        (mapWithIndex (\index item -> contactCardView push item index) contacts)
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , stroke $ "1," <> Color.grey900
        , padding $ Padding 16 16 16 16
        , visibility $ boolToVisibility $ length contacts /= 3
        , cornerRadius 8.0
        , margin $ MarginTop 10
        , onClick push $ const AddContacts
        , gravity CENTER_VERTICAL
        ]
        [ imageView
            [ imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_add_filled"
            , height $ V 24
            , width $ V 24
            , margin $ MarginRight 12
            ]
        , textView
            $ [ text $ getString ADD_A_CONTACT
              , color Color.blue900
              ]
            <> FontStyle.subHeading1 TypoGraphy
        ]
    ]

contactCardView :: forall w. (Action -> Effect Unit) -> NewContacts -> Int -> PrestoDOM (Effect Unit) w
contactCardView push contact index =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 18 18 18 18
    , margin $ MarginVertical 5 5
    , cornerRadius 8.0
    , gravity CENTER_VERTICAL
    , stroke
        ( "1,"
            <> case contact.priority == 0 of
                true -> Color.green900
                false -> Color.grey900
        )
    , onClick push $ const $ ContactCardClicked index
    ]
    [ radioButtonView buttonConfig $ contact.priority == 0
    , ContactCircle.view contactConfig (push <<< ContactCircleAction)
    , textView
        $ [ height $ WRAP_CONTENT
          , width $ WRAP_CONTENT
          , weight 1.0
          , text contact.name
          , color Color.black800
          ]
        <> FontStyle.subHeading1 LanguageStyle
    , textView
        [ height $ WRAP_CONTENT
        , width $ WRAP_CONTENT
        , text (getString REMOVE)
        , accessibilityHint "Remove : Button"
        , accessibility ENABLE
        , color Color.blue900
        , textSize 14
        , onClick push $ const $ RemoveButtonClicked contact
        ]
    ]
  where
  buttonConfig = case contact.priority == 0 of
    true -> GenericRadioButton.config.activeButtonConfig { buttonColor = Color.black800 }
    false -> GenericRadioButton.config.inActiveButtonConfig { buttonColor = Color.black600 }
  contactConfig = {
    contact : contact,
    index : index,
    enableCheckmark : false
  }

data Action
  = ContactCardClicked Int
  | RemoveButtonClicked NewContacts
  | AddContacts
  | ContactCircleAction ContactCircle.Action