{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Constant.TestIdType where

import Prelude

import Data.Generic.Rep (class Generic)
import Foreign.Class (class Decode)
import Halogen.VDom.DOM.Prop (Prop(..))
import Presto.Core.Utils.Encoding (defaultDecode)
import PrestoDOM as PD

data TestID
    = Button Check
    | Screen String
    | Container String
    | ToolBar String
    | Component String
    | List String
    | Element String
    | Select String
    | DropDown String
    | Bar String
    | Object String
    | Text String
    | Option String
    | TextField String
    | Status String
    | Toggle String
    | RadioButton String
    | Hamburger String

data Check
  = BtnConfig String

instance showTestID :: Show TestID where
    show (Screen a) = "screen_" <> a
    show (Component a) = "component_" <> a
    show (Button a) = "button_" <> show a
    show (Container a) = "container_" <> a
    show (ToolBar a) = "toolbar_" <> a
    show (List a) = "list_" <> a
    show (Element a) = "element_" <> a
    show (Select a) = "select_" <> a
    show (DropDown a) = "dropdown_" <> a
    show (Bar a) = "bar_" <> a
    show (Object a) = "object_" <> a
    show (Text a) = "text_" <> a
    show (Option a) = "option_" <> a
    show (TextField a) = "textfield_" <> a
    show (Status a) = "status_" <> a
    show (Toggle a) = "toggle_" <> a
    show (RadioButton a) = "radiobutton_" <> a
    show (Hamburger a) = "hamburger_" <> a



derive instance genericCheck :: Generic Check _
instance decodeCheck :: Decode Check where decode = defaultDecode
instance showCheck :: Show Check where
  show (BtnConfig a) = a

testId :: forall a. TestID -> Prop a
testId = PD.testID <<< show