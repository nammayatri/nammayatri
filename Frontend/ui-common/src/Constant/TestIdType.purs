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
    | Click String
    | Component String
    | List String
    | Empty
    | Element String
    | Select String
    | PrimaryButton String
    | DropDown String
    | Bar String
    | Object String
    | Text String
    | Option String
    | TextField String

data Check
  = BtnConfig String

instance showTestID :: Show TestID where
    show (Button a) = "btn_" <> show a
    show (Screen a) = "scr_" <> a
    show (Container a) = "con_" <> a
    show (ToolBar a) = "tlb_" <> a
    show (Click a) = "clk_" <> a
    show (Component a) = "component_" <> a
    show (List a) = "list_" <> a
    show (Empty) = ""
    show (Element a) = "element_" <> a
    show (Select a) = "select_" <> a
    show (PrimaryButton a) = "primaryBtn_" <> a
    show (DropDown a) = "dropDown_" <> a
    show (Bar a) = "bar_" <> a
    show (Object a) = "object_" <> a
    show (Text a) = "txt_" <> a
    show (Option a) = "option_" <> a
    show (TextField a) = "txtField_" <> a



derive instance genericCheck :: Generic Check _
instance decodeCheck :: Decode Check where decode = defaultDecode
instance showCheck :: Show Check where
  show (BtnConfig a) = a

testId :: forall a. TestID -> Prop a
testId = PD.testID <<< show