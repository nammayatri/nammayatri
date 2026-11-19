{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Styles.Types
 ( Color
 , FontSize
 , FontStyle
 , Dimension
 , FontType(..)
 ) where

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign.Generic (class Decode, class Encode)
import Prelude (class Eq, class Show)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode)

type Color = String

type FontSize = Int

type FontStyle = String

type Dimension = Int

data FontType = System | Assets

derive instance genericFontType :: Generic FontType _
instance decodeFontType :: Decode FontType where decode = defaultEnumDecode
instance encodeFontType :: Encode FontType where encode = defaultEnumEncode
instance eqFontType :: Eq FontType where eq = genericEq

