{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Font.Size where

import Prelude (($), (*))
import Data.Maybe (fromMaybe)
import Data.Int (fromNumber)
import Styles.Types (FontSize)

multiplier :: Number 
multiplier = 1.0

a_8 :: FontSize
a_8 = fromMaybe 8 $ fromNumber $ 8.0 * multiplier

a_9 :: FontSize
a_9 = fromMaybe 9 $ fromNumber $ 9.0 * multiplier

a_10 :: FontSize
a_10 = fromMaybe 10 $ fromNumber $ 10.0 * multiplier

a_11 :: FontSize
a_11 = fromMaybe 11 $ fromNumber $ 11.0 * multiplier

a_12 :: FontSize
a_12 = fromMaybe 12 $ fromNumber $ 12.0 * multiplier

a_13 :: FontSize
a_13 = fromMaybe 13 $ fromNumber $ 13.0 * multiplier

a_14 :: FontSize
a_14 = fromMaybe 14 $ fromNumber $ 14.0 * multiplier

a_15 :: FontSize
a_15 = fromMaybe 15 $ fromNumber $ 15.0 * multiplier

a_16 :: FontSize
a_16 = fromMaybe 16 $ fromNumber $ 16.0 * multiplier

a_17 :: FontSize
a_17 = fromMaybe 17 $ fromNumber $ 17.0 * multiplier

a_18 :: FontSize
a_18 = fromMaybe 18 $ fromNumber $ 18.0 * multiplier

a_19 :: FontSize
a_19 = fromMaybe 19 $ fromNumber $ 19.0 * multiplier

a_20 :: FontSize
a_20 = fromMaybe 20 $ fromNumber $ 20.0 * multiplier

a_21 :: FontSize
a_21 = fromMaybe 21 $ fromNumber $ 21.0 * multiplier

a_22 :: FontSize
a_22 = fromMaybe 22 $ fromNumber $ 22.0 * multiplier

a_23 :: FontSize
a_23 = fromMaybe 23 $ fromNumber $ 23.0 * multiplier

a_24 :: FontSize
a_24 = fromMaybe 24 $ fromNumber $ 24.0 * multiplier

a_26 :: FontSize
a_26 = fromMaybe 26 $ fromNumber $ 26.0 * multiplier

a_27 :: FontSize
a_27 = fromMaybe 27 $ fromNumber $ 27.0 * multiplier

a_28 :: FontSize
a_28 = fromMaybe 28 $ fromNumber $ 28.0 * multiplier

a_30 :: FontSize
a_30 = fromMaybe 30 $ fromNumber $ 30.0 * multiplier

a_32 :: FontSize
a_32 = fromMaybe 32 $ fromNumber $ 32.0 * multiplier

a_38 :: FontSize
a_38 = fromMaybe 38 $ fromNumber $ 32.0 * multiplier

a_40 :: FontSize 
a_40 = fromMaybe 40 $ fromNumber $ 40.0 * multiplier 

a_44 :: FontSize
a_44 = fromMaybe 44 $ fromNumber $ 44.0 * multiplier

a_48 :: FontSize
a_48 = fromMaybe 48 $ fromNumber $ 48.0 * multiplier

a_52 :: FontSize
a_52 = fromMaybe 52 $ fromNumber $ 52.0 * multiplier


primaryButtonSize :: FontSize
primaryButtonSize = fromMaybe 20 $ fromNumber $ 20.0 * multiplier

smallText :: FontSize
smallText = fromMaybe 12 $ fromNumber $ 12.0 * multiplier

titleSize :: FontSize
titleSize = fromMaybe 28 $ fromNumber $ 28.0 * multiplier

stepNumberSize :: FontSize
stepNumberSize = fromMaybe 52 $ fromNumber $ 52.0 * multiplier

a_72 :: FontSize
a_72 = fromMaybe 72 $ fromNumber $ 72.0 * multiplier 

a_80 :: FontSize
a_80 = fromMaybe 80 $ fromNumber $ 80.0 * multiplier