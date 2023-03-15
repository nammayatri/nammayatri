{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ChooseLanguageScreen.ScreenData where

import Screens.Types (ChooseLanguageScreenState)

initData :: ChooseLanguageScreenState
initData = 
  { data: 
      {
        languages : [
        { name : "English"
        , value : "EN_US"
        , subTitle : ""
        },
        { name : "ಕನ್ನಡ"
        , value : "KN_IN"
        , subTitle : "Kannada"
        },
        { name : "हिंदी"
        , value : "HI_IN"
        , subTitle : "Hindi"
        }]
        , isSelected : false
       }
    ,props:
      {
          selectedLanguage : "EN_US"
        , btnActive : true
        , exitAnimation : false
      }
    }