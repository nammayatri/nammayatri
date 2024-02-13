{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.FavProviderScreen.ComponentConfig where 

import Components.GenericHeader as GenericHeader 
import Components.PrimaryButton as PrimaryButton 
import Components.ProviderModel as ProviderModel
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types as ST 
import Styles.Colors as Color
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Helpers.Utils as HU
import Prelude ((<>), (==))
import Common.Types.App(LazyCheck(..))
import MerchantConfig.Types as MRC
import Resources.Constants as CONS
import Mobility.Prelude (boolToVisibility)
import Data.Array (elem)
import Data.Maybe (maybe)


primaryButtonConfig :: ST.FavProviderScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      {   textConfig
         { text = "Confirm Favourites"
         } 
        , margin = Margin 16 16 16 16
        , id = "ConfirmFavourites"
        , isClickable = state.props.buttonActive
        , alpha = if state.props.buttonActive then 1.0 else 0.4
      }
  in primaryButtonConfig'

genericHeaderConfig :: ST.FavProviderScreenState -> GenericHeader.Config 
genericHeaderConfig state = let 
  genericHeaderConfig' = GenericHeader.config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = HU.fetchImage HU.FF_COMMON_ASSET "ny_ic_chevron_left" 
      } 
    , textConfig {
        text = "Edit Favourite"
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig' 

providerModelConfig :: ST.FavProviderScreenState -> MRC.Provider -> ProviderModel.Config
providerModelConfig state providerElement = 
  let provider = HU.getProviderById providerElement.id state.data.currentCityConfig.iopConfig.providersList
      ourProvider = providerElement.id == CONS.nyProviderId
      logo = if ourProvider then "ny_ic_ny_network" else "ny_ic_ondc_network"
      name = maybe "" _.name provider
      isActive = elem providerElement.id state.data.selectedProviders
  in ProviderModel.config {
      isActive = isActive,
      name = name,
      id = providerElement.id,
      logo = logo,
      pillsVisibility = boolToVisibility ourProvider,
      selectButtonVisibility = boolToVisibility isActive
  }