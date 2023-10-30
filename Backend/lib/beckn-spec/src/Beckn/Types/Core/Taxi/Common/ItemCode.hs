{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Common.ItemCode
  ( module Beckn.Types.Core.Taxi.Common.ItemCode,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Common.FareProductType as Reexport
import Beckn.Types.Core.Taxi.Common.Vehicle as Reexport
import Data.Aeson
import qualified Data.List as List
import Data.OpenApi
import qualified Data.Text as T
import GHC.Show (show)
import Kernel.Prelude hiding (show)
import Kernel.Types.Common

data ItemCode = ItemCode
  { fareProductType :: FareProductType,
    vehicleVariant :: VehicleVariant,
    distance :: Maybe Kilometers, -- only RentalTrip
    duration :: Maybe Hours -- only RentalTrip
  }
  deriving (Generic, Eq)

instance Show ItemCode where
  show (ItemCode ONE_WAY_TRIP vehVar Nothing Nothing) =
    show ONE_WAY_TRIP <> "_" <> show vehVar
  show (ItemCode RENTAL_TRIP vehVar (Just dist) (Just dur)) =
    show RENTAL_TRIP <> "_" <> show vehVar <> "_" <> show dist <> "_" <> show dur
  show (ItemCode DRIVER_OFFER_ESTIMATE vehVar Nothing Nothing) =
    show DRIVER_OFFER_ESTIMATE <> "_" <> show vehVar
  show (ItemCode DRIVER_OFFER vehVar Nothing Nothing) =
    show DRIVER_OFFER <> "_" <> show vehVar
  show (ItemCode ONE_WAY_SPECIAL_ZONE vehVar Nothing Nothing) =
    show ONE_WAY_SPECIAL_ZONE <> "_" <> show vehVar
  show _ = error "ItemCode content doesn't correspond its FareProductType/"

instance Read ItemCode where
  readsPrec d' =
    readParen
      (d' > app_prec)
      ( \r ->
          [ (ItemCode ONE_WAY_TRIP v1 Nothing Nothing, r2)
            | r1 <- stripPrefix "ONE_WAY_TRIP_" r,
              (v1, r2) <- readsPrec (app_prec + 1) r1
          ]
            ++ [ (ItemCode RENTAL_TRIP v1 (Just v2) (Just v3), r6)
                 | r1 <- stripPrefix "RENTAL_TRIP_" r,
                   (v1, r2) <- parseValueAtStart (app_prec + 1) r1,
                   r3 <- stripPrefix "_" r2,
                   (v2, r4) <- parseValueAtStart (app_prec + 1) r3,
                   r5 <- stripPrefix "_" r4,
                   (v3, r6) <- readsPrec (app_prec + 1) r5
               ]
            ++ [ (ItemCode DRIVER_OFFER_ESTIMATE v1 Nothing Nothing, r2)
                 | r1 <- stripPrefix "DRIVER_OFFER_ESTIMATE_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (ItemCode DRIVER_OFFER v1 Nothing Nothing, r2)
                 | r1 <- stripPrefix "DRIVER_OFFER_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (ItemCode ONE_WAY_SPECIAL_ZONE v1 Nothing Nothing, r2)
                 | r1 <- stripPrefix "ONE_WAY_SPECIAL_ZONE_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r
      parseValueAtStart d r =
        [ (val, r1)
          | let r1 = dropWhile (/= '_') r,
            (val, _) <- readsPrec d $ takeWhile (/= '_') r
        ]

instance ToJSON ItemCode where
  toJSON = String . T.pack . show

instance FromJSON ItemCode where
  parseJSON = withText "ItemCode" $ \s -> do
    case readMaybe $ T.unpack s of
      Nothing -> fail "Unable to parse ItemCode"
      Just ic -> return ic

instance ToSchema ItemCode where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)
