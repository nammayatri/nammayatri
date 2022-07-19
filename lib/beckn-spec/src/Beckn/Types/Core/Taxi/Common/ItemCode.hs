module Beckn.Types.Core.Taxi.Common.ItemCode
  ( module Beckn.Types.Core.Taxi.Common.ItemCode,
    module Reexport,
  )
where

import Beckn.Prelude hiding (show)
import Beckn.Types.Common
import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Common.FareProductType as Reexport
import Beckn.Types.Core.Taxi.Common.VehicleVariant as Reexport
import Data.Aeson
import qualified Data.List as List
import Data.OpenApi
import qualified Data.Text as T
import GHC.Show (show)

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
  show (ItemCode AUTO_TRIP vehVar Nothing Nothing) =
    show AUTO_TRIP <> "_" <> show vehVar
  show _ = error "ItemCode content doesn't correspond its FareProductType/"

instance Read ItemCode where
  readsPrec d' r' =
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
            ++ [ (ItemCode AUTO_TRIP v1 Nothing Nothing, r2)
                 | r1 <- stripPrefix "AUTO_TRIP_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
      )
      r'
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
