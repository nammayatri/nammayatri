{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Trip where

import Control.Lens.Operators hiding ((.=))
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict.InsOrd as HMSIO
import qualified Data.List as List
import Data.OpenApi hiding (name)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import Domain.Types.ServiceTierType
import EulerHS.Prelude hiding (length, map, readMaybe, (.~))
import Kernel.Prelude
import Kernel.Utils.GenericPretty
import Servant
import qualified Text.Show

data MultimodalTravelMode = Metro | Bus | Walk | Taxi | Subway
  deriving (Generic, FromJSON, ToJSON, Read, ToSchema, Show, Eq, Ord)

data TripCategory
  = OneWay OneWayMode
  | Rental RentalMode
  | RideShare RideShareMode
  | InterCity OneWayMode (Maybe Text)
  | CrossCity OneWayMode (Maybe Text)
  | Ambulance OneWayMode
  | Delivery OneWayMode
  deriving stock (Eq, Ord, Generic)

-- deriving anyclass (ToSchema)

data PartyRole
  = Initiator
  | DeliveryRoleSender
  | DeliveryRoleReceiver
  deriving (Eq, Ord, Show, FromJSON, Generic, ToSchema)

instance ToJSON PartyRole where
  toJSON r = String (show r)

data TripParty
  = DeliveryParty DeliveryParties
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToSchema)

instance ToJSON TripParty where
  toJSON (DeliveryParty party) =
    object
      [ "tag" .= ("DeliveryParty" :: Text),
        "contents" .= party
      ]

instance FromJSON TripParty where
  parseJSON = withObject "TripParty" $ \v -> do
    tag <- v .: "tag"
    case tag of
      "DeliveryParty" -> DeliveryParty <$> v .: "contents"
      _ -> fail $ "Unknown tag: " ++ tag

data DeliveryParties
  = Sender
  | Receiver
  | SomeoneElse
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable DeliveryParties

instance Show TripParty where
  show (DeliveryParty party) = "DeliveryParty_" <> show party

instance Read TripParty where
  readsPrec d' =
    readParen
      (d' > app_prec)
      ( \r ->
          [ (DeliveryParty v1, r2)
            | r1 <- stripPrefix "DeliveryParty_" r,
              (v1, r2) <- readsPrec (app_prec + 1) r1
          ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r

-- This is done to handle backward compatibility, as UI is expected "contents" to be a string but due to multiple in InterCity and CrossCity, it got changed into an array
instance ToJSON TripCategory where
  toJSON (OneWay mode) =
    object
      [ "tag" .= ("OneWay" :: Text),
        "contents" .= mode
      ]
  toJSON (Rental mode) =
    object
      [ "tag" .= ("Rental" :: Text),
        "contents" .= mode
      ]
  toJSON (RideShare mode) =
    object
      [ "tag" .= ("RideShare" :: Text),
        "contents" .= mode
      ]
  toJSON (InterCity mode text) =
    object
      [ "tag" .= ("InterCity" :: Text),
        "contents" .= mode,
        "city" .= text
      ]
  toJSON (CrossCity mode text) =
    object
      [ "tag" .= ("CrossCity" :: Text),
        "contents" .= mode,
        "city" .= text
      ]
  toJSON (Ambulance mode) =
    object
      [ "tag" .= ("Ambulance" :: Text),
        "contents" .= mode
      ]
  toJSON (Delivery mode) =
    object
      [ "tag" .= ("Delivery" :: Text),
        "contents" .= mode
      ]

instance FromJSON TripCategory where
  parseJSON = withObject "TripCategory" $ \v -> do
    tag <- v .: "tag"
    case tag of
      "OneWay" -> OneWay <$> v .: "contents"
      "Rental" -> Rental <$> v .: "contents"
      "RideShare" -> RideShare <$> v .: "contents"
      "InterCity" -> InterCity <$> v .: "contents" <*> v .:? "city"
      "CrossCity" -> CrossCity <$> v .: "contents" <*> v .:? "city"
      "Ambulance" -> Ambulance <$> v .: "contents"
      "Delivery" -> Delivery <$> v .: "contents"
      _ -> fail $ "Unknown tag: " ++ tag

instance ToSchema TripCategory where
  declareNamedSchema _ = do
    oneWayModeSchema <- declareSchemaRef (Proxy :: Proxy OneWayMode)
    rentalModeSchema <- declareSchemaRef (Proxy :: Proxy RentalMode)
    rideShareModeSchema <- declareSchemaRef (Proxy :: Proxy RideShareMode)
    textSchema <- declareSchemaRef (Proxy :: Proxy Text)

    return $
      NamedSchema (Just "TripCategory") $
        mempty
          & type_ ?~ OpenApiObject
          & oneOf
            ?~ [ tripCategorySchema oneWayModeSchema [] ["OneWay"],
                 tripCategorySchema rentalModeSchema [] ["Rental"],
                 tripCategorySchema rideShareModeSchema [] ["RideShare"],
                 tripCategorySchema oneWayModeSchema [("city", textSchema)] ["InterCity"],
                 tripCategorySchema oneWayModeSchema [("city", textSchema)] ["CrossCity"],
                 tripCategorySchema oneWayModeSchema [] ["Ambulance"],
                 tripCategorySchema oneWayModeSchema [] ["Delivery"]
               ]
    where
      tripCategorySchema contentSchema otherSchemas enums =
        Inline $
          mempty
            & type_ ?~ OpenApiObject
            & properties
              .~ HMSIO.fromList
                ( [ ("contents", contentSchema),
                    ("tag", enumInlineSchema enums)
                  ]
                    ++ otherSchemas
                )
            & required .~ ["tag", "contents"]

      enumInlineSchema enums =
        Inline $
          mempty
            & type_ ?~ OpenApiString
            & enum_ ?~ enums

data TripOption = TripOption
  { schedule :: UTCTime,
    isScheduled :: Bool,
    tripCategories :: [TripCategory]
  }
  deriving stock (Eq, Show, Read, Ord, Generic)

data OneWayMode = OneWayRideOtp | OneWayOnDemandStaticOffer | OneWayOnDemandDynamicOffer | MeterRide
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable OneWayMode

type RentalMode = TripMode

type RideShareMode = TripMode

data TripMode = RideOtp | OnDemandStaticOffer
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving (PrettyShow) via Showable TripMode

instance FromHttpApiData TripMode where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData TripMode where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

instance FromHttpApiData OneWayMode where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = left T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData OneWayMode where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

instance Show TripCategory where
  show (OneWay s) = "OneWay_" <> show s
  show (Rental s) = "Rental_" <> show s
  show (RideShare s) = "RideShare_" <> show s
  show (InterCity s Nothing) = "InterCity_" <> show s
  show (InterCity s (Just city)) = "InterCity_" <> show s <> "_" <> T.unpack city
  show (CrossCity s Nothing) = "CrossCity_" <> show s
  show (CrossCity s (Just city)) = "CrossCity_" <> show s <> "_" <> T.unpack city
  show (Ambulance s) = "Ambulance_" <> show s
  show (Delivery s) = "Delivery_" <> show s

generateTripCategoryShowInstances :: [String]
generateTripCategoryShowInstances =
  [show (OneWay mode) | mode <- oneWayModes]
    ++ [show (Rental mode) | mode <- tripModes]
    ++ [show (RideShare mode) | mode <- tripModes]
    ++ [show (InterCity mode Nothing) | mode <- oneWayModes]
    ++ [show (CrossCity mode Nothing) | mode <- oneWayModes]
    ++ [show (Ambulance mode) | mode <- oneWayModes]
    ++ [show (Delivery mode) | mode <- oneWayModes]
  where
    oneWayModes = [OneWayRideOtp, OneWayOnDemandStaticOffer, OneWayOnDemandDynamicOffer]
    tripModes = [RideOtp, OnDemandStaticOffer]

instance ToParamSchema TripCategory where
  toParamSchema _ =
    mempty
      & title ?~ "TripCategory"
      & type_ ?~ OpenApiString
      & enum_
        ?~ map (String . T.pack) generateTripCategoryShowInstances

instance Read TripCategory where
  readsPrec d' =
    readParen
      (d' > app_prec)
      ( \r ->
          [ (OneWay v1, r2)
            | r1 <- stripPrefix "OneWay_" r,
              (v1, r2) <- readsPrec (app_prec + 1) r1
          ]
            ++ [ (Rental v1, r2)
                 | r1 <- stripPrefix "Rental_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (RideShare v1, r2)
                 | r1 <- stripPrefix "RideShare_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (InterCity v1 Nothing, r3)
                 | r1 <- stripPrefix "InterCity_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1,
                   r3 <- [r2]
               ]
            ++ [ (InterCity OneWayRideOtp (Just v1), [])
                 | r1 <- stripPrefix "InterCity_OneWayRideOtp_" r,
                   let v1 = T.pack r1
               ]
            ++ [ (InterCity OneWayOnDemandStaticOffer (Just v1), [])
                 | r1 <- stripPrefix "InterCity_OneWayOnDemandStaticOffer_" r,
                   let v1 = T.pack r1
               ]
            ++ [ (InterCity OneWayOnDemandDynamicOffer (Just v1), [])
                 | r1 <- stripPrefix "InterCity_OneWayOnDemandDynamicOffer_" r,
                   let v1 = T.pack r1
               ]
            ++ [ (CrossCity v1 Nothing, r3)
                 | r1 <- stripPrefix "CrossCity_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1,
                   r3 <- [r2]
               ]
            ++ [ (CrossCity OneWayRideOtp (Just v1), [])
                 | r1 <- stripPrefix "CrossCity_OneWayRideOtp_" r,
                   let v1 = T.pack r1
               ]
            ++ [ (CrossCity OneWayOnDemandStaticOffer (Just v1), [])
                 | r1 <- stripPrefix "CrossCity_OneWayOnDemandStaticOffer_" r,
                   let v1 = T.pack r1
               ]
            ++ [ (CrossCity OneWayOnDemandDynamicOffer (Just v1), [])
                 | r1 <- stripPrefix "CrossCity_OneWayOnDemandDynamicOffer_" r,
                   let v1 = T.pack r1
               ]
            ++ [ (Ambulance v1, r2)
                 | r1 <- stripPrefix "Ambulance_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (Delivery v1, r2)
                 | r1 <- stripPrefix "Delivery_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r

instance FromHttpApiData TripCategory where
  parseQueryParam = readEither

instance ToHttpApiData TripCategory where
  toUrlPiece = show

data PricingPolicy
  = EstimateBased {nightShiftOverlapChecking :: Bool}
  | QuoteBased {nightShiftOverlapChecking :: Bool}
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

tripCategoryToPricingPolicy :: TripCategory -> PricingPolicy
tripCategoryToPricingPolicy (OneWay OneWayOnDemandDynamicOffer) = EstimateBased False
tripCategoryToPricingPolicy (CrossCity OneWayOnDemandDynamicOffer _) = EstimateBased False
tripCategoryToPricingPolicy (InterCity OneWayOnDemandDynamicOffer _) = EstimateBased False
tripCategoryToPricingPolicy (Ambulance OneWayOnDemandDynamicOffer) = EstimateBased True
tripCategoryToPricingPolicy (Delivery OneWayOnDemandDynamicOffer) = EstimateBased False
tripCategoryToPricingPolicy (Rental _) = QuoteBased True
tripCategoryToPricingPolicy _ = QuoteBased False

skipDriverPoolCheck :: TripCategory -> Bool
skipDriverPoolCheck (OneWay OneWayOnDemandStaticOffer) = False
skipDriverPoolCheck (OneWay OneWayOnDemandDynamicOffer) = False
skipDriverPoolCheck (Ambulance OneWayOnDemandDynamicOffer) = False
skipDriverPoolCheck (Delivery OneWayOnDemandDynamicOffer) = False
skipDriverPoolCheck _ = True

-- Move it to configs later if required
isEndOtpRequired :: TripCategory -> Bool
isEndOtpRequired (Rental _) = True
isEndOtpRequired (InterCity _ _) = True
isEndOtpRequired (Delivery _) = True
isEndOtpRequired _ = False

isRideOtpTrip :: TripCategory -> Bool
isRideOtpTrip (OneWay OneWayRideOtp) = True
isRideOtpTrip (CrossCity OneWayRideOtp _) = True
isRideOtpTrip (InterCity OneWayRideOtp _) = True
isRideOtpTrip (Delivery OneWayRideOtp) = True
isRideOtpTrip (RideShare RideOtp) = True
isRideOtpTrip (Rental RideOtp) = True
isRideOtpTrip _ = False

-- Move it to configs later if required
isOdometerReadingsRequired :: TripCategory -> Bool
isOdometerReadingsRequired (Rental _) = False
isOdometerReadingsRequired _ = False

-- Move it to configs later if required
isGoHomeAvailable :: TripCategory -> Bool
isGoHomeAvailable (OneWay _) = True
isGoHomeAvailable (Delivery _) = True
isGoHomeAvailable _ = False

shouldRectifyDistantPointsSnapToRoadFailure :: TripCategory -> Bool
shouldRectifyDistantPointsSnapToRoadFailure tripCategory = case tripCategory of
  Rental _ -> True
  InterCity _ _ -> True
  OneWay MeterRide -> True
  _ -> False

isRentalTrip :: TripCategory -> Bool
isRentalTrip tripCategory = case tripCategory of
  Rental _ -> True
  _ -> False

isAmbulanceTrip :: TripCategory -> Bool
isAmbulanceTrip tripCategory = case tripCategory of
  Ambulance _ -> True
  _ -> False

isFixedNightCharge :: TripCategory -> Bool
isFixedNightCharge tripCategory = isRentalTrip tripCategory || isInterCityTrip tripCategory

isInterCityTrip :: TripCategory -> Bool
isInterCityTrip tripCategory = case tripCategory of
  InterCity _ _ -> True
  _ -> False

isDynamicOfferTrip :: TripCategory -> Bool
isDynamicOfferTrip (OneWay OneWayOnDemandDynamicOffer) = True
isDynamicOfferTrip (CrossCity OneWayOnDemandDynamicOffer _) = True
isDynamicOfferTrip (InterCity OneWayOnDemandDynamicOffer _) = True
isDynamicOfferTrip (Delivery OneWayOnDemandDynamicOffer) = True
isDynamicOfferTrip _ = False

isTollApplicableForTrip :: ServiceTierType -> TripCategory -> Bool
isTollApplicableForTrip AUTO_RICKSHAW _ = False
isTollApplicableForTrip _ (OneWay _) = True
isTollApplicableForTrip _ (CrossCity _ _) = True
isTollApplicableForTrip _ (Delivery _) = True
isTollApplicableForTrip _ _ = False

isDeliveryTrip :: TripCategory -> Bool
isDeliveryTrip (Delivery _) = True
isDeliveryTrip _ = False
