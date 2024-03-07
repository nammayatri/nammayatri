{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicyProgressiveDetails
  ( module Reexport,
    module Domain.Types.FarePolicy.FarePolicyProgressiveDetails,
  )
where

-- import qualified Data.ByteString.Lazy as BL

-- import qualified Data.Text.Encoding as DTE

-- import qualified Data.Aeson as DA

import Control.Lens.Combinators
import Control.Lens.Fold
import "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant
import qualified Data.Aeson as DA
import Data.Aeson.Key as DAK
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.List.NonEmpty
import Data.Text as Text
import qualified Data.Vector as DV
import Debug.Trace as T
import Domain.Types.Common
import Domain.Types.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as Reexport
import Kernel.Prelude
import Kernel.Prelude as KP
import Kernel.Types.Cac
import Kernel.Types.Common

-- import Kernel.Types.Error

data FPProgressiveDetailsD (s :: UsageSafety) = FPProgressiveDetails
  { baseFare :: Money,
    baseDistance :: Meters,
    perExtraKmRateSections :: NonEmpty (FPProgressiveDetailsPerExtraKmRateSectionD s),
    deadKmFare :: Money,
    waitingChargeInfo :: Maybe WaitingChargeInfo,
    nightShiftCharge :: Maybe NightShiftCharge
  }
  deriving (Generic, Show)

type FPProgressiveDetails = FPProgressiveDetailsD 'Safe

instance FromJSON (FPProgressiveDetailsD 'Unsafe)

instance ToJSON (FPProgressiveDetailsD 'Unsafe)

instance FromJSON (FPProgressiveDetailsD 'Safe)

instance ToJSON (FPProgressiveDetailsD 'Safe)

-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------APIEntity--------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

data FPProgressiveDetailsAPIEntity = FPProgressiveDetailsAPIEntity
  { baseFare :: Money,
    baseDistance :: Meters,
    perExtraKmRateSections :: NonEmpty FPProgressiveDetailsPerExtraKmRateSectionAPIEntity,
    deadKmFare :: Money,
    waitingChargeInfo :: Maybe WaitingChargeInfo,
    nightShiftCharge :: Maybe NightShiftCharge
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- readWithInfo' :: (Read a, Show a) => String -> Value -> Maybe a
-- readWithInfo' msg s = case s of
--   String str -> case KP.readMaybe (Text.unpack str) of
--     Just val -> Just val
--     Nothing -> Nothing
--   Number scientific -> case KP.readMaybe (show scientific) of
--     Just val -> Just val
--     Nothing -> Nothing
--   Null -> Nothing
--   _ -> error . Text.pack $ "Failed to parse: for key: mes " <> msg <> " and value: " ++ show s

-- valueToType :: FromJSON a => Value -> Maybe a
-- valueToType value = case value of
--   String str -> DA.decode (BL.fromStrict (DTE.encodeUtf8 (replaceSingleQuotes str)))
--   _ -> error $ "Not able to parse value" <> show value

-- jsonToWaitingChargeInfo :: Object -> Parser (Maybe WaitingChargeInfo)
-- jsonToWaitingChargeInfo k = do
--   waitingCharge <- readWithInfo' "farePolicyProgressiveDetails:waitingCharge" <$> k .: DAK.fromText (Text.pack "farePolicyProgressiveDetails:waitingCharge")
--   freeWaitingTime <- readWithInfo' "farePolicyProgressiveDetails:freeWatingTime" <$> k .: DAK.fromText (Text.pack "farePolicyProgressiveDetails:freeWatingTime")
--   return $ WaitingChargeInfo <$> waitingCharge <*> freeWaitingTime

jsonToFPProgressiveDetailsPerExtraKmRateSection :: String -> String -> [FPProgressiveDetailsPerExtraKmRateSection]
jsonToFPProgressiveDetailsPerExtraKmRateSection config key' = do
  let res' = (config ^@.. _Value . _Object . reindexed (dropPrefixFromConfig "farePolicyProgressiveDetailsPerExtraKmRateSection:") (itraversed . indices (\k -> Text.isPrefixOf "farePolicyProgressiveDetailsPerExtraKmRateSection:" (DAK.toText k))))
      res'' = T.trace ("perKmRateSectionvalue" <> show res' <> "and key" <> show key') $ fromMaybe (DA.Array (DV.fromList [])) (KM.lookup (DAK.fromText (Text.pack key')) (KM.fromList res'))
      res = T.trace ("perKmRateSectionValue'" <> show res'') $ res'' ^? _JSON :: (Maybe [FPProgressiveDetailsPerExtraKmRateSection])
  fromMaybe [] res

-- jsonToFPProgressiveDetailsPerExtraKmRateSection' :: Object -> String -> Parser (NonEmpty FPProgressiveDetailsPerExtraKmRateSection)
-- jsonToFPProgressiveDetailsPerExtraKmRateSection' k id = do
--   val <- jsonToFPProgressiveDetailsPerExtraKmRateSection k id
--   let res = nonEmpty val
--   case res of
--     Nothing -> error "FromLocation not found"
--     Just val' -> pure val'

parsingMiddleware :: KM.KeyMap Value -> String -> String -> KM.KeyMap Value
parsingMiddleware config configS key' =
  let perExtraKmRateSections = jsonToFPProgressiveDetailsPerExtraKmRateSection configS key'
      waitingCharge = KM.lookup ("waitingCharge") config >>= fromJSONHelper
      freeWaitingTime = KM.lookup ("freeWatingTime") config >>= fromJSONHelper
      waitingChargeInfo = WaitingChargeInfo <$> waitingCharge <*> freeWaitingTime
   in T.trace ("perExtraKmRateSection" <> show perExtraKmRateSections) $ KP.foldr (\(k, v) acc -> KM.insert k v acc) config [("perExtraKmRateSections", toJSON perExtraKmRateSections), ("waitingChargeInfo", DA.toJSON waitingChargeInfo)]

jsonToFPProgressiveDetails :: String -> String -> (Maybe FPProgressiveDetails)
jsonToFPProgressiveDetails config key' =
  let res' = (config ^@.. _Value . _Object . reindexed (dropPrefixFromConfig "farePolicyProgressiveDetails:") (itraversed . indices (\k -> Text.isPrefixOf "farePolicyProgressiveDetails:" (DAK.toText k))))
      res'' = T.trace ("the farePolicyProgressiveDetails " <> show res' <> "and the config" <> show config) $ parsingMiddleware (KM.fromList res') config key'
      res = T.trace ("farePolicyProgressiveDetails" <> show res'') $ (DA.Object res'') ^? _JSON :: (Maybe FPProgressiveDetails)
   in T.trace ("farePolicyProgressiveDetails parsed " <> show res) $ res

-- jsonToFPProgressiveDetails :: String -> Object -> Parser FPProgressiveDetails
-- jsonToFPProgressiveDetails id k = do
--   -- let fullFPPDP =  parse (())  id
--   -- case fullFPPDP of
--   --   Success fPPDP ->
--   FPProgressiveDetails
--     <$> ((readWithInfo "farePolicyProgressiveDetails:baseFare" <$> k .: DAK.fromText (Text.pack "farePolicyProgressiveDetails:baseFare")) :: (Parser Money))
--     <*> ((readWithInfo "farePolicyProgressiveDetails:baseDistance" <$> k .: DAK.fromText (Text.pack "farePolicyProgressiveDetails:baseDistance")) :: (Parser Meters))
--     <*> (jsonToFPProgressiveDetailsPerExtraKmRateSection' k id)
--     <*> ((readWithInfo "farePolicyProgressiveDetails:deadKmFare" <$> k .: DAK.fromText (Text.pack "farePolicyProgressiveDetails:deadKmFare")) :: (Parser Money))
--     <*> (jsonToWaitingChargeInfo k)
--     <*> ((valueToType <$> k .: DAK.fromText (Text.pack "farePolicyProgressiveDetails:nightShiftCharge")) :: (Parser (Maybe NightShiftCharge)))

-- Error e -> error "FromLocation not found with error " <> show e

makeFPProgressiveDetailsAPIEntity :: FPProgressiveDetails -> FPProgressiveDetailsAPIEntity
makeFPProgressiveDetailsAPIEntity FPProgressiveDetails {..} =
  FPProgressiveDetailsAPIEntity
    { perExtraKmRateSections = makeFPProgressiveDetailsPerExtraKmRateSectionAPIEntity <$> perExtraKmRateSections,
      ..
    }
