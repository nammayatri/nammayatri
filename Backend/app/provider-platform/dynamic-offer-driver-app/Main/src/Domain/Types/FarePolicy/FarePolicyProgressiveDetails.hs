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

import "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant
import Data.Aeson.Key as DAK
import Data.Aeson.Types
import Data.List.NonEmpty
import Data.Text as Text
import Domain.Types.Common
import Domain.Types.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as Reexport
import Kernel.Prelude
import Kernel.Prelude as KP
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

readWithInfo' :: (Read a, Show a) => String -> Value -> Maybe a
readWithInfo' msg s = case s of
  String str -> case KP.readMaybe (Text.unpack str) of
    Just val -> Just val
    Nothing -> Nothing
  Number scientific -> case KP.readMaybe (show scientific) of
    Just val -> Just val
    Nothing -> Nothing
  _ -> error . Text.pack $ "Failed to parse: for key: mes " <> msg <> " and value: " ++ show s

jsonToWaitingChargeInfo :: Object -> Parser (Maybe WaitingChargeInfo)
jsonToWaitingChargeInfo k = do
  waitingCharge <- readWithInfo' "farePolicyProgressiveDetails:waitingCharge" <$> k .: DAK.fromText (Text.pack "farePolicyProgressiveDetails:waitingCharge")
  freeWaitingTime <- readWithInfo' "farePolicyProgressiveDetails:freeWatingTime" <$> k .: DAK.fromText (Text.pack "farePolicyProgressiveDetails:freeWatingTime")
  return $ WaitingChargeInfo <$> waitingCharge <*> freeWaitingTime

jsonToFPProgressiveDetailsPerExtraKmRateSection' :: Object -> String -> Parser (NonEmpty FPProgressiveDetailsPerExtraKmRateSection)
jsonToFPProgressiveDetailsPerExtraKmRateSection' k id = do
  val <- jsonToFPProgressiveDetailsPerExtraKmRateSection k id
  let res = nonEmpty val
  case res of
    Nothing -> error "FromLocation not found"
    Just val' -> pure val'

-- case val of
--   Nothing -> InternalError "FromLocation not found"
--   Just val' -> nonEmpty val'

jsonToFPProgressiveDetails :: String -> Object -> Parser FPProgressiveDetails
jsonToFPProgressiveDetails id k = do
  -- let fullFPPDP =  parse (())  id
  -- case fullFPPDP of
  --   Success fPPDP ->
  FPProgressiveDetails
    <$> ((readWithInfo "farePolicyProgressiveDetails:baseFare" <$> k .: DAK.fromText (Text.pack "farePolicyProgressiveDetails:baseFare")) :: (Parser Money))
    <*> ((readWithInfo "farePolicyProgressiveDetails:baseDistance" <$> k .: DAK.fromText (Text.pack "farePolicyProgressiveDetails:baseDistance")) :: (Parser Meters))
    <*> (jsonToFPProgressiveDetailsPerExtraKmRateSection' k id)
    <*> ((readWithInfo "farePolicyProgressiveDetails:deadKmFare" <$> k .: DAK.fromText (Text.pack "farePolicyProgressiveDetails:deadKmFare")) :: (Parser Money))
    <*> (jsonToWaitingChargeInfo k)
    <*> ((readWithInfo' "farePolicyProgressiveDetails:nightShiftCharge" <$> k .: DAK.fromText (Text.pack "farePolicyProgressiveDetails:nightShiftCharge")) :: (Parser (Maybe NightShiftCharge)))

-- Error e -> error "FromLocation not found with error " <> show e

makeFPProgressiveDetailsAPIEntity :: FPProgressiveDetails -> FPProgressiveDetailsAPIEntity
makeFPProgressiveDetailsAPIEntity FPProgressiveDetails {..} =
  FPProgressiveDetailsAPIEntity
    { perExtraKmRateSections = makeFPProgressiveDetailsPerExtraKmRateSectionAPIEntity <$> perExtraKmRateSections,
      ..
    }
