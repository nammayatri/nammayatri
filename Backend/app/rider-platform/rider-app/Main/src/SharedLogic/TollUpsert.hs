{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.TollUpsert
  ( upsertTollsFromCsv,
  )
where

import qualified "dashboard-helper-api" API.Types.RiderPlatform.Management.Merchant as Common
import qualified Dashboard.Common.Merchant as DM
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment
import qualified EulerHS.Language as L
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.TollDashboard
import Storage.Beam.Toll ()
import qualified Toll.Storage.Queries.Toll as QToll
import Tools.Error

data TollCSVRow = TollCSVRow
  { city :: Text,
    tollId :: Text,
    name :: Text,
    price :: Text,
    currency :: Text,
    isAutoRickshawAllowed :: Text,
    isTwoWheelerAllowed :: Text,
    tollStartGates :: Text,
    tollEndGates :: Text
  }
  deriving (Show)

instance FromNamedRecord TollCSVRow where
  parseNamedRecord r =
    TollCSVRow
      <$> r .: "city"
      <*> r .: "toll_id"
      <*> r .: "name"
      <*> r .: "price"
      <*> r .: "currency"
      <*> r .: "is_auto_rickshaw_allowed"
      <*> r .: "is_two_wheeler_allowed"
      <*> r .: "toll_start_gates"
      <*> r .: "toll_end_gates"

upsertTollsFromCsv ::
  Context.City ->
  DMOC.MerchantOperatingCity ->
  FilePath ->
  Flow Common.APISuccessWithUnprocessedEntities
upsertTollsFromCsv opCity merchantOpCity csvFile = do
  rows <- readCsv csvFile
  unprocessedEntities <-
    foldlM
      ( \acc row -> do
          withTryCatch "processTollCsvRow" (processRow opCity merchantOpCity row)
            >>= \case
              Left err -> pure $ acc <> ["Unable to add toll: " <> show err]
              Right _ -> pure acc
      )
      []
      rows
  pure $ Common.APISuccessWithUnprocessedEntities unprocessedEntities

readCsv :: FilePath -> Flow [TollCSVRow]
readCsv csvFile = do
  csvData <- L.runIO $ BS.readFile csvFile
  case decodeByName $ LBS.fromStrict csvData :: Either String (Header, V.Vector TollCSVRow) of
    Left err -> throwError $ InvalidRequest $ show err
    Right (_, v) -> pure $ V.toList v

processRow :: Context.City -> DMOC.MerchantOperatingCity -> TollCSVRow -> Flow ()
processRow opCity merchantOpCity row = do
  city :: Context.City <- readCSVField row.city "city"
  when (city /= opCity) $
    throwError $
      InvalidRequest $
        "Can't process toll for different city: "
          <> show city
          <> ", please login with this city in dashboard"
  name <- cleanCSVField row.name "name"
  price <- readCSVField row.price "price"
  currency <- readCSVField row.currency "currency"
  isAutoRickshawAllowed <- readCSVField row.isAutoRickshawAllowed "is_auto_rickshaw_allowed"
  isTwoWheelerAllowed <- readMaybeCSVField row.isTwoWheelerAllowed "is_two_wheeler_allowed"
  tollStartGates <- parseGates row.tollStartGates "toll_start_gates"
  tollEndGates <- parseGates row.tollEndGates "toll_end_gates"
  let upsertReq =
        DM.UpsertTollReq
          { name = name,
            price = price,
            currency = currency,
            tollStartGates = tollStartGates,
            tollEndGates = tollEndGates,
            isAutoRickshawAllowed = isAutoRickshawAllowed,
            isTwoWheelerAllowed = isTwoWheelerAllowed
          }
  mbTollIdText <- pure $ cleanField row.tollId
  mbExisting <- case mbTollIdText of
    Just tid -> QToll.findByPrimaryKey (Id tid)
    Nothing -> pure Nothing
  tollId <- case mbTollIdText of
    Just tid -> pure $ Id tid
    Nothing -> maybe generateGUID (return . (.id)) mbExisting
  now <- getCurrentTime
  let toll =
        mkTollFromUpsertReq
          upsertReq
          tollId
          now
          merchantOpCity.id.getId
          merchantOpCity.merchantId.getId
          mbExisting
  void $
    if isJust mbExisting
      then QToll.updateByPrimaryKey toll
      else QToll.create toll
  invalidateTollCache merchantOpCity.id.getId

parseGates :: Text -> Text -> Flow [DM.TollGateAPIEntity]
parseGates fieldValue fieldName =
  case cleanField fieldValue of
    Nothing -> pure []
    Just gatesText ->
      case Aeson.eitherDecodeStrict (TE.encodeUtf8 gatesText) of
        Left err -> throwError $ InvalidRequest $ "Invalid " <> fieldName <> ": " <> T.pack err
        Right gates -> pure gates

cleanField :: Text -> Maybe Text
cleanField t =
  case T.strip t of
    "" -> Nothing
    s -> Just s

readCSVField :: Read a => Text -> Text -> Flow a
readCSVField fieldValue fieldName =
  cleanField fieldValue >>= readMaybe . T.unpack & fromMaybeM (InvalidRequest $ "Invalid " <> fieldName <> ": " <> show fieldValue)

readMaybeCSVField :: Read a => Text -> Text -> Flow (Maybe a)
readMaybeCSVField fieldValue _ =
  pure $ cleanField fieldValue >>= readMaybe . T.unpack

cleanCSVField :: Text -> Text -> Flow Text
cleanCSVField fieldValue fieldName =
  cleanField fieldValue & fromMaybeM (InvalidRequest $ "Invalid " <> fieldName <> ": " <> show fieldValue)
