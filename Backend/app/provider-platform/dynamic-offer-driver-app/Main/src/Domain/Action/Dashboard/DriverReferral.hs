{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.DriverReferral
  ( updateReferralLinkPassword,
    linkDriverReferralCode,
  )
where

import Control.Applicative
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.DriverReferral as Common
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import Data.Either (isRight)
import Data.List (partition)
import qualified Data.Vector as V
import Domain.Action.UI.DriverReferral (ReferralLinkReq (..), createDriverReferral)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Text as TU
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantConfig as SCT
import Tools.Error

---------------------------------------------------------------------
updateReferralLinkPassword ::
  ShortId DM.Merchant ->
  Common.ReferralLinkPasswordUpdateAPIReq ->
  Flow APISuccess
updateReferralLinkPassword merchantShortId req = do
  unless (TU.validateAllDigitWithMinLength 5 req.referralLinkPassword) $
    throwError (InvalidRequest "Password should be minimum 5 digits in length")
  merchant <- findMerchantByShortId merchantShortId
  merchantConfig <- SCT.findByMerchantId merchant.id >>= fromMaybeM (MerchantDoesNotExist merchant.id.getId)
  _ <- SCT.updateReferralLinkPassword merchant.id req.referralLinkPassword
  SCT.clearCache merchantConfig
  logTagInfo "dashboard -> updateReferralLinkPassword : " (show merchant.id)
  pure Success

data CSVRow = CSVRow {driverId :: Id DP.Person, referralCode :: Text}

instance FromNamedRecord CSVRow where
  parseNamedRecord r =
    CSVRow
      <$> fmap Id (r .: "driverId")
      <*> r .: "referralCode"

linkDriverReferralCode ::
  ShortId DM.Merchant ->
  Common.ReferralLinkReq ->
  Flow Common.LinkReport
linkDriverReferralCode merchantShortId Common.ReferralLinkReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  csvData <- L.runIO $ BS.readFile file
  dIdRefIdMap <-
    case (decodeByName $ LBS.fromStrict csvData :: Either String (Header, V.Vector CSVRow)) of
      Left err -> throwError . InvalidRequest $ show err
      Right (_, v) -> pure $ V.toList v
  linkingResult <-
    mapM
      ( \(CSVRow dId refId) -> do
          linkingRes <- try @_ @SomeException (createDriverReferral (dId, merchant.id) True (mkRefLinkReq refId))
          pure (dId, linkingRes)
      )
      dIdRefIdMap
  let (success, failure) = partition (isRight . snd) linkingResult
      failureReasons = map (\(dId, err) -> Common.FailureReasons dId.getId (show err)) failure
  logTagInfo "dashboard -> linkDriverReferralCode : " (show merchant.id)
  logTagInfo "dashboard -> linkDriverReferralCode -> failures" (show failureReasons)
  pure $ Common.LinkReport (length success) failureReasons
  where
    mkRefLinkReq refId =
      ReferralLinkReq
        { referralCode = refId,
          referralLinkPassword = ""
        }
