{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Merchant where

import qualified Data.Text as T
import qualified Domain.Action.Dashboard.Person as DPerson
import Domain.Types.Merchant
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.ServerName as DTServer
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QMerchantAccess
import Tools.Auth
import Tools.Error

data CreateMerchantReq = CreateMerchantReq
  { shortId :: Text,
    is2faMandatory :: Bool,
    defaultOperatingCity :: City.City,
    supportedOperatingCities :: [City.City],
    companyName :: Text,
    domain :: Text,
    website :: Text,
    email :: Text,
    password :: Text,
    numberOfUsers :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

createMerchant ::
  (BeamFlow m r, EncFlow m r) =>
  TokenInfo ->
  CreateMerchantReq ->
  m DMerchant.MerchantAPIEntity
createMerchant _ req = do
  let shortId = ShortId req.shortId :: ShortId DMerchant.Merchant
  mbExistingMerchant <- QMerchant.findByShortId shortId
  whenJust mbExistingMerchant $ \_ -> throwError (MerchantAlreadyExist req.shortId)
  merchant <- buildMerchant req
  decMerchant <- decrypt merchant
  -- Esq.runNoTransaction $
  QMerchant.create merchant
  pure $ DMerchant.mkMerchantAPIEntity decMerchant

buildMerchant ::
  (BeamFlow m r, EncFlow m r) =>
  CreateMerchantReq ->
  m DMerchant.Merchant
buildMerchant req = do
  uid <- generateGUID
  now <- getCurrentTime
  passwordHash <- getDbHash req.password
  email <- encrypt (T.toLower req.email)
  pure
    DMerchant.Merchant
      { id = uid,
        shortId = ShortId req.shortId :: ShortId DMerchant.Merchant,
        serverNames = [],
        is2faMandatory = req.is2faMandatory,
        defaultOperatingCity = req.defaultOperatingCity,
        supportedOperatingCities = req.supportedOperatingCities,
        companyName = Just req.companyName,
        domain = Just req.domain,
        website = Just req.website,
        email = Just email,
        passwordHash = Just passwordHash,
        createdAt = now
      }

listMerchants ::
  (BeamFlow m r, EncFlow m r) =>
  TokenInfo ->
  m [DMerchant.MerchantAPIEntity]
listMerchants _ = do
  merchantList <- QMerchant.findAllMerchants
  forM merchantList $ \encMerchant -> do
    decMerchant <- decrypt encMerchant
    pure $ DMerchant.mkMerchantAPIEntity decMerchant

createUserForMerchant ::
  (BeamFlow m r, EncFlow m r, HasFlowEnv m r '["dataServers" ::: [DTServer.DataServer]], HasFlowEnv m r '["merchantUserAccountNumber" ::: Int]) =>
  TokenInfo ->
  DPerson.CreatePersonReq ->
  m DPerson.CreatePersonRes
createUserForMerchant tokenInfo req = do
  merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
  merchantAssociatedAccount <- QMerchantAccess.findAllUserAccountForMerchant merchant.id
  merchantUserAccountLimit <- asks (.merchantUserAccountNumber)
  when (length merchantAssociatedAccount >= merchantUserAccountLimit) $ throwError (MerchantAccountLimitExceeded (merchant.shortId.getShortId))
  personEntity <- DPerson.createPerson tokenInfo req
  _ <- DPerson.assignMerchantCityAccess tokenInfo personEntity.person.id DPerson.MerchantCityAccessReq {operatingCity = tokenInfo.city, merchantId = merchant.shortId}
  pure personEntity
