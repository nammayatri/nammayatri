module SharedLogic.DriverIdentityInfo
  ( IdentityInfo (..),
    getIdentityInfo,
    upsertDriverIdentityInfo,
  )
where

import Control.Applicative ((<|>))
import Data.Time (Day)
import qualified Domain.Types.DriverIdentityInfo as DII
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Beckn.Context (IndianState)
import Kernel.Types.Id (Id)
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Storage.Queries.DriverIdentityInfo as QDII

data IdentityInfo = IdentityInfo
  { nomineeName :: Maybe Text,
    nomineeRelationship :: Maybe Text,
    nomineeDob :: Maybe Day,
    address :: Maybe Text,
    addressDocumentType :: Maybe DI.AddressDocumentType,
    addressState :: Maybe IndianState
  }

getIdentityInfo ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  DI.DriverInformation ->
  m IdentityInfo
getIdentityInfo driverId driverInfo = do
  mbInfo <- QDII.findByDriverId driverId
  pure
    IdentityInfo
      { nomineeName = (mbInfo >>= (.nomineeName)) <|> driverInfo.nomineeName,
        nomineeRelationship = (mbInfo >>= (.nomineeRelationship)) <|> driverInfo.nomineeRelationship,
        nomineeDob = mbInfo >>= (.nomineeDob),
        address = (mbInfo >>= (.address)) <|> driverInfo.address,
        addressDocumentType = (mbInfo >>= (.addressDocumentType)) <|> driverInfo.addressDocumentType,
        addressState = mbInfo >>= (.addressState)
      }

upsertDriverIdentityInfo ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DI.DriverInformation ->
  Maybe Text ->
  Maybe Text ->
  Maybe Day ->
  Maybe Text ->
  Maybe DI.AddressDocumentType ->
  Maybe IndianState ->
  m ()
upsertDriverIdentityInfo driverId merchantId mocId driverInfo pName pRel pDob pAddr pAddrDoc pState = do
  mbExisting <- QDII.findByDriverId driverId
  now <- getCurrentTime
  let row =
        DII.DriverIdentityInfo
          { driverId = driverId,
            nomineeName = pName <|> (mbExisting >>= (.nomineeName)) <|> driverInfo.nomineeName,
            nomineeRelationship = pRel <|> (mbExisting >>= (.nomineeRelationship)) <|> driverInfo.nomineeRelationship,
            nomineeDob = pDob <|> (mbExisting >>= (.nomineeDob)),
            address = pAddr <|> (mbExisting >>= (.address)) <|> driverInfo.address,
            addressDocumentType = pAddrDoc <|> (mbExisting >>= (.addressDocumentType)) <|> driverInfo.addressDocumentType,
            addressState = pState <|> (mbExisting >>= (.addressState)),
            courtRecord = mbExisting >>= (.courtRecord),
            merchantId = merchantId,
            merchantOperatingCityId = mocId,
            createdAt = maybe now (.createdAt) mbExisting,
            updatedAt = now
          }
  case mbExisting of
    Just _ -> QDII.updateByPrimaryKey row
    Nothing -> QDII.create row
