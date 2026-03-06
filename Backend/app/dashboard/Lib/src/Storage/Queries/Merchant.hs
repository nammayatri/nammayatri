{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Merchant where

import Domain.Types.Merchant as Domain
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Beckn.City (City)
import Kernel.Types.Id
import Sequelize as Se
import Storage.Beam.BeamFlow
import qualified Storage.Beam.Merchant as BeamM

create :: BeamFlow m r => Merchant -> m ()
create = createWithKV

findById ::
  BeamFlow m r =>
  Id Merchant ->
  m (Maybe Merchant)
findById merchantId = findOneWithKV [Se.Is BeamM.id $ Se.Eq $ getId merchantId]

findByShortId :: BeamFlow m r => ShortId Merchant -> m (Maybe Merchant)
findByShortId shortId = do
  findOneWithKV [Se.Is BeamM.shortId $ Se.Eq $ getShortId shortId]

findAllMerchants ::
  BeamFlow m r =>
  m [Merchant]
findAllMerchants = findAllWithKV [Se.Is BeamM.id $ Se.Not $ Se.Eq ""]

updateSupportedOperatingCities ::
  BeamFlow m r =>
  ShortId Merchant ->
  [City] ->
  m ()
updateSupportedOperatingCities mShortId cities = do
  updateWithKV
    [Se.Set BeamM.supportedOperatingCities cities]
    [Se.Is BeamM.shortId $ Se.Eq $ getShortId mShortId]

findAllMerchants' :: BeamFlow m r => Int -> Int -> m [Merchant]
findAllMerchants' limit offset = do
  findAllWithOptionsKV
    [Se.Is BeamM.id $ Se.Not $ Se.Eq ""]
    (Se.Desc BeamM.createdAt)
    (Just limit)
    (Just offset)

findByAuthToken ::
  (BeamFlow m r, EncFlow m r) =>
  Text ->
  m (Maybe Merchant)
findByAuthToken authToken = do
  authTokenHash <- getDbHash authToken
  findOneWithKV [Se.Is BeamM.authTokenHash $ Se.Eq $ Just authTokenHash]

findAllByShortIds ::
  BeamFlow m r =>
  [ShortId Merchant] ->
  m [Merchant]
findAllByShortIds shortIds = do
  findAllWithKV [Se.Is BeamM.shortId $ Se.In $ getShortId <$> shortIds]

updateEnableStatus ::
  BeamFlow m r =>
  ShortId Merchant ->
  Bool ->
  m ()
updateEnableStatus mShortId status = do
  updateWithKV
    [Se.Set BeamM.enabled (Just status)]
    [Se.Is BeamM.shortId $ Se.Eq $ getShortId mShortId]

instance FromTType' BeamM.Merchant Domain.Merchant where
  fromTType' BeamM.MerchantT {..} = do
    pure $
      Just
        Domain.Merchant
          { id = Id id,
            shortId = ShortId shortId,
            authToken = case (authTokenEncrypted, authTokenHash) of
              (Just token, Just hash) -> Just $ EncryptedHashed (Encrypted token) hash
              _ -> Nothing,
            enableGetRequestAuditLogs = enableGetRequestAuditLogs,
            ..
          }

instance ToTType' BeamM.Merchant Domain.Merchant where
  toTType' Domain.Merchant {..} =
    BeamM.MerchantT
      { id = getId id,
        shortId = getShortId shortId,
        authTokenEncrypted = authToken <&> (unEncrypted . (.encrypted)),
        authTokenHash = authToken <&> (.hash),
        ..
      }
