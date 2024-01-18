{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Person where

import Control.Applicative (liftA2)
import qualified Data.Text as T
import Database.Esqueleto.PostgreSQL
import Domain.Types.Merchant as Merchant
import Domain.Types.Person as Person
import Domain.Types.Role as Role
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import Sequelize as Se
import Storage.Beam.BeamFlow
import qualified Storage.Beam.Person as BeamP
import Storage.Tabular.MerchantAccess as Access
import Storage.Tabular.Person as Person
import Storage.Tabular.Role as Role

create :: BeamFlow m r => Person -> m ()
create = createWithKV

findById ::
  BeamFlow m r =>
  Id Person ->
  m (Maybe Person)
findById personId = findOneWithKV [Se.Is BeamP.id $ Se.Eq $ getId personId]

findByEmail ::
  (BeamFlow m r, EncFlow m r) =>
  Text ->
  m (Maybe Person)
findByEmail email = do
  emailDbHash <- getDbHash (T.toLower email)
  findOneWithKV [Se.Is BeamP.emailHash $ Se.Eq $ Just emailDbHash]

findByEmailAndPassword ::
  (BeamFlow m r, EncFlow m r) =>
  Text ->
  Text ->
  m (Maybe Person)
findByEmailAndPassword email password = do
  emailDbHash <- getDbHash (T.toLower email)
  passwordDbHash <- getDbHash password
  findOneWithKV [Se.And [Se.Is BeamP.emailHash $ Se.Eq $ Just emailDbHash, Se.Is BeamP.passwordHash $ Se.Eq $ Just passwordDbHash]]

findByMobileNumber ::
  (BeamFlow m r, EncFlow m r) =>
  Text ->
  Text ->
  m (Maybe Person)
findByMobileNumber mobileNumber mobileCountryCode = do
  mobileDbHash <- getDbHash mobileNumber
  findOneWithKV [Se.And [Se.Is BeamP.mobileNumberHash $ Se.Eq $ mobileDbHash, Se.Is BeamP.mobileCountryCode $ Se.Eq $ mobileCountryCode]]

-- TODO add filtering by role ---todo to be done in beam queries
findAllWithLimitOffset ::
  Transactionable m =>
  Maybe Text ->
  Maybe DbHash ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe (Id Person.Person) ->
  m [(Person, Role, [ShortId Merchant.Merchant], [City.City])]
findAllWithLimitOffset mbSearchString mbSearchStrDBHash mbLimit mbOffset personId =
  fromMaybeList <$> do
    findAll $ do
      merchantAccessAggTable <- with $ do
        merchantAccess <-
          from $
            table @MerchantAccessT
        groupBy (merchantAccess ^. MerchantAccessPersonId)
        return (merchantAccess ^. MerchantAccessPersonId, arrayAggWith AggModeAll (merchantAccess ^. MerchantAccessMerchantShortId) [desc $ merchantAccess ^. MerchantAccessCreatedAt], arrayAggWith AggModeAll (merchantAccess ^. MerchantAccessOperatingCity) [desc $ merchantAccess ^. MerchantAccessCreatedAt])
      (person :& role :& (_, mbMerchantShortIds, mbMerchantOperatingCityIds)) <-
        from $
          table @PersonT
            `innerJoin` table @RoleT
              `Esq.on` ( \(person :& role) ->
                           person ^. Person.PersonRoleId ==. role ^. Role.RoleTId
                       )
            `leftJoin` merchantAccessAggTable
              `Esq.on` ( \(person :& _ :& (mbPersonId, _mbMerchantShortIds, _mbMerchantOperatingCityIds)) ->
                           just (person ^. Person.PersonTId) ==. mbPersonId
                       )
      where_ $
        Esq.whenJust_ (liftA2 (,) mbSearchString mbSearchStrDBHash) (filterBySearchString person)
          &&. Esq.whenJust_ personId (\defaultPerson -> person ^. PersonId ==. val (getId defaultPerson))
      orderBy [desc $ person ^. PersonCreatedAt]
      limit limitVal
      offset offsetVal
      return (person, role, mbMerchantShortIds, mbMerchantOperatingCityIds)
  where
    limitVal = maybe 100 fromIntegral mbLimit
    offsetVal = maybe 0 fromIntegral mbOffset

    filterBySearchString person (searchStr, searchStrDBHash) = do
      let likeSearchStr = (%) ++. val searchStr ++. (%)
      ( concat_ @Text [person ^. PersonFirstName, person ^. PersonLastName]
          `ilike` likeSearchStr
        )
        ||. person ^. PersonMobileNumberHash ==. val searchStrDBHash --find by email also?
    fromMaybeList :: [(Person, Role, Maybe [Text], Maybe [City.City])] -> [(Person, Role, [ShortId Merchant.Merchant], [City.City])]
    fromMaybeList = map (\(person, role, mbMerchantShortIds, mbMerchantOperatingCityIds) -> (person, role, ShortId <$> fromMaybe [] mbMerchantShortIds, fromMaybe [] mbMerchantOperatingCityIds))

updatePersonRole :: BeamFlow m r => Id Person -> Id Role -> m ()
updatePersonRole personId roleId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.roleId $ getId roleId,
      Se.Set BeamP.updatedAt now
    ]
    [ Se.Is BeamP.id $ Se.Eq $ getId personId
    ]

updatePersonPassword :: BeamFlow m r => Id Person -> DbHash -> m ()
updatePersonPassword personId newPasswordHash = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.passwordHash $ Just newPasswordHash,
      Se.Set BeamP.updatedAt now
    ]
    [ Se.Is BeamP.id $ Se.Eq $ getId personId
    ]

updatePersonEmail :: BeamFlow m r => Id Person -> EncryptedHashed Text -> m ()
updatePersonEmail personId encEmail = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.emailEncrypted $ Just (unEncrypted encEmail.encrypted),
      Se.Set BeamP.emailHash $ Just encEmail.hash,
      Se.Set BeamP.updatedAt now
    ]
    [ Se.Is BeamP.id $ Se.Eq $ getId personId
    ]

updatePersonMobile :: BeamFlow m r => Id Person -> EncryptedHashed Text -> m ()
updatePersonMobile personId encMobileNumber = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.mobileNumberEncrypted $ unEncrypted encMobileNumber.encrypted,
      Se.Set BeamP.mobileNumberHash $ encMobileNumber.hash,
      Se.Set BeamP.updatedAt now
    ]
    [ Se.Is BeamP.id $ Se.Eq $ getId personId
    ]

instance FromTType' BeamP.Person Person.Person where
  fromTType' BeamP.PersonT {..} = do
    return $
      Just
        Person.Person
          { id = Id id,
            roleId = Id roleId,
            email = case (emailEncrypted, emailHash) of
              (Just email, Just hash) -> Just $ EncryptedHashed (Encrypted email) hash
              _ -> Nothing,
            mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
            ..
          }

instance ToTType' BeamP.Person Person.Person where
  toTType' Person.Person {..} =
    BeamP.PersonT
      { id = getId id,
        roleId = getId roleId,
        emailEncrypted = email <&> (unEncrypted . (.encrypted)),
        emailHash = email <&> (.hash),
        mobileNumberEncrypted = mobileNumber & unEncrypted . (.encrypted),
        mobileNumberHash = mobileNumber.hash,
        ..
      }
