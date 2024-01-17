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
import qualified Storage.Beam.Person as BeamP
import Storage.Tabular.MerchantAccess as Access
import Storage.Tabular.Person as Person
import Storage.Tabular.Role as Role

create :: Person -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id Person ->
  m (Maybe Person)
findById = Esq.findById

findByEmail ::
  (Transactionable m, EncFlow m r) =>
  Text ->
  m (Maybe Person)
findByEmail email = do
  emailDbHash <- getDbHash (T.toLower email)
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonEmailHash ==. val (Just emailDbHash)
    return person

findByEmailAndPassword ::
  (Transactionable m, EncFlow m r) =>
  Text ->
  Text ->
  m (Maybe Person)
findByEmailAndPassword email password = do
  emailDbHash <- getDbHash (T.toLower email)
  passwordDbHash <- getDbHash password
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonEmailHash ==. val (Just emailDbHash)
        &&. person ^. PersonPasswordHash ==. val (Just passwordDbHash)
    return person

findByMobileNumber ::
  (Transactionable m, EncFlow m r) =>
  Text ->
  Text ->
  m (Maybe Person)
findByMobileNumber mobileNumber mobileCountryCode = do
  mobileDbHash <- getDbHash mobileNumber
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonMobileNumberHash ==. val mobileDbHash
        &&. person ^. PersonMobileCountryCode ==. val mobileCountryCode
    return person

-- TODO add filtering by role
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

updatePersonRole :: Id Person -> Id Role -> SqlDB ()
updatePersonRole personId roleId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonRoleId =. val (toKey roleId),
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

updatePersonPassword :: Id Person -> DbHash -> SqlDB ()
updatePersonPassword personId newPasswordHash = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonPasswordHash =. val (Just newPasswordHash),
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

updatePersonEmail :: Id Person -> EncryptedHashed Text -> SqlDB ()
updatePersonEmail personId encEmail = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonEmailEncrypted =. val (Just (unEncrypted encEmail.encrypted)),
        PersonEmailHash =. val (Just encEmail.hash),
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

updatePersonMobile :: Id Person -> EncryptedHashed Text -> SqlDB ()
updatePersonMobile personId encMobileNumber = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonMobileNumberEncrypted =. val (unEncrypted encMobileNumber.encrypted),
        PersonMobileNumberHash =. val encMobileNumber.hash,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

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
