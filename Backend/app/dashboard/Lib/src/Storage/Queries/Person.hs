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

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Database.Beam as B
import Domain.Types.Merchant as Merchant
import Domain.Types.MerchantAccess as MerchantAccess
import Domain.Types.Person as Person
import Domain.Types.Role as Role
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import Sequelize as Se
import Storage.Beam.BeamFlow
import qualified Storage.Beam.Common as SBC
import qualified Storage.Beam.MerchantAccess as BeamMA
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Beam.Role as BeamR
import Storage.Queries.MerchantAccess ()
import Storage.Queries.Role ()

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

findAllByIds ::
  BeamFlow m r =>
  [Id Person] ->
  m [Person]
findAllByIds personIds = findAllWithKV [Se.Is BeamP.id $ Se.In $ getId <$> personIds]

findAllByIdsAndReceiveNotification ::
  BeamFlow m r =>
  [Id Person] ->
  m [Person]
findAllByIdsAndReceiveNotification personIds = findAllWithKV [Se.And [Se.Is BeamP.id $ Se.In $ getId <$> personIds, Se.Or [Se.Is BeamP.receiveNotification $ Se.Eq $ Just True, Se.Is BeamP.receiveNotification $ Se.Eq Nothing]]]

findAllByRole ::
  BeamFlow m r =>
  Id Role ->
  m [Person]
findAllByRole roleId = findAllWithKV [Se.Is BeamP.roleId $ Se.Eq $ getId roleId]

findAllByRoleAndReciveNotification ::
  BeamFlow m r =>
  Id Role ->
  m [Person]
findAllByRoleAndReciveNotification roleId = findAllWithKV [Se.And [Se.Is BeamP.roleId $ Se.Eq $ getId roleId, Se.Or [Se.Is BeamP.receiveNotification $ Se.Eq $ Just True, Se.Is BeamP.receiveNotification $ Se.Eq Nothing]]]

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
  findOneWithKV [Se.And [Se.Is BeamP.mobileNumberHash $ Se.Eq mobileDbHash, Se.Is BeamP.mobileCountryCode $ Se.Eq mobileCountryCode]]

updatePersonVerifiedStatus :: BeamFlow m r => Id Person -> Bool -> m ()
updatePersonVerifiedStatus personId verified = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.verified $ Just verified,
      Se.Set BeamP.updatedAt now
    ]
    [ Se.Is BeamP.id $ Se.Eq $ getId personId
    ]

findAllWithLimitOffset ::
  BeamFlow m r =>
  Maybe Text ->
  Maybe DbHash ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe (Id Person.Person) ->
  m [(Person, Role, [ShortId Merchant.Merchant], [City.City])]
findAllWithLimitOffset mbSearchString mbSearchStrDBHash mbLimit mbOffset personId = do
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ limitVal $
          B.offset_ offsetVal $
            B.orderBy_ (\(person, _, _) -> B.desc_ person.createdAt) $
              B.filter_'
                ( \(person, _role, _) ->
                    ( maybe (B.sqlBool_ $ B.val_ True) (\searchString -> B.sqlBool_ (B.concat_ [person.firstName, person.lastName] `B.like_` B.val_ ("%" <> searchString <> "%"))) mbSearchString
                        B.||?. maybe (B.sqlBool_ $ B.val_ True) (\searchStrDBHash -> person.mobileNumberHash B.==?. B.val_ searchStrDBHash) mbSearchStrDBHash
                    )
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\defaultPerson -> person.id B.==?. B.val_ (getId defaultPerson)) personId
                )
                $ do
                  person <- B.all_ (SBC.person SBC.atlasDB)
                  role <- B.join_' (SBC.role SBC.atlasDB) (\role -> BeamP.roleId person B.==?. BeamR.id role)
                  merchantAccess <- B.leftJoin_' (B.all_ $ SBC.merchantAccess SBC.atlasDB) (\merchantAccess -> BeamP.id person B.==?. BeamMA.personId merchantAccess)
                  pure (person, role, merchantAccess)
  case res of
    Right res' -> do
      finalRes <- forM res' $ \(person, role, mbMerchantAccess) -> runMaybeT $ do
        p <- MaybeT $ fromTType' person
        r <- MaybeT $ fromTType' role
        ma <- forM mbMerchantAccess (MaybeT . fromTType')
        pure (p, r, ma)
      pure $ groupByPerson $ catMaybes finalRes
    Left _ -> pure []
  where
    limitVal = fromMaybe 100 mbLimit
    offsetVal = fromMaybe 0 mbOffset
    groupByPerson ::
      [(Person, Role, Maybe MerchantAccess)] ->
      [(Person, Role, [ShortId Merchant.Merchant], [City.City])]
    groupByPerson inputList =
      map processGroup $ groupByPerson' inputList

    groupByPerson' ::
      [(Person, Role, Maybe MerchantAccess)] ->
      [NonEmpty (Person, Role, Maybe MerchantAccess)]
    groupByPerson' = NE.groupBy ((==) `on` (\(p, _, _) -> p.id))

    processGroup ::
      NonEmpty (Person, Role, Maybe MerchantAccess) ->
      (Person, Role, [ShortId Merchant.Merchant], [City.City])
    processGroup group =
      let (person, role, _) = NE.head group
          merchantAccessList = mapMaybe (\(_, _, ma) -> ma) $ toList group
          cities = merchantAccessList <&> (.operatingCity)
          merchantIds = merchantAccessList <&> MerchantAccess.merchantShortId
       in (person, role, merchantIds, cities)

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

updatePerson :: BeamFlow m r => Id Person -> Person -> m ()
updatePerson personId person = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.firstName $ person.firstName,
      Se.Set BeamP.lastName $ person.lastName,
      Se.Set BeamP.emailEncrypted $ person.email <&> (unEncrypted . (.encrypted)),
      Se.Set BeamP.emailHash $ person.email <&> (.hash),
      Se.Set BeamP.mobileNumberEncrypted $ unEncrypted (person.mobileNumber.encrypted),
      Se.Set BeamP.mobileNumberHash $ person.mobileNumber.hash,
      Se.Set BeamP.mobileCountryCode $ person.mobileCountryCode,
      Se.Set BeamP.updatedAt now
    ]
    [ Se.Is BeamP.id $ Se.Eq $ getId personId
    ]

deletePerson :: BeamFlow m r => Id Person -> m ()
deletePerson personId = deleteWithKV [Se.Is BeamP.id $ Se.Eq $ getId personId]

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

-- findAllByIdAndRoleId :: BeamFlow m r => [Id Person] -> Id Role -> m [Person]
-- findAllByIdAndRoleId personIds roleId = findAllWithKV [Se.And [Se.Is BeamP.id $ Se.In $ getId <$> personIds, Se.Is BeamP.roleId $ Se.Eq $ getId roleId]]

updatePersonReceiveNotificationStatus :: BeamFlow m r => Id Person -> Bool -> m ()
updatePersonReceiveNotificationStatus personId receiveNotification = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.receiveNotification $ Just receiveNotification,
      Se.Set BeamP.updatedAt now
    ]
    [ Se.Is BeamP.id $ Se.Eq $ getId personId
    ]

findAllByIdRoleIdAndReceiveNotification :: BeamFlow m r => [Id Person] -> Id Role -> m [Person]
findAllByIdRoleIdAndReceiveNotification personIds roleId = findAllWithKV [Se.And [Se.Is BeamP.id $ Se.In $ getId <$> personIds, Se.Is BeamP.roleId $ Se.Eq $ getId roleId, Se.Or [Se.Is BeamP.receiveNotification $ Se.Eq $ Just True, Se.Is BeamP.receiveNotification $ Se.Eq Nothing]]]

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
