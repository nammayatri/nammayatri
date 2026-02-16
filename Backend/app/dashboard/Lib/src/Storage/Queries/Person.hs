{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
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

import API.Types.ProviderPlatform.Management.Endpoints.Account (FleetOwnerStatus (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Database.Beam as B
import Domain.Types.Merchant as Merchant
import Domain.Types.MerchantAccess as MerchantAccess
import Domain.Types.Person as Person
import qualified Domain.Types.Person.Type as DPT
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
import Storage.Queries.OrphanInstances.Role ()

create :: BeamFlow m r => Person -> m ()
create = createWithKV

findById ::
  BeamFlow m r =>
  Id Person ->
  m (Maybe Person)
findById personId = findOneWithKV [Se.Is BeamP.id $ Se.Eq $ getId personId]

findByEmailWithType ::
  forall (t :: DPT.DashboardTypeTag) m r.
  (BeamFlow m r, EncFlow m r, DPT.KnownDashboardType t) =>
  Text ->
  m (Maybe Person)
findByEmailWithType email = do
  emailDbHash <- getDbHash (T.toLower email)
  findOneWithKV
    [ Se.Is BeamP.emailHash $ Se.Eq $ Just emailDbHash,
      Se.Is BeamP.dashboardType $ Se.Eq (DPT.dashboardTypeVal (Proxy @t))
    ]

findByEmailOrMobile ::
  (BeamFlow m r, EncFlow m r) =>
  Maybe Text ->
  Text ->
  Text ->
  m [Person]
findByEmailOrMobile mbEmail mobileNumber mobileCountryCode = do
  mobileDbHash <- getDbHash mobileNumber
  case mbEmail of
    Just email -> do
      emailDbHash <- getDbHash email
      findAllWithKV
        [ Se.Or
            [ Se.And
                [ Se.Is BeamP.mobileNumberHash $ Se.Eq mobileDbHash,
                  Se.Is BeamP.mobileCountryCode $ Se.Eq mobileCountryCode
                ],
              Se.Is BeamP.emailHash $ Se.Eq $ Just emailDbHash
            ]
        ]
    Nothing -> findAllWithKV [Se.Is BeamP.mobileNumberHash $ Se.Eq mobileDbHash, Se.Is BeamP.mobileCountryCode $ Se.Eq mobileCountryCode]

findByEmail ::
  (BeamFlow m r, EncFlow m r) =>
  Text ->
  m (Maybe Person)
findByEmail = findByEmailWithType @'DPT.DefaultDashboard

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

findAllByRoleWithType ::
  forall (t :: DPT.DashboardTypeTag) m r.
  (BeamFlow m r, DPT.KnownDashboardType t) =>
  Id Role ->
  m [Person]
findAllByRoleWithType roleId =
  findAllWithKV
    [ Se.Is BeamP.roleId $ Se.Eq $ getId roleId,
      Se.Is BeamP.dashboardType $ Se.Eq (DPT.dashboardTypeVal (Proxy @t))
    ]

findAllByRole ::
  BeamFlow m r =>
  Id Role ->
  m [Person]
findAllByRole roleId = findAllByRoleWithType @'DPT.DefaultDashboard roleId

findAllByRoleAndReciveNotificationWithType ::
  forall (t :: DPT.DashboardTypeTag) m r.
  (BeamFlow m r, DPT.KnownDashboardType t) =>
  Id Role ->
  m [Person]
findAllByRoleAndReciveNotificationWithType roleId = findAllWithKV [Se.And [Se.Is BeamP.roleId $ Se.Eq $ getId roleId, Se.Or [Se.Is BeamP.receiveNotification $ Se.Eq $ Just True, Se.Is BeamP.receiveNotification $ Se.Eq Nothing], Se.Is BeamP.dashboardType $ Se.Eq (DPT.dashboardTypeVal (Proxy @t))]]

findAllByRoleAndReciveNotification ::
  BeamFlow m r =>
  Id Role ->
  m [Person]
findAllByRoleAndReciveNotification roleId = findAllByRoleAndReciveNotificationWithType @'DPT.DefaultDashboard roleId

findByEmailAndPasswordWithType ::
  forall (t :: DPT.DashboardTypeTag) m r.
  (BeamFlow m r, EncFlow m r, DPT.KnownDashboardType t) =>
  Text ->
  Text ->
  m (Maybe Person)
findByEmailAndPasswordWithType email password = do
  emailDbHash <- getDbHash (T.toLower email)
  passwordDbHash <- getDbHash password
  findOneWithKV
    [ Se.Is BeamP.emailHash $ Se.Eq $ Just emailDbHash,
      Se.Is BeamP.passwordHash $ Se.Eq $ Just passwordDbHash,
      Se.Is BeamP.dashboardType $ Se.Eq (DPT.dashboardTypeVal (Proxy @t))
    ]

findByEmailAndPassword ::
  (BeamFlow m r, EncFlow m r) =>
  Text ->
  Text ->
  m (Maybe Person)
findByEmailAndPassword = findByEmailAndPasswordWithType @'DPT.DefaultDashboard

findByMobileNumberWithType ::
  forall (t :: DPT.DashboardTypeTag) m r.
  (BeamFlow m r, EncFlow m r, DPT.KnownDashboardType t) =>
  Text ->
  Text ->
  m (Maybe Person)
findByMobileNumberWithType mobileNumber mobileCountryCode = do
  mobileDbHash <- getDbHash mobileNumber
  findOneWithKV
    [ Se.Is BeamP.mobileNumberHash $ Se.Eq mobileDbHash,
      Se.Is BeamP.mobileCountryCode $ Se.Eq mobileCountryCode,
      Se.Is BeamP.dashboardType $ Se.Eq (DPT.dashboardTypeVal (Proxy @t))
    ]

findByMobileNumber ::
  forall m r.
  (BeamFlow m r, EncFlow m r) =>
  Text ->
  Text ->
  m (Maybe Person)
findByMobileNumber = findByMobileNumberWithType @'DPT.DefaultDashboard

findByMobileNumberAndRoleIdWithType ::
  forall (t :: DPT.DashboardTypeTag) m r.
  (BeamFlow m r, EncFlow m r, DPT.KnownDashboardType t) =>
  Text ->
  Text ->
  Id Role ->
  m (Maybe Person)
findByMobileNumberAndRoleIdWithType mobileNumber mobileCountryCode roleId = do
  mobileDbHash <- getDbHash mobileNumber
  findOneWithKV [Se.And [Se.Is BeamP.mobileNumberHash $ Se.Eq mobileDbHash, Se.Is BeamP.mobileCountryCode $ Se.Eq mobileCountryCode, Se.Is BeamP.roleId $ Se.Eq $ getId roleId, Se.Is BeamP.dashboardType $ Se.Eq (DPT.dashboardTypeVal (Proxy @t))]]

findByMobileNumberAndRoleId ::
  (BeamFlow m r, EncFlow m r) =>
  Text ->
  Text ->
  Id Role ->
  m (Maybe Person)
findByMobileNumberAndRoleId mobileNumber mobileCountryCode roleId = findByMobileNumberAndRoleIdWithType @'DPT.DefaultDashboard mobileNumber mobileCountryCode roleId

findByMobileNumberAndRoleIdsWithType ::
  forall (t :: DPT.DashboardTypeTag) m r.
  (BeamFlow m r, EncFlow m r, DPT.KnownDashboardType t) =>
  Text ->
  Text ->
  [Id Role] ->
  m (Maybe Person)
findByMobileNumberAndRoleIdsWithType mobileNumber mobileCountryCode roleIds = do
  mobileDbHash <- getDbHash mobileNumber
  findOneWithKV [Se.And [Se.Is BeamP.mobileNumberHash $ Se.Eq mobileDbHash, Se.Is BeamP.mobileCountryCode $ Se.Eq mobileCountryCode, Se.Is BeamP.roleId $ Se.In $ getId <$> roleIds, Se.Is BeamP.dashboardType $ Se.Eq (DPT.dashboardTypeVal (Proxy @t))]]

findByMobileNumberAndRoleIds ::
  (BeamFlow m r, EncFlow m r) =>
  Text ->
  Text ->
  [Id Role] ->
  m (Maybe Person)
findByMobileNumberAndRoleIds mobileNumber mobileCountryCode roleIds = findByMobileNumberAndRoleIdsWithType @'DPT.DefaultDashboard mobileNumber mobileCountryCode roleIds

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

updatePersonRole :: BeamFlow m r => Id Person -> Role -> m ()
updatePersonRole personId role = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.roleId $ getId role.id,
      Se.Set BeamP.dashboardAccessType $ Just role.dashboardAccessType,
      Se.Set BeamP.updatedAt now
    ]
    [ Se.Is BeamP.id $ Se.Eq $ getId personId
    ]

updatePersonPassword :: BeamFlow m r => Id Person -> DbHash -> m ()
updatePersonPassword personId newPasswordHash = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.passwordHash $ Just newPasswordHash,
      Se.Set BeamP.updatedAt now,
      Se.Set BeamP.passwordUpdatedAt $ Just now
    ]
    [ Se.Is BeamP.id $ Se.Eq $ getId personId
    ]

updatePersonPasswordUpdatedAt :: BeamFlow m r => Id Person -> m ()
updatePersonPasswordUpdatedAt personId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.passwordUpdatedAt $ Just now
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
            dashboardType = dashboardType,
            approvedBy = approvedBy <&> Id,
            rejectedBy = rejectedBy <&> Id,
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
        dashboardType = dashboardType,
        approvedBy = approvedBy <&> getId,
        rejectedBy = rejectedBy <&> getId,
        ..
      }

findAllByFromDateAndToDateAndMobileNumberAndStatusWithLimitOffset ::
  (BeamFlow m r, EncFlow m r) =>
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Text ->
  Maybe FleetOwnerStatus ->
  Maybe Int ->
  Maybe Int ->
  m [Person]
findAllByFromDateAndToDateAndMobileNumberAndStatusWithLimitOffset mbFromDate mbToDate mbMobileNumber mbStatus mbLimit mbOffset = do
  mbMobileNumberDbHash <- traverse getDbHash mbMobileNumber
  findAllWithOptionsDb
    [ Se.And
        ( [ Se.Or
              [ Se.Is BeamP.verified $ Se.Eq (Just False),
                Se.Is BeamP.verified $ Se.Eq Nothing
              ]
          ]
            <> [Se.Is BeamP.createdAt $ Se.GreaterThanOrEq (fromJust mbFromDate) | isJust mbFromDate]
            <> [Se.Is BeamP.createdAt $ Se.LessThanOrEq (fromJust mbToDate) | isJust mbToDate]
            <> [Se.Is BeamP.mobileNumberHash $ Se.Eq (fromJust mbMobileNumberDbHash) | isJust mbMobileNumber]
            <> [Se.Is BeamP.verified $ checkStatus | isJust mbStatus]
            <> [Se.Is BeamP.dashboardType $ Se.Eq DPT.DEFAULT_DASHBOARD]
        )
    ]
    (Se.Asc BeamP.createdAt)
    (Just . min 10 . fromMaybe 5 $ mbLimit)
    (Just $ fromMaybe 0 mbOffset)
  where
    checkStatus =
      case mbStatus of
        Just Rejected -> Se.Eq (Just False)
        _ -> Se.Not $ Se.Eq (Just False)

softDeletePerson :: BeamFlow m r => Id Person -> Maybe Text -> m ()
softDeletePerson personId mbReason = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.verified $ Just False,
      Se.Set BeamP.updatedAt now,
      Se.Set BeamP.rejectionReason mbReason,
      Se.Set BeamP.rejectedAt $ Just now
    ]
    [ Se.Is BeamP.id $ Se.Eq $ getId personId
    ]

updatePersonApprovedBy :: BeamFlow m r => Id Person -> Id Person -> m ()
updatePersonApprovedBy personId approverId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.approvedBy $ Just (getId approverId),
      Se.Set BeamP.updatedAt now
    ]
    [ Se.Is BeamP.id $ Se.Eq $ getId personId
    ]

updatePersonRejectedBy :: BeamFlow m r => Id Person -> Id Person -> m ()
updatePersonRejectedBy personId rejecterId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamP.rejectedBy $ Just (getId rejecterId),
      Se.Set BeamP.updatedAt now
    ]
    [ Se.Is BeamP.id $ Se.Eq $ getId personId
    ]

findByIdWithRoleAndCheckMobileHash ::
  BeamFlow m r =>
  Id Person ->
  Maybe DbHash ->
  m (Maybe (Person, Role), [Person])
findByIdWithRoleAndCheckMobileHash personId mbMobileHash = do
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $ do
        person <- B.all_ (SBC.person SBC.atlasDB)
        role <- B.join_' (SBC.role SBC.atlasDB) (\role -> BeamP.roleId person B.==?. BeamR.id role)
        _ <-
          B.filter_
            ( \_ ->
                case mbMobileHash of
                  Just mobileHash ->
                    (person.id B.==. B.val_ (getId personId)) B.||. (person.mobileNumberHash B.==. B.val_ mobileHash)
                  Nothing ->
                    person.id B.==. B.val_ (getId personId)
            )
            (pure ())

        pure (person, role)

  case res of
    Right res' -> do
      finalRes <- forM res' $ \(person, role) -> runMaybeT $ do
        p <- MaybeT $ fromTType' person
        r <- MaybeT $ fromTType' role
        pure (p, r)

      let results = catMaybes finalRes

      let targetPersonAndRole = find (\(p, _) -> p.id == personId) results

          conflictingPersons =
            map fst $
              filter
                ( \(p, _) ->
                    p.id /= personId
                      && case mbMobileHash of
                        Just hash -> p.mobileNumber.hash == hash
                        Nothing -> False
                )
                results

      pure (targetPersonAndRole, conflictingPersons)
    Left _ -> pure (Nothing, [])
