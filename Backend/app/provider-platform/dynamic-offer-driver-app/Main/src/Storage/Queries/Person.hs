{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Person where

import Control.Applicative ((<|>))
import qualified Data.Maybe as Mb
import Data.Text (splitOn)
import qualified Domain.Types.Booking as Booking
import Domain.Types.DriverInformation as DriverInfo
import Domain.Types.DriverLocation
import Domain.Types.Merchant
import Domain.Types.Person as Person
import Domain.Types.Ride as Ride
import Domain.Types.Vehicle as Vehicle
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.External.Encryption
import Kernel.External.FCM.Types (FCMRecipientToken)
import qualified Kernel.External.FCM.Types as FCM
import Kernel.External.Maps as Maps
import qualified Kernel.External.Whatsapp.Interface.Types as Whatsapp (OptApiMethods)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common hiding (Value)
import Kernel.Utils.GenericPretty
import Kernel.Utils.Version
import qualified Lib.Mesh as Mesh
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.Person as BeamP
import Storage.Tabular.Booking
import Storage.Tabular.Booking.BookingLocation
import Storage.Tabular.DriverInformation
import Storage.Tabular.DriverLocation
import Storage.Tabular.DriverOnboarding.DriverLicense
import Storage.Tabular.DriverOnboarding.DriverRCAssociation
import Storage.Tabular.DriverOnboarding.VehicleRegistrationCertificate
import Storage.Tabular.DriverQuote
import Storage.Tabular.Person as TPerson
import Storage.Tabular.Ride
import qualified Storage.Tabular.VechileNew as VN
import Storage.Tabular.Vehicle as Vehicle

baseFullPersonQuery ::
  From
    ( Table PersonT
        :& Table DriverLocationT
        :& Table DriverInformationT
        :& Table VehicleT
    )
baseFullPersonQuery =
  table @PersonT
    `innerJoin` table @DriverLocationT
    `Esq.on` ( \(person :& location) ->
                 person ^. PersonTId ==. location ^. DriverLocationDriverId
             )
    `innerJoin` table @DriverInformationT
    `Esq.on` ( \(person :& _ :& driverInfo) ->
                 person ^. PersonTId ==. driverInfo ^. DriverInformationDriverId
             )
    `innerJoin` table @VehicleT
    `Esq.on` ( \(person :& _ :& _ :& vehicle) ->
                 person ^. PersonTId ==. vehicle ^. VehicleDriverId
             )

-- create :: Person -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => Person.Person -> m (MeshResult ())
create person = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' VN.meshConfig (transformDomainPersonToBeam person)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- findById ::
--   Transactionable m =>
--   Id Person ->
--   m (Maybe Person)
-- findById = Esq.findById

findById :: (L.MonadFlow m, Log m) => Id Person -> m (Maybe Person)
findById (Id personId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' VN.meshConfig [Se.Is BeamP.id $ Se.Eq personId]
      case result of
        Right p -> traverse transformBeamPersonToDomain p
        Left _ -> pure Nothing
    Nothing -> pure Nothing

data FullDriver = FullDriver
  { person :: Person,
    location :: DriverLocation,
    info :: DriverInformation,
    vehicle :: Vehicle
  }

mkFullDriver :: (Person, DriverLocation, DriverInformation, Vehicle) -> FullDriver
mkFullDriver (p, l, i, v) = FullDriver p l i v

findAllDriversWithInfoAndVehicle ::
  Transactionable m =>
  Id Merchant ->
  Int ->
  Int ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe DbHash ->
  Maybe Text ->
  m [(Person, DriverInformation, Maybe Vehicle)]
findAllDriversWithInfoAndVehicle merchantId limitVal offsetVal mbVerified mbEnabled mbBlocked mbSearchPhoneDBHash mbVehicleNumberSearchString = do
  Esq.findAll $ do
    person :& info :& mbVeh <-
      from $
        table @PersonT
          `innerJoin` table @DriverInformationT
            `Esq.on` ( \(person :& driverInfo) ->
                         person ^. PersonTId ==. driverInfo ^. DriverInformationDriverId
                     )
          `leftJoin` table @VehicleT
            `Esq.on` ( \(person :& _ :& mbVehicle) ->
                         just (person ^. PersonTId) ==. mbVehicle ?. VehicleDriverId
                     )
    where_ $
      person ^. PersonMerchantId ==. (val . toKey $ merchantId)
        &&. person ^. PersonRole ==. val Person.DRIVER
        &&. maybe (val True) (\vehicleNumber -> mbVeh ?. VehicleRegistrationNo `Esq.like` just (val vehicleNumber)) mbVehicleNumberSearchString
        &&. maybe (val True) (\verified -> info ^. DriverInformationVerified ==. val verified) mbVerified
        &&. maybe (val True) (\enabled -> info ^. DriverInformationEnabled ==. val enabled) mbEnabled
        &&. maybe (val True) (\blocked -> info ^. DriverInformationBlocked ==. val blocked) mbBlocked
        &&. maybe (val True) (\searchStrDBHash -> person ^. PersonMobileNumberHash ==. val (Just searchStrDBHash)) mbSearchPhoneDBHash
    limit $ fromIntegral limitVal
    offset $ fromIntegral offsetVal
    pure (person, info, mbVeh)

-- countDrivers :: Transactionable m => Id Merchant -> m Int
-- countDrivers merchantId =
--   mkCount <$> do
--     Esq.findAll $ do
--       person <- from $ table @PersonT
--       where_ $
--         person ^. PersonMerchantId ==. val (toKey merchantId)
--           &&. person ^. PersonRole ==. val Person.DRIVER
--       return (countRows :: SqlExpr (Esq.Value Int))
--   where
--     mkCount [counter] = counter
--     mkCount _ = 0

findAllDriversByIdsFirstNameAsc ::
  (Transactionable m, Functor m) =>
  Id Merchant ->
  [Id Person] ->
  m [FullDriver]
findAllDriversByIdsFirstNameAsc merchantId driverIds = fmap (map mkFullDriver) $
  Esq.findAll $ do
    (person :& driverLocation :& driverInfo :& vehicle) <-
      from baseFullPersonQuery
    where_ $
      person ^. PersonRole ==. val Person.DRIVER
        &&. person ^. PersonTId `in_` valList (map toKey driverIds)
        &&. person ^. PersonMerchantId ==. (val . toKey $ merchantId)
    orderBy [asc (person ^. PersonFirstName)]
    return (person, driverLocation, driverInfo, vehicle)

data DriverWithRidesCount = DriverWithRidesCount
  { person :: Person,
    info :: DriverInformation,
    vehicle :: Maybe Vehicle,
    ridesCount :: Maybe Int
  }

mkDriverWithRidesCount :: (Person, DriverInformation, Maybe Vehicle, Maybe Int) -> DriverWithRidesCount
mkDriverWithRidesCount (person, info, vehicle, ridesCount) = DriverWithRidesCount {..}

fetchDriverInfoWithRidesCount :: (Transactionable m, MonadTime m) => Id Merchant -> Maybe (DbHash, Text) -> Maybe Text -> Maybe DbHash -> Maybe DbHash -> m (Maybe DriverWithRidesCount)
fetchDriverInfoWithRidesCount merchantId mbMobileNumberDbHashWithCode mbVehicleNumber mbDlNumberHash mbRcNumberHash = do
  mbDriverInfo <- fetchDriverInfo merchantId mbMobileNumberDbHashWithCode mbVehicleNumber mbDlNumberHash mbRcNumberHash
  addRidesCount `mapM` mbDriverInfo
  where
    addRidesCount :: Transactionable m => (Person, DriverInformation, Maybe Vehicle) -> m DriverWithRidesCount
    addRidesCount (person, info, vehicle) = do
      ridesCount <-
        Esq.findOne $ do
          ride <- from $ table @RideT
          where_ $
            ride ^. RideDriverId ==. val (toKey person.id)
              &&. not_ (ride ^. RideStatus `in_` valList [Ride.NEW, Ride.CANCELLED])
          groupBy $ ride ^. RideDriverId
          return (count @Int $ ride ^. RideId)
      return $ mkDriverWithRidesCount (person, info, vehicle, ridesCount)

fetchDriverInfo :: (Transactionable m, MonadTime m) => Id Merchant -> Maybe (DbHash, Text) -> Maybe Text -> Maybe DbHash -> Maybe DbHash -> m (Maybe (Person, DriverInformation, Maybe Vehicle))
fetchDriverInfo merchantId mbMobileNumberDbHashWithCode mbVehicleNumber mbDlNumberHash mbRcNumberHash = do
  now <- getCurrentTime
  Esq.findOne $ do
    person :& driverInfo :& mbVehicle :& mbDriverLicense :& _mbRcAssoc :& mbRegCert <-
      from $
        table @PersonT
          `innerJoin` table @DriverInformationT
          `Esq.on` ( \(person :& driverInfo) ->
                       person ^. PersonTId ==. driverInfo ^. DriverInformationDriverId
                   )
          `leftJoin` table @VehicleT
          `Esq.on` ( \(person :& _ :& mbVehicle) ->
                       just (person ^. PersonTId) ==. mbVehicle ?. VehicleDriverId
                   )
          `leftJoin` table @DriverLicenseT
          `Esq.on` ( \(person :& _ :& _ :& mbDriverLicense) ->
                       joinOnlyWhenJust mbDlNumberHash $ just (person ^. PersonTId) ==. mbDriverLicense ?. DriverLicenseDriverId
                   )
          `leftJoin` table @DriverRCAssociationT
          `Esq.on` ( \(person :& _ :& _ :& _ :& mbRcAssoc) ->
                       joinOnlyWhenJust mbRcNumberHash $
                         do
                           just (person ^. PersonTId) ==. mbRcAssoc ?. DriverRCAssociationDriverId
                           &&. just (just (val now)) <. mbRcAssoc ?. DriverRCAssociationAssociatedTill
                   )
          `leftJoin` table @VehicleRegistrationCertificateT
          `Esq.on` ( \(_ :& _ :& _ :& _ :& mbRcAssoc :& mbRegCert) ->
                       joinOnlyWhenJust mbRcNumberHash $
                         mbRcAssoc ?. DriverRCAssociationRcId ==. mbRegCert ?. VehicleRegistrationCertificateTId
                   )
    where_ $
      person ^. PersonMerchantId ==. (val . toKey $ merchantId)
        &&. person ^. PersonRole ==. val Person.DRIVER
        &&. whenJust_
          mbMobileNumberDbHashWithCode
          ( \(mobileNumberDbHash, mobileCountryCode) ->
              person ^. PersonMobileCountryCode ==. val (Just mobileCountryCode)
                &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberDbHash)
          )
        &&. whenJust_ mbVehicleNumber (\vehicleNumber -> mbVehicle ?. VehicleRegistrationNo ==. just (val vehicleNumber))
        &&. whenJust_ mbDlNumberHash (\dlNumberHash -> mbDriverLicense ?. DriverLicenseLicenseNumberHash ==. just (val dlNumberHash))
        &&. whenJust_ mbRcNumberHash (\rcNumberHash -> mbRegCert ?. VehicleRegistrationCertificateCertificateNumberHash ==. just (val rcNumberHash))
    pure (person, driverInfo, mbVehicle)
  where
    -- used only for dl and rc entites, because they are not required for final result, only for filters
    joinOnlyWhenJust mbFilter cond = maybe (val False) (const cond) mbFilter

-- findByIdAndRoleAndMerchantId ::
--   Transactionable m =>
--   Id Person ->
--   Person.Role ->
--   Id Merchant ->
--   m (Maybe Person)
-- findByIdAndRoleAndMerchantId pid role_ merchantId =
--   Esq.findOne $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonTId ==. val (toKey pid)
--         &&. person ^. PersonRole ==. val role_
--         &&. person ^. PersonMerchantId ==. val (toKey merchantId)
--     return person

findByIdAndRoleAndMerchantId :: (L.MonadFlow m, Log m) => Id Person -> Person.Role -> Id Merchant -> m (Maybe Person)
findByIdAndRoleAndMerchantId (Id pid) role_ (Id merchantId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' VN.meshConfig [Se.And [Se.Is BeamP.id $ Se.Eq pid, Se.Is BeamP.role $ Se.Eq role_, Se.Is BeamP.merchantId $ Se.Eq merchantId]]
      case result of
        Right p -> traverse transformBeamPersonToDomain p
        Left _ -> pure Nothing
    Nothing -> pure Nothing

-- findAllByMerchantId ::
--   Transactionable m =>
--   [Person.Role] ->
--   Id Merchant ->
--   m [Person]
-- findAllByMerchantId roles merchantId =
--   Esq.findAll $ do
--     person <- from $ table @PersonT
--     where_ $
--       (person ^. PersonRole `in_` valList roles ||. val (null roles))
--         &&. person ^. PersonMerchantId ==. val (toKey merchantId)
--     return person

findAllByMerchantId :: (L.MonadFlow m, Log m) => [Person.Role] -> Id Merchant -> m [Person]
findAllByMerchantId roles (Id merchantId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findAllWithKVConnector dbConf' VN.meshConfig [Se.And [Se.Is BeamP.merchantId $ Se.Eq merchantId, Se.Is BeamP.role $ Se.In roles]]
      case result of
        Right p -> traverse transformBeamPersonToDomain p
        Left _ -> pure []
    Nothing -> pure []

-- findAdminsByMerchantId :: Transactionable m => Id Merchant -> m [Person]
-- findAdminsByMerchantId merchantId =
--   Esq.findAll $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonMerchantId ==. val (toKey merchantId)
--         &&. person ^. PersonRole ==. val Person.ADMIN
--     return person

findAdminsByMerchantId :: (L.MonadFlow m, Log m) => Id Merchant -> m [Person]
findAdminsByMerchantId (Id merchantId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findAllWithKVConnector dbConf' VN.meshConfig [Se.And [Se.Is BeamP.merchantId $ Se.Eq merchantId, Se.Is BeamP.role $ Se.Eq Person.ADMIN]]
      case result of
        Right p -> traverse transformBeamPersonToDomain p
        Left _ -> pure []
    Nothing -> pure []

-- findByMobileNumberAndMerchant ::
--   (Transactionable m) =>
--   Text ->
--   DbHash ->
--   Id Merchant ->
--   m (Maybe Person)
-- findByMobileNumberAndMerchant countryCode mobileNumberHash merchantId = do
--   findOne $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonMobileCountryCode ==. val (Just countryCode)
--         &&. ( person ^. PersonMobileNumberHash ==. val (Just mobileNumberHash)
--                 ||. person ^. PersonAlternateMobileNumberHash ==. val (Just mobileNumberHash)
--             )
--         &&. person ^. PersonMerchantId ==. val (toKey merchantId)
--     return person

findByMobileNumberAndMerchant :: (L.MonadFlow m, Log m) => Text -> DbHash -> Id Merchant -> m (Maybe Person)
findByMobileNumberAndMerchant countryCode mobileNumberHash (Id merchantId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <-
        KV.findWithKVConnector
          dbConf'
          VN.meshConfig
          [ Se.And
              [ Se.Is BeamP.mobileCountryCode $ Se.Eq $ Just countryCode,
                Se.Is BeamP.merchantId $ Se.Eq merchantId,
                Se.Or [Se.Is BeamP.mobileNumberHash $ Se.Eq $ Just mobileNumberHash, Se.Is BeamP.alternateMobileNumberHash $ Se.Eq $ Just mobileNumberHash]
              ]
          ]
      case result of
        Right p -> traverse transformBeamPersonToDomain p
        Left _ -> pure Nothing
    Nothing -> pure Nothing

-- findByIdentifierAndMerchant ::
--   Transactionable m =>
--   Id Merchant ->
--   Text ->
--   m (Maybe Person)
-- findByIdentifierAndMerchant merchantId identifier_ =
--   findOne $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonIdentifier ==. val (Just identifier_)
--         &&. person ^. PersonMerchantId ==. val (toKey merchantId)
--     return person

findByIdentifierAndMerchant :: (L.MonadFlow m, Log m) => Id Merchant -> Text -> m (Maybe Person)
findByIdentifierAndMerchant (Id merchantId) identifier_ = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' VN.meshConfig [Se.And [Se.Is BeamP.identifier $ Se.Eq $ Just identifier_, Se.Is BeamP.merchantId $ Se.Eq merchantId]]
      case result of
        Right p -> traverse transformBeamPersonToDomain p
        Left _ -> pure Nothing
    Nothing -> pure Nothing

-- findByEmailAndMerchant ::
--   Transactionable m =>
--   Id Merchant ->
--   Text ->
--   m (Maybe Person)
-- findByEmailAndMerchant merchantId email_ =
--   findOne $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonEmail ==. val (Just email_)
--         &&. person ^. PersonMerchantId ==. val (toKey merchantId)
--     return person

findByEmailAndMerchant :: (L.MonadFlow m, Log m) => Id Merchant -> Text -> m (Maybe Person)
findByEmailAndMerchant (Id merchantId) email_ = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' VN.meshConfig [Se.And [Se.Is BeamP.email $ Se.Eq $ Just email_, Se.Is BeamP.merchantId $ Se.Eq merchantId]]
      case result of
        Right p -> traverse transformBeamPersonToDomain p
        Left _ -> pure Nothing
    Nothing -> pure Nothing

-- findByRoleAndMobileNumberAndMerchantId ::
--   (Transactionable m, EncFlow m r) =>
--   Role ->
--   Text ->
--   Text ->
--   Id Merchant ->
--   m (Maybe Person)
-- findByRoleAndMobileNumberAndMerchantId role_ countryCode mobileNumber_ merchantId = do
--   mobileNumberDbHash <- getDbHash mobileNumber_
--   findOne $ do
--     person <- from $ table @PersonT
--     where_ $
--       person ^. PersonRole ==. val role_
--         &&. person ^. PersonMobileCountryCode ==. val (Just countryCode)
--         &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberDbHash)
--         &&. person ^. PersonMerchantId ==. val (toKey merchantId)
--     return person

findByRoleAndMobileNumberAndMerchantId :: (L.MonadFlow m, Log m, EncFlow m r) => Role -> Text -> Text -> Id Merchant -> m (Maybe Person)
findByRoleAndMobileNumberAndMerchantId role_ countryCode mobileNumber_ (Id merchantId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  mobileNumberDbHash <- getDbHash mobileNumber_
  case dbConf of
    Just dbConf' -> do
      result <-
        KV.findWithKVConnector
          dbConf'
          VN.meshConfig
          [ Se.And
              [ Se.Is BeamP.role $ Se.Eq role_,
                Se.Is BeamP.mobileCountryCode $ Se.Eq $ Just countryCode,
                Se.Is BeamP.mobileNumberHash $ Se.Eq $ Just mobileNumberDbHash,
                Se.Is BeamP.merchantId $ Se.Eq merchantId
              ]
          ]
      case result of
        Right p -> traverse transformBeamPersonToDomain p
        Left _ -> pure Nothing
    Nothing -> pure Nothing

personDriverTable ::
  From
    ( Table PersonT
        :& Table DriverInformationT
    )
personDriverTable =
  table @PersonT
    `innerJoin` table @DriverInformationT
    `Esq.on` ( \(person :& driver) ->
                 person ^. PersonTId ==. driver ^. DriverInformationDriverId
                   &&. Esq.not_ (driver ^. DriverInformationBlocked)
             )

findAllDriverIdExceptProvided :: Transactionable m => Id Merchant -> [Id Driver] -> m [Id Driver]
findAllDriverIdExceptProvided merchantId driverIdsToBeExcluded = do
  res <- Esq.findAll $ do
    (person :& driver) <- from personDriverTable
    where_ $
      person ^. PersonMerchantId ==. val (toKey merchantId)
        &&. not_ ((driver ^. DriverInformationDriverId) `Esq.in_` valList (map (toKey . driverIdToPersonId) driverIdsToBeExcluded))
        &&. driver ^. DriverInformationVerified
        &&. driver ^. DriverInformationEnabled
    return $ driver ^. DriverInformationDriverId
  pure $ personIdToDrivrId <$> res
  where
    personIdToDrivrId :: Id Person -> Id Driver
    personIdToDrivrId = cast

    driverIdToPersonId :: Id Driver -> Id Person
    driverIdToPersonId = cast

-- updateMerchantIdAndMakeAdmin :: Id Person -> Id Merchant -> SqlDB ()
-- updateMerchantIdAndMakeAdmin personId merchantId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonMerchantId =. val (toKey merchantId),
--         PersonRole =. val Person.ADMIN,
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateMerchantIdAndMakeAdmin :: (L.MonadFlow m, MonadTime m) => Id Person -> Id Merchant -> m (MeshResult ())
updateMerchantIdAndMakeAdmin (Id personId) (Id merchantId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        VN.meshConfig
        [ Se.Set BeamP.merchantId merchantId,
          Se.Set BeamP.role Person.ADMIN,
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- updateName :: Id Person -> Text -> SqlDB ()
-- updateName personId name = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonFirstName =. val name,
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateName :: (L.MonadFlow m, MonadTime m) => Id Person -> Text -> m (MeshResult ())
updateName (Id personId) name = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        VN.meshConfig
        [ Se.Set BeamP.firstName name,
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- updatePersonRec :: Id Person -> Person -> SqlDB ()
-- updatePersonRec personId person = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonFirstName =. val (person.firstName),
--         PersonMiddleName =. val (person.middleName),
--         PersonLastName =. val (person.lastName),
--         PersonRole =. val (person.role),
--         PersonGender =. val (person.gender),
--         PersonEmail =. val (person.email),
--         PersonIdentifier =. val (person.identifier),
--         PersonRating =. val (person.rating),
--         PersonLanguage =. val (person.language),
--         PersonDeviceToken =. val (person.deviceToken),
--         PersonMerchantId =. val (toKey person.merchantId),
--         PersonDescription =. val (person.description),
--         PersonUpdatedAt =. val now,
--         PersonClientVersion =. val (versionToText <$> person.clientVersion),
--         PersonBundleVersion =. val (versionToText <$> person.bundleVersion)
--       ]
--     where_ $ tbl ^. PersonTId ==. val (toKey personId)

updatePersonRec :: (L.MonadFlow m, MonadTime m) => Id Person -> Person -> m (MeshResult ())
updatePersonRec (Id personId) person = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        VN.meshConfig
        [ Se.Set BeamP.firstName $ person.firstName,
          Se.Set BeamP.middleName $ person.middleName,
          Se.Set BeamP.lastName $ person.lastName,
          Se.Set BeamP.gender $ person.gender,
          Se.Set BeamP.email $ person.email,
          Se.Set BeamP.identifier $ person.identifier,
          Se.Set BeamP.rating $ person.rating,
          Se.Set BeamP.language $ person.language,
          Se.Set BeamP.deviceToken $ person.deviceToken,
          Se.Set BeamP.merchantId $ getId person.merchantId,
          Se.Set BeamP.description $ person.description,
          Se.Set BeamP.updatedAt now,
          Se.Set BeamP.clientVersion (versionToText <$> person.clientVersion),
          Se.Set BeamP.bundleVersion (versionToText <$> person.bundleVersion)
        ]
        [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- updatePersonVersions :: Person -> Maybe Version -> Maybe Version -> SqlDB ()
-- updatePersonVersions person mbBundleVersion mbClientVersion =
--   when
--     ((isJust mbBundleVersion || isJust mbClientVersion) && (person.bundleVersion /= mbBundleVersion || person.clientVersion /= mbClientVersion))
--     do
--       now <- getCurrentTime
--       let mbBundleVersionText = versionToText <$> (mbBundleVersion <|> person.bundleVersion)
--           mbClientVersionText = versionToText <$> (mbClientVersion <|> person.clientVersion)
--       Esq.update $ \tbl -> do
--         set
--           tbl
--           [ PersonUpdatedAt =. val now,
--             PersonClientVersion =. val mbClientVersionText,
--             PersonBundleVersion =. val mbBundleVersionText
--           ]
--         where_ $
--           tbl ^. PersonTId ==. val (toKey person.id)

updatePersonVersions :: (L.MonadFlow m, MonadTime m) => Person -> Maybe Version -> Maybe Version -> m ()
updatePersonVersions person mbBundleVersion mbClientVersion =
  when
    ((isJust mbBundleVersion || isJust mbClientVersion) && (person.bundleVersion /= mbBundleVersion || person.clientVersion /= mbClientVersion))
    do
      now <- getCurrentTime
      let mbBundleVersionText = versionToText <$> (mbBundleVersion <|> person.bundleVersion)
          mbClientVersionText = versionToText <$> (mbClientVersion <|> person.clientVersion)
      dbConf <- L.getOption Extra.EulerPsqlDbCfg
      case dbConf of
        Just dbConf' ->
          void $
            KV.updateWoReturningWithKVConnector
              dbConf'
              VN.meshConfig
              [ Se.Set BeamP.clientVersion mbClientVersionText,
                Se.Set BeamP.clientVersion mbBundleVersionText,
                Se.Set BeamP.updatedAt now
              ]
              [Se.Is BeamP.id (Se.Eq $ getId person.id)]
        Nothing -> pure ()

-- updateDeviceToken :: Id Person -> Maybe FCMRecipientToken -> SqlDB ()
-- updateDeviceToken personId mbDeviceToken = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonDeviceToken =. val mbDeviceToken,
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateDeviceToken :: (L.MonadFlow m, MonadTime m) => Id Person -> Maybe FCMRecipientToken -> m (MeshResult ())
updateDeviceToken (Id personId) mbDeviceToken = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        VN.meshConfig
        [ Se.Set BeamP.deviceToken mbDeviceToken,
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- updateWhatsappNotificationEnrollStatus :: Id Person -> Maybe Whatsapp.OptApiMethods -> SqlDB ()
-- updateWhatsappNotificationEnrollStatus personId enrollStatus = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonWhatsappNotificationEnrollStatus =. val enrollStatus,
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateWhatsappNotificationEnrollStatus :: (L.MonadFlow m, MonadTime m) => Id Person -> Maybe Whatsapp.OptApiMethods -> m (MeshResult ())
updateWhatsappNotificationEnrollStatus (Id personId) enrollStatus = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        VN.meshConfig
        [ Se.Set BeamP.whatsappNotificationEnrollStatus enrollStatus,
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- updateMobileNumberAndCode :: Person -> SqlDB ()
-- updateMobileNumberAndCode person = do
--   let personT = toTType person
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonMobileCountryCode =. val (TPerson.mobileCountryCode personT),
--         PersonMobileNumberEncrypted =. val (TPerson.mobileNumberEncrypted personT),
--         PersonMobileNumberHash =. val (TPerson.mobileNumberHash personT),
--         PersonUnencryptedMobileNumber =. val (TPerson.unencryptedMobileNumber personT),
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonTId ==. val (toKey person.id)

updateMobileNumberAndCode :: (L.MonadFlow m, MonadTime m, Log m, EncFlow m r) => Person -> m (MeshResult ())
updateMobileNumberAndCode person = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        VN.meshConfig
        [ Se.Set BeamP.mobileCountryCode $ person.mobileCountryCode,
          Se.Set BeamP.mobileNumberEncrypted $ person.mobileNumber <&> unEncrypted . (.encrypted),
          Se.Set BeamP.mobileNumberHash $ person.mobileNumber <&> (.hash),
          Se.Set BeamP.unencryptedMobileNumber $ person.unencryptedMobileNumber,
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq $ getId person.id)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- setIsNewFalse :: Id Person -> SqlDB ()
-- setIsNewFalse personId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonIsNew =. val False,
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonTId ==. val (toKey personId)

setIsNewFalse :: (L.MonadFlow m, MonadTime m) => Id Person -> m (MeshResult ())
setIsNewFalse (Id personId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        VN.meshConfig
        [ Se.Set BeamP.isNew False,
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

-- deleteById :: Id Person -> SqlDB ()
-- deleteById = Esq.deleteByKey @PersonT

deleteById :: L.MonadFlow m => Id Person -> m ()
deleteById (Id personId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          Mesh.meshConfig
          [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure ()

-- updateAverageRating :: Id Person -> Centesimal -> SqlDB ()
-- updateAverageRating personId newAverageRating = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonRating =. val (Just newAverageRating),
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateAverageRating :: (L.MonadFlow m, MonadTime m) => Id Person -> Centesimal -> m (MeshResult ())
updateAverageRating (Id personId) newAverageRating = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        VN.meshConfig
        [ Se.Set BeamP.rating (Just newAverageRating),
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq personId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

data NearestDriversResult = NearestDriversResult
  { driverId :: Id Driver,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    language :: Maybe Maps.Language,
    onRide :: Bool,
    distanceToDriver :: Meters,
    variant :: Vehicle.Variant,
    lat :: Double,
    lon :: Double,
    mode :: Maybe DriverInfo.DriverMode
  }
  deriving (Generic, Show, PrettyShow, HasCoordinates)

getNearestDrivers ::
  (Transactionable m, MonadTime m) =>
  Maybe Variant ->
  LatLong ->
  Int ->
  Id Merchant ->
  Bool ->
  Maybe Seconds ->
  m [NearestDriversResult]
getNearestDrivers mbVariant LatLong {..} radiusMeters merchantId onlyNotOnRide mbDriverPositionInfoExpiry = do
  now <- getCurrentTime
  res <- Esq.findAll $ do
    (person :& location :& driverInfo :& vehicle) <-
      from baseFullPersonQuery
    where_ $
      person ^. PersonRole ==. val Person.DRIVER
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
        &&. ((Esq.isNothing (driverInfo ^. DriverInformationMode) &&. driverInfo ^. DriverInformationActive) ||. (not_ (Esq.isNothing (driverInfo ^. DriverInformationMode)) &&. (driverInfo ^. DriverInformationMode ==. val (Just DriverInfo.SILENT) ||. driverInfo ^. DriverInformationMode ==. val (Just DriverInfo.ONLINE))))
        &&. (if onlyNotOnRide then not_ (driverInfo ^. DriverInformationOnRide) else val True)
        &&. not_ (driverInfo ^. DriverInformationBlocked)
        &&. ( val (Mb.isNothing mbDriverPositionInfoExpiry)
                ||. (location ^. DriverLocationCoordinatesCalculatedAt +. Esq.interval [Esq.SECOND $ maybe 0 getSeconds mbDriverPositionInfoExpiry] >=. val now)
            )
        &&. ( Esq.isNothing (val mbVariant) ||. just (vehicle ^. VehicleVariant) ==. val mbVariant -- when mbVariant = Nothing, we use all variants, is it correct?
                ||. ( case mbVariant of
                        Just SEDAN ->
                          driverInfo ^. DriverInformationCanDowngradeToSedan ==. val True
                            &&. vehicle ^. VehicleVariant ==. val SUV
                        Just HATCHBACK ->
                          driverInfo ^. DriverInformationCanDowngradeToHatchback ==. val True
                            &&. (vehicle ^. VehicleVariant ==. val SUV ||. vehicle ^. VehicleVariant ==. val SEDAN)
                        Just TAXI ->
                          driverInfo ^. DriverInformationCanDowngradeToTaxi ==. val True
                            &&. vehicle ^. VehicleVariant ==. val TAXI_PLUS
                        _ -> val False
                    )
            )
        &&. buildRadiusWithin (location ^. DriverLocationPoint) (lat, lon) (val radiusMeters)
    orderBy [asc (location ^. DriverLocationPoint <->. Esq.getPoint (val lat, val lon))]
    return
      ( person ^. PersonTId,
        person ^. PersonDeviceToken,
        person ^. PersonLanguage,
        driverInfo ^. DriverInformationOnRide,
        driverInfo ^. DriverInformationCanDowngradeToSedan,
        driverInfo ^. DriverInformationCanDowngradeToHatchback,
        driverInfo ^. DriverInformationCanDowngradeToTaxi,
        location ^. DriverLocationPoint <->. Esq.getPoint (val lat, val lon),
        location ^. DriverLocationLat,
        location ^. DriverLocationLon,
        vehicle ^. VehicleVariant,
        driverInfo ^. DriverInformationMode
      )
  return (makeNearestDriversResult =<< res)
  where
    makeNearestDriversResult :: (Id Person, Maybe FCM.FCMRecipientToken, Maybe Maps.Language, Bool, Bool, Bool, Bool, Double, Double, Double, Variant, Maybe DriverInfo.DriverMode) -> [NearestDriversResult]
    makeNearestDriversResult (personId, mbDeviceToken, mblang, onRide, canDowngradeToSedan, canDowngradeToHatchback, canDowngradeToTaxi, dist, dlat, dlon, variant, mode) = do
      case mbVariant of
        Nothing -> do
          let autoResult = getResult AUTO_RICKSHAW $ variant == AUTO_RICKSHAW
              suvResult = getResult SUV $ variant == SUV
              sedanResult = getResult SEDAN $ variant == SEDAN || (variant == SUV && canDowngradeToSedan)
              hatchbackResult = getResult HATCHBACK $ variant == HATCHBACK || ((variant == SUV || variant == SEDAN) && canDowngradeToHatchback)
              taxiPlusResult = getResult TAXI_PLUS $ variant == TAXI_PLUS
              taxiResult = getResult TAXI $ variant == TAXI || (variant == TAXI_PLUS && canDowngradeToTaxi)
          autoResult <> suvResult <> sedanResult <> hatchbackResult <> taxiResult <> taxiPlusResult
        Just poolVariant -> getResult poolVariant True
      where
        getResult var cond = [NearestDriversResult (cast personId) mbDeviceToken mblang onRide (roundToIntegral dist) var dlat dlon mode | cond]

data NearestDriversResultCurrentlyOnRide = NearestDriversResultCurrentlyOnRide
  { driverId :: Id Driver,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    language :: Maybe Maps.Language,
    onRide :: Bool,
    lat :: Double,
    lon :: Double,
    variant :: Vehicle.Variant,
    destinationLat :: Double,
    destinationLon :: Double,
    distanceToDriver :: Meters,
    distanceFromDriverToDestination :: Meters,
    mode :: Maybe DriverInfo.DriverMode
  }
  deriving (Generic, Show, PrettyShow, HasCoordinates)

baseFullPersonQueryWithRideInfo ::
  From
    ( Table PersonT
        :& Table DriverLocationT
        :& Table DriverInformationT
        :& Table VehicleT
        :& Table DriverQuoteT
        :& Table BookingT
        :& Table BookingLocationT
    )
baseFullPersonQueryWithRideInfo =
  table @PersonT
    `innerJoin` table @DriverLocationT
    `Esq.on` ( \(person :& location) ->
                 person ^. PersonTId ==. location ^. DriverLocationDriverId
             )
    `innerJoin` table @DriverInformationT
    `Esq.on` ( \(person :& _ :& driverInfo) ->
                 person ^. PersonTId ==. driverInfo ^. DriverInformationDriverId
             )
    `innerJoin` table @VehicleT
    `Esq.on` ( \(person :& _ :& _ :& vehicle) ->
                 person ^. PersonTId ==. vehicle ^. VehicleDriverId
             )
    `innerJoin` table @DriverQuoteT
    `Esq.on` ( \(person :& _ :& _ :& _ :& driverQuoteInfo) ->
                 person ^. PersonTId ==. driverQuoteInfo ^. DriverQuoteDriverId
             )
    `innerJoin` table @BookingT
    `Esq.on` ( \(_ :& _ :& _ :& _ :& driverQuoteInfo :& bookingInfo) ->
                 driverQuoteInfo ^. DriverQuoteId ==. bookingInfo ^. BookingQuoteId
                   &&. bookingInfo ^. BookingStatus ==. val Booking.TRIP_ASSIGNED
             )
    `innerJoin` table @BookingLocationT
    `Esq.on` ( \(_ :& _ :& _ :& _ :& _ :& bookingInfo :& bookingLocationInfo) ->
                 bookingInfo ^. BookingToLocationId ==. bookingLocationInfo ^. BookingLocationTId
             )

getNearestDriversCurrentlyOnRide ::
  (Transactionable m, MonadTime m) =>
  Maybe Variant ->
  LatLong ->
  Int ->
  Id Merchant ->
  Maybe Seconds ->
  Int ->
  m [NearestDriversResultCurrentlyOnRide]
getNearestDriversCurrentlyOnRide mbVariant LatLong {..} radiusMeters merchantId mbDriverPositionInfoExpiry reduceRadiusValue = do
  now <- getCurrentTime
  res <- Esq.findAll $ do
    (personInfo :& locationInfo :& driverInfo :& vehicleInfo :& _ :& _ :& bookingLocationInfo) <-
      from baseFullPersonQueryWithRideInfo
    let destinationPoint = Esq.getPoint (bookingLocationInfo ^. BookingLocationLat, bookingLocationInfo ^. BookingLocationLon)
        distanceFromDriverToDestination = locationInfo ^. DriverLocationPoint <->. destinationPoint
        onRideRadius = val (fromIntegral (radiusMeters - reduceRadiusValue) :: Double)
        distanceFromDestinationToPickup = Esq.getPoint (val lat, val lon) <->. destinationPoint
    where_ $
      personInfo ^. PersonRole ==. val Person.DRIVER
        &&. personInfo ^. PersonMerchantId ==. val (toKey merchantId)
        &&. ((Esq.isNothing (driverInfo ^. DriverInformationMode) &&. driverInfo ^. DriverInformationActive) ||. (not_ (Esq.isNothing (driverInfo ^. DriverInformationMode)) &&. (driverInfo ^. DriverInformationMode ==. val (Just DriverInfo.SILENT) ||. driverInfo ^. DriverInformationMode ==. val (Just DriverInfo.ONLINE))))
        &&. driverInfo ^. DriverInformationOnRide
        &&. not_ (driverInfo ^. DriverInformationBlocked)
        &&. ( val (Mb.isNothing mbDriverPositionInfoExpiry)
                ||. (locationInfo ^. DriverLocationCoordinatesCalculatedAt +. Esq.interval [Esq.SECOND $ maybe 0 getSeconds mbDriverPositionInfoExpiry] >=. val now)
            )
        &&. ( Esq.isNothing (val mbVariant) ||. just (vehicleInfo ^. VehicleVariant) ==. val mbVariant -- when mbVariant = Nothing, we use all variants, is it correct?
                ||. ( case mbVariant of
                        Just SEDAN ->
                          driverInfo ^. DriverInformationCanDowngradeToSedan ==. val True
                            &&. vehicleInfo ^. VehicleVariant ==. val SUV
                        Just HATCHBACK ->
                          driverInfo ^. DriverInformationCanDowngradeToHatchback ==. val True
                            &&. (vehicleInfo ^. VehicleVariant ==. val SUV ||. vehicleInfo ^. VehicleVariant ==. val SEDAN)
                        Just TAXI ->
                          driverInfo ^. DriverInformationCanDowngradeToTaxi ==. val True
                            &&. vehicleInfo ^. VehicleVariant ==. val TAXI_PLUS
                        _ -> val False
                    )
            )
        &&. (distanceFromDriverToDestination +. distanceFromDestinationToPickup) <. onRideRadius
    orderBy [asc (distanceFromDestinationToPickup +. distanceFromDriverToDestination)]
    pure
      ( personInfo ^. PersonTId,
        personInfo ^. PersonDeviceToken,
        personInfo ^. PersonLanguage,
        driverInfo ^. DriverInformationOnRide,
        driverInfo ^. DriverInformationCanDowngradeToSedan,
        driverInfo ^. DriverInformationCanDowngradeToHatchback,
        driverInfo ^. DriverInformationCanDowngradeToTaxi,
        locationInfo ^. DriverLocationLat,
        locationInfo ^. DriverLocationLon,
        vehicleInfo ^. VehicleVariant,
        bookingLocationInfo ^. BookingLocationLat,
        bookingLocationInfo ^. BookingLocationLon,
        distanceFromDriverToDestination +. distanceFromDestinationToPickup,
        distanceFromDriverToDestination,
        driverInfo ^. DriverInformationMode
      )
  return (makeNearestDriversResult =<< res)
  where
    makeNearestDriversResult :: (Id Person, Maybe FCM.FCMRecipientToken, Maybe Maps.Language, Bool, Bool, Bool, Bool, Double, Double, Variant, Double, Double, Double, Double, Maybe DriverInfo.DriverMode) -> [NearestDriversResultCurrentlyOnRide]
    makeNearestDriversResult (personId, mbDeviceToken, mblang, onRide, canDowngradeToSedan, canDowngradeToHatchback, canDowngradeToTaxi, dlat, dlon, variant, destinationEndLat, destinationEndLon, dist :: Double, distanceFromDriverToDestination :: Double, mode) =
      case mbVariant of
        Nothing -> do
          let autoResult = getResult AUTO_RICKSHAW $ variant == AUTO_RICKSHAW
              suvResult = getResult SUV $ variant == SUV
              sedanResult = getResult SEDAN $ variant == SEDAN || (variant == SUV && canDowngradeToSedan)
              hatchbackResult = getResult HATCHBACK $ variant == HATCHBACK || ((variant == SUV || variant == SEDAN) && canDowngradeToHatchback)
              taxiPlusResult = getResult TAXI_PLUS $ variant == TAXI_PLUS
              taxiResult = getResult TAXI $ variant == TAXI || (variant == TAXI_PLUS && canDowngradeToTaxi)
          autoResult <> suvResult <> sedanResult <> hatchbackResult <> taxiResult <> taxiPlusResult
        Just poolVariant -> getResult poolVariant True
      where
        getResult var cond = [NearestDriversResultCurrentlyOnRide (cast personId) mbDeviceToken mblang onRide dlat dlon var destinationEndLat destinationEndLon (roundToIntegral dist) (roundToIntegral distanceFromDriverToDestination) mode | cond]

-- updateAlternateMobileNumberAndCode :: Person -> SqlDB ()
-- updateAlternateMobileNumberAndCode person = do
--   now <- getCurrentTime
--   let personT = toTType person
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ PersonAlternateMobileNumberEncrypted =. val (TPerson.alternateMobileNumberEncrypted personT),
--         PersonUnencryptedAlternateMobileNumber =. val (TPerson.unencryptedAlternateMobileNumber personT),
--         PersonAlternateMobileNumberHash =. val (TPerson.alternateMobileNumberHash personT),
--         PersonUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. PersonTId ==. val (toKey person.id)

updateAlternateMobileNumberAndCode :: (L.MonadFlow m, MonadTime m) => Person -> m (MeshResult ())
updateAlternateMobileNumberAndCode person = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        VN.meshConfig
        [ Se.Set BeamP.alternateMobileNumberEncrypted (person.alternateMobileNumber <&> unEncrypted . (.encrypted)),
          Se.Set BeamP.unencryptedAlternateMobileNumber person.unencryptedAlternateMobileNumber,
          Se.Set BeamP.updatedAt now
        ]
        [Se.Is BeamP.id (Se.Eq $ getId person.id)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

transformBeamPersonToDomain :: (L.MonadFlow m, Log m) => BeamP.Person -> m Person
transformBeamPersonToDomain BeamP.PersonT {..} = do
  bundleVersion' <- forM bundleVersion readVersion
  clientVersion' <- forM clientVersion readVersion
  pure
    Person
      { id = Id id,
        firstName = firstName,
        middleName = middleName,
        lastName = lastName,
        role = role,
        gender = gender,
        identifierType = identifierType,
        email = email,
        unencryptedMobileNumber = unencryptedMobileNumber,
        mobileNumber = EncryptedHashed <$> (Encrypted <$> mobileNumberEncrypted) <*> mobileNumberHash,
        mobileCountryCode = mobileCountryCode,
        passwordHash = passwordHash,
        identifier = identifier,
        rating = rating,
        isNew = isNew,
        merchantId = Id merchantId,
        deviceToken = deviceToken,
        whatsappNotificationEnrollStatus = whatsappNotificationEnrollStatus,
        language = language,
        description = description,
        createdAt = createdAt,
        updatedAt = updatedAt,
        bundleVersion = bundleVersion',
        clientVersion = clientVersion',
        unencryptedAlternateMobileNumber = unencryptedAlternateMobileNumber,
        alternateMobileNumber = EncryptedHashed <$> (Encrypted <$> alternateMobileNumberEncrypted) <*> alternateMobileNumberHash
      }

transformDomainPersonToBeam :: Person -> BeamP.Person
transformDomainPersonToBeam Person {..} =
  BeamP.PersonT
    { BeamP.id = getId id,
      BeamP.firstName = firstName,
      BeamP.middleName = middleName,
      BeamP.lastName = lastName,
      BeamP.role = role,
      BeamP.gender = gender,
      BeamP.identifierType = identifierType,
      BeamP.email = email,
      BeamP.unencryptedMobileNumber = unencryptedMobileNumber,
      BeamP.mobileNumberEncrypted = mobileNumber <&> unEncrypted . (.encrypted),
      BeamP.mobileNumberHash = mobileNumber <&> (.hash),
      BeamP.mobileCountryCode = mobileCountryCode,
      BeamP.passwordHash = passwordHash,
      BeamP.identifier = identifier,
      BeamP.rating = rating,
      BeamP.isNew = isNew,
      BeamP.merchantId = getId merchantId,
      BeamP.deviceToken = deviceToken,
      BeamP.whatsappNotificationEnrollStatus = whatsappNotificationEnrollStatus,
      BeamP.language = language,
      BeamP.description = description,
      BeamP.createdAt = createdAt,
      BeamP.updatedAt = updatedAt,
      BeamP.bundleVersion = versionToText <$> bundleVersion,
      BeamP.clientVersion = versionToText <$> clientVersion,
      BeamP.unencryptedAlternateMobileNumber = unencryptedAlternateMobileNumber,
      BeamP.alternateMobileNumberHash = alternateMobileNumber <&> (.hash),
      BeamP.alternateMobileNumberEncrypted = alternateMobileNumber <&> unEncrypted . (.encrypted)
    }
