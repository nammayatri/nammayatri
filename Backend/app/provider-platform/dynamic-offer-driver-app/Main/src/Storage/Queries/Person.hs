{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Person
  ( module Storage.Queries.Person,
    module Reexport,
  )
where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HashMap
import Domain.Types.DriverInformation as DriverInfo
import Domain.Types.DriverLocation as DriverLocation
import Domain.Types.Merchant
import Domain.Types.Person as Person
import Domain.Types.Ride as Ride
import Domain.Types.Vehicle as DV
import Kernel.External.Encryption
import Kernel.External.Notification.FCM.Types (FCMRecipientToken)
import qualified Kernel.External.Whatsapp.Interface.Types as Whatsapp (OptApiMethods)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqLocRepDBFlow)
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common hiding (Value)
import qualified Storage.Queries.DriverInformation.Internal as Int
import qualified Storage.Queries.DriverLocation.Internal as Int
import Storage.Queries.Person.GetNearestDrivers as Reexport
import Storage.Queries.Person.GetNearestDriversCurrentlyOnRide as Reexport
import Storage.Queries.Person.GetNearestGoHomeDrivers as Reexport
import qualified Storage.Queries.Person.Internal as Int
import qualified Storage.Queries.Vehicle.Internal as Int
import Storage.Tabular.DriverInformation
import Storage.Tabular.DriverOnboarding.DriverLicense
import Storage.Tabular.DriverOnboarding.DriverRCAssociation
import Storage.Tabular.DriverOnboarding.VehicleRegistrationCertificate
import Storage.Tabular.Person as TPerson
import Storage.Tabular.Ride
import Storage.Tabular.Vehicle as Vehicle

create :: Person -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id Person ->
  m (Maybe Person)
findById = Esq.findById

findAllDriversWithInfoAndVehicle ::
  Transactionable m =>
  Id Merchant ->
  Int ->
  Int ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe DbHash ->
  Maybe Text ->
  m [(Person, DriverInformation, Maybe Vehicle)]
findAllDriversWithInfoAndVehicle merchantId limitVal offsetVal mbVerified mbEnabled mbBlocked mbSubscribed mbSearchPhoneDBHash mbVehicleNumberSearchString = do
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
        &&. maybe (val True) (\subscribed -> info ^. DriverInformationSubscribed ==. val subscribed) mbSubscribed
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

data FullDriver = FullDriver
  { person :: Person,
    location :: DriverLocation,
    info :: DriverInformation,
    vehicle :: Vehicle
  }

findAllDriversByIdsFirstNameAsc ::
  (Transactionable m, Functor m, EsqLocRepDBFlow m r, EsqDBReplicaFlow m r) =>
  Id Merchant ->
  [Id Person] ->
  m [FullDriver]
findAllDriversByIdsFirstNameAsc merchantId driverIds = do
  driverLocs <- Int.getDriverLocsWithMerchantId driverIds merchantId
  driverInfos <- Int.getDriverInfos (driverLocs <&> (.driverId))
  vehicle <- Int.getVehicles (driverInfos <&> (.driverId))
  drivers <- Int.getDrivers (vehicle <&> (.driverId))
  logDebug $ "FindAllDriversByIdsFirstNameAsc - DLoc:- " <> show (length driverLocs) <> " DInfo:- " <> show (length driverInfos) <> " Vec:- " <> show (length vehicle) <> " Drivers:- " <> show (length drivers)
  return (linkArrays driverLocs driverInfos vehicle drivers)
  where
    linkArrays driverLocations driverInformations vehicles persons =
      let personHashMap = HashMap.fromList $ (\p -> (p.id, p)) <$> persons
          vehicleHashMap = HashMap.fromList $ (\v -> (v.driverId, v)) <$> vehicles
          driverInfoHashMap = HashMap.fromList $ (\di -> (di.driverId, di)) <$> driverInformations
       in mapMaybe (buildFullDriver personHashMap vehicleHashMap driverInfoHashMap) driverLocations

    buildFullDriver personHashMap vehicleHashMap driverInfoHashMap location = do
      let driverId' = location.driverId
      person <- HashMap.lookup driverId' personHashMap
      vehicle <- HashMap.lookup driverId' vehicleHashMap
      info <- HashMap.lookup driverId' driverInfoHashMap
      Just $ FullDriver person location info vehicle
      
getOnRideStuckDriverIds :: Transactionable m => m [DriverInformation]
getOnRideStuckDriverIds = do
  Esq.findAll $ do
    driverInfos <- from $ table @DriverInformationT
    where_ $
      driverInfos ^. DriverInformationOnRide ==. val True
        &&. not_ (driverInfos ^. DriverInformationDriverId `in_` rideSubQuery)
    return driverInfos
  where
    rideSubQuery = subList_select $ do
      r <- from $ table @RideT
      where_ (r ^. RideStatus `in_` valList [Ride.INPROGRESS, Ride.NEW])
      pure (r ^. RideDriverId)

getDriversByIdIn ::
  Transactionable m =>
  [Id Person] ->
  m [Person]
getDriversByIdIn driverIds = do
  Esq.findAll $ do
    persons <- from $ table @PersonT
    where_ $
      persons ^. PersonTId `in_` valList personIds
    return persons
  where
    personIds = toKey . cast <$> driverIds

data DriverWithRidesCount = DriverWithRidesCount
  { person :: Person,
    info :: DriverInformation,
    vehicle :: Maybe Vehicle,
    ridesCount :: Maybe Int
  }

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
      return $ DriverWithRidesCount person info vehicle ridesCount

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
                &&. ( person ^. PersonMobileNumberHash ==. val (Just mobileNumberDbHash)
                        ||. person ^. PersonAlternateMobileNumberHash ==. val (Just mobileNumberDbHash)
                    )
          )
        &&. whenJust_ mbVehicleNumber (\vehicleNumber -> mbVehicle ?. VehicleRegistrationNo ==. just (val vehicleNumber))
        &&. whenJust_ mbDlNumberHash (\dlNumberHash -> mbDriverLicense ?. DriverLicenseLicenseNumberHash ==. just (val dlNumberHash))
        &&. whenJust_ mbRcNumberHash (\rcNumberHash -> mbRegCert ?. VehicleRegistrationCertificateCertificateNumberHash ==. just (val rcNumberHash))
    pure (person, driverInfo, mbVehicle)
  where
    -- used only for dl and rc entites, because they are not required for final result, only for filters
    joinOnlyWhenJust mbFilter cond = maybe (val False) (const cond) mbFilter

findByIdAndRoleAndMerchantId ::
  Transactionable m =>
  Id Person ->
  Person.Role ->
  Id Merchant ->
  m (Maybe Person)
findByIdAndRoleAndMerchantId pid role_ merchantId =
  Esq.findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonTId ==. val (toKey pid)
        &&. person ^. PersonRole ==. val role_
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
    return person

findAllByMerchantId ::
  Transactionable m =>
  [Person.Role] ->
  Id Merchant ->
  m [Person]
findAllByMerchantId roles merchantId =
  Esq.findAll $ do
    person <- from $ table @PersonT
    where_ $
      (person ^. PersonRole `in_` valList roles ||. val (null roles))
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
    return person

findAdminsByMerchantId :: Transactionable m => Id Merchant -> m [Person]
findAdminsByMerchantId merchantId =
  Esq.findAll $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonMerchantId ==. val (toKey merchantId)
        &&. person ^. PersonRole ==. val Person.ADMIN
    return person

findByMobileNumberAndMerchant ::
  (Transactionable m) =>
  Text ->
  DbHash ->
  Id Merchant ->
  m (Maybe Person)
findByMobileNumberAndMerchant countryCode mobileNumberHash merchantId = do
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonMobileCountryCode ==. val (Just countryCode)
        &&. ( person ^. PersonMobileNumberHash ==. val (Just mobileNumberHash)
                ||. person ^. PersonAlternateMobileNumberHash ==. val (Just mobileNumberHash)
            )
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
    return person

findByIdentifierAndMerchant ::
  Transactionable m =>
  Id Merchant ->
  Text ->
  m (Maybe Person)
findByIdentifierAndMerchant merchantId identifier_ =
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonIdentifier ==. val (Just identifier_)
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
    return person

findByEmailAndMerchant ::
  Transactionable m =>
  Id Merchant ->
  Text ->
  m (Maybe Person)
findByEmailAndMerchant merchantId email_ =
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonEmail ==. val (Just email_)
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
    return person

findByRoleAndMobileNumberAndMerchantId ::
  (Transactionable m, EncFlow m r) =>
  Role ->
  Text ->
  Text ->
  Id Merchant ->
  m (Maybe Person)
findByRoleAndMobileNumberAndMerchantId role_ countryCode mobileNumber_ merchantId = do
  mobileNumberDbHash <- getDbHash mobileNumber_
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonRole ==. val role_
        &&. person ^. PersonMobileCountryCode ==. val (Just countryCode)
        &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberDbHash)
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
    return person

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

updateMerchantIdAndMakeAdmin :: Id Person -> Id Merchant -> SqlDB ()
updateMerchantIdAndMakeAdmin personId merchantId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonMerchantId =. val (toKey merchantId),
        PersonRole =. val Person.ADMIN,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateName :: Id Person -> Text -> SqlDB ()
updateName personId name = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonFirstName =. val name,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

updatePersonRec :: Id Person -> Person -> SqlDB ()
updatePersonRec personId person = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonFirstName =. val (person.firstName),
        PersonMiddleName =. val (person.middleName),
        PersonLastName =. val (person.lastName),
        PersonRole =. val (person.role),
        PersonGender =. val (person.gender),
        PersonEmail =. val (person.email),
        PersonIdentifier =. val (person.identifier),
        PersonRating =. val (person.rating),
        PersonLanguage =. val (person.language),
        PersonDeviceToken =. val (person.deviceToken),
        PersonMerchantId =. val (toKey person.merchantId),
        PersonDescription =. val (person.description),
        PersonUpdatedAt =. val now,
        PersonClientVersion =. val (versionToText <$> person.clientVersion),
        PersonBundleVersion =. val (versionToText <$> person.bundleVersion)
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

updatePersonVersions :: Person -> Maybe Version -> Maybe Version -> SqlDB ()
updatePersonVersions person mbBundleVersion mbClientVersion =
  when
    ((isJust mbBundleVersion || isJust mbClientVersion) && (person.bundleVersion /= mbBundleVersion || person.clientVersion /= mbClientVersion))
    do
      now <- getCurrentTime
      let mbBundleVersionText = versionToText <$> (mbBundleVersion <|> person.bundleVersion)
          mbClientVersionText = versionToText <$> (mbClientVersion <|> person.clientVersion)
      Esq.update $ \tbl -> do
        set
          tbl
          [ PersonUpdatedAt =. val now,
            PersonClientVersion =. val mbClientVersionText,
            PersonBundleVersion =. val mbBundleVersionText
          ]
        where_ $
          tbl ^. PersonTId ==. val (toKey person.id)

updateDeviceToken :: Id Person -> Maybe FCMRecipientToken -> SqlDB ()
updateDeviceToken personId mbDeviceToken = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonDeviceToken =. val mbDeviceToken,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateWhatsappNotificationEnrollStatus :: Id Person -> Maybe Whatsapp.OptApiMethods -> SqlDB ()
updateWhatsappNotificationEnrollStatus personId enrollStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonWhatsappNotificationEnrollStatus =. val enrollStatus,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateMobileNumberAndCode :: Person -> SqlDB ()
updateMobileNumberAndCode person = do
  let personT = toTType person
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonMobileCountryCode =. val (TPerson.mobileCountryCode personT),
        PersonMobileNumberEncrypted =. val (TPerson.mobileNumberEncrypted personT),
        PersonMobileNumberHash =. val (TPerson.mobileNumberHash personT),
        PersonUnencryptedMobileNumber =. val (TPerson.unencryptedMobileNumber personT),
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey person.id)

setIsNewFalse :: Id Person -> SqlDB ()
setIsNewFalse personId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonIsNew =. val False,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

deleteById :: Id Person -> SqlDB ()
deleteById = Esq.deleteByKey @PersonT

updateAverageRating :: Id Person -> Centesimal -> SqlDB ()
updateAverageRating personId newAverageRating = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonRating =. val (Just newAverageRating),
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

updateAlternateMobileNumberAndCode :: Person -> SqlDB ()
updateAlternateMobileNumberAndCode person = do
  now <- getCurrentTime
  let personT = toTType person
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonAlternateMobileNumberEncrypted =. val (TPerson.alternateMobileNumberEncrypted personT),
        PersonUnencryptedAlternateMobileNumber =. val (TPerson.unencryptedAlternateMobileNumber personT),
        PersonAlternateMobileNumberHash =. val (TPerson.alternateMobileNumberHash personT),
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey person.id)
