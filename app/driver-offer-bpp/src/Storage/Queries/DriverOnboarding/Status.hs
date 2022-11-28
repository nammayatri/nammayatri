{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverOnboarding.Status where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Data.Coerce
import Domain.Types.DriverInformation
import Domain.Types.DriverOnboarding.DriverLicense
import Domain.Types.DriverOnboarding.DriverRCAssociation
import qualified Domain.Types.DriverOnboarding.IdfyVerification as IV
import qualified Domain.Types.DriverOnboarding.Image as Image
import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate (VehicleRegistrationCertificate)
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person
import qualified Domain.Types.Ride as Ride
import Domain.Types.Vehicle as Vehicle
import Storage.Tabular.DriverInformation
import Storage.Tabular.DriverOnboarding.DriverLicense
import Storage.Tabular.DriverOnboarding.DriverRCAssociation
import Storage.Tabular.DriverOnboarding.IdfyVerification
import Storage.Tabular.DriverOnboarding.Image
import Storage.Tabular.DriverOnboarding.VehicleRegistrationCertificate
import Storage.Tabular.Person
import Storage.Tabular.Ride
import Storage.Tabular.Vehicle as Vehicle

data DriverDocsInfo = DriverDocsInfo
  { person :: Person,
    license :: Maybe DriverLicense,
    licenseVerificationReq :: Maybe IV.IdfyVerification,
    assocReg :: Maybe (DriverRCAssociation, VehicleRegistrationCertificate),
    regVerificationReq :: Maybe IV.IdfyVerification,
    driverInfo :: DriverInformation,
    numLicenseImages :: Int,
    numVehRegImages :: Int
  }

imagesAggTableCTEbyDoctype :: Image.ImageType -> SqlQuery (From (SqlExpr (Value PersonTId), SqlExpr (Value Int)))
imagesAggTableCTEbyDoctype imageType = with $ do
  image <- from $ table @ImageT
  where_ $ image ^. ImageImageType ==. val imageType
  groupBy $ image ^. ImagePersonId
  pure (image ^. ImagePersonId, count @Int $ image ^. ImageId)

baseDriverDocumentsInfoQuery ::
  From (SqlExpr (Value PersonTId), SqlExpr (Value Int)) ->
  From (SqlExpr (Value PersonTId), SqlExpr (Value Int)) ->
  From
    ( Table PersonT
        :& MbTable DriverLicenseT
        :& MbTable IdfyVerificationT
        :& MbTable DriverRCAssociationT
        :& MbTable VehicleRegistrationCertificateT
        :& MbTable IdfyVerificationT
        :& Table DriverInformationT
        :& (SqlExpr (Value (Maybe PersonTId)), SqlExpr (Value (Maybe Int)))
        :& (SqlExpr (Value (Maybe PersonTId)), SqlExpr (Value (Maybe Int)))
    )
baseDriverDocumentsInfoQuery licenseImagesAggTable vehicleRegistrationImagesAggTable =
  table @PersonT
    `Esq.leftJoin` table @DriverLicenseT `Esq.on` (\(p :& l) -> just (p ^. PersonTId) ==. l ?. DriverLicenseDriverId)
    `Esq.leftJoin` table @IdfyVerificationT
      `Esq.on` ( \(p :& _ :& licReq) ->
                   just (p ^. PersonTId) ==. licReq ?. IdfyVerificationDriverId
                     &&. licReq ?. IdfyVerificationDocType ==. just (val Image.DriverLicense)
               )
    `Esq.leftJoin` table @DriverRCAssociationT
      `Esq.on` (\(p :& _ :& _ :& rcAssoc) -> just (p ^. PersonTId) ==. rcAssoc ?. DriverRCAssociationDriverId)
    `Esq.leftJoin` table @VehicleRegistrationCertificateT
      `Esq.on` (\(_ :& _ :& _ :& rcAssoc :& regCert) -> rcAssoc ?. DriverRCAssociationRcId ==. regCert ?. VehicleRegistrationCertificateTId)
    `Esq.leftJoin` table @IdfyVerificationT
      `Esq.on` ( \(p :& _ :& _ :& _ :& _ :& regReq) ->
                   just (p ^. PersonTId) ==. regReq ?. IdfyVerificationDriverId
                     &&. regReq ?. IdfyVerificationDocType ==. just (val Image.VehicleRegistrationCertificate)
               )
    `Esq.innerJoin` table @DriverInformationT
      `Esq.on` (\(p :& _ :& _ :& _ :& _ :& _ :& driverInfo) -> p ^. PersonTId ==. driverInfo ^. DriverInformationDriverId)
    `Esq.leftJoin` licenseImagesAggTable
      `Esq.on` (\(p :& _ :& _ :& _ :& _ :& _ :& _ :& (licImgPersonId, _)) -> just (p ^. PersonTId) ==. licImgPersonId)
    `Esq.leftJoin` vehicleRegistrationImagesAggTable
      `Esq.on` (\(p :& _ :& _ :& _ :& _ :& _ :& _ :& _ :& (vehRegImgPersonId, _)) -> just (p ^. PersonTId) ==. vehRegImgPersonId)

fetchDriverDocsInfo :: (Transactionable m) => Id Merchant -> Maybe (NonEmpty (Id Driver)) -> m [DriverDocsInfo]
fetchDriverDocsInfo merchantId mbDriverIds = fmap (map mkDriverDocsInfo) $
  Esq.findAll $ do
    imagesCountLic <- imagesAggTableCTEbyDoctype Image.DriverLicense
    imagesCountVehReg <- imagesAggTableCTEbyDoctype Image.VehicleRegistrationCertificate
    person :& license :& licReq :& assoc :& registration :& regReq :& driverInfo :& licImages :& vehRegImages <-
      from $ baseDriverDocumentsInfoQuery imagesCountLic imagesCountVehReg

    where_ $
      maybe (val True) (\ids -> person ^. PersonTId `in_` valList (map (toKey . coerce) $ toList ids)) mbDriverIds
        &&. person ^. PersonMerchantId ==. (just . val . toKey $ merchantId)
    pure (person, license, licReq, assoc, registration, regReq, driverInfo, snd licImages, snd vehRegImages)

mkDriverDocsInfo ::
  ( Person,
    Maybe DriverLicense,
    Maybe IV.IdfyVerification,
    Maybe DriverRCAssociation,
    Maybe VehicleRegistrationCertificate,
    Maybe IV.IdfyVerification,
    DriverInformation,
    Maybe Int,
    Maybe Int
  ) ->
  DriverDocsInfo
mkDriverDocsInfo (p, l, licReq, a, r, regReq, driverInfo, licImages, vehRegImages) =
  DriverDocsInfo p l licReq ((,) <$> a <*> r) regReq driverInfo (def0 licImages) (def0 vehRegImages)
  where
    def0 = fromMaybe 0

-- this datatype is a bit ad hoc, but I think it's ok (_Yuri_)
data FullDriverWithDocs = FullDriverWithDocs
  { person :: Person,
    license :: Maybe DriverLicense,
    registration :: Maybe (DriverRCAssociation, VehicleRegistrationCertificate),
    info :: DriverInformation,
    vehicle :: Maybe Vehicle,
    ridesCount :: Maybe Int -- it's not used in all queries
  }

baseFullDriverWithDocsQuery ::
  From
    ( Table PersonT
        :& MbTable DriverLicenseT
        :& MbTable DriverRCAssociationT
        :& MbTable VehicleRegistrationCertificateT
        :& Table DriverInformationT
        :& MbTable VehicleT
    )
baseFullDriverWithDocsQuery =
  table @PersonT
    `Esq.leftJoin` table @DriverLicenseT `Esq.on` (\(p :& l) -> just (p ^. PersonTId) ==. l ?. DriverLicenseDriverId)
    `Esq.leftJoin` table @DriverRCAssociationT
      `Esq.on` (\(p :& _ :& rcAssoc) -> just (p ^. PersonTId) ==. rcAssoc ?. DriverRCAssociationDriverId)
    `Esq.leftJoin` table @VehicleRegistrationCertificateT
      `Esq.on` (\(_ :& _ :& rcAssoc :& regCert) -> rcAssoc ?. DriverRCAssociationRcId ==. regCert ?. VehicleRegistrationCertificateTId)
    `innerJoin` table @DriverInformationT
      `Esq.on` ( \(person :& _ :& _ :& _ :& driverInfo) ->
                   person ^. PersonTId ==. driverInfo ^. DriverInformationDriverId
               )
    `leftJoin` table @VehicleT
      `Esq.on` ( \(person :& _ :& _ :& _ :& _ :& vehicle) ->
                   just (person ^. PersonTId) ==. vehicle ?. VehicleDriverId
               )

------

maxLimit :: Int
maxLimit = 20

defaultLimit :: Int
defaultLimit = 10

calcLimit :: Maybe Int -> Int
calcLimit = min maxLimit . fromMaybe defaultLimit

-- TODO move it to Person
findAllDriversWithInfoAndVehicle ::
  ( Transactionable m,
    EncFlow m r
  ) =>
  Id Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Text ->
  m [(Person, DriverInformation, Maybe Vehicle)]
findAllDriversWithInfoAndVehicle merchantId mbLimit mbOffset mbVerified mbEnabled mbSearchPhone = do
  mbSearchPhoneDBHash <- getDbHash `traverse` mbSearchPhone
  Esq.findAll $ do
    let limitVal = fromIntegral $ calcLimit mbLimit
        offsetVal = maybe 0 fromIntegral mbOffset
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
      person ^. PersonMerchantId ==. (just . val . toKey $ merchantId)
        &&. maybe (val True) (\verified -> info ^. DriverInformationVerified ==. val verified) mbVerified
        &&. maybe (val True) (\enabled -> info ^. DriverInformationEnabled ==. val enabled) mbEnabled
        &&. maybe (val True) (\searchStrDBHash -> person ^. PersonMobileNumberHash ==. val (Just searchStrDBHash)) mbSearchPhoneDBHash
    orderBy [asc (person ^. PersonFirstName)]
    limit limitVal
    offset offsetVal
    pure (person, info, mbVeh)

ridesCountAggTable :: SqlQuery (From (SqlExpr (Value PersonTId), SqlExpr (Value Int)))
ridesCountAggTable = with $ do
  ride <- from $ table @RideT
  where_ (not_ $ ride ^. RideStatus `in_` valList [Ride.NEW, Ride.CANCELLED])
  groupBy $ ride ^. RideDriverId
  pure (ride ^. RideDriverId, count @Int $ ride ^. RideId)

mkFullDriverWithRidesCountQuery ::
  From (SqlExpr (Value PersonTId), SqlExpr (Value Int)) ->
  From
    ( Table PersonT
        :& MbTable DriverLicenseT
        :& MbTable DriverRCAssociationT
        :& MbTable VehicleRegistrationCertificateT
        :& Table DriverInformationT
        :& MbTable VehicleT
        :& (SqlExpr (Value (Maybe PersonTId)), SqlExpr (Value (Maybe Int)))
    )
mkFullDriverWithRidesCountQuery ridesCountAggQuery =
  baseFullDriverWithDocsQuery
    `Esq.leftJoin` ridesCountAggQuery
      `Esq.on` ( \(person :& _ :& _ :& _ :& _ :& _ :& (mbPersonId, _mbRidesCount)) ->
                   just (person ^. PersonTId) ==. mbPersonId
               )

fetchFullDriverInfoWithDocsByMobileNumber :: (Transactionable m, EncFlow m r) => Id Merchant -> Text -> Text -> m (Maybe FullDriverWithDocs)
fetchFullDriverInfoWithDocsByMobileNumber merchantId mobileNumber mobileCountryCode = fmap (fmap mkFullDriverInfoWithDocs) $ do
  mobileNumberDbHash <- getDbHash mobileNumber
  Esq.findOne $ do
    ridesCountAggQuery <- ridesCountAggTable
    person :& license :& assoc :& registration :& info :& mbVeh :& (_, mbRidesCount) <-
      from $ mkFullDriverWithRidesCountQuery ridesCountAggQuery
    where_ $
      (person ^. PersonRole ==. val DRIVER)
        &&. person ^. PersonMobileCountryCode ==. val (Just mobileCountryCode)
        &&. person ^. PersonMobileNumberHash ==. val (Just mobileNumberDbHash)
        &&. person ^. PersonMerchantId ==. (just . val . toKey $ merchantId)
    pure (person, license, assoc, registration, info, mbVeh, mbRidesCount)

fetchFullDriverInfoWithDocsByVehNumber :: Transactionable m => Id Merchant -> Text -> m (Maybe FullDriverWithDocs)
fetchFullDriverInfoWithDocsByVehNumber merchantId vehicleNumber = fmap (fmap mkFullDriverInfoWithDocs) $
  Esq.findOne $ do
    ridesCountAggQuery <- ridesCountAggTable
    person :& license :& assoc :& registration :& info :& mbVeh :& (_, mbRidesCount) <-
      from $ mkFullDriverWithRidesCountQuery ridesCountAggQuery
    where_ $
      (person ^. PersonRole ==. val DRIVER)
        &&. mbVeh ?. VehicleRegistrationNo ==. val (Just vehicleNumber)
        &&. person ^. PersonMerchantId ==. (just . val . toKey $ merchantId)
    pure (person, license, assoc, registration, info, mbVeh, mbRidesCount)

mkFullDriverInfoWithDocs ::
  ( Person,
    Maybe DriverLicense,
    Maybe DriverRCAssociation,
    Maybe VehicleRegistrationCertificate,
    DriverInformation,
    Maybe Vehicle,
    Maybe Int
  ) ->
  FullDriverWithDocs
mkFullDriverInfoWithDocs (p, l, a, v, i, mbVeh, mbRidesCount) = FullDriverWithDocs p l ((,) <$> a <*> v) i mbVeh mbRidesCount
