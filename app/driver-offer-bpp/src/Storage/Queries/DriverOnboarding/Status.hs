module Storage.Queries.DriverOnboarding.Status where

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
import Domain.Types.Person
import Domain.Types.Vehicle as Vehicle
import Storage.Tabular.DriverInformation
import Storage.Tabular.DriverOnboarding.DriverLicense
import Storage.Tabular.DriverOnboarding.DriverRCAssociation
import Storage.Tabular.DriverOnboarding.IdfyVerification
import Storage.Tabular.DriverOnboarding.Image
import Storage.Tabular.DriverOnboarding.VehicleRegistrationCertificate
import Storage.Tabular.Person
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

fetchDriverDocsInfo :: (Transactionable m) => Maybe (NonEmpty (Id Driver)) -> m [DriverDocsInfo]
fetchDriverDocsInfo mbDriverIds = fmap (map mkDriverDocsInfo) $
  Esq.findAll $ do
    imagesCountLic <- imagesAggTableCTEbyDoctype Image.DriverLicense
    imagesCountVehReg <- imagesAggTableCTEbyDoctype Image.VehicleRegistrationCertificate
    person :& license :& licReq :& assoc :& registration :& regReq :& driverInfo :& licImages :& vehRegImages <-
      from $ baseDriverDocumentsInfoQuery imagesCountLic imagesCountVehReg

    where_ $ maybe (val True) (\ids -> person ^. PersonTId `in_` valList (map (toKey . coerce) $ toList ids)) mbDriverIds
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
    vehicle :: Maybe Vehicle
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

fetchFullDriverInfoWithDocsFirstnameAsc :: (Transactionable m) => Maybe (NonEmpty (Id Driver)) -> m [FullDriverWithDocs]
fetchFullDriverInfoWithDocsFirstnameAsc mbDriverIds = fmap (map mkFullDriverInfoWithDocs) $
  Esq.findAll $ do
    person :& license :& assoc :& registration :& info :& mbVeh <- from baseFullDriverWithDocsQuery
    where_ $ maybe (val True) (\ids -> person ^. PersonTId `in_` valList (map (toKey . coerce) $ toList ids)) mbDriverIds
    orderBy [asc (person ^. PersonFirstName)]
    pure (person, license, assoc, registration, info, mbVeh)

mkFullDriverInfoWithDocs ::
  ( Person,
    Maybe DriverLicense,
    Maybe DriverRCAssociation,
    Maybe VehicleRegistrationCertificate,
    DriverInformation,
    Maybe Vehicle
  ) ->
  FullDriverWithDocs
mkFullDriverInfoWithDocs (p, l, a, v, i, mbVeh) = FullDriverWithDocs p l ((,) <$> a <*> v) i mbVeh
