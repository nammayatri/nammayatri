{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverOnboarding.Status where

import Data.Coerce
import Domain.Types.DriverInformation
import Domain.Types.DriverOnboarding.DriverLicense
import Domain.Types.DriverOnboarding.DriverRCAssociation
import qualified Domain.Types.DriverOnboarding.IdfyVerification as IV
import qualified Domain.Types.DriverOnboarding.Image as Image
import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate (VehicleRegistrationCertificate)
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.DriverInformation
import Storage.Tabular.DriverOnboarding.DriverLicense
import Storage.Tabular.DriverOnboarding.DriverRCAssociation
import Storage.Tabular.DriverOnboarding.IdfyVerification
import Storage.Tabular.DriverOnboarding.Image
import Storage.Tabular.DriverOnboarding.VehicleRegistrationCertificate
import Storage.Tabular.Person

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

fetchDriverDocsInfo :: forall m ma. (Transactionable ma m) => Id Merchant -> Maybe (NonEmpty (Id Driver)) -> Proxy ma -> m [DriverDocsInfo]
fetchDriverDocsInfo merchantId mbDriverIds _ = fmap (map mkDriverDocsInfo) $
  Esq.findAll @m @ma $ do
    imagesCountLic <- imagesAggTableCTEbyDoctype Image.DriverLicense
    imagesCountVehReg <- imagesAggTableCTEbyDoctype Image.VehicleRegistrationCertificate
    person :& license :& licReq :& assoc :& registration :& regReq :& driverInfo :& licImages :& vehRegImages <-
      from $ baseDriverDocumentsInfoQuery imagesCountLic imagesCountVehReg

    where_ $
      maybe (val True) (\ids -> person ^. PersonTId `in_` valList (map (toKey . coerce) $ toList ids)) mbDriverIds
        &&. person ^. PersonMerchantId ==. (val . toKey $ merchantId)
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
