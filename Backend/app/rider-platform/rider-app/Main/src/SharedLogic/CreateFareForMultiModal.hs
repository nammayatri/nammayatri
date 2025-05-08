{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CreateFareForMultiModal where

import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import qualified Domain.Types.FRFSTicketBooking as FTBooking
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.VendorSplitDetails as VendorSplitDetails
import EulerHS.Prelude ((+||), (||+))
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Types as JPT
import Lib.Payment.Storage.Beam.BeamFlow
import Storage.Beam.Payment ()
import qualified Storage.Queries.IntegratedBPPConfig as QIBP
import qualified Storage.Queries.VendorSplitDetails as QVendorSplitDetails
import Tools.Error
import qualified Tools.Payment as Payment

fareProcessingLockKey :: Text -> Text
fareProcessingLockKey journeyId = "Fare:Processing:JourneyId" <> journeyId

createFares :: (EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r) => Text -> Maybe JPT.JourneySearchData -> m () -> m Bool
createFares searchId journeyLegInfo updateInSearchReqFunc = do
  whenJust journeyLegInfo $ \_ -> updateInSearchReqFunc
  mbShouldConfirmFare <- getConfirmOnceGetFare searchId
  when (mbShouldConfirmFare == Just True) $ resetConfirmOnceGetFare searchId
  return (mbShouldConfirmFare == Just True)

confirmOnceGetFare :: Text -> Text
confirmOnceGetFare searchId = "COGF:SRID-" <> searchId

setConfirmOnceGetFare :: CacheFlow m r => Text -> m ()
setConfirmOnceGetFare searchId = do
  Hedis.withCrossAppRedis $ Hedis.setExp (confirmOnceGetFare searchId) True 600

resetConfirmOnceGetFare :: CacheFlow m r => Text -> m ()
resetConfirmOnceGetFare searchId = do
  Hedis.withCrossAppRedis $ Hedis.setExp (confirmOnceGetFare searchId) False 600

getConfirmOnceGetFare :: CacheFlow m r => Text -> m (Maybe Bool)
getConfirmOnceGetFare searchId = Hedis.withCrossAppRedis (Hedis.safeGet (confirmOnceGetFare searchId))

createVendorSplitFromBookings ::
  ( EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasField "isMetroTestTransaction" r Bool
  ) =>
  [FTBooking.FRFSTicketBooking] ->
  m ([Payment.VendorSplitDetails], HighPrecMoney)
createVendorSplitFromBookings allJourneyBookings = do
  let amount = sum $ allJourneyBookings <&> (.price.amount)
  let vehicleTypeList = allJourneyBookings <&> (.vehicleType)
  let _booking = listToMaybe allJourneyBookings
  case _booking of
    Just booking -> do
      _integratedBPPConfigList <-
        mapM
          ( \vehicleType ->
              QIBP.findByDomainAndCityAndVehicleCategory
                (show Spec.FRFS)
                booking.merchantOperatingCityId
                (frfsVehicleCategoryToBecknVehicleCategory vehicleType)
                DIBC.MULTIMODAL
                >>= fromMaybeM
                  ( IntegratedBPPConfigNotFound $
                      "MerchantOperatingCityId:" +|| booking.merchantOperatingCityId
                        ||+ "Domain:" +|| Spec.FRFS
                        ||+ "Vehicle:" +|| (frfsVehicleCategoryToBecknVehicleCategory vehicleType)
                        ||+ "Platform Type:" +|| DIBC.MULTIMODAL
                        ||+ ""
                  )
          )
          vehicleTypeList
      _vendorSplitDetailsList <- mapM (QVendorSplitDetails.findAllByIntegratedBPPConfigId . (.id)) _integratedBPPConfigList
      _vendorSplitDetails <- convertVendorDetails (concat _vendorSplitDetailsList) allJourneyBookings
      return (_vendorSplitDetails, amount)
    Nothing -> return ([], 0.0)

convertVendorDetails ::
  ( EsqDBReplicaFlow m r,
    BeamFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    HasField "isMetroTestTransaction" r Bool
  ) =>
  [VendorSplitDetails.VendorSplitDetails] ->
  [FTBooking.FRFSTicketBooking] ->
  m [Payment.VendorSplitDetails]
convertVendorDetails vendorDetails bookings
  | length vendorDetails /= length bookings = throwError $ InternalError "Number of vendor details does not match number of bookings"
  | otherwise = return $ zipWith toPaymentVendorDetails vendorDetails bookings
  where
    toPaymentVendorDetails vd booking =
      Payment.VendorSplitDetails
        { splitAmount = booking.price.amount,
          splitType = vendorSplitDetailSplitTypeToPaymentSplitType vd.splitType,
          vendorId = vd.vendorId
        }

vendorSplitDetailSplitTypeToPaymentSplitType :: VendorSplitDetails.SplitType -> Payment.SplitType
vendorSplitDetailSplitTypeToPaymentSplitType = \case
  VendorSplitDetails.FIXED -> Payment.FIXED
  VendorSplitDetails.FLEXIBLE -> Payment.FLEXIBLE
