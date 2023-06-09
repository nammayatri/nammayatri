{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DeleteDriver where

import Domain.Types.Issue.IssueReport (IssueReport (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Transactionable (runInLocationDB)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.DriverInformation as CQDriverInfo
import qualified Storage.CachedQueries.Issue.IssueReport as CQIR
import qualified Storage.Queries.Driver.DriverFlowStatus as QDriverFlowStatus
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverLocation as QDriverLocation
import qualified Storage.Queries.DriverOnboarding.DriverLicense as QDriverLicense
import qualified Storage.Queries.DriverOnboarding.DriverRCAssociation as QRCAssociation
import qualified Storage.Queries.DriverOnboarding.IdfyVerification as QIV
import qualified Storage.Queries.DriverOnboarding.Image as QImage
import qualified Storage.Queries.DriverQuote as QDriverQuote
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Issue.IssueReport as QIssueReport
import qualified Storage.Queries.Message.MessageReport as QMessage
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequestForDriver as QSearchReqForDriver
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Tools.Auth as Auth
import Tools.Error

deleteDriver :: ShortId DM.Merchant -> Id DP.Person -> Flow APISuccess
deleteDriver merchantShortId reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId
  driver <-
    QPerson.findById reqDriverId
      >>= fromMaybeM (PersonDoesNotExist reqDriverId.getId)

  driverDeleteCheck <- validateDriver merchant driver
  when driverDeleteCheck $ throwError $ InvalidRequest "Driver can't be deleted"

  issueReports <- CQIR.findAllByDriver reqDriverId

  -- this function uses tokens from db, so should be called before transaction
  Auth.clearDriverSession reqDriverId
  Esq.runTransaction $ do
    QIV.deleteByPersonId reqDriverId
    QImage.deleteByPersonId reqDriverId
    QDriverLicense.deleteByDriverId reqDriverId
    QRCAssociation.deleteByDriverId reqDriverId
    QDriverQuote.deleteByDriverId reqDriverId
    QSearchReqForDriver.deleteByDriverId reqDriverId
    QDriverStats.deleteById (cast reqDriverId)
    QR.deleteByPersonId reqDriverId
    QVehicle.deleteById reqDriverId
    QDriverInfo.deleteById (cast reqDriverId)
    QDriverFlowStatus.deleteById reqDriverId
    QMessage.deleteByPersonId reqDriverId
    QIssueReport.deleteByPersonId reqDriverId
    QPerson.deleteById reqDriverId
  runInLocationDB $ QDriverLocation.deleteById reqDriverId
  CQDriverInfo.clearDriverInfoCache (cast reqDriverId)
  CQIR.invalidateIssueReportCache Nothing (Just reqDriverId)
  mapM_ (\IssueReport {id} -> CQIR.invalidateIssueReportCache (Just id) Nothing) issueReports
  logTagInfo "deleteDriver : " (show reqDriverId)
  return Success

validateDriver :: (EsqDBFlow m r, EncFlow m r, HasCacheConfig r, Redis.HedisFlow m r) => DM.Merchant -> DP.Person -> m Bool
validateDriver merchant driver = do
  let personId = driver.id
  ride <- QRide.findOneByDriverId personId
  driverInformation <- CQDriverInfo.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  return (merchant.id /= driver.merchantId || driver.role /= DP.DRIVER || isJust ride || driverInformation.enabled)
