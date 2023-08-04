{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverInformation.Internal where

import Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.DriverInformation

getDriverInfos ::
  Transactionable m =>
  [Id DP.Person] ->
  m [DriverInformation]
getDriverInfos personIds = do
  Esq.findAll $ do
    driverInfos <- from $ table @DriverInformationT
    where_ $
      driverInfos ^. DriverInformationDriverId `in_` valList (toKey <$> personIds)
    return driverInfos

data GetDriverInfosWithOnRideCondIsOnRide = NotOnRide | OnRide | OnAndNotOnRide

getDriverInfosWithOnRideCond ::
  Transactionable m =>
  [Id DP.Person] ->
  GetDriverInfosWithOnRideCondIsOnRide ->
  m [DriverInformation]
getDriverInfosWithOnRideCond personIds isOnRide = do
  Esq.findAll $ do
    driverInfos <- from $ table @DriverInformationT
    where_ $
      driverInfos ^. DriverInformationDriverId `in_` valList (toKey <$> personIds)
        &&. ((Esq.isNothing (driverInfos ^. DriverInformationMode) &&. driverInfos ^. DriverInformationActive) ||. (not_ (Esq.isNothing (driverInfos ^. DriverInformationMode)) &&. (driverInfos ^. DriverInformationMode ==. val (Just DriverInfo.SILENT) ||. driverInfos ^. DriverInformationMode ==. val (Just DriverInfo.ONLINE))))
        &&. ( case isOnRide of
                OnAndNotOnRide -> val True
                NotOnRide -> not_ (driverInfos ^. DriverInformationOnRide)
                OnRide -> driverInfos ^. DriverInformationOnRide
            )
        &&. not_ (driverInfos ^. DriverInformationBlocked)
    return driverInfos
