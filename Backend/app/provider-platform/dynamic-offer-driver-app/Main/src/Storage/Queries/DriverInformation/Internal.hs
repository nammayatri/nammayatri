{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Storage.Queries.DriverInformation.Internal where

import qualified Domain.Types.Common as DriverInfo
import qualified Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.Person as DP
import Domain.Types.VehicleCategory as DV
import Kernel.Beam.Functions (findAllWithKV, findOneWithKV, updateOneWithKV)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Queries.OrphanInstances.DriverInformation ()

updateOnboardingVehicleCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe DV.VehicleCategory -> Id DP.Person -> m ())
updateOnboardingVehicleCategory onboardingVehicleCategory driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set BeamDI.onboardingVehicleCategory onboardingVehicleCategory, Se.Set BeamDI.updatedAt _now] [Se.Is BeamDI.driverId $ Se.Eq driverId.getId]

getDriverInfos ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [Text] ->
  m [DriverInfo.DriverInformation]
getDriverInfos personKeys = do
  findAllWithKV [Se.Is BeamDI.driverId $ Se.In personKeys]

getDriverInfosWithCond :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id DP.Person] -> Bool -> Bool -> Bool -> Bool -> m [DriverInfo.DriverInformation]
getDriverInfosWithCond driverLocs onlyNotOnRide onlyOnRide isRental isInterCity = do
  findAllWithKV
    [ Se.And
        ( [ Se.Is BeamDI.driverId $ Se.In personsKeys,
            Se.Or
              [ Se.And
                  [ Se.Is BeamDI.mode $ Se.Eq Nothing,
                    Se.Is BeamDI.active $ Se.Eq True
                  ],
                Se.And
                  [ Se.Is BeamDI.mode $ Se.Not $ Se.Eq Nothing,
                    Se.Or
                      [ Se.Is BeamDI.mode $ Se.Eq $ Just DriverInfo.SILENT,
                        Se.Is BeamDI.mode $ Se.Eq $ Just DriverInfo.ONLINE
                      ]
                  ]
              ],
            Se.Is BeamDI.blocked $ Se.Eq False
          ]
            <> ([Se.Is BeamDI.onRide $ Se.Eq (not onlyNotOnRide) | not (onlyNotOnRide && onlyOnRide)])
            <> ([Se.Is BeamDI.hasAdvanceBooking $ Se.Eq (Just (not onlyOnRide)) | onlyOnRide])
            <> ([Se.Is BeamDI.forwardBatchingEnabled $ Se.Eq (Just True) | onlyOnRide])
            <> ([Se.Is BeamDI.canSwitchToRental $ Se.Eq (Just True) | isRental])
            <> ([Se.Is BeamDI.canSwitchToInterCity $ Se.Eq (Just True) | isInterCity])
            <> ([Se.Is BeamDI.canSwitchToIntraCity $ Se.Eq (Just True) | (not isInterCity && not isRental)])
            <> [Se.Is BeamDI.subscribed $ Se.Eq True]
        )
    ]
  where
    personsKeys = getId . cast <$> driverLocs

getSpecialLocWarriorDriverInfoWithCond :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id DP.Person] -> Bool -> Bool -> Bool -> Bool -> m [DriverInfo.DriverInformation]
getSpecialLocWarriorDriverInfoWithCond driverLocs onlyNotOnRide onlyOnRide isRental isInterCity =
  findAllWithKV
    [ Se.And
        ( [ Se.Is BeamDI.driverId $ Se.In personsKeys,
            Se.Or
              [ Se.And
                  [ Se.Is BeamDI.mode $ Se.Eq Nothing,
                    Se.Is BeamDI.active $ Se.Eq True
                  ],
                Se.And
                  [ Se.Is BeamDI.mode $ Se.Not $ Se.Eq Nothing,
                    Se.Or
                      [ Se.Is BeamDI.mode $ Se.Eq $ Just DriverInfo.SILENT,
                        Se.Is BeamDI.mode $ Se.Eq $ Just DriverInfo.ONLINE
                      ]
                  ]
              ],
            Se.Is BeamDI.blocked $ Se.Eq False
          ]
            <> ([Se.Is BeamDI.onRide $ Se.Eq (not onlyNotOnRide) | not (onlyNotOnRide && onlyOnRide)])
            <> ([Se.Is BeamDI.hasAdvanceBooking $ Se.Eq (Just (not onlyOnRide)) | onlyOnRide])
            <> ([Se.Is BeamDI.forwardBatchingEnabled $ Se.Eq (Just True) | onlyOnRide])
            <> ([Se.Is BeamDI.canSwitchToRental $ Se.Eq (Just True) | isRental])
            <> ([Se.Is BeamDI.canSwitchToInterCity $ Se.Eq (Just True) | isInterCity])
            <> ([Se.Is BeamDI.canSwitchToIntraCity $ Se.Eq (Just True) | (not isInterCity && not isRental)])
            <> [Se.Is BeamDI.isSpecialLocWarrior $ Se.Eq (Just True)]
        )
    ]
  where
    personsKeys = getId . cast <$> driverLocs

getSpecialLocWarriorDriverInfo :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m (Maybe DriverInfo.DriverInformation)
getSpecialLocWarriorDriverInfo driverId =
  findOneWithKV
    [ Se.And
        ( [ Se.Is BeamDI.driverId $ Se.Eq driverId,
            Se.Is BeamDI.blocked $ Se.Eq False
          ]
            <> [Se.Is BeamDI.isSpecialLocWarrior $ Se.Eq (Just True)]
        )
    ]
