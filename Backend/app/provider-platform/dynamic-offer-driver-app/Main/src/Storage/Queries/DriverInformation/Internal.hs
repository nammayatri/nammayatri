{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverInformation.Internal where

import qualified Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions (findAllWithKV)
import Kernel.Prelude
import Kernel.Types.App (MonadFlow)
import Kernel.Types.Id
import qualified Sequelize as Se
import Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Queries.Instances.DriverInformation ()

-- getDriverInfos ::
--   Transactionable m =>
--   [Id DP.Person] ->
--   m [DriverInformation]
-- getDriverInfos personIds = do
--   Esq.findAll $ do
--     driverInfos <- from $ table @DriverInformationT
--     where_ $
--       driverInfos ^. DriverInformationDriverId `in_` valList (toKey <$> personIds)
--     return driverInfos

getDriverInfos ::
  MonadFlow m =>
  [Text] ->
  m [DriverInfo.DriverInformation]
getDriverInfos personKeys = do
  findAllWithKV [Se.Is BeamDI.driverId $ Se.In personKeys]

-- getDriverInfosWithOnRideCond ::
--   Transactionable m =>
--   [Id DP.Person] ->
--   GetDriverInfosWithOnRideCondIsOnRide ->
--   m [DriverInformation]
-- getDriverInfosWithOnRideCond personIds isOnRide = do
--   Esq.findAll $ do
--     driverInfos <- from $ table @DriverInformationT
--     where_ $
--       driverInfos ^. DriverInformationDriverId `in_` valList (toKey <$> personIds)
--         &&. ((Esq.isNothing (driverInfos ^. DriverInformationMode) &&. driverInfos ^. DriverInformationActive) ||. (not_ (Esq.isNothing (driverInfos ^. DriverInformationMode)) &&. (driverInfos ^. DriverInformationMode ==. val (Just DriverInfo.SILENT) ||. driverInfos ^. DriverInformationMode ==. val (Just DriverInfo.ONLINE))))
--         &&. ( case isOnRide of
--                 OnAndNotOnRide -> val True
--                 NotOnRide -> not_ (driverInfos ^. DriverInformationOnRide)
--                 OnRide -> driverInfos ^. DriverInformationOnRide
--             )
--         &&. not_ (driverInfos ^. DriverInformationBlocked)
--     return driverInfos

getDriverInfosWithCond :: MonadFlow m => [Id DP.Person] -> Bool -> Bool -> m [DriverInfo.DriverInformation]
getDriverInfosWithCond driverLocs onlyNotOnRide onlyOnRide =
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
            Se.Is BeamDI.blocked $ Se.Eq False,
            Se.Is BeamDI.subscribed $ Se.Eq True
          ]
            <> ([Se.Is BeamDI.onRide $ Se.Eq (not onlyNotOnRide) | not (onlyNotOnRide && onlyOnRide)])
        )
    ]
  where
    personsKeys = getId . cast <$> driverLocs
