{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverOnboarding.AadhaarOtp where

import Domain.Types.DriverOnboarding.AadhaarOtp
import Domain.Types.Person (Person)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.DriverOnboarding.AadhaarOtpReq
import Storage.Tabular.DriverOnboarding.AadhaarOtpVerify

createForGenerate :: AadhaarOtpReq -> Esq.SqlDB ()
createForGenerate = Esq.create

createForVerify :: AadhaarOtpVerify -> Esq.SqlDB ()
createForVerify = Esq.create

deleteByPersonIdForGenerate :: Id Person -> SqlDB ()
deleteByPersonIdForGenerate personId =
  Esq.delete $ do
    verifications <- from $ table @AadhaarOtpReqT
    where_ $ verifications ^. AadhaarOtpReqDriverId ==. val (toKey personId)

deleteByPersonIdForVerify :: Id Person -> SqlDB ()
deleteByPersonIdForVerify personId =
  Esq.delete $ do
    verifications <- from $ table @AadhaarOtpVerifyT
    where_ $ verifications ^. AadhaarOtpVerifyDriverId ==. val (toKey personId)
