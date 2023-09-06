{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DisabilityType
  ( disabilityList,
  )
where

import qualified Domain.Types.DisabilityType as DTL
import qualified Domain.Types.Person as SP
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import Kernel.Types.Id
import qualified Storage.CachedQueries.DisabilityType as SCQD
import qualified Storage.Queries.Person as QP

makeDisabilityListAPIEntity :: DTL.DisabilityType -> DTL.DisabilityListAPIEntity
makeDisabilityListAPIEntity DTL.DisabilityType {..} = DTL.DisabilityListAPIEntity {..}

getLanguage :: Id SP.Person -> Flow Language
getLanguage driverId = do
  extractLanguage <- runMaybeT $ do
    driverDetail <- MaybeT . B.runInReplica $ QP.findById driverId
    MaybeT $ pure driverDetail.language
  return $ fromMaybe ENGLISH extractLanguage

disabilityList :: Id SP.Person -> Flow [DTL.DisabilityListAPIEntity]
disabilityList driverId = do
  language <- getLanguage driverId
  fmap makeDisabilityListAPIEntity <$> B.runInReplica (SCQD.findAllByLanguage language)
