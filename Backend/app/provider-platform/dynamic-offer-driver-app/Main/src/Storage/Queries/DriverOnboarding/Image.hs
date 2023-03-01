{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverOnboarding.Image where

import qualified Data.Time as DT
import Domain.Types.DriverOnboarding.Error
import Domain.Types.DriverOnboarding.Image
import Domain.Types.Merchant
import Domain.Types.Person (Person)
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Field
import Kernel.Types.Id
import Storage.Tabular.DriverOnboarding.Image

create :: Image -> SqlDB m ()
create = Esq.create

findById ::
  forall m ma.
  Transactionable ma m =>
  Proxy ma ->
  Id Image ->
  m (Maybe Image)
findById _ = Esq.findById @m @ma

findImagesByPersonAndType ::
  forall m ma.
  (Transactionable ma m) =>
  Id Merchant ->
  Id Person ->
  ImageType ->
  Proxy ma ->
  m [Image]
findImagesByPersonAndType merchantId personId imgType _ = do
  findAll @m @ma $ do
    images <- from $ table @ImageT
    where_ $
      images ^. ImagePersonId ==. val (toKey personId)
        &&. images ^. ImageImageType ==. val imgType
        &&. images ^. ImageMerchantId ==. val (toKey merchantId)
    return images

findRecentByPersonIdAndImageType ::
  forall m ma r.
  ( Transactionable ma m,
    MonadFlow m,
    HasFlowEnv m r '["driverOnboardingConfigs" ::: DriverOnboardingConfigs]
  ) =>
  Id Person ->
  ImageType ->
  Proxy ma ->
  m [Image]
findRecentByPersonIdAndImageType personId imgtype _ = do
  DriverOnboardingConfigs {..} <- asks (.driverOnboardingConfigs)
  let onBoardingRetryTimeinHours = intToNominalDiffTime onboardingRetryTimeinHours
  now <- getCurrentTime
  findAll @m @ma $ do
    images <- from $ table @ImageT
    where_ $
      images ^. ImagePersonId ==. val (toKey personId)
        &&. images ^. ImageImageType ==. val imgtype
        &&. images ^. ImageCreatedAt >. val (hoursAgo onBoardingRetryTimeinHours now)
    return images
  where
    hoursAgo i now = negate (3600 * i) `DT.addUTCTime` now

updateToValid :: Id Image -> SqlDB m ()
updateToValid id = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ImageIsValid =. val True]
    where_ $ tbl ^. ImageTId ==. val (toKey id)

findByMerchantId ::
  forall m ma.
  Transactionable ma m =>
  Id Merchant ->
  Proxy ma ->
  m [Image]
findByMerchantId merchantId _ = do
  findAll @m @ma $ do
    images <- from $ table @ImageT
    where_ $ images ^. ImageMerchantId ==. val (toKey merchantId)
    return images

addFailureReason :: Id Image -> DriverOnboardingError -> SqlDB m ()
addFailureReason id reason = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ImageFailureReason =. val (Just reason)]
    where_ $ tbl ^. ImageTId ==. val (toKey id)

deleteByPersonId :: Id Person -> SqlDB m ()
deleteByPersonId personId =
  Esq.delete $ do
    images <- from $ table @ImageT
    where_ $ images ^. ImagePersonId ==. val (toKey personId)
