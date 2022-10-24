{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverOnboarding.Image where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Field
import Beckn.Types.Id
import qualified Data.Time as DT
import Domain.Types.DriverOnboarding.Error
import Domain.Types.DriverOnboarding.Image
import Domain.Types.Organization
import Domain.Types.Person (Person)
import Environment
import Storage.Tabular.DriverOnboarding.Image

create :: Image -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id Image ->
  m (Maybe Image)
findById = Esq.findById

findImagesByPersonAndType ::
  (Transactionable m) =>
  Id Person ->
  ImageType ->
  m [Image]
findImagesByPersonAndType personId imgType = do
  findAll $ do
    images <- from $ table @ImageT
    where_ $
      images ^. ImagePersonId ==. val (toKey personId)
        &&. images ^. ImageImageType ==. val imgType
    return images

findRecentByPersonIdAndImageType ::
  ( Transactionable m,
    MonadFlow m,
    HasFlowEnv m r '["driverOnboardingConfigs" ::: DriverOnboardingConfigs]
  ) =>
  Id Person ->
  ImageType ->
  m [Image]
findRecentByPersonIdAndImageType personId imgtype = do
  DriverOnboardingConfigs {..} <- asks (.driverOnboardingConfigs)
  let onBoardingRetryTimeinHours = intToNominalDiffTime onboardingRetryTimeinHours
  now <- getCurrentTime
  findAll $ do
    images <- from $ table @ImageT
    where_ $
      images ^. ImagePersonId ==. val (toKey personId)
        &&. images ^. ImageImageType ==. val imgtype
        &&. images ^. ImageCreatedAt >. val (hoursAgo onBoardingRetryTimeinHours now)
    return images
  where
    hoursAgo i now = negate (3600 * i) `DT.addUTCTime` now

updateToValid :: Id Image -> SqlDB ()
updateToValid id = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ImageIsValid =. val True]
    where_ $ tbl ^. ImageTId ==. val (toKey id)

findByorgId ::
  Transactionable m =>
  Id Organization ->
  m [Image]
findByorgId orgId = do
  findAll $ do
    images <- from $ table @ImageT
    where_ $ images ^. ImageOrganizationId ==. val (toKey orgId)
    return images

addFailureReason :: Id Image -> DriverOnboardingError -> SqlDB ()
addFailureReason id reason = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ImageFailureReason =. val (Just reason)]
    where_ $ tbl ^. ImageTId ==. val (toKey id)
