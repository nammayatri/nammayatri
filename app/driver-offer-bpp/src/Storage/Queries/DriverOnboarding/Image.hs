{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverOnboarding.Image where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.DriverOnboarding.Image
import Domain.Types.Organization
import Domain.Types.Person (Person)
import Storage.Tabular.DriverOnboarding.Image

create :: Image -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id Image ->
  m (Maybe Image)
findById = Esq.findById

findByPersonId ::
  Transactionable m =>
  Id Person ->
  m [Image]
findByPersonId personId = do
  findAll $ do
    images <- from $ table @ImageT
    where_ $ images ^. ImagePersonId ==. val (toKey personId)
    return images

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
