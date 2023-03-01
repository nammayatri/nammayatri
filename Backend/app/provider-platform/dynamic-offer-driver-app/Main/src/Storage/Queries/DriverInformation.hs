{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverInformation where

import Control.Applicative (liftA2)
import Domain.Types.DriverInformation
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person as Person
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.DriverInformation
import Storage.Tabular.DriverLocation
import Storage.Tabular.Person

create :: DriverInformation -> SqlDB m ()
create = Esq.create

findById :: forall m ma. Transactionable ma m => Proxy ma -> Id Person.Driver -> m (Maybe DriverInformation)
findById _ = Esq.findById @m @ma . cast

fetchAllByIds :: forall m ma. Transactionable ma m => Id Merchant -> [Id Driver] -> Proxy ma -> m [DriverInformation]
fetchAllByIds merchantId driversIds _ = Esq.findAll @m @ma $ do
  (driverInformation :& person) <-
    from $
      table @DriverInformationT
        `innerJoin` table @PersonT
          `Esq.on` ( \(driverInformation :& person) ->
                       driverInformation ^. DriverInformationDriverId ==. person ^. PersonTId
                   )
  where_ $
    driverInformation ^. DriverInformationDriverId `in_` valList personsKeys
      &&. (person ^. PersonMerchantId ==. (val . toKey $ merchantId))
  return driverInformation
  where
    personsKeys = toKey . cast <$> driversIds

fetchAllAvailableByIds :: forall m ma. Transactionable ma m => [Id Person.Driver] -> Proxy ma -> m [DriverInformation]
fetchAllAvailableByIds driversIds _ = Esq.findAll @m @ma $ do
  driverInformation <- from $ table @DriverInformationT
  where_ $
    driverInformation ^. DriverInformationDriverId `in_` valList personsKeys
      &&. driverInformation ^. DriverInformationActive
      &&. not_ (driverInformation ^. DriverInformationOnRide)
  return driverInformation
  where
    personsKeys = toKey . cast <$> driversIds

updateActivity :: Id Person.Driver -> Bool -> SqlDB m ()
updateActivity driverId isActive = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverInformationActive =. val isActive,
        DriverInformationUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverInformationDriverId ==. val (toKey $ cast driverId)

updateEnabledState :: Id Driver -> Bool -> SqlDB m ()
updateEnabledState driverId isEnabled = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      $ [ DriverInformationEnabled =. val isEnabled,
          DriverInformationUpdatedAt =. val now
        ]
        <> [DriverInformationLastEnabledOn =. val (Just now) | isEnabled]
    where_ $ tbl ^. DriverInformationDriverId ==. val (toKey $ cast driverId)

updateEnabledVerifiedState :: Id Person.Driver -> Bool -> Bool -> SqlDB m ()
updateEnabledVerifiedState driverId isEnabled isVerified = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      $ [ DriverInformationEnabled =. val isEnabled,
          DriverInformationVerified =. val isVerified,
          DriverInformationUpdatedAt =. val now
        ]
        <> [DriverInformationLastEnabledOn =. val (Just now) | isEnabled]
    where_ $ tbl ^. DriverInformationDriverId ==. val (toKey $ cast driverId)

updateBlockedState :: Id Person.Driver -> Bool -> SqlDB m ()
updateBlockedState driverId isBlocked = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverInformationBlocked =. val isBlocked,
        DriverInformationUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverInformationDriverId ==. val (toKey $ cast driverId)

verifyAndEnableDriver :: Id Person -> SqlDB m ()
verifyAndEnableDriver driverId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverInformationEnabled =. val True,
        DriverInformationVerified =. val True,
        DriverInformationUpdatedAt =. val now,
        DriverInformationLastEnabledOn =. val (Just now)
      ]
    where_ $ tbl ^. DriverInformationDriverId ==. val (toKey driverId)

updateEnabledStateReturningIds :: forall m r. EsqDBFlow m r => Id Merchant -> [Id Driver] -> Bool -> m [Id Driver]
updateEnabledStateReturningIds merchantId driverIds isEnabled =
  Esq.runTransaction $ do
    present <- fmap (cast . (.driverId)) <$> fetchAllByIds merchantId driverIds (Proxy @m)
    updateEnabledStateForIds present
    pure present
  where
    updateEnabledStateForIds :: [Id Driver] -> SqlDB m ()
    updateEnabledStateForIds present = do
      now <- getCurrentTime
      Esq.update $ \tbl -> do
        set
          tbl
          $ [ DriverInformationEnabled =. val isEnabled,
              DriverInformationUpdatedAt =. val now
            ]
            <> [DriverInformationLastEnabledOn =. val (Just now) | isEnabled]
        where_ $ tbl ^. DriverInformationDriverId `in_` valList (map (toKey . cast) present)

updateOnRide ::
  Id Person.Driver ->
  Bool ->
  SqlDB m ()
updateOnRide driverId onRide = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverInformationOnRide =. val onRide,
        DriverInformationUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverInformationDriverId ==. val (toKey $ cast driverId)

updateNotOnRideMultiple :: [Id Person.Driver] -> SqlDB m ()
updateNotOnRideMultiple driverIds = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverInformationOnRide =. val False,
        DriverInformationUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverInformationDriverId `in_` valList (toKey . cast <$> driverIds)

deleteById :: Id Person.Driver -> SqlDB m ()
deleteById = Esq.deleteByKey @DriverInformationT . cast

findAllWithLimitOffsetByMerchantId ::
  forall m ma.
  Transactionable ma m =>
  Maybe Text ->
  Maybe DbHash ->
  Maybe Integer ->
  Maybe Integer ->
  Id Merchant ->
  Proxy ma ->
  m [(Person, DriverInformation)]
findAllWithLimitOffsetByMerchantId mbSearchString mbSearchStrDBHash mbLimit mbOffset merchantId _ = do
  findAll @m @ma $ do
    (person :& driverInformation) <-
      from $
        table @PersonT
          `innerJoin` table @DriverInformationT
            `Esq.on` ( \(person :& driverInformation) ->
                         driverInformation ^. DriverInformationDriverId ==. person ^. PersonTId
                     )
    where_ $
      person ^. PersonRole ==. val Person.DRIVER
        &&. person ^. PersonMerchantId ==. val (toKey merchantId)
        &&. Esq.whenJust_ (liftA2 (,) mbSearchString mbSearchStrDBHash) (filterBySearchString person)
    orderBy [desc $ driverInformation ^. DriverInformationCreatedAt]
    limit limitVal
    offset offsetVal
    return (person, driverInformation)
  where
    limitVal = maybe 100 fromIntegral mbLimit
    offsetVal = maybe 0 fromIntegral mbOffset

    filterBySearchString person (searchStr, searchStrDBHash) = do
      let likeSearchStr = (%) ++. val searchStr ++. (%)
      ( concat_ @Text [person ^. PersonFirstName, val " ", unMaybe $ person ^. PersonMiddleName, val " ", unMaybe $ person ^. PersonLastName]
          `ilike` likeSearchStr
        )
        ||. person ^. PersonMobileNumberHash ==. val (Just searchStrDBHash)
    unMaybe = maybe_ (val "") identity

getDriversWithOutdatedLocationsToMakeInactive :: forall m ma. Transactionable ma m => UTCTime -> Proxy ma -> m [Person]
getDriversWithOutdatedLocationsToMakeInactive before _ = do
  findAll @m @ma $ do
    (driverInformation :& _ :& person) <-
      from $
        table @DriverInformationT
          `innerJoin` table @DriverLocationT
            `Esq.on` ( \(driverInformation :& drLoc) ->
                         driverInformation ^. DriverInformationDriverId ==. drLoc ^. DriverLocationDriverId
                           &&. drLoc ^. DriverLocationUpdatedAt <. val before
                     )
          `innerJoin` table @PersonT
            `Esq.on` ( \(driverInformation :& _ :& person) ->
                         driverInformation ^. DriverInformationDriverId ==. person ^. PersonTId
                     )
    where_ $ driverInformation ^. DriverInformationActive
    orderBy [asc $ driverInformation ^. DriverInformationUpdatedAt]
    pure person

addReferralCode :: Id Person -> EncryptedHashedField 'AsEncrypted Text -> SqlDB m ()
addReferralCode personId code = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverInformationReferralCode =. val (Just (code & unEncrypted . (.encrypted)))
      ]
    where_ $ tbl ^. DriverInformationDriverId ==. val (toKey personId)

countDrivers :: forall m ma. Transactionable ma m => Id Merchant -> Proxy ma -> m (Int, Int)
countDrivers merchantId _ =
  getResults <$> do
    findAll @m @ma $ do
      (driverInformation :& person) <-
        from $
          table @DriverInformationT
            `innerJoin` table @PersonT
              `Esq.on` ( \(driverInformation :& person) ->
                           driverInformation ^. DriverInformationDriverId ==. person ^. PersonTId
                       )
      where_ $
        person ^. PersonMerchantId ==. (val . toKey $ merchantId)
      groupBy (driverInformation ^. DriverInformationActive)
      pure (driverInformation ^. DriverInformationActive, count @Int $ person ^. PersonId)
  where
    getResults :: [(Bool, Int)] -> (Int, Int)
    getResults = foldl func (0, 0)

    func (active, inactive) (activity, counter) =
      if activity then (active + counter, inactive) else (active, inactive + counter)
