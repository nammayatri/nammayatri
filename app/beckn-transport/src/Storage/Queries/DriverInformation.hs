module Storage.Queries.DriverInformation where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Control.Applicative (liftA2)
import Domain.Types.DriverInformation
import Domain.Types.Organization (Organization)
import Domain.Types.Person as Person
import Storage.Tabular.DriverInformation
import Storage.Tabular.DriverLocation
import Storage.Tabular.Person
import Types.App (Driver)

create :: DriverInformation -> SqlDB ()
create = Esq.create'

findById :: Transactionable m => Id Driver -> m (Maybe DriverInformation)
findById = Esq.findById . cast

fetchAllAvailableByIds :: Transactionable m => [Id Driver] -> m [DriverInformation]
fetchAllAvailableByIds driversIds = Esq.findAll $ do
  driverInformation <- from $ table @DriverInformationT
  where_ $
    driverInformation ^. DriverInformationDriverId `in_` valList personsKeys
      &&. driverInformation ^. DriverInformationActive
      &&. not_ (driverInformation ^. DriverInformationOnRide)
  return driverInformation
  where
    personsKeys = toKey . cast <$> driversIds

updateActivity :: Id Driver -> Bool -> SqlDB ()
updateActivity driverId isActive = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ DriverInformationActive =. val isActive,
        DriverInformationUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverInformationDriverId ==. val (toKey $ cast driverId)

updateEnabledState :: Id Driver -> Bool -> SqlDB ()
updateEnabledState driverId isEnabled = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ DriverInformationEnabled =. val isEnabled,
        DriverInformationUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverInformationDriverId ==. val (toKey $ cast driverId)

updateOnRide ::
  Id Driver ->
  Bool ->
  SqlDB ()
updateOnRide driverId onRide = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ DriverInformationOnRide =. val onRide,
        DriverInformationUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverInformationDriverId ==. val (toKey $ cast driverId)

updateDowngradingOptions :: Id Driver -> Bool -> Bool -> SqlDB ()
updateDowngradingOptions driverId canDowngradeToSedan canDowngradeToHatchback = do
  now <- getCurrentTime
  update' $ \tbl -> do
    set
      tbl
      [ DriverInformationCanDowngradeToSedan =. val canDowngradeToSedan,
        DriverInformationCanDowngradeToHatchback =. val canDowngradeToHatchback,
        DriverInformationUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverInformationDriverId ==. val (toKey $ cast driverId)

resetDowngradingOptions :: Id Driver -> SqlDB ()
resetDowngradingOptions driverId = updateDowngradingOptions driverId False False

deleteById :: Id Driver -> SqlDB ()
deleteById = Esq.deleteByKey' @DriverInformationT . cast

findAllWithLimitOffsetByOrgId ::
  ( Transactionable m,
    EncFlow m r
  ) =>
  Maybe Text ->
  Maybe Integer ->
  Maybe Integer ->
  Id Organization ->
  m [(Person, DriverInformation)]
findAllWithLimitOffsetByOrgId mbSearchString mbLimit mbOffset orgId = do
  mbSearchStrDBHash <- getDbHash `traverse` mbSearchString
  findAll $ do
    (person :& driverInformation) <-
      from $
        table @PersonT
          `innerJoin` table @DriverInformationT
            `Esq.on` ( \(person :& driverInformation) ->
                         driverInformation ^. DriverInformationDriverId ==. person ^. PersonTId
                     )
    where_ $
      person ^. PersonRole ==. val Person.DRIVER
        &&. person ^. PersonOrganizationId ==. val (Just $ toKey orgId)
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
      ( concat_ @Text [unMaybe $ person ^. PersonFirstName, val " ", unMaybe $ person ^. PersonMiddleName, val " ", unMaybe $ person ^. PersonLastName]
          `ilike` likeSearchStr
        )
        ||. person ^. PersonMobileNumberHash ==. val (Just searchStrDBHash)
    unMaybe = maybe_ (val "") identity

getDriversWithOutdatedLocationsToMakeInactive :: Transactionable m => UTCTime -> m [Person]
getDriversWithOutdatedLocationsToMakeInactive before = do
  findAll $ do
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
