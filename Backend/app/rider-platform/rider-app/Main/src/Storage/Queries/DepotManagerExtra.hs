{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DepotManagerExtra where

import qualified API.Types.Dashboard.AppManagement.DepotManager as API
import qualified Domain.Action.UI.Registration as Registration
import qualified Domain.Types.Depot as DDepot
import qualified Domain.Types.DepotManager as DDepotManager
import Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment (Flow)
import Kernel.Beam.Functions
import Kernel.External.Encryption (getDbHash)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DepotManager as Beam
import qualified Storage.Queries.DepotManager as QDepotManager
import qualified Storage.Queries.PersonExtra as QP

findAll ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  m [DDepotManager.DepotManager]
findAll merchantId mLimit mOffset = do
  let limit = fromMaybe 20 mLimit
      offset = fromMaybe 0 mOffset
  findAllWithOptionsKV
    [Se.Is Beam.merchantId $ Se.Eq $ getId merchantId]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

findAllByDepotCode ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  Id DDepot.Depot ->
  Maybe Int ->
  Maybe Int ->
  m [DDepotManager.DepotManager]
findAllByDepotCode merchantId depotId mLimit mOffset = do
  let limit = fromMaybe 20 mLimit
      offset = fromMaybe 0 mOffset
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq $ getId merchantId,
          Se.Is Beam.depotCode $ Se.Eq $ getId depotId
        ]
    ]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

upsertDepotManagerDetail ::
  DM.Merchant ->
  DMOC.MerchantOperatingCity ->
  API.DepotManagerDetail ->
  Flow ()
upsertDepotManagerDetail merchant merchantOperatingCity req = do
  mobileNumberHash <- getDbHash req.mobileNumber
  mbPerson <- QP.findByMobileNumberAndMerchantId req.countryCode mobileNumberHash merchant.id

  personId <- case mbPerson of
    Just person -> pure person.id
    Nothing -> Registration.createPersonWithPhoneNumber merchant.id req.mobileNumber (Just req.countryCode)

  let depotId = Id req.depotCode :: Id DDepot.Depot
  mbExistingDepotManager <- QDepotManager.findByPrimaryKey depotId personId

  now <- getCurrentTime

  case mbExistingDepotManager of
    Just existing -> do
      let isAdmin = fromMaybe existing.isAdmin req.isAdmin
      let updatedDepotManager = existing {DDepotManager.isAdmin = isAdmin, DDepotManager.enabled = True, DDepotManager.updatedAt = now}
      QDepotManager.updateByPrimaryKey updatedDepotManager
    Nothing -> do
      let isAdmin = fromMaybe False req.isAdmin
      let newDepotManager =
            DDepotManager.DepotManager
              { DDepotManager.createdAt = now,
                DDepotManager.depotCode = depotId,
                DDepotManager.enabled = True,
                DDepotManager.isAdmin = isAdmin,
                DDepotManager.merchantId = merchant.id,
                DDepotManager.merchantOperatingCityId = merchantOperatingCity.id,
                DDepotManager.personId = personId,
                DDepotManager.updatedAt = now
              }
      QDepotManager.create newDepotManager

deleteByPrimaryKey ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DDepot.Depot ->
  Id DP.Person ->
  m ()
deleteByPrimaryKey depotId personId = do
  deleteWithKV
    [ Se.And
        [ Se.Is Beam.depotCode $ Se.Eq (getId depotId),
          Se.Is Beam.personId $ Se.Eq (getId personId)
        ]
    ]
