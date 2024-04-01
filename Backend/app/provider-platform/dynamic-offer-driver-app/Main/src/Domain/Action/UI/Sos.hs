{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.UI.Sos where

import API.Types.UI.Sos
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Sos as DSos
import Environment
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.IssueManagement ()
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Sos as QSos
import Tools.Error

postSosCreate ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Person.Person),
    Kernel.Types.Id.Id DM.Merchant,
    Kernel.Types.Id.Id DMOC.MerchantOperatingCity
  ) ->
  SosReq ->
  Flow SosRes
postSosCreate (mbPersonId, _, _) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  sosDetails <- buildSosDetails person req
  void $ QSos.create sosDetails
  return $ SosRes sosDetails.id

buildSosDetails :: (EncFlow m r) => Person.Person -> SosReq -> m DSos.Sos
buildSosDetails person req = do
  pid <- generateGUID
  now <- getCurrentTime
  return
    DSos.Sos
      { id = pid,
        personId = cast person.id,
        status = DSos.Pending,
        flow = req.flow,
        rideId = cast req.rideId,
        ticketId = Nothing,
        merchantId = Just $ cast person.merchantId,
        merchantOperatingCityId = Just $ cast person.merchantOperatingCityId,
        createdAt = now,
        updatedAt = now
      }
