{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Confirm (buildConfirmReq) where

import qualified Beckn.Types.Core.Taxi.Confirm as Confirm
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified Domain.Types.LocationAddress as DBL
import Environment
import EulerHS.Prelude hiding (id, state)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common

buildConfirmReq ::
  (HasFlowEnv m r ["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl]) =>
  DOnInit.OnInitRes ->
  m (BecknReq Confirm.ConfirmMessage)
buildConfirmReq res = do
  bapURIs <- asks (.bapSelfURIs)
  bapIDs <- asks (.bapSelfIds)
  messageId <- generateGUID
  context <- buildTaxiContext Context.CONFIRM messageId (Just res.transactionId) bapIDs.cabs bapURIs.cabs (Just res.bppId) (Just res.bppUrl)
  pure $ BecknReq context $ mkConfirmMessage res

mkConfirmMessage :: DOnInit.OnInitRes -> Confirm.ConfirmMessage
mkConfirmMessage res =
  Confirm.ConfirmMessage
    { order =
        Confirm.Order
          { id = getId res.bppBookingId,
            fulfillment = mkFulfillment res.fromLocationAddress res.mbToLocationAddress,
            customer =
              Confirm.OrderCustomer
                { contact =
                    Confirm.Contact
                      { phone =
                          Confirm.Phone
                            { country_code = res.riderPhoneCountryCode,
                              number = res.riderPhoneNumber
                            }
                      },
                  person =
                    res.mbRiderName <&> \riderName ->
                      Confirm.OrderPerson
                        { name = riderName
                        }
                },
            payment = mkPayment res.estimatedTotalFare
          }
    }

mkFulfillment :: DBL.LocationAddress -> Maybe DBL.LocationAddress -> Confirm.FulfillmentInfo
mkFulfillment startLoc mbStopLoc =
  Confirm.FulfillmentInfo
    { start =
        Confirm.StartInfo
          { location = mkLocation startLoc
          },
      end =
        mbStopLoc <&> \stopLoc ->
          Confirm.StopInfo
            { location = mkLocation stopLoc
            }
    }

mkLocation :: DBL.LocationAddress -> Confirm.Location
mkLocation DBL.LocationAddress {..} =
  Confirm.Location
    { address =
        Confirm.Address
          { area_code = areaCode,
            locality = area,
            ward = ward,
            door = door,
            ..
          }
    }

mkPayment :: Money -> Confirm.Payment
mkPayment estimatedTotalFare =
  Confirm.Payment
    { collected_by = "BAP",
      params =
        Confirm.PaymentParams
          { amount = realToFrac estimatedTotalFare,
            currency = "INR"
          },
      time = Confirm.TimeDuration "P2D",
      _type = Confirm.ON_FULFILLMENT
    }
