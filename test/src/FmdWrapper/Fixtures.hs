module FmdWrapper.Fixtures (module ReExport) where

import FmdWrapper.Fixtures.API.Confirm as ReExport (confirmOrderObject, fulfillment, payment)
import FmdWrapper.Fixtures.Address as ReExport (address)
import FmdWrapper.Fixtures.Contact as ReExport (contact)
import FmdWrapper.Fixtures.FulfillmentDetails as ReExport (endFulfillmentDetails, startFulfillmentDetails)
import FmdWrapper.Fixtures.Gps as ReExport (differentCity, validDunzoGps1, validDunzoGps2)
import FmdWrapper.Fixtures.Location as ReExport (endLocation, startLocation)
import FmdWrapper.Fixtures.Name as ReExport (name)
import FmdWrapper.Fixtures.Order as ReExport (orderItem)
import FmdWrapper.Fixtures.Person as ReExport (person)
