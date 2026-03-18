{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Dashboard API routes for the Driver Directory admin view.
-- Provides endpoints for browsing, searching, and managing driver directory listings.
--
-- Routes:
--   GET  /driver-directory/list           - Paginated, filterable directory listing
--   GET  /driver-directory/:driverId/profile - Comprehensive driver profile
--   PUT  /driver-directory/:driverId/status  - Update directory enrollment status
--   POST /driver-directory/bulk/approve      - Bulk approve pending entries
--   GET  /driver-directory/stats             - Summary statistics
--   GET  /driver-directory/export            - CSV export of filtered results
module API.ProviderPlatform.DynamicOfferDriver.DriverDirectory where

-- This module defines the Servant API type and handler wiring for
-- the Driver Directory dashboard endpoints. The actual business logic
-- resides in Domain.Action.Dashboard.Driver.Directory in the
-- dynamic-offer-driver-app package.
--
-- Implementation follows the same pattern as other provider-dashboard
-- API modules (e.g., CacAuth.hs, InternalAuth.hs).
--
-- TODO: Wire up Servant API types and handler delegation once
-- the provider-dashboard API registration pattern is finalized.
-- The endpoints should be registered under the DynamicOfferDriver
-- module group with appropriate access control middleware.
