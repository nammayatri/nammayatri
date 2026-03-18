{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Dashboard API routes for Ride Allocation Framework
-- Provides endpoints for CRUD, activation/deactivation, simulation, and monitoring
-- of ride allocation policies for fleet owners.
--
-- Endpoints:
--   GET    /ride-allocation/policies                         — List policies with filters
--   GET    /ride-allocation/policies/:policyId               — Get policy detail
--   POST   /ride-allocation/policies                         — Create policy
--   PUT    /ride-allocation/policies/:policyId               — Update policy
--   POST   /ride-allocation/policies/:policyId/activate      — Activate policy
--   POST   /ride-allocation/policies/:policyId/deactivate    — Deactivate policy
--   POST   /ride-allocation/policies/:policyId/simulate      — Simulate policy
--   GET    /ride-allocation/policies/:policyId/history        — Policy audit trail
--   GET    /ride-allocation/monitoring                        — Allocation monitoring dashboard
--
-- All endpoints are scoped under:
--   /dashboard/provider/{merchantId}/{city}/ride-allocation/...
--
-- These routes follow the same registration pattern as existing Fleet dashboard APIs
-- and are consumed by the control-center admin dashboard.
module API.ProviderPlatform.DynamicOfferDriver.RideAllocation where
