Driver-App:
1. Backend:

    a. Add new role in person table - BusDriver.

    b. Tables:

        routeMappingConfigs: {
            id: string,
            allowEndingMidRoute: boolean -- govt will have false, private will have true, default false
        }

        vehicleRouteMapping: {
            fleetOwnerId: string, -- person with role fleet.
            vehicleId: string,
            routeCode: string,
            blocked: boolean,
            configId: routeMappingConfigs
        } 1-N mapping 
        pk: vehicleId, routeCode

        tripTransaction: {
            id: string,
            fleetOwnerId: string,
            vehicleId: string,
            tripId: Maybe string, -- GET IT FROM BAP
            driverId: string,
            status: TripStatus,
            isCurrentlyDeviated: boolean, -- default false, will come in v2
            deviationCount: int,
            startLocation: Maybe latLon,
            endLocation: Maybe latLon,
            startedNearStopId: StopId, -- if change is made somewhere between stops, then this will store the upcoming stopId.
            endStopId: Maybe stopId,
            allowEndingMidRoute: boolean -- from vehicleRouteMapping
        }
        * route to trip Id mapping already exist in some tables, just create a new entry in tripTransaction using the schedules of trip.

        driverRequest: {
            id: string,
            requestType: string,
            description: Maybe string,
            tripTransactionId: string,
            status: Maybe RequestStatus,
            reason: Maybe string, -- details from dashboard admin on accept/reject reason
        }

    c. Types:

        data TripStatus = TRIP_ASSIGNED | IN_PROGRESS | PAUSED | COMPLETED

        data RequestStatus = ACCEPTED | REJECTED

        data LocationInfo = {
            lat: double,
            long: double,
            address: string,
            details: string
        }
        
        data DriverRequestType = END_RIDE EndRideData | CHANGE_ROUTE ChangeRouteData

        data EndRideData = {
            lat: double, 
            lon: double,
            reason: Maybe string
        }

    d. NewAPIs:

        a.1 - GET /ui/wmb/availableRoutes/{vehicleNumber} (vehicleNumber from OR, required for Private Drivers, on QR scan)
              response: [{
                    routeCode: string,
                    source: LocationInfo,
                    destination: LocationInfo
                }]
              * this should have limit of number of possible routeIds assignable
        a.2 - POST /ui/wmb/trip/link (required for Private Drivers) (on QR scan) (start duty)
              request:{ 
                vehicleNumber: string,
                routeCode: string,
              }
              response: {
                tripTransactionId: string,
                busDetails: {
                  number: string,
                  tyep: AC | NON-AC
                },
                source: LocationInfo,
                destination: LocationInfo
              }
              * generate a tripTransaction without tripId with status TRIP_ASSIGNED
              -- mark onRide = true in driverInfo
              -- upsert vehicle table.
        a.3 - GRPC Notification and GET /ui/wmb/trip/active (for dashbord linking usecase)
              response: {
                tripTransactionId: string,
                busDetails: {
                  number: string,
                  tyep: AC | NON-AC
                },
                source: LocationInfo,
                destination: LocationInfo
              }
        a.4 - POST /ui/wmb/trip/{tripTransactionId}/start (go online)
              request: {
                location: latLon
              }
              response: {
                result: Success
              }
              * Map the tripId according to location -> stop matching.
                // figure out the nearest crossed or on stop as the `start stop`
                allTrips <- findALlRouteTrips routeId
                tripId = undefined;
                tripStopBusIsAt ::[(stop, trip, scheduledStopArrivalTime)] <- 
                    for all trips in allTrips:
                    do
                        let startStop = fold (\currLoc tripStop acc -> min (distance currStop tripStop) acc) 1000000 tripStops
                    done
                let tripId = least duration stop from currentTime in tripStopBusIsAt
                // set this as tripId in tripTransaction.
                // without TripId start shouldn't be allowed.
                // map using the current time nearest to trip start stop scheudle arrival time.

        a.5 - POST /ui/wmb/trip/{tripTransactionId}/request
              request: DriverRequestType,
              response: {
                requestId: string,
              }

        a.6 - POST /ui/wmb/trip/{tripTransactionId}/end
              reqeust: {
                lat: double,
                lon: double
              }
              response: {
                result: SUCCESS | FLOW_STATUS_VALA_WAITING_RESPONSE
              }
              - if (End location within X meters of route's last stop) {
                    allow ending trip.
                } else {
                    if(allowEndingMidRoute){
                        // allow ending trip with confirmation popup on UI.
                    } else {
                        // create request for dashbaord admin to accept END_RIDE request.
                        // sendNotificationTofleetOwnerIdDashboard (GRPC)
                    }
                }
        
        --- Might be required:
        a.* - GET /ui/wmb/routes?searchString=<Maybe string> (Just in case, they might change their routes from UI)
              response: {
                schedules: [{
                    routeCode: string,
                    source: LocationInfo,
                    destination: LocationInfo,
                }]
              }
              * Paginated, no limit of routes max routes in this API.
        a.* - POST /ui/wmb/vehicleRouteMapping/upsert (Just in case, they might change their routes from UI)
              response: {
                password: string,             -- randomly generated for a fleetOwnerId on rotation or some other machenism, lets sync up once.
                routeIds: ["routeCode<string>"] -- validate max selection with a configurable limit.
                routeCode: string,
                blocked: boolean,
                allowEndingMidRoute: boolean
              }
              * if vehicle is not there create vehicle

3. Dashboard Backend:

    a. Types:

        {fleetOwnerId: string} -> name/uuid of bus fleetOwnerIds.

    b. NewAPIs:            
        -- Bulk Upsert CSV vehicle, driver,routes mapping
        a.1 GET /dashboard/wmb/routes?searchString=<Maybe string>&limit=&offset=
            -- do internal API call to BAP
            -- same as /ui/wmb/routes?searchString=<Maybe string> but you would need extra API in backend to support fleetOwnerId based.


        a.2 GET /dashboard/wmb/vehicleRouteMapping/list?searchString=vehicleNumber&limit=&offset=
            response: [{
                vechileNumber: string,
                vehicleId: string,
                type: AC | NON_AC...,
                routesLinked: [{RouteDetails}]
            }]

        a.2 POST /dashboard/wmb/vehicleRouteMapping/{vehicleId}/upsert
            -- same as /ui/wmb/vehicleRouteMapping/upsert but you would need extra API in backend to support fleetOwnerId based. 

        a.3 POST /dashboard/wmb/{vehicleId}/trip/link
            request: {
                driverNumber: string,
                routeCode: string,
            }
            response: {
                tripTransactionId: string,
                busDetails: {
                  number: string,
                  tyep: AC | NON-AC
                },
                source: LocationInfo,
                destination: LocationInfo
            }

        a.4 POST /dashboard/wmb/{vehicleId}/trip/{tripTransactionId}/start
            -- same as /ui/wmb/trip/{tripTransactionId}/start but you would need extra API in backend to support vehicleId based.

        a.5 POST /dashboard/wmb/{vehicleId}/trip/{tripTransactionId}/end
            -- same as /ui/wmb/trip/{tripTransactionId}/end but you would need extra API in backend to support vehicleId based.

        a.6 on GRPC or GET /dashboard/wmb/driverRequests&status=Accept&date=date
            response: [{
                reqeustId: string,
                driverId: string,
                description: string,
                type: DriverRequestType,
                data: JSON{} according to notification type if required
            }]
        a.7 POST /dasboard/wmb/driverRequest/{requestId}/accept
            response: {
                result: SUCCESS
            }
        a.8 POST /dasboard/wmb/driverRequest/{requestId}/reject
            response: {
                result: SUCCESS
            }

4. Dashboard Frontend:

    a.1 Onboarding / login for fleetOwnerIds.

    a.2 Add vehicle/bulkvehicle.

    a.3 Add routes/bulkroutes.

    a.4 Onboard driver.

            - Upload DL and Adhaar (both configurable).
            - Phone number and name.

    a.5 Link Driver for routes. (start duty)

    a.6 Start trip for a driver + route link (tripTransactionId).

    a.7 End trip 

    -- design might be required for some or all of the above.

