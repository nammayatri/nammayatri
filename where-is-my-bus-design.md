LTS:
1. Use lts redis in the python-proxy script.
2. 


OTP:
1. Create a proxy api in python-proxy directly giving protobuf.
2. Check for bottlenecks.

Driver-App:
1. Backend:

    a. Add new role in person table - BusDriver.

    b. Tables:
        provider: {
            id: string,
            name: string,
        }
        vehicleRouteMapping:{
            providerId: string,
            vehicleId: string,
            routeId: string,
            blocked: boolean,
            allowEndingMidRoute: boolean
        }
        tripTransaction: {
            id: string,
            providerId: string,
            vehicleId: string,
            tripId: Maybe string, -- maybe not be available
            driverId: string,
            startLocation: latLon,
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

        data FlowStatus = ....... | WAITING_FOR_ADMIN_APPROVAL | ADMIN_APPROVED | ADMIN_DECLINED

    d. NewAPIs:

        a.1 - GET /ui/wmb/driver/vehicleId (required for Private Drivers, on QR scan)
              response: {
                routesAvailable: [{
                    routeId: string,
                    source: LocationInfo,
                    destination: LocationInfo
                }]
              }
              * this should have limit of number of possible routeIds assignable
        a.2 - POST /ui/wmb/driver/{routeId}/link (required for Private Drivers) (on QR scan) (start duty)
              response: {
                tripTransactionId: string
              }
        a.3 - GRPS Notification and GET /ui/wmb/driver/journey
              response: {
                tripTransactionId: string,
                busDetails: {
                  number: string,
                  tyep: AC | NON-AC
                },
                source: LocationInfo,
                destination: LocationInfo
              }
        a.4 - POST /ui/wmb/driver/{tripTransactionId}/start (go online)
              response: {
                result: Success
              }
        a.5 - POST /ui/wmb/driver/{tripTransactionId}/request
              request: DriverRequestType,
              response: {
                requestId: string,
              }
        a.6 - GRPC notification and GET update in FlowStatus API for request status.

        a.5 - POST /ui/wmb/driver/{tripTransactionId}/end
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
                        // sendNotificationToProviderDashboard (GRPC)
                    }
                }
        
        --- Might be required:
        a.* - GET /ui/wmb/driver/routes (Just in case, they might change their routes from UI)
              response: {
                schedules: [{
                    routeId: string,
                    source: LocationInfo,
                    destination: LocationInfo,
                }]
              }
              * Paginated, no limit of routes max routes in this API.
        a.* - POST /ui/wmb/driver/routes/update (Just in case, they might change their routes from UI)
              response: {
                password: string,             -- randomly generated for a provider on rotation or some other machenism, lets sync up once.
                routeIds: ["routeId<string>"] -- validate max selection with a configurable limit.
              }
              * this will override all existing routes and replace with new selected. any other behaviour if required can be handled at UI.
3. Dashboard Backend:

    a. Types:

        {provider: string} -> name/uuid of bus providers.

    b. NewAPIs:

        a.1 GET /dashboard/wmb/routes/{provider}
            -- same as /ui/wmb/driver/routes but you would need extra API in backend to support provider based.

        a.2 POST /dashboard/wmb/routes/{provider}/update
            -- same as /ui/wmb/driver/routes/update but you would need extra API in backend to support provider based. 

        a.2 POST /dashboard/wmb/driver/{driverId}/{routeId}/link
            -- same as /ui/wmb/driver/link but you would need extra API in backend to support driverId based.

        a.3 POST /dashboard/wmb/driver/{driverId}/{tripTransactionId}/start
            -- same as /ui/wmb/driver/{tripTransactionId}/start but you would need extra API in backend to support driverId based.

        a.4 POST /dashboard/wmb/driver/{driverId}/{tripTransactionId}/end
            -- same as /ui/wmb/driver/{tripTransactionId}/end but you would need extra API in backend to support driverId based.

        a.5 on GRPC or GET /dashboard/wmb/provider/requests
            response: [{
                reqeustId: string,
                driverId: string,
                description: string,
                type: DriverRequestType,
                data: JSON{} according to notification type if required
            }]
        a.6 POST /dasboard/wmb/provider/request/{requestId}/accept
            response: {
                result: SUCCESS
            }
        a.7 POST /dasboard/wmb/provider/request/{requestId}/reject
            response: {
                result: SUCCESS
            }
      
4. Dashboard Frontend:

    a.1 Onboarding / login for providers.

    a.2 Add vehicle/bulkvehicle.

    a.3 Add routes/bulkroutes.

    a.4 Onboard driver.

            - Upload DL and Adhaar (both configurable).
            - Phone number and name.

    a.5 Link Driver for routes. (start duty)

    a.6 Start trip for a driver + route link (tripTransactionId).

    a.7 End trip 

    -- design might be required for some or all of the above.

