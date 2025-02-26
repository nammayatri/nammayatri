import "./libs/shim/core.js";
import "./libs/shim/urijs.js";
import { sleep } from 'k6';
import { group } from "k6";

export let options = { maxRedirects: 4 };

const Request = Symbol.for("request");
postman[Symbol.for("initial")]({
  options,
  collection: {
    baseUrl_app: "http://localhost:8013/v2",
    baseURL_namma_P: "http://localhost:8016/ui",
    curr_rider_authId: "--",
    user_Mobile: "",
    curr_token: "",
    driver_auth_token: "",
    cust_token: "",
    reg_token: "",
    route_count: "",
    current_ride_loc: "0",
    route: "",
    iteration: "0"
  }
});

export default function () {
  
  group("Driver Login set location, activity", function () {
    postman[Request]({
      name: "/auth",
      id: "8137a910-cb6f-45c5-8e2a-62e7b5fad26c",
      method: "POST",
      address: "{{baseURL_namma_P}}/auth",
      data: JSON.stringify({
        mobileNumber: "6666666666",
        mobileCountryCode: "+91",
        merchantId: "favorit0-0000-0000-0000-00000favorit"
      }),
      headers: {
        "Content-Type": "application/json"
      },
      post(response) {
        console.log(`VU: ${__VU}  -  ITER: ${__ITER}`);
        var jsonData = JSON.parse(responseBody);
        pm.collectionVariables.set("curr_rider_authId", jsonData.authId);
        console.log("logging ", response.body)
        pm.test("auth status code 200", function() {
          pm.response.to.have.status(200);

        });
      },
      auth(config, Var) {
        config.options["token"] = `${pm[Var]("app-reg-token")}`;
      }
    });
    postman[Request]({
      name: "verify",
      id: "2b93c6b5-9455-49fa-a4c4-090c5846c0ab",
      method: "POST",
      address: "{{baseURL_namma_P}}/auth/{{curr_rider_authId}}/verify",
      data:
        '{\n    "otp": "7891",\n    "deviceToken": "5497873d-10ca-42f3-8a32-22de4c916026"\n}',
      headers: {
        "Content-Type": "application/json;charset=utf-8"
      },
      post(response) {
        var jsonData = JSON.parse(responseBody);
        pm.collectionVariables.set("driver_auth_token", jsonData.token);
        console.log("verifying ", response.body)
        pm.test("verify code 200", function() {
          pm.response.to.have.status(200);
        });
      },
      auth(config, Var) {
        config.options["token"] = `${pm[Var]("app-reg-token")}`;
      }
    });

    postman[Request]({
      name: "/driver/location",
      id: "6c7c13e7-e203-400c-a78b-61b8f141b49e",
      method: "POST",
      address: "{{baseURL_namma_P}}/driver/location",
        data: JSON.stringify(
          [
            {
              "pt": {
                "lat": 13.01635918,
                "lon": 77.47569521
              },
              "ts": "{{current_time}}"
            }
          ]
        ),
        headers: {
          "Content-Type": "application/json;charset=utf-8",
        },
        pre() {
          let now = new Date();

          pm.globals.set(
            "current_time",
            now.toISOString()
          );
        },
        auth(config, Var) {
          config.headers["token"] = `${pm[Var]("driver_auth_token")}`;
        },
        post(response) {
          console.log("setting location ", response.body)
          pm.test("location update status code 200", function() {
            pm.response.to.have.status(200);
          });
        }
      })
      postman[Request]({
        name: "/driver/setActivity?active=true",
        id: "d8440596-c777-4179-9aba-6ad2c1a76db0",
        method: "POST",
        address: "{{baseURL_namma_P}}/driver/setActivity?active=true",
        data: "\n",
        headers: {
          "Content-Type": "application/json;charset=utf-8",
        },
        auth(config, Var) {
          config.headers["token"] = `${pm[Var]("driver_auth_token")}`;
        },
        post(response) {
          console.log("setting activity ", response.body)
          pm.test("activity code 200", function() {
            pm.response.to.have.status(200);
          });
        }
      });
  
  
      postman[Request]({
        name: "/driver/profile",
        id: "a5019a4e-3330-4af5-bfb3-278c3f4cc291",
        method: "GET",
        address: "{{baseURL_namma_P}}/driver/profile",
        headers: {
          "Content-Type": "application/json;charset=utf-8",
        },
        auth(config, Var) {
          config.headers["token"] = `${pm[Var]("driver_auth_token")}`;
        },
        post(response) {
          console.log("getting profile ", response.body)
          pm.test("profile 200", function() {
            pm.response.to.have.status(200);
          });
        }
    })
    });
        
    group("User login and ridesearch", function() {
      postman[Request]({
        name: "/auth",
        id: "3341b5a6-c6ee-41e4-9455-23959c0a3473",
        method: "POST",
        address: "{{baseUrl_app}}/auth",
        data:
          '{\n    "mobileNumber": "9898989892",\n    "mobileCountryCode": "+91",\n    "merchantId" : "NAMMA_YATRI"\n}',
        headers: {
          "Content-Type": "application/json;charset=utf-8"
        },
        pre() {
          var random = Math.floor(Math.random() * 100000);

          var user_Mobile = "98765" + random;

          pm.collectionVariables.set("user_Mobile", user_Mobile);

          console.log(user_Mobile);
        },
        post(response) {
          var jsonData = JSON.parse(responseBody);
          pm.collectionVariables.set("curr_rider_authId", jsonData.authId);
          console.log("getting customer profile ", response.body)
          pm.test("getting customer profile status code 200", function() {
            pm.response.to.have.status(200);
          });
        },
        auth(config, Var) {
          config.options["token"] = `${pm[Var]("app-reg-token")}`;
        }
      });

      postman[Request]({
        name: "/auth/:authId/verify",
        id: "5cddc11c-c935-488e-89ec-80540be7ded7",
        method: "POST",
        address: "{{baseUrl_app}}/auth/{{curr_rider_authId}}/verify",
        data:
          '{\n    "otp": "7891",\n    "deviceToken": "8e83b5dc-99a0-4306-b90d-2345f3050972"\n}',
        headers: {
          "Content-Type": "application/json;charset=utf-8"
        },
        post(response) {
          var jsonData = JSON.parse(responseBody);
          pm.collectionVariables.set("cust_token", jsonData.token);
          console.log("verifying customer profile ", response.body)
          pm.test("verify status code 200", function() {
            pm.response.to.have.status(200);
          });
        },
        auth(config, Var) {
          config.options["token"] = `${pm[Var]("app-reg-token")}`;
        }
      });

      postman[Request]({
        name: "/rideSearch",
        id: "7ee6dc74-9fc0-425b-a8a1-28e674b7186e",
        method: "POST",
        address: "{{baseUrl_app}}/rideSearch",
        data:
          '{\n    "fareProductType": "ONE_WAY",\n    "contents": {\n        "origin": {\n            "address": {\n                "area": "8th Block Koramangala",\n                "areaCode": "560047",\n                "building": "Juspay Buildings",\n                "city": "Bangalore",\n                "country": "India",\n                "door": "#444",\n                "street": "18th Main",\n                "state": "Karnataka"\n            },\n            "gps": {\n                "lat": 13.01635918,\t\n                "lon": 77.47569521\n            }\n        },\n        "destination": {\n            "address": {\n                "area": "6th Block Koramangala",\n                "areaCode": "560047",\n                "building": "Juspay Apartments",\n                "city": "Bangalore",\n                "country": "India",\n                "door": "#444",\n                "street": "18th Main",\n                "state": "Karnataka"\n            },\n            "gps": {\n                "lat": 13.0109355,\t\n                "lon": 77.46879019\n            }\n        }\n    }\n}',
        headers: {
          "Content-Type": "application/json;charset=utf-8",
          token: "{{app-reg-token}}"
        },
        post(response) {
          const body = JSON.parse(responseBody);
          pm.collectionVariables.set("curr_app_searchId", body.searchId);
          pm.collectionVariables.set("route", body.routeInfo.points);
          pm.collectionVariables.set(
            "route_count",
            body.routeInfo.points.length
          );
          var route = pm.collectionVariables.get("route");
          console.log("searching a ride", response.status)
          pm.test("ride search status code 200", function() {
            pm.response.to.have.status(200);
          });
        },
        auth(config, Var) {
          config.headers["token"] = `${pm[Var]("cust_token")}`;
        }
      });

      postman[Request]({
        name: "/rideSearch/:searchId/results",
        id: "95f732aa-4fd1-462c-8523-390ac91818c4",
        method: "GET",
        address: "{{baseUrl_app}}/rideSearch/{{curr_app_searchId}}/results",
        headers: {
          token: "{{app_reg_token}}"
        },
        pre() {
          sleep(3);
        },
        post(response) {
          console.log("results of search id ", response.body);
          var jsonData = JSON.parse(responseBody);
          if (jsonData.estimates.length > 0) {
            
            pm.collectionVariables.set(
              "curr_estimateid",
              jsonData.estimates[0].id
            );
          }
          pm.test("result of search id code 200", function() {
            pm.response.to.have.status(200);
          });
        },
        auth(config, Var) {
          config.headers["token"] = `${pm[Var]("cust_token")}`;
        }
      });

      postman[Request]({
        name: "/estimate/:estimateId/select",
        id: "5d02b05c-fb91-46c7-8710-dc5d1da790fc",
        method: "POST",
        address: "{{baseUrl_app}}/estimate/{{curr_estimateid}}/select",
        post(response) {
          console.log(response.body)
          pm.test("selecting estimate status code 200", function() {
            pm.response.to.have.status(200);
          });
        },
        auth(config, Var) {
          config.headers["token"] = `${pm[Var]("cust_token")}`;
        }
      });
    });

    group("Driver Quote Offer", function() {
      postman[Request]({
        name: "/driver/nearbyRideRequest",
        id: "929dad0d-2c85-457c-88a0-46c0b1024a1c",
        method: "GET",
        address: "{{baseURL_namma_P}}/driver/nearbyRideRequest",
        post(response) {
          const body = JSON.parse(responseBody);
          console.log("BODY for setting accept_searchId", body);

          if (body.searchRequestsForDriver.length > 0) {
            pm.collectionVariables.set(
              "accept_searchId",
              body.searchRequestsForDriver[0].searchRequestId
            );
          }
          pm.test("nearby ride request code 200", function() {
            pm.response.to.have.status(200);
          });
        },
        auth(config, Var) {
          config.headers["token"] = `${pm[Var]("driver_auth_token")}`;
        }
      });

      postman[Request]({
        name: "/driver/searchRequest/quote/offer",
        id: "2880bbfb-3a77-43c7-9179-3dfc35b0bff7",
        method: "POST",
        address: "{{baseURL_namma_P}}/driver/searchRequest/quote/offer",
        data:
          '{\n "searchRequestId": "{{accept_searchId}}",\n "offeredFare": null\n}',
        auth(config, Var) {
          config.headers["token"] = `${pm[Var]("driver_auth_token")}`;
        },
        post(response) {
          console.log("offering a quote",response.body)
          pm.test("offering a quote status code 200", function() {
            pm.response.to.have.status(200);
          });
        }
      });
    });
    group("Customer Accept Quote", function() {
      postman[Request]({
        name: "/estimate/:estimateId/quotes",
        id: "1300334c-5078-4142-a35d-2b432f2579c0",
        method: "GET",
        address: "{{baseUrl_app}}/estimate/{{curr_estimateid}}/quotes",
        headers: {
          token: "{{app-reg-token}}"
        },
        post(response) {
          var jsonData = JSON.parse(responseBody);
          console.log("getting quotes",response.body)
          if (jsonData.selectedQuotes.length > 0) {
            pm.collectionVariables.set(
              "quoteIdnew",
              jsonData.selectedQuotes[0].id
            );
            
          }
          pm.test("getting quotes status code 200", function() {
            pm.response.to.have.status(200);
          });
        },
        auth(config, Var) {
          config.headers["token"] = `${pm[Var]("cust_token")}`;
        }
      });

      postman[Request]({
        name: "/rideSearch/quotes/:quoteId/confirm",
        id: "cc25598f-acd6-4fc3-a58c-81b2268c3919",
        method: "POST",
        address: "{{baseUrl_app}}/rideSearch/quotes/{{quoteIdnew}}/confirm",
        data:
          '{\n  "fromLocation": {\n    "area": "string",\n    "state": "string",\n    "country": "string",\n    "building": "string",\n    "door": "string",\n    "street": "string",\n    "city": "string",\n    "areaCode": "string"\n  },\n  "toLocation": {\n    "area": "string",\n    "state": "string",\n    "country": "string",\n    "building": "string",\n    "door": "string",\n    "street": "string",\n    "city": "string",\n    "areaCode": "string"\n  }\n}\n',
        headers: {
          // token: "{{app_reg_token}}"
        },
        post(response) {
          const body = JSON.parse(responseBody);
          pm.collectionVariables.set("curr_app_bookingId", body.bookingId);
          console.log("confirming quotes",response.body)
          pm.test("confirming quote status code 200", function() {
            pm.response.to.have.status(200);
          });
        },
        auth(config, Var) {
          config.options["token"] = `${pm[Var]("cust_token")}`;
        }
      });
    });

    group("Ride flow", function() {
      postman[Request]({
        name: "v2/rideBooking/list",
        id: "86d814a2-b134-4fa4-a5f9-7ee98bbe356d",
        method: "GET",
        address: "{{baseUrl_app}}/rideBooking/list?limit=20",
        pre() {
          sleep(3);
        },
        post(response) {
          var jsonData = JSON.parse(responseBody);
          if (jsonData.list.length > 0) {
            console.log("otp", jsonData.list[0].rideList[0].rideOtp);
            pm.collectionVariables.set(
              "otp",
              jsonData.list[0].rideList[0].rideOtp
            );
          }
          console.log("getting the list ",response.body)
          pm.test("ride booking list status code 200", function() {
            pm.response.to.have.status(200);
          });
        },
        auth(config, Var) {
          config.headers["token"] = `${pm[Var]("cust_token")}`;
        }
      });

      postman[Request]({
        name: "driver/ride/list",
        id: "805d0857-eb6d-4437-8ee8-2e2805a6cfeb",
        method: "GET",
        address: "{{baseURL_namma_P}}/driver/ride/list?limit=30",
        headers: {
          token: "{{app-reg-token}}"
        },
        post(response) {
          var jsonData = JSON.parse(responseBody);
          console.log("ride_id ", jsonData.list[0].id);
          pm.collectionVariables.set("ride_id", jsonData.list[0].id);
          pm.test("driver ride list code 200", function() {
            pm.response.to.have.status(200);
          });

        },
        auth(config, Var) {
          config.headers["token"] = `${pm[Var]("driver_auth_token")}`;
        }
      });

    
      postman[Request]({
        name: "driver/ride/rideId/start",
        id: "c2ebcc61-d2b1-4f77-a20d-c6707d43954a",
        method: "POST",
        address: "{{baseURL_namma_P}}/driver/ride/{{ride_id}}/start",
        data:
          '{\n    "rideOtp": "{{otp}}",\n    "point": {\n        "lat": 13.01635918,\n        "lon": 77.47569521\n    }\n}',
        headers: {
          token: "{{app-reg-token}}"
        },
        post(response) {
          console.log("starting the ride",response.body)
          pm.test("starting the ride status code 200", function() {
            pm.response.to.have.status(200);
          });
        },
        auth(config, Var) {
          config.headers["token"] = `${pm[Var]("driver_auth_token")}`;
        }
      });

      postman[Request]({
        name: "/driver/locationUpdate",
        id: "3a9f4770-6f39-479e-a821-da46486ecf75",
        method: "POST",
        address: "{{baseURL_namma_P}}/driver/location",
        data:
          '[\n    {\n        "pt": {\n            "lat": {{lat}},\n            "lon":{{lon}}\n        },\n        "ts": "{{current_time}}"\n    }\n]',
        headers: {
          "Content-Type": "application/json;charset=utf-8"
        },
        pre() {
          let now = new Date();

          pm.globals.set(
            "current_time",
            now.toISOString()
          );

          sleep(3);

          var route = pm.collectionVariables.get("route");
          let current_ride_loc = pm.collectionVariables.get("current_ride_loc");
          pm.variables.set("lat", route[current_ride_loc].lat);
          pm.variables.set("lon", route[current_ride_loc].lon);

          console.log(route[current_ride_loc]);
        },
        post(response) {
          console.log("ending the ride",response.body)
          pm.test("location update status code 200", function() {
            pm.response.to.have.status(200);
          });
          var current_ride_loc = pm.collectionVariables.get("current_ride_loc");
          var route_count = pm.collectionVariables.get("route_count");

          if (current_ride_loc < route_count - 1) {
            pm.collectionVariables.set("current_ride_loc", ++current_ride_loc);
          } else {
            pm.collectionVariables.set("current_ride_loc", 0);
          }
          pm.test("location update status code 200", function() {
            pm.response.to.have.status(200);
          });
        },
        auth(config, Var) {
          config.options["token"] = `${pm[Var]("driver_auth_token")}`;
        }
      });

      postman[Request]({
        name: "driver/ride/rideId/end",
        id: "4725c88b-29a8-49d2-ba62-187cce62304d",
        method: "POST",
        address: "{{baseURL_namma_P}}/driver/ride/{{ride_id}}/end",
        data:
          '{\n    "point": {\n        "lat": 13.0109355,\n        "lon": 77.46879019\n    }\n}',
        headers: {
          token: "{{app-reg-token}}"
        },
        post(response) {
          const body = JSON.parse(responseBody);
          pm.collectionVariables.set("curr_app_bookingId", body.bookingId);
          pm.test("ending the ride status code 200", function() {
            pm.response.to.have.status(200);
          });
        },
        auth(config, Var) {
          config.headers["token"] = `${pm[Var]("driver_auth_token")}`;
        }
      });

      postman[Request]({
        name: "/driver/setActivity?active=false",
        id: "e593e596-c9b1-4273-86d3-53cf2f700500",
        method: "POST",
        address: "{{baseURL_namma_P}}/driver/setActivity?active=false",
        data: "\n",
        headers: {
          "Content-Type": "application/json;charset=utf-8"
        },
        auth(config, Var) {
          config.options["token"] = `${pm[Var]("driver_auth_token")}`;
        },

        post(response){
          pm.test("setting activity status code 200", function() {
            pm.response.to.have.status(200);
          });
        }
      });
    });
  }