import http from "k6/http";
import { sleep, check } from "k6";

function formRequest(transaction_id) {
  return {
    context: {
      ttl: null,
      country: "IND",
      domain: "MOBILITY",
      bpp_uri: "http://localhost",
      transaction_id: transaction_id,
      action: "search",
      message_id: "55976074-97f9-4eee-bf65-be0d593aa91b",
      city: null,
      bap_uri: "http://localhost",
      domain_version: "0.8.2",
      timestamp: "2021-03-29T03:58:08.104737Z",
      core_version: "0.8.2",
    },
    message: {
      intent: {
        pickups: [
          {
            location: {
              circle: null,
              country: null,
              "3dspace": null,
              address: null,
              gps: null,
              polygon: null,
              city: null,
              station_code: null,
            },
            id: "83eff7fd-859a-497b-9098-e511e48b491a",
            transfers: [],
            descriptor: null,
            departure_time: {
              est: "2021-03-29T05:58:08.104737Z",
              act: null,
            },
            arrival_time: { est: "2021-03-29T05:58:08.104737Z", act: null },
          },
        ],
        item_id: null,
        drops: [
          {
            location: {
              circle: null,
              country: null,
              "3dspace": null,
              address: null,
              gps: null,
              polygon: null,
              city: null,
              station_code: null,
            },
            id: "c1a134d3-bc99-4e10-b2e4-76108ca73675",
            transfers: [],
            descriptor: null,
            departure_time: {
              est: "2021-03-29T05:58:08.104737Z",
              act: null,
            },
            arrival_time: { est: "2021-03-29T05:58:08.104737Z", act: null },
          },
        ],
        category_id: null,
        fare: {
          maximum_value: null,
          computed_value: null,
          value: { fractional: null, integral: "800" },
          estimated_value: null,
          listed_value: null,
          currency: "INR",
          minimum_value: null,
          offered_value: null,
        },
        query_string: null,
        provider_id: null,
        payload: {
          traveller_count: null,
          travellers: [],
          luggage: null,
          travel_group: null,
        },
        vehicle: {
          variant: "SUV",
          energy_type: null,
          color: "Black",
          size: null,
          category: "CAR",
          capacity: null,
          model: null,
          make: null,
          registration: null,
        },
        transfer: null,
        tags: null,
      },
    },
  };
}

export default function () {
  var url =
    "http://127.0.0.1:8014/v1/7f7896dd-787e-4a0b-8675-e9e6fe93bb8f/search";
  var requestMap = [
    {
      transaction_id: "965609d6-5cbb-463a-b4cf-d27e9cc7c7ce",
      signature:
        'Signature keyId="JUSPAY.MOBILITY.APP.UAT.1|juspay-mobility-bap-1-key|ed25519",algorithm="ed25519",created=1616993588,expires=1616994188,headers="(created) (expires) digest",signature="PVw0sOgUVzibNjAjjrvq566miA4/rSU6xwA+it9pZD2F0i1EBQ4S4Fcv4Ov8Or36rEhxwCNCDjuDZtEsaloLDw=="',
    },
    {
      transaction_id: "ad4d82c8-a845-49bd-9230-823edeeaf7bd",
      signature:
        'Signature keyId="JUSPAY.MOBILITY.APP.UAT.1|juspay-mobility-bap-1-key|ed25519",algorithm="ed25519",created=1616993588,expires=1616994188,headers="(created) (expires) digest",signature="xWCoU1YNpzI05oqtB8lSQ9bx6zythdc/CD4kLIOB/e109bd/pStQv6+c8aG8FHJST9LuqEwrdauFdcKi/kwkCQ=="',
    },
    {
      transaction_id: "596d832e-2d63-46fc-9c34-e252f5d69162",
      signature:
        'Signature keyId="JUSPAY.MOBILITY.APP.UAT.1|juspay-mobility-bap-1-key|ed25519",algorithm="ed25519",created=1616993588,expires=1616994188,headers="(created) (expires) digest",signature="bi4qhQKyUpg5lbuRpJZjX0eSfRD0MJtY05D82Mbh1R8B5C5f3OsmGM8JncSUM9mYDqccZUw8Dy49hkEPE8M7Aw=="',
    },
    {
      transaction_id: "6d5f374a-344c-46be-8fd8-20da3fd0347b",
      signature:
        'Signature keyId="JUSPAY.MOBILITY.APP.UAT.1|juspay-mobility-bap-1-key|ed25519",algorithm="ed25519",created=1616993588,expires=1616994188,headers="(created) (expires) digest",signature="abvrlk0V5j7LXvuTgpjrAJ72EB3ksynAl55091/CLPYjSLWGaA6+F65BxrxE6jmfkiLyG/oDp6e8rQlsmEc+Ag=="',
    },
    {
      transaction_id: "c22dfb77-1eaf-405c-9ec9-65dc891cf415",
      signature:
        'Signature keyId="JUSPAY.MOBILITY.APP.UAT.1|juspay-mobility-bap-1-key|ed25519",algorithm="ed25519",created=1616993588,expires=1616994188,headers="(created) (expires) digest",signature="Mp3uJ/4feQyUfhAe2JGSEQUG5AbjSl0TxYZQs4KQU9qjhNR1l1+j1l9OK8/bONvXMNAngpQ7GZX8t2jBpVM2BA=="',
    },
    {
      transaction_id: "85bef4b6-6e88-4e1e-98ba-2128f1c8cb3e",
      signature:
        'Signature keyId="JUSPAY.MOBILITY.APP.UAT.1|juspay-mobility-bap-1-key|ed25519",algorithm="ed25519",created=1616993588,expires=1616994188,headers="(created) (expires) digest",signature="eRKMJ0ButcPLqkqR0ZLctBgKcxKkt35hs25qvwTLZnQcfdNBOmWggjvcmOOf/Rwy1hZ8gcluROhClgbBU7shCg=="',
    },
    {
      transaction_id: "84533d9f-76c4-4bbb-859a-71d8bc5e2d6b",
      signature:
        'Signature keyId="JUSPAY.MOBILITY.APP.UAT.1|juspay-mobility-bap-1-key|ed25519",algorithm="ed25519",created=1616993588,expires=1616994188,headers="(created) (expires) digest",signature="wxfhjHMNcF9QEEDP7meONveorIb4JA9j8nl3gMIIOxU82GF4tD8NoPNvyxrjKD2psnpIUJY3krnewSeOB76oAw=="',
    },
    {
      transaction_id: "db08d14a-82ab-43f2-8055-5eb3807afc09",
      signature:
        'Signature keyId="JUSPAY.MOBILITY.APP.UAT.1|juspay-mobility-bap-1-key|ed25519",algorithm="ed25519",created=1616993588,expires=1616994188,headers="(created) (expires) digest",signature="i5I9X/RaKPZB1JY0wMB2vehAm0PwqVqODVlPpPn2jEvVw19mC18YYx4DNK/4axvHPLw/PR9DIOrE5NMXd5PBAQ=="',
    },
    {
      transaction_id: "f2410654-c3c5-40d3-8cdc-72b4374f3d33",
      signature:
        'Signature keyId="JUSPAY.MOBILITY.APP.UAT.1|juspay-mobility-bap-1-key|ed25519",algorithm="ed25519",created=1616993588,expires=1616994188,headers="(created) (expires) digest",signature="836qtBBOIF/0gQE6hyl4WWTJ2vux7KiPvx2yPLk14s1Oy3rVVOApsE3Wgv9Urpg6tkVJd7bcSapanHngfV0uDA=="',
    },
    {
      transaction_id: "5342c660-3e5b-4a21-90db-549668e88cef",
      signature:
        'Signature keyId="JUSPAY.MOBILITY.APP.UAT.1|juspay-mobility-bap-1-key|ed25519",algorithm="ed25519",created=1616993588,expires=1616994188,headers="(created) (expires) digest",signature="fvAGjh/gueyyoKm7iVUWWUIPMWw90AX4wDhYclCLWML7LrVfkZlCn0JkLS7MvXT34wcgwXS/MtGgZbuUJaE0Bw=="',
    },
  ];
  requestMap.forEach((elem) => {
    var payload = JSON.stringify(formRequest(elem.transaction_id));
    var params = {
      headers: {
        "Content-Type": "application/json",
        Authorization: elem.signature,
        "Proxy-Authorization": elem.signature,
      },
    };
    let res = http.post(url, payload, params);
    check(res, { "status was 200": (r) => r.status == 200 });
    sleep(1);
  });
}
