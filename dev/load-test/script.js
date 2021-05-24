import http from "k6/http";
import { check } from "k6";
import { SharedArray } from "k6/data";

// Data shared across VUs
const data = new SharedArray("Request data", function () {
  return JSON.parse(open(__ENV.FILE_PATH))
});
const url = __ENV.LOAD_TEST_URL + "/search";

// Keeping max VUs to 10
const maxVUs = 10

// Calculate total req that can be processed without collisions
let nReq
if (__ENV.N_REQ <= maxVUs || __ENV.N_REQ % maxVUs == 0) {
  nReq = __ENV.N_REQ
} else {
  const r = __ENV.N_REQ % maxVUs
  nReq = __ENV.N_REQ - r
}

const nVUs = Math.min(maxVUs, nReq)

const maxIterPerVu = Math.floor(nReq / nVUs)

function toRequest(elem, url) {
  const params = {
    headers: {
      "Content-Type": "application/json",
      Authorization: elem.signature,
      "Proxy-Authorization": elem.signature,
    },
  };
  return ["POST", url, elem.rawRequest, params];
}

export let options = {
  discardResponseBodies: true,
  scenarios: {
    contacts: {
      executor: 'shared-iterations',
      vus: nVUs,
      iterations: nReq
    },
  },
};

export default function () {
  const index = ((__VU - 1) * maxIterPerVu) + __ITER
  console.log(`VU=${__VU} ITER=${__ITER} index=${index}`)
  const dataV = toRequest(data[index], url)
  const response = http.request(...dataV);

  check(response, { "status was 200": (r) => r.status == 200 })
}
