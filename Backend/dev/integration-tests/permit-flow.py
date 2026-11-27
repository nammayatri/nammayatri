#!/usr/bin/env python3
"""End-to-end checks of the permit APIs against a locally running driver app.

Citizen checks need only the driver app and a 'Guwahati' operating city for the merchant.
Dashboard checks also run when PERMIT_REVIEWER_TOKEN, PERMIT_DC_TOKEN and
PERMIT_CHECKPOINT_TOKEN hold dashboard session tokens for that city.

A freshly submitted trip is at least 7 days away, so a live scan cannot turn green.
Green and amber timing are not covered here.

Local settings use a fixed test OTP (useFakeSms), so no SMS is sent.
"""
import base64
import datetime
import json
import os
import random
import sys
import urllib.error
import urllib.parse
import urllib.request

BASE = os.environ.get("DRIVER_APP_URL", "http://localhost:8116")
MERCHANT = os.environ.get("PERMIT_MERCHANT", "NAMMA_YATRI_PARTNER")
CITY = "Guwahati"
OTP = os.environ.get("PERMIT_TEST_OTP", "7891")

PNG = base64.b64encode(
    bytes.fromhex(
        "89504e470d0a1a0a0000000d4948445200000001000000010806000000"
        "1f15c4890000000d49444154789c6360000002000100ffff030000060005"
        "57bfabd40000000049454e44ae426082"
    )
).decode()
PDF = base64.b64encode(b"%PDF-1.4 not a photo").decode()

results = []


def check(name, condition, info=""):
    results.append(bool(condition))
    print(("PASS  " if condition else "FAIL  ") + name + ("" if condition else "   -> " + str(info)[:300]))
    return condition


def call(method, path, body=None, headers=None):
    request = urllib.request.Request(
        BASE + path,
        data=None if body is None else json.dumps(body).encode(),
        method=method,
        headers={"Content-Type": "application/json", **(headers or {})},
    )
    try:
        with urllib.request.urlopen(request, timeout=60) as response:
            raw = response.read().decode()
            return response.status, json.loads(raw) if raw else None
    except urllib.error.HTTPError as error:
        raw = error.read().decode()
        try:
            return error.code, json.loads(raw)
        except ValueError:
            return error.code, raw


def enum(value):
    """Fixed-value URL parameters are JSON on this platform, quotes included."""
    return urllib.parse.quote(json.dumps(value))


PICNIC_TYPE = enum("PICNIC")


def mobile():
    return "9" + "".join(random.choice("0123456789") for _ in range(9))


def sign_in(number):
    body = {"merchantShortId": MERCHANT, "city": CITY, "mobileCountryCode": "+91", "mobileNumber": number}
    status, response = call("POST", "/ui/permit/auth/otp", body)
    if status != 200:
        sys.exit("cannot request a sign-in OTP: %s %s" % (status, response))
    status, response = call("POST", "/ui/permit/auth/verify", {**body, "otp": OTP})
    if status != 200:
        sys.exit("cannot sign in: %s %s" % (status, response))
    return {"x-permit-token": response["token"]}


def picnic(travel_date, registration, licence, drive_minutes=40):
    return {
        "picnic": {
            "leaderName": "Test Leader",
            "travelDate": travel_date,
            "departureTime": "07:30",
            "returnTime": "15:30",
            "startPoint": "Ganeshguri",
            "destinationDistrict": "Kamrup",
            "spot": "Test picnic spot",
            "routeOut": "Ganeshguri to the spot by the main road",
            "routeBack": "The same road back",
            "driveMinutes": drive_minutes,
            "vehicle": {
                "registrationNo": registration,
                "category": "SUV_VAN",
                "usage": "PRIVATE_USE",
                "seatingCapacity": 7,
                "ownerName": "Test Owner",
                "ownerMobileNumber": mobile(),
                "fitnessRequired": False,
                "permitRequired": False,
                "speedLimiterRequired": False,
            },
            "challans": {"onRecord": 0, "unpaid": 0, "unpaidAmount": 0},
            "driver": {
                "name": "Test Driver",
                "mobileNumber": mobile(),
                "licenceNo": licence,
                "licenceClass": "LMV",
                "licenceClassCovers": True,
                "suspended": False,
                "recentConviction": False,
                "medicallyFit": True,
                "knowsRoute": True,
            },
            "handymanName": None,
            "passengerNames": ["Test Leader", "Passenger Two", "Passenger Three"],
        }
    }


def upload(headers, application_id, document_type, valid_until, content=PNG):
    return call(
        "POST",
        "/ui/permit/application/%s/document" % application_id,
        {"documentType": document_type, "imageBase64": content, "contentType": "image/png", "validUntil": valid_until, "lifetime": False},
        headers,
    )


def create(headers, details):
    return call("POST", "/ui/permit/application", {"permitType": "PICNIC", "details": details}, headers)


def upload_all(headers, application_id, required, valid_until):
    return all(upload(headers, application_id, kind, valid_until)[0] == 200 for kind in required)


def otps(headers, application_id):
    return call("POST", "/ui/permit/application/%s/undertakingOtp" % application_id, None, headers)


def submit(headers, application_id, otp=OTP):
    return call(
        "POST",
        "/ui/permit/application/%s/submit" % application_id,
        {"applicantOtp": otp, "ownerOtp": otp, "acceptUndertaking": True, "ownerCertified": True, "dataConsent": True},
        headers,
    )


def ready_application(headers, details, valid_until):
    status, created = create(headers, details)
    if status != 200:
        return None, (status, created)
    application_id = created["applicationId"]
    _, mine = call("GET", "/ui/permit/application/%s" % application_id, headers=headers)
    upload_all(headers, application_id, mine["requiredDocuments"], valid_until)
    return created, None


def citizen_checks():
    travel = (datetime.date.today() + datetime.timedelta(days=10)).isoformat()
    valid_until = (datetime.date.today() + datetime.timedelta(days=400)).isoformat()
    run = "".join(random.choice("0123456789") for _ in range(4))
    registration, licence = "AS01TZ" + run, "AS01 2020 000" + run

    body = {"merchantShortId": MERCHANT, "city": "Bangalore", "mobileCountryCode": "+91", "mobileNumber": mobile()}
    status, response = call("POST", "/ui/permit/auth/otp", body)
    check("a city other than the pilot city is refused", status == 400, (status, response))
    status, response = call("POST", "/ui/permit/auth/otp", {**body, "city": CITY, "mobileNumber": "123"})
    check("a malformed mobile number is refused", status == 400, (status, response))

    number = mobile()
    login = {"merchantShortId": MERCHANT, "city": CITY, "mobileCountryCode": "+91", "mobileNumber": number}
    call("POST", "/ui/permit/auth/otp", login)
    status, response = call("POST", "/ui/permit/auth/verify", {**login, "otp": "0000"})
    check("a wrong sign-in OTP is refused", status == 400, (status, response))
    citizen = sign_in(number)
    other = sign_in(mobile())

    status, response = call("GET", "/ui/driver/profile", headers={"token": citizen["x-permit-token"]})
    check("a citizen token is refused by a driver API", status == 401, (status, response))
    status, response = call("GET", "/ui/permit/application/list?permitType=" + PICNIC_TYPE, headers={"x-permit-token": "not-a-token"})
    check("an unknown permit token is refused", status in (400, 401), (status, response))

    status, limits = call("GET", "/ui/permit/limits?permitType=%s&travelDate=%s" % (PICNIC_TYPE, travel), headers=citizen)
    week_ahead = (datetime.datetime.utcnow() + datetime.timedelta(hours=5, minutes=30, days=7)).date().isoformat()
    day_after = (datetime.date.fromisoformat(week_ahead) + datetime.timedelta(days=1)).isoformat()
    check("the limits give the earliest travel date and that day's return deadline",
          status == 200 and limits["picnic"]["earliestTravelDate"] in (week_ahead, day_after) and len(limits["picnic"]["latestReturnTime"]) == 5, (status, limits))
    status, response = call("GET", "/ui/permit/limits?permitType=" + PICNIC_TYPE, headers={"x-permit-token": "not-a-token"})
    check("the limits need a signed-in citizen", status == 401, (status, response))

    bad = picnic(travel, registration, licence)
    bad["picnic"]["departureTime"] = "7:30"
    status, response = create(citizen, bad)
    check("a malformed time is refused when the draft is saved", status == 400, (status, response))

    details = picnic(travel, registration, licence)
    status, created = create(citizen, details)
    if not check("a draft is created", status == 200, (status, created)):
        return None
    application_id = created["applicationId"]

    status, response = call("GET", "/ui/permit/application/%s" % application_id, headers=other)
    check("another citizen cannot read the application", status == 400, (status, response))

    status, response = otps(citizen, application_id)
    codes = [f["code"] for f in (response or {}).get("ruleFailures", [])] if status == 200 else []
    check("missing photos block the undertaking OTPs", status == 200 and not response["otpSent"] and "DOCUMENT_MISSING" in codes, (status, response))

    status, fresh = call("GET", "/ui/permit/application/%s" % application_id, headers=citizen)
    check("a draft already says what stops submission", status == 200 and "DOCUMENT_MISSING" in [x["code"] for x in fresh["ruleFailures"]], (status, fresh))
    status, response = upload(citizen, application_id, "RC", valid_until, PDF)
    check("a PDF is refused even when labelled as an image", status == 400, (status, response))
    status, response = upload(citizen, application_id, "RC", valid_until, base64.b64encode(b"x" * (5 * 1024 * 1024 + 10)).decode())
    check("a photo over 5 MB is refused", status in (400, 413), (status, str(response)[:200]))
    status, response = upload(other, application_id, "RC", valid_until)
    check("another citizen cannot upload to the application", status == 400, (status, response))

    status, mine = call("GET", "/ui/permit/application/%s" % application_id, headers=citizen)
    required = mine["requiredDocuments"] if status == 200 else []
    check("all required photos upload", required and upload_all(citizen, application_id, required, valid_until), (status, mine))

    def uploaded():
        _, current = call("GET", "/ui/permit/application/%s" % application_id, headers=citizen)
        return sorted(d["documentType"] for d in current["documents"])

    def save(changed_details):
        return call("PUT", "/ui/permit/application/%s" % application_id, {"details": changed_details}, citizen)[0]

    respelled = json.loads(json.dumps(details))
    respelled["picnic"]["vehicle"]["registrationNo"] = " " + registration.lower()
    check("respelling the same registration keeps the photos", save(respelled) == 200 and uploaded() == sorted(required), uploaded())
    new_driver = json.loads(json.dumps(details))
    new_driver["picnic"]["driver"]["licenceNo"] = "AS0155" + run
    check("a new driver removes only the licence photo", save(new_driver) == 200 and uploaded() == sorted(set(required) - {"DRIVING_LICENCE"}), uploaded())
    new_vehicle = json.loads(json.dumps(new_driver))
    new_vehicle["picnic"]["vehicle"]["registrationNo"] = "AS03TZ" + run
    check("a new vehicle removes the vehicle photos", save(new_vehicle) == 200 and uploaded() == [], uploaded())
    check("the original details and photos can be restored", save(details) == 200 and upload_all(citizen, application_id, required, valid_until), uploaded())

    status, response = otps(citizen, application_id)
    check("the undertaking OTPs are sent for a complete application", status == 200 and response["otpSent"], (status, response))
    changed = json.loads(json.dumps(details))
    changed["picnic"]["spot"] = "A different spot"
    status, response = call("PUT", "/ui/permit/application/%s" % application_id, {"details": changed}, citizen)
    check("a draft can be edited", status == 200, (status, response))
    status, response = submit(citizen, application_id)
    check("an edit invalidates the OTPs already sent", status == 400, (status, response))

    otps(citizen, application_id)
    status, response = submit(citizen, application_id, "0000")
    check("wrong undertaking OTPs are refused", status == 400, (status, response))
    status, response = submit(citizen, application_id)
    if not check("the application is submitted", status == 200, (status, response)):
        return None

    status, response = call("PUT", "/ui/permit/application/%s" % application_id, {"details": details}, citizen)
    check("a submitted application cannot be edited", status == 400, (status, response))
    status, response = call("GET", "/ui/permit/application/%s/pass" % application_id, headers=citizen)
    check("there is no pass before approval", status == 400, (status, response))

    same_vehicle = picnic(travel, " as-01 tz " + run, "AS0199" + run)
    duplicate, error = ready_application(other, same_vehicle, valid_until)
    status, response = otps(other, duplicate["applicationId"]) if duplicate else error
    codes = [f["code"] for f in (response or {}).get("ruleFailures", [])] if status == 200 else []
    check("the same vehicle on the same date is refused, however it is spelled", "DUPLICATE" in codes, (status, response))

    far, error = ready_application(citizen, picnic(travel, "AS02TZ" + run, "AS0277" + run, drive_minutes=75), valid_until)
    if far:
        otps(citizen, far["applicationId"])
        submit(citizen, far["applicationId"])
    check("a 75 minute trip is submitted", far is not None, error)

    status, listing = call("GET", "/ui/permit/application/list?permitType=" + PICNIC_TYPE, headers=citizen)
    check("the citizen sees their own applications", status == 200 and len(listing["applications"]) == 2, (status, listing))

    return {"citizen": citizen, "standard": created, "dc": far}


def dashboard_checks(state):
    tokens = {role: os.environ.get("PERMIT_%s_TOKEN" % role) for role in ("REVIEWER", "DC", "CHECKPOINT")}
    if not all(tokens.values()):
        print("SKIP  dashboard checks: set PERMIT_REVIEWER_TOKEN, PERMIT_DC_TOKEN and PERMIT_CHECKPOINT_TOKEN")
        return
    root = "/direct-dashboard/%s/%s/permit" % (MERCHANT, CITY)
    reviewer, dc, police = ({"token": tokens[r]} for r in ("REVIEWER", "DC", "CHECKPOINT"))
    standard, far = state["standard"]["applicationId"], state["dc"]["applicationId"]
    approve = {"approve": True, "reason": "Details match the photos"}

    status, listing = call("GET", root + "/application/list?permitType=%s&status=%s&limit=50" % (PICNIC_TYPE, enum("SUBMITTED")), headers=reviewer)
    check("the reviewer sees the queue", status == 200 and any(a["applicationId"] == standard for a in listing["applications"]), (status, listing))
    status, details = call("GET", root + "/application/" + standard, headers=reviewer)
    check("the reviewer sees the details with no failed rules", status == 200 and details["ruleFailures"] == [], (status, details))
    status, photo = call("GET", root + "/application/%s/document/%s" % (standard, enum("RC")), headers=reviewer)
    check("the reviewer sees a photo", status == 200 and photo["imageBase64"] == PNG, (status, str(photo)[:200]))

    status, response = call("GET", root + "/application/" + standard, headers=police)
    check("a checkpoint user cannot open an application", status in (401, 403), (status, response))
    status, response = call("GET", root + "/application/%s/document/%s" % (standard, enum("RC")), headers=police)
    check("a checkpoint user cannot open a photo", status in (401, 403), (status, str(response)[:200]))
    status, response = call("POST", root + "/application/%s/decision" % standard, approve, police)
    check("a checkpoint user cannot decide", status in (401, 403), (status, response))

    status, response = call("POST", root + "/application/%s/decision" % far, approve, reviewer)
    check("a reviewer cannot decide a DC trip with the standard decision", status == 400, (status, response))
    status, response = call("POST", root + "/application/%s/dcDecision" % far, approve, reviewer)
    check("a reviewer cannot call the DC decision", status in (401, 403), (status, response))
    status, response = call("POST", root + "/application/%s/decision" % far, approve, dc)
    check("even the DC cannot decide a DC trip with the standard decision", status == 400, (status, response))
    status, response = call("POST", root + "/application/%s/dcDecision" % far, approve, dc)
    check("the DC approves the DC trip", status == 200, (status, response))

    status, response = call("POST", root + "/application/%s/decision" % standard, {"approve": True, "reason": "  "}, reviewer)
    check("a decision needs a reason", status == 400, (status, response))
    status, response = call("POST", root + "/application/%s/decision" % standard, approve, reviewer)
    check("the reviewer approves the standard trip", status == 200, (status, response))
    status, response = call("POST", root + "/application/%s/decision" % standard, approve, reviewer)
    check("an application cannot be decided twice", status == 400, (status, response))

    status, pass_info = call("GET", "/ui/permit/application/%s/pass" % standard, headers=state["citizen"])
    if not check("the citizen gets the pass", status == 200 and pass_info["qrText"].startswith("PMT1:"), (status, pass_info)):
        return
    status, scan = call("POST", root + "/pass/verify", {"qrText": pass_info["qrText"]}, police)
    codes = [r["code"] for r in scan["reasons"]] if status == 200 else []
    check("a pass for a later date scans red", status == 200 and scan["result"] == "RED" and "WRONG_DATE" in codes, (status, scan))
    status, scan = call("POST", root + "/pass/verify", {"referenceNo": state["standard"]["referenceNo"].lower()}, police)
    codes = [r["code"] for r in scan["reasons"]] if status == 200 else []
    check("lookup by reference number says the QR was not scanned", status == 200 and "MANUAL_LOOKUP" in codes, (status, scan))
    status, scan = call("POST", root + "/pass/verify", {"qrText": "PMT1:made-up"}, police)
    check("an unknown code scans red with no details", status == 200 and scan["result"] == "RED" and scan.get("pass") is None, (status, scan))

    status, response = call("POST", "/ui/permit/application/%s/withdraw" % standard, None, state["citizen"])
    check("the citizen withdraws the approved pass", status == 200, (status, response))
    status, scan = call("POST", root + "/pass/verify", {"qrText": pass_info["qrText"]}, police)
    codes = [r["code"] for r in scan["reasons"]] if status == 200 else []
    check("a withdrawn pass scans red", status == 200 and scan["result"] == "RED" and "NOT_APPROVED" in codes, (status, scan))

    status, response = call("GET", "/direct-dashboard/%s/Bangalore/permit/application/%s" % (MERCHANT, far), headers=reviewer)
    check("the application cannot be reached through another city's path", status in (400, 401, 403), (status, response))


if __name__ == "__main__":
    state = citizen_checks()
    if state and state["dc"]:
        dashboard_checks(state)
    print("\n%d passed, %d failed" % (sum(results), len(results) - sum(results)))
    sys.exit(0 if results and all(results) else 1)
