#!/usr/bin/env python3
"""Talk to the HDFC CBX mock the way the shared-kernel adapter does, and self-test it.

  python3 dev/mock-servers/hdfc-cbx/client.py selftest [--base http://localhost:8080/hdfc]
  python3 dev/mock-servers/hdfc-cbx/client.py inquiry <batchnum> <filerefno> <dd/mm/yyyy>
  python3 dev/mock-servers/hdfc-cbx/client.py batchnum <filerefno> <dd/mm/yyyy>

Requests are signed with the client key and encrypted to the bank key from the kit; responses are
decrypted and verified the other way, so this exercises both directions of the envelope.
"""

import argparse
import base64
import json
import os
import random
import sys
import time
import urllib.error
import urllib.request
import uuid
from datetime import date

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from services import hdfc_cbx as jose  # noqa: E402

GROUP, USER, CLIENT, SCOPE = "MOCKGROUP", "MOCKUSER", "9999", "MOCKSCOPE"


def _keys():
    from cryptography.hazmat.primitives import serialization

    jose.ensure_keys()

    def read(name):
        with open(os.path.join(jose.KEYS_DIR, name), "rb") as f:
            return f.read()

    return (
        serialization.load_pem_private_key(read("client_priv.pem"), password=None),
        serialization.load_pem_public_key(read("bank_pub.pem")),
    )


class Client:
    def __init__(self, base):
        self.base = base.rstrip("/")
        self.priv, self.bank_pub = _keys()

    def token(self):
        req = urllib.request.Request(
            f"{self.base}/auth/oauth/v1/token?grant_type=client_credentials&scope={SCOPE}",
            data=b"",
            method="POST",
            headers={
                "Authorization": "Basic " + base64.b64encode(b"mock-key:mock-secret").decode(),
                "Content-Type": "application/x-www-form-urlencoded",
            },
        )
        with urllib.request.urlopen(req) as r:
            return json.load(r)["access_token"]

    def call(self, path, payload):
        """(status, decoded envelope or problem document)."""
        signed = jose.sign_jws(self.priv, jose._compact_json(payload))
        envelope = jose.encrypt_jwe(self.bank_pub, signed.encode())
        req = urllib.request.Request(
            f"{self.base}{path}",
            data=envelope.encode(),
            method="POST",
            headers={
                "Content-Type": "application/jose",
                "apikey": "mock-key",
                "Scope": SCOPE,
                "transactionId": uuid.uuid4().hex,
                "Authorization": f"Bearer {self.token()}",
            },
        )
        try:
            with urllib.request.urlopen(req) as r:
                status, ctype, body = r.status, r.headers.get("Content-Type", ""), r.read().decode()
        except urllib.error.HTTPError as e:
            status, ctype, body = e.code, e.headers.get("Content-Type", ""), e.read().decode()
        if "jose" in ctype:
            inner = jose.decrypt_jwe(self.priv, body).decode()
            return status, json.loads(jose.verify_jws(self.bank_pub, inner))
        return status, json.loads(body)

    def payment(self, filerefno, when, items):
        return self.call(
            "/api/v1/cbx-nodal-bulkPayment",
            {"clientcode": CLIENT, "groupid": GROUP, "iduser": USER, "nooftran": str(len(items)), "filerefno": filerefno, "trans": items},
        )

    def inquiry(self, batchnum, filerefno, when):
        return self.call("/api/v1/cbx-nodal-bulkPaymentInq", {"gcif": GROUP, "iduser": USER, "batchnum": batchnum, "reqdexctndt": when, "filerefno": filerefno})

    def batchnum(self, filerefno, when):
        return self.call("/api/v2/cbx-getBatchNo", {"gcif": GROUP, "iduser": USER, "reqdexctndt": when, "filerefno": filerefno})

    def control(self, path, payload=None):
        req = urllib.request.Request(f"{self.base}/mock/{path}", data=json.dumps(payload or {}).encode(), method="POST", headers={"Content-Type": "application/json"})
        with urllib.request.urlopen(req) as r:
            return json.load(r)


def item(ref, accno, ifsc="SBIN0000001", cdflag="N", name="Test Driver"):
    blank = {k: "" for k in ("code", "adrline", "prtlctn", "adrline1", "adrline2", "adrline3", "adrline4", "adrline5", "instrefno",
                             "payaddinfo1", "payaddinfo2", "payaddinfo3", "payaddinfo4", "payaddinfo5", "payaddinfo6", "payaddinfo7",
                             "chqnb", "micrno", "bankname", "branch", "email")}
    return {**blank, "cdflag": cdflag, "accno": accno, "amount": "100.00", "name": name, "custrefno": ref, "reqdexctndt": date.today().strftime("%d/%m/%Y"), "ifsc": ifsc}


def selftest(c):
    when = date.today().strftime("%d/%m/%Y")
    failures = []

    def check(label, cond, detail=""):
        print(("PASS " if cond else "FAIL ") + label + ("" if cond else f"  -- {detail}"))
        if not cond:
            failures.append(label)

    def rows_by_ref(resp):
        return {r["custrefno"]: r for r in resp.get("trans") or []}

    c.control("reset")
    c.control("config", {"enquireAgainFirst": True, "readyAfterSeconds": 0, "autoApprove": True, "stepsPerInquiry": 1, "echoConvertedCdflag": False})
    fref = f"{random.randint(100000, 999999)}"
    items = [
        item("NEFTOK0001", "50100001111"),
        item("INTRA00001", "50100002222", ifsc="HDFC0000123", cdflag="I"),
        item("CONVERT001", "50100003333", ifsc="HDFC0000456", cdflag="N"),
        item("IMPSOK0001", "50100004444", cdflag="M"),
        item("REJECT0001", "50100009001"),
        item("RETURN0001", "50100009002"),
        item("DEEMED0001", "50100009003"),
        item("LONGNAME01", "50100005555", name="X" * 41),
        item("BADINTRA01", "50100006666", ifsc="SBIN0000001", cdflag="I"),
    ]
    st, ack = c.payment(fref, when, items)
    check("payment accepted", st == 200 and ack.get("codstatus") == "0" and ack.get("batchnum"), ack)
    batch = ack.get("batchnum")

    st, dup = c.payment(fref, when, items[:1])
    check("same filerefno+date is a duplicate quoting the batch", dup.get("codstatus") == "1" and "duplicate" in dup.get("txtstatus", "").lower() and dup.get("batchnum") == batch, dup)

    st, bad = c.payment("12345", when, items[:1])
    check("5-digit filerefno refused", bad.get("codstatus") == "1", bad)
    st, bad = c.payment(f"{random.randint(100000, 999999)}", when, [item("X" * 21, "1")])
    check("custrefno over 20 refused", bad.get("codstatus") == "1" and "20" in bad.get("txtstatus", ""), bad)

    st, rec = c.batchnum(fref, when)
    check("getBatchNo recovers the batch", rec.get("batchnum") == batch, rec)
    st, rec = c.batchnum("999998", when)
    check("getBatchNo unknown file -> No record", rec.get("batchnum") is None and "no record" in rec.get("message", "").lower(), rec)

    st, note = c.inquiry(batch, fref, when)
    check("first hit -> 202 enquire again", st == 202 and "enquire again" in note["errors"][0]["reason"], (st, note))
    st, resp = c.inquiry(batch, fref, when)
    rows = rows_by_ref(resp)
    check("second hit -> actual rows", st == 200 and len(rows) == len(items), (st, resp))
    check("valid items start at P", rows["NEFTOK0001"]["codstatus"] == "P", rows.get("NEFTOK0001"))
    check("name > 40 rejected at once", rows["LONGNAME01"]["codstatus"] == "R", rows.get("LONGNAME01"))
    check("cdflag I to non-HDFC IFSC rejected", rows["BADINTRA01"]["codstatus"] == "R", rows.get("BADINTRA01"))

    for _ in range(4):  # 4 more answered inquiries: P -> C -> E -> E/TXSIP -> E/TXSETT
        c.inquiry(batch, fref, when)
        st, resp = c.inquiry(batch, fref, when)
    rows = rows_by_ref(resp)
    neft = rows["NEFTOK0001"]
    check("NEFT settles: E/TXSETT with UTR", neft["codstatus"] == "E" and neft["rbistatus"] == "TXSETT" and neft["refno"].startswith("HDFCN"), neft)
    intra = rows["INTRA00001"]
    check("intra-bank: E, FT number, no rbistatus", intra["codstatus"] == "E" and intra["bankrefno"].startswith("FT") and not intra["rbistatus"], intra)
    conv = rows["CONVERT001"]
    check("NEFT to HDFC IFSC runs intra-bank, echoes sent cdflag N", conv["codstatus"] == "E" and conv["bankrefno"] and not conv["rbistatus"] and conv["cdflag"] == "N", conv)
    imps = rows["IMPSOK0001"]
    check("IMPS settles with 12-digit RRN", imps["rbistatus"] == "TXSETT" and len(imps["refno"]) == 12 and imps["refno"].isdigit(), imps)
    check("...9001 rejected", rows["REJECT0001"]["codstatus"] == "R", rows["REJECT0001"])
    check("...9002 returned after debit (TXREJE + rbireason)", rows["RETURN0001"]["rbistatus"] == "TXREJE" and rows["RETURN0001"]["rbireason"], rows["RETURN0001"])
    check("...9003 deemed settled", rows["DEEMED0001"]["rbistatus"] == "TXDSETT", rows["DEEMED0001"])

    # the answer is not ready until readyAfterSeconds after the first hit
    c.control("config", {"readyAfterSeconds": 2})
    st, _ = c.inquiry(batch, fref, when)
    check("next check starts over: first hit -> 202", st == 202, st)
    st, _ = c.inquiry(batch, fref, when)
    check("second hit too soon -> 202 again (still processing)", st == 202, st)
    time.sleep(2.2)
    st, resp = c.inquiry(batch, fref, when)
    check("hit after readyAfterSeconds -> actual rows", st == 200 and resp.get("trans"), st)
    c.control("config", {"readyAfterSeconds": 0})

    st, resp = c.inquiry("MOCKCBXNOPE", fref, when)
    check("unknown batch -> NDF", resp.get("codstatus") == "NDF", resp)

    # faults
    c.control("fault", {"api": "inquiry", "kind": "ndf"})
    st, resp = c.inquiry(batch, fref, when)
    check("fault ndf", resp.get("codstatus") == "NDF", resp)
    c.control("fault", {"api": "payment", "kind": "5xx", "process": True})
    fref2 = f"{random.randint(100000, 999999)}"
    st, resp = c.payment(fref2, when, [item("FAULT00001", "50100007777")])
    check("payment 5xx (process) -> 500 problem", st == 500 and resp["errors"][0]["code"] == "TH99500", (st, resp))
    st, rec = c.batchnum(fref2, when)
    check("...but the file landed: getBatchNo finds it", rec.get("batchnum"), rec)
    c.control("fault", {"api": "payment", "kind": "accept-no-batchnum"})
    st, resp = c.payment(f"{random.randint(100000, 999999)}", when, [item("NOREF00001", "50100008888")])
    check("accept-no-batchnum", resp.get("codstatus") == "0" and not resp.get("batchnum"), resp)
    c.control("fault", {"api": "batchnum", "kind": "norecord"})
    st, rec = c.batchnum(fref, when)
    check("fault norecord", rec.get("batchnum") is None, rec)
    c.control("fault", {"api": "batchnum", "kind": "unknown"})
    st, rec = c.batchnum(fref, when)
    check("fault unknown getBatchNo: no batch, no 'no record'", rec.get("batchnum") is None and rec.get("codstatus") == "9" and "no record" not in rec.get("message", "").lower(), rec)

    # a payment fault scoped to one account hits only the file carrying it
    c.control("fault", {"api": "payment", "kind": "reject", "match": {"accno": "50100008181"}})
    st, other = c.payment(f"{random.randint(100000, 999999)}", when, [item("OTHERFILE1", "50100007171")])
    check("accno-scoped fault skips a file without that account", other.get("codstatus") == "0", other)
    st, mine = c.payment(f"{random.randint(100000, 999999)}", when, [item("MYFILE0001", "50100008181")])
    check("accno-scoped fault hits the file with that account", mine.get("codstatus") == "1", mine)

    # long names: HDFC cap them at 40 on NEFT/RTGS only
    fref7 = f"{random.randint(100000, 999999)}"
    st, ack7 = c.payment(fref7, when, [item("LONGIMPS01", "50100009191", cdflag="M", name="Y" * 45), item("LONGNEFT01", "50100009292", name="Z" * 45)])
    c.inquiry(ack7["batchnum"], fref7, when)
    st, resp7 = c.inquiry(ack7["batchnum"], fref7, when)
    rows7 = rows_by_ref(resp7)
    check("45-char name on IMPS is not rejected", rows7["LONGIMPS01"]["codstatus"] != "R", rows7.get("LONGIMPS01"))
    check("45-char name on NEFT is rejected", rows7["LONGNEFT01"]["codstatus"] == "R", rows7.get("LONGNEFT01"))

    # submit answers other than accepted / duplicate
    c.control("fault", {"api": "payment", "kind": "reject", "reason": "Debit account frozen"})
    fref4 = f"{random.randint(100000, 999999)}"
    st, resp = c.payment(fref4, when, [item("REJFILE001", "50100003434")])
    check("fault reject: codstatus 1, not a duplicate", resp.get("codstatus") == "1" and "duplicate" not in resp.get("txtstatus", "").lower(), resp)
    st, rec = c.batchnum(fref4, when)
    check("...and the rejected file was not stored", rec.get("batchnum") is None, rec)
    c.control("fault", {"api": "payment", "kind": "unknown-ack"})
    fref5 = f"{random.randint(100000, 999999)}"
    st, resp = c.payment(fref5, when, [item("UNKACK0001", "50100004545")])
    check("fault unknown-ack: codstatus 2", resp.get("codstatus") == "2", resp)
    st, rec = c.batchnum(fref5, when)
    check("...not stored without process", rec.get("batchnum") is None, rec)
    c.control("fault", {"api": "payment", "kind": "unknown-ack", "codstatus": "", "process": True})
    fref6 = f"{random.randint(100000, 999999)}"
    st, resp = c.payment(fref6, when, [item("UNKACK0002", "50100005656")])
    check("fault unknown-ack with no codstatus", resp.get("codstatus") is None, resp)
    st, rec = c.batchnum(fref6, when)
    check("...stored with process: getBatchNo finds it", rec.get("batchnum"), rec)

    # approval hold
    c.control("config", {"autoApprove": False})
    fref3 = f"{random.randint(100000, 999999)}"
    st, ack = c.payment(fref3, when, [item("HOLD000001", "50100001212")])
    held = ack["batchnum"]
    for _ in range(3):
        c.inquiry(held, fref3, when)
        st, resp = c.inquiry(held, fref3, when)
    check("unapproved batch stays at P", resp["trans"][0]["codstatus"] == "P", resp["trans"][0])
    c.control(f"batch/{held}/approve")
    c.inquiry(held, fref3, when)
    st, resp = c.inquiry(held, fref3, when)
    check("approved batch moves to C", resp["trans"][0]["codstatus"] == "C", resp["trans"][0])
    c.control("item/HOLD000001", {"codstatus": "E", "rbistatus": "TXSETT", "refno": "FORCEDUTR1"})
    c.inquiry(held, fref3, when)
    st, resp = c.inquiry(held, fref3, when)
    check("forced row is returned verbatim", resp["trans"][0]["refno"] == "FORCEDUTR1" and resp["trans"][0]["rbistatus"] == "TXSETT", resp["trans"][0])
    c.control("config", {"autoApprove": True})

    print(f"\n{'ALL PASS' if not failures else f'{len(failures)} FAILED'}")
    return 0 if not failures else 1


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--base", default=os.environ.get("HDFC_MOCK_BASE", "http://localhost:8080/hdfc"))
    sub = p.add_subparsers(dest="cmd", required=True)
    sub.add_parser("selftest")
    q = sub.add_parser("inquiry")
    q.add_argument("batchnum")
    q.add_argument("filerefno")
    q.add_argument("date")
    b = sub.add_parser("batchnum")
    b.add_argument("filerefno")
    b.add_argument("date")
    args = p.parse_args()
    c = Client(args.base)
    if args.cmd == "selftest":
        sys.exit(selftest(c))
    if args.cmd == "inquiry":
        print(json.dumps(c.inquiry(args.batchnum, args.filerefno, args.date), indent=1))
    if args.cmd == "batchnum":
        print(json.dumps(c.batchnum(args.filerefno, args.date), indent=1))


if __name__ == "__main__":
    main()
