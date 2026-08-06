#!/usr/bin/env python3
"""Local RSF tester: plays the collector (BAP) against a locally running BPP.

  collector   start a mock collector that ACKs /on_receiver_recon and prints it
  recon       POST /receiver_recon to the BPP                      (Phase 1 + 2)
  verify      finance enters the bank amount for a UTR             (Phase 3)
  send        trigger on_receiver_recon for an inbound message     (Phase 4)

Each --order is  <bookingId>:<amount>:<utr>=<legAmount>[,<utr>=<legAmount>...]
e.g.  --order 3f2a...:1200:SREF1=700,SREF2=500
Only the standard library is used.
"""
import argparse
import json
import sys
import urllib.error
import urllib.request
import uuid
from datetime import datetime, timezone
from http.server import BaseHTTPRequestHandler, HTTPServer

DEFAULT_BPP_URL = "http://localhost:8016"
DEFAULT_MERCHANT_ID = "840327a8-f17c-4d7c-8199-a583cfaadc5f"
DEFAULT_BPP_ID = "localhost:8016/beckn/" + DEFAULT_MERCHANT_ID
DEFAULT_COLLECTOR_PORT = 8099


def now_iso():
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S.") + "%03dZ" % (datetime.now(timezone.utc).microsecond // 1000)


def post(url, body):
    req = urllib.request.Request(url, data=json.dumps(body).encode(), headers={"Content-Type": "application/json"}, method="POST")
    try:
        with urllib.request.urlopen(req) as res:
            status, text = res.status, res.read().decode()
    except urllib.error.HTTPError as err:
        status, text = err.code, err.read().decode()
    except urllib.error.URLError as err:
        sys.exit("Could not reach %s: %s" % (url, err.reason))
    print("POST %s -> %s" % (url, status))
    try:
        print(json.dumps(json.loads(text), indent=2))
    except ValueError:
        print(text)
    return status


def parse_order(spec):
    try:
        order_id, amount, legs = spec.split(":", 2)
        return order_id, amount, [(utr, float(leg)) for utr, leg in (pair.split("=") for pair in legs.split(","))]
    except ValueError:
        sys.exit("Bad --order '%s', expected <bookingId>:<amount>:<utr>=<legAmount>[,...]" % spec)


def build_order(args, spec, timestamp):
    order_id, amount, legs = parse_order(spec)
    return {
        "id": order_id,
        "invoice_no": "INV-" + order_id[:8],
        "collector_app_id": args.bap_id,
        "receiver_app_id": args.bpp_id,
        "state": "Completed",
        "provider": {"name": {"name": "Local test provider", "code": "LOCAL-1"}, "address": "Local"},
        "payment": {
            "uri": "local/upi",
            "tl_method": "http/get",
            "params": {"transaction_id": "txn-" + order_id[:8], "transaction_status": args.payment_status, "amount": amount, "currency": "INR"},
            "type": "ON-ORDER",
            "status": args.payment_status,
            "collected_by": "BAP",
            "@ondc/org/buyer_app_finder_fee_type": args.bff_type,
            "@ondc/org/buyer_app_finder_fee_amount": args.bff_amount,
            "@ondc/org/settlement_basis": "Collection",
            "@ondc/org/settlement_window": "P8D",
            "@ondc/org/settlement_details": [
                {
                    "settlement_counterparty": "buyer-app",
                    "settlement_amount": int(leg) if leg == int(leg) else leg,
                    "settlement_type": "neft",
                    "settlement_bank_account_no": "99679007677676",
                    "settlement_ifsc_code": "HDFC900008",
                    "settlement_status": "PAID",
                    "settlement_reference": utr,
                    "settlement_timestamp": timestamp,
                }
                for utr, leg in legs
            ],
        },
        "withholding_tax_gst": {"currency": "INR", "value": "0"},
        "withholding_tax_tds": {"currency": "INR", "value": "0"},
        "deduction_by_collector": {"currency": "INR", "value": "0"},
        "payerdetails": {"payer_name": "Local collector", "payer_address": "Local", "payer_account_no": 509424924294248, "payer_bank_code": "HDFC0000000", "payer_virtual_payment_address": "local@upi"},
        "settlement_reason_code": "01",
        "transaction_id": args.transaction_id,
        "settlement_id": args.settlement_id,
        "settlement_reference_no": legs[-1][0],
        "recon_status": "01",
        "order_recon_status": "01",
        "created_at": timestamp,
        "updated_at": timestamp,
    }


def cmd_recon(args):
    timestamp = now_iso()
    message_id = args.message_id or str(uuid.uuid4())
    payload = {
        "context": {
            "domain": "ONDC:NTS10",
            "country": "IND",
            "city": args.city,
            "action": "receiver_recon",
            "core_version": "1.0.0",
            "bap_id": args.bap_id,
            "bap_uri": args.bap_uri,
            "bpp_id": args.bpp_id,
            "bpp_uri": args.bpp_url,
            "transaction_id": args.transaction_id,
            "message_id": message_id,
            "timestamp": timestamp,
            "ttl": "P2D",
        },
        "message": {"orderbook": {"orders": [build_order(args, spec, timestamp) for spec in args.order]}},
    }
    if args.print:
        print(json.dumps(payload, indent=2))
    post(args.bpp_url.rstrip("/") + "/receiver_recon", payload)
    print("\nmessage_id: %s   (use it with: send --message-id %s)" % (message_id, message_id))


def cmd_verify(args):
    body = {"bankVerifiedAmount": args.amount, "verifiedBy": args.by, "reason": args.reason}
    post("%s/internal/rsf/%s/utrs/%s/bank-verify" % (args.bpp_url.rstrip("/"), args.merchant_id, args.utr), body)


def cmd_send(args):
    post("%s/internal/rsf/%s/messages/%s/send" % (args.bpp_url.rstrip("/"), args.merchant_id, args.message_id), {})


def cmd_collector(args):
    class Handler(BaseHTTPRequestHandler):
        def do_POST(self):
            raw = self.rfile.read(int(self.headers.get("Content-Length", 0))).decode()
            print("\n=== %s %s ===" % (self.command, self.path))
            try:
                print(json.dumps(json.loads(raw), indent=2))
            except ValueError:
                print(raw)
            ack = json.dumps({"message": {"ack": {"status": "NACK" if args.nack else "ACK"}}}).encode()
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(ack)))
            self.end_headers()
            self.wfile.write(ack)

        def log_message(self, *_):
            pass

    print("Mock collector on http://localhost:%d (answers %s)" % (args.port, "NACK" if args.nack else "ACK"))
    HTTPServer(("0.0.0.0", args.port), Handler).serve_forever()


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = parser.add_subparsers(dest="command", required=True)

    recon = sub.add_parser("recon", help="POST /receiver_recon")
    recon.add_argument("--order", action="append", required=True, help="<bookingId>:<amount>:<utr>=<legAmount>[,...]; repeat for more orders")
    recon.add_argument("--bpp-url", default=DEFAULT_BPP_URL)
    recon.add_argument("--bpp-id", default=DEFAULT_BPP_ID, help="must equal merchant.subscriber_id")
    recon.add_argument("--bap-id", default="local.collector")
    recon.add_argument("--bap-uri", default="http://localhost:%d" % DEFAULT_COLLECTOR_PORT, help="where on_receiver_recon will be POSTed")
    recon.add_argument("--message-id", help="default: a fresh uuid (reuse one to test the duplicate NACK)")
    recon.add_argument("--transaction-id", default="T1")
    recon.add_argument("--settlement-id", default="123123")
    recon.add_argument("--city", default="std:011")
    recon.add_argument("--bff-type", default="percent")
    recon.add_argument("--bff-amount", default="0")
    recon.add_argument("--payment-status", default="PAID")
    recon.add_argument("--print", action="store_true", help="print the request payload")
    recon.set_defaults(func=cmd_recon)

    verify = sub.add_parser("verify", help="bank-verify a UTR")
    verify.add_argument("--utr", required=True)
    verify.add_argument("--amount", required=True, type=float)
    verify.add_argument("--by", default="local-finance")
    verify.add_argument("--reason", default="local test")
    verify.add_argument("--bpp-url", default=DEFAULT_BPP_URL)
    verify.add_argument("--merchant-id", default=DEFAULT_MERCHANT_ID)
    verify.set_defaults(func=cmd_verify)

    send = sub.add_parser("send", help="send on_receiver_recon for a message")
    send.add_argument("--message-id", required=True)
    send.add_argument("--bpp-url", default=DEFAULT_BPP_URL)
    send.add_argument("--merchant-id", default=DEFAULT_MERCHANT_ID)
    send.set_defaults(func=cmd_send)

    collector = sub.add_parser("collector", help="mock collector that ACKs on_receiver_recon")
    collector.add_argument("--port", type=int, default=DEFAULT_COLLECTOR_PORT)
    collector.add_argument("--nack", action="store_true", help="answer NACK instead of ACK")
    collector.set_defaults(func=cmd_collector)

    args = parser.parse_args()
    args.func(args)


if __name__ == "__main__":
    main()
