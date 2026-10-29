#!/usr/bin/env python3
"""(Re)generate the HDFC CBX mock's TEST-ONLY key kit.

  python3 dev/mock-servers/hdfc-cbx/make_keys.py --force
  python3 dev/mock-servers/hdfc-cbx/seed_config.py        # then re-point the cities so their config carries the new keys

The kit in dev/mock-servers/hdfc-cbx/test-keys is generated on first use (by seed_config.py or the mock) and
gitignored. After regenerating it, re-run seed_config.py and restart the mock server: both must use the same kit.
Test keys only, never real ones.
"""

import argparse
import os
import shutil
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from services import hdfc_cbx  # noqa: E402


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--force", action="store_true", help="discard the existing kit and generate a new one")
    args = parser.parse_args()
    if args.force and os.path.isdir(hdfc_cbx.KEYS_DIR):
        shutil.rmtree(hdfc_cbx.KEYS_DIR)
    keys_dir = hdfc_cbx.ensure_keys()
    for name in sorted(os.listdir(keys_dir)):
        print(os.path.join(keys_dir, name))


if __name__ == "__main__":
    main()
