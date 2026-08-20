#!/usr/bin/env python3
"""
Apply the Algeria (+213) patches to a checkout of the Namma Yatri backend.

    ./apply-patches.py <source-root>

<source-root> is the directory that contains Backend/ — i.e. the root of a
checkout of nammayatri at the pinned 2023 baseline
(03a753113af1fdcddf3378d9dc2fc31170e385e4).

Upstream hard-codes the Indian dial code in five places. Every one of them
rejects, or silently fails to find, an Algerian number. We replace +91 with
+213 rather than accepting any country code: a permissive check would let
anyone in the world trigger an OTP SMS, which is how SMS-pumping fraud works.
Widening it later is a one-line change.

The script is idempotent — running it twice is a no-op — and it fails loudly
rather than silently skipping a site, because a missed patch produces a binary
that looks fine and still rejects +213 at runtime.
"""

import sys
from pathlib import Path

BACKEND = "Backend"
RIDER = f"{BACKEND}/app/rider-platform/rider-app/Main/src/Domain/Action/UI"
DRIVER = f"{BACKEND}/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action"

# (path, documented line number, old text, new text, note)
#
# The two Registration.hs sites differ in how they name the Regex type, and it
# is not cosmetic:
#
#   rider  Registration.hs:50  import Kernel.Types.Predicate            (unqualified)
#   driver Registration.hs:45  import qualified Kernel.Types.Predicate as P
#
# `Regex` is a type synonym (`type Regex = RE Char`) exported by
# Kernel.Types.Predicate, so it is in scope bare in the rider file and only as
# P.Regex in the driver file. Getting this wrong is a compile error four hours
# into the build. Neither site needs a new import.
PATCHES = [
    (
        f"{RIDER}/Registration.hs",
        81,
        'validateField "mobileCountryCode" mobileCountryCode P.mobileIndianCode',
        'validateField "mobileCountryCode" mobileCountryCode ("+213" :: Regex)',
        "rider: POST /v2/auth country-code validation",
    ),
    (
        f"{DRIVER}/UI/Registration.hs",
        76,
        'validateField "mobileCountryCode" mobileCountryCode P.mobileIndianCode',
        'validateField "mobileCountryCode" mobileCountryCode ("+213" :: P.Regex)',
        "driver: driver login country-code validation",
    ),
    (
        f"{DRIVER}/Dashboard/Driver.hs",
        301,
        'mobileIndianCode = "+91"',
        'mobileIndianCode = "+213"',
        "driver: dashboard driver lookup default country code",
    ),
    (
        f"{DRIVER}/UI/Call.hs",
        61,
        'QPerson.findByMobileNumber "+91" mobileNumberHash',
        'QPerson.findByMobileNumber "+213" mobileNumberHash',
        "driver: Exotel inbound-call driver lookup",
    ),
    (
        f"{DRIVER}/UI/DriverOnboarding/Image.hs",
        189,
        'Person.findByMobileNumber "+91" mobileNumberHash',
        'Person.findByMobileNumber "+213" mobileNumberHash',
        "driver: onboarding document lookup by phone",
    ),
]


def main() -> int:
    if len(sys.argv) != 2:
        print(__doc__.strip(), file=sys.stderr)
        return 2

    root = Path(sys.argv[1]).resolve()
    if not (root / BACKEND / "stack.yaml").is_file():
        fail(f"{root} does not look like a Namma Yatri checkout "
             f"({BACKEND}/stack.yaml is missing).")

    applied, already = 0, 0
    for rel, want_line, old, new, note in PATCHES:
        path = root / rel
        if not path.is_file():
            fail(f"missing file: {rel}\n"
                 f"  The source ref is probably not the pinned 2023 baseline.")

        text = path.read_text(encoding="utf-8")

        if new in text:
            print(f"  = {rel}  (already patched)")
            already += 1
            continue

        n = text.count(old)
        if n == 0:
            fail(f"{rel}: could not find the text to patch.\n"
                 f"  looking for: {old}\n"
                 f"  This site moved or changed upstream. Re-check the patch "
                 f"list against the source ref before building — a build that "
                 f"skips a site still rejects +213 at runtime.")
        if n > 1:
            fail(f"{rel}: found {n} occurrences of the text to patch, "
                 f"expected exactly 1. Refusing to guess.")

        # Line number is a sanity check only. Drift is a warning, not an
        # error: the content match above is what actually matters.
        got_line = text[: text.index(old)].count("\n") + 1
        if got_line != want_line:
            print(f"  ! {rel}: expected line {want_line}, found line "
                  f"{got_line} — content matched, continuing")

        path.write_text(text.replace(old, new), encoding="utf-8")
        print(f"  + {rel}:{got_line}  {note}")
        applied += 1

    print(f"\nAlgeria patches: {applied} applied, {already} already in place, "
          f"{len(PATCHES)} total")

    # Belt and braces: prove no +91 survives in the five patched files.
    for rel, *_ in PATCHES:
        text = (root / rel).read_text(encoding="utf-8")
        for i, line in enumerate(text.splitlines(), 1):
            if '"+91"' in line or "P.mobileIndianCode" in line:
                fail(f"{rel}:{i} still hard-codes +91 after patching:\n"
                     f"  {line.strip()}")

    return 0


def fail(msg: str):
    print(f"\nFAILED: {msg}", file=sys.stderr)
    sys.exit(1)


if __name__ == "__main__":
    sys.exit(main())
