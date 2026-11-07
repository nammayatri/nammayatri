#!/usr/bin/env python3
"""The README and docs/ link only to files and headings that exist (phase 7).
A thin wrapper so run-all.sh, and therefore CI, runs docs/check-links.py."""
import os, subprocess, sys
here = os.path.dirname(os.path.abspath(__file__))
sys.exit(subprocess.run([sys.executable, os.path.join(here, '..', 'docs', 'check-links.py')]).returncode)
