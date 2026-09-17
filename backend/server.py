#!/usr/bin/env python3
"""NORMA 3 server entry point: python3 backend/server.py --help"""

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from norma.main import main  # noqa: E402

if __name__ == "__main__":
    main()
