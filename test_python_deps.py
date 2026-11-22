#!/usr/bin/env python3
"""
Test Python dependencies for the bridge
"""

import sys
import os

def test_python_version():
    print(f"Python version: {sys.version}")
    if sys.version_info >= (3, 7):
        print("✓ Python version OK")
        return True
    else:
        print("✗ Python version too old")
        return False

def test_ib_insync():
    try:
        import ib_insync
        print(f"✓ ib_insync available: {ib_insync.__version__}")
        return True
    except ImportError:
        print("✗ ib_insync not available")
        print("Install with: pip install ib_insync>=0.9.86")
        return False

def test_other_deps():
    deps = ['asyncio', 'json', 'struct', 'time', 'logging']
    all_ok = True
    for dep in deps:
        try:
            __import__(dep)
            print(f"✓ {dep} available")
        except ImportError:
            print(f"✗ {dep} not available")
            all_ok = False
    return all_ok

if __name__ == '__main__':
    print("=== Testing Python Dependencies ===")
    
    python_ok = test_python_version()
    deps_ok = test_other_deps()
    ib_ok = test_ib_insync()
    
    if python_ok and deps_ok and ib_ok:
        print("\n✓ All dependencies OK - bridge should work")
        sys.exit(0)
    else:
        print("\n✗ Some dependencies missing")
        sys.exit(1)