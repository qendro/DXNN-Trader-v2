#!/bin/bash
# Safe cleanup script for Phase 5 - TEMPORARY FILE
# Performs systematic cleanup with validation

echo "=== Phase 5: Safe Code Cleanup ==="
echo

# Step 1: Create backup
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="../DXNN_backup_${TIMESTAMP}"

echo "1. Creating backup..."
if [ ! -d "$BACKUP_DIR" ]; then
    cp -r . "$BACKUP_DIR"
    echo "   ✓ Backup created: $BACKUP_DIR"
else
    echo "   ⚠ Backup directory already exists"
fi

echo

# Step 2: Remove completely obsolete files
echo "2. Removing obsolete files..."

FILES_TO_REMOVE=(
    "ib_proto.erl"
    "ib_diag.erl" 
    "debug_tws_trust.erl"
    "test_ib_fixes.erl"
)

for file in "${FILES_TO_REMOVE[@]}"; do
    if [ -f "$file" ]; then
        mv "$file" "delete/${file}.removed"
        echo "   ✓ Moved $file to delete/ folder"
    else
        echo "   - $file not found (already removed)"
    fi
done

echo

# Step 3: Replace ib_connector.erl
echo "3. Replacing ib_connector.erl with bridge..."

if [ -f "ib_connector.erl" ]; then
    # Backup original
    mv "ib_connector.erl" "delete/ib_connector_original.erl"
    echo "   ✓ Original ib_connector.erl backed up to delete/"
    
    # Replace with bridge
    if [ -f "ib_bridge_connector.erl" ]; then
        cp "ib_bridge_connector.erl" "ib_connector.erl"
        echo "   ✓ ib_connector.erl replaced with bridge version"
    else
        echo "   ✗ ib_bridge_connector.erl not found!"
        exit 1
    fi
else
    echo "   - ib_connector.erl already replaced"
fi

echo

# Step 4: Show cleanup results
echo "4. Cleanup Results:"
echo "   Files removed: ${#FILES_TO_REMOVE[@]}"
echo "   Files replaced: 1 (ib_connector.erl)"
echo "   Backup location: $BACKUP_DIR"

echo

# Step 5: Validation check
echo "5. Validation Check:"
if [ -f "ib_connector.erl" ] && [ -f "priv/ib_service.py" ]; then
    echo "   ✓ Bridge files present"
else
    echo "   ✗ Bridge files missing!"
    exit 1
fi

echo
echo "=== Cleanup Complete ==="
echo "Next steps:"
echo "1. Test system: make:all([load])."
echo "2. Run integration test: test_phase4_integration:quick_phase4_test()."
echo "3. If issues, restore from: $BACKUP_DIR"