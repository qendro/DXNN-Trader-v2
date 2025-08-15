#!/bin/bash
# Backup original ib_connector.erl before replacement - TEMPORARY FILE

echo "=== Backing up original ib_connector.erl ==="

# Create backup with timestamp
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_FILE="ib_connector_original_${TIMESTAMP}.erl"

if [ -f "ib_connector.erl" ]; then
    cp ib_connector.erl "delete/${BACKUP_FILE}"
    echo "✓ Original ib_connector.erl backed up to delete/${BACKUP_FILE}"
    
    # Show file sizes for comparison
    echo ""
    echo "File size comparison:"
    echo "Original: $(wc -l < ib_connector.erl) lines"
    echo "Bridge:   $(wc -l < ib_bridge_connector.erl) lines"
    
    echo ""
    echo "To replace original with bridge:"
    echo "  mv ib_connector.erl delete/ib_connector_original.erl"
    echo "  cp ib_bridge_connector.erl ib_connector.erl"
    echo ""
    echo "To restore original:"
    echo "  cp delete/${BACKUP_FILE} ib_connector.erl"
    
else
    echo "✗ ib_connector.erl not found"
    exit 1
fi