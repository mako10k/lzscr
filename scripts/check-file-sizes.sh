#!/bin/bash
# check-file-sizes.sh
# Checks for files exceeding line count thresholds
# Usage: ./scripts/check-file-sizes.sh

set -euo pipefail

# Thresholds from docs/coding-standards.md
RUST_WARN=1000
RUST_MUST_SPLIT=2000
LZSCR_WARN=300
LZSCR_MUST_SPLIT=500

RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

check_rust_files() {
    echo "Checking Rust files (*.rs)..."
    local has_issues=0
    
    while IFS= read -r file; do
        lines=$(wc -l < "$file")
        
        if [ "$lines" -ge "$RUST_MUST_SPLIT" ]; then
            echo -e "${RED}🔴 MUST SPLIT: $file ($lines lines, threshold: $RUST_MUST_SPLIT)${NC}"
            has_issues=1
        elif [ "$lines" -ge "$RUST_WARN" ]; then
            echo -e "${YELLOW}🟡 WARNING: $file ($lines lines, threshold: $RUST_WARN)${NC}"
            has_issues=1
        fi
    done < <(find crates -name "*.rs" -type f -not -path "*/target/*")
    
    return $has_issues
}

check_lzscr_files() {
    echo ""
    echo "Checking LazyScript files (*.lzscr)..."
    local has_issues=0
    
    while IFS= read -r file; do
        lines=$(wc -l < "$file")
        
        if [ "$lines" -ge "$LZSCR_MUST_SPLIT" ]; then
            echo -e "${RED}🔴 MUST SPLIT: $file ($lines lines, threshold: $LZSCR_MUST_SPLIT)${NC}"
            has_issues=1
        elif [ "$lines" -ge "$LZSCR_WARN" ]; then
            echo -e "${YELLOW}🟡 WARNING: $file ($lines lines, threshold: $LZSCR_WARN)${NC}"
            has_issues=1
        fi
    done < <(find stdlib -name "*.lzscr" -type f)
    
    return $has_issues
}

main() {
    echo "File Size Checker (based on docs/coding-standards.md)"
    echo "======================================================"
    
    local rust_status=0
    local lzscr_status=0
    
    check_rust_files || rust_status=$?
    check_lzscr_files || lzscr_status=$?
    
    echo ""
    echo "======================================================"
    
    if [ "$rust_status" -eq 0 ] && [ "$lzscr_status" -eq 0 ]; then
        echo -e "${GREEN}✅ All files within acceptable size limits${NC}"
        exit 0
    else
        echo -e "${RED}❌ Some files exceed size thresholds. See above for details.${NC}"
        echo "   Refer to docs/coding-standards.md for refactoring guidelines."
        exit 1
    fi
}

main "$@"
