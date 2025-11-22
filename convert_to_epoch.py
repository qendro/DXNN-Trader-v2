#!/usr/bin/env python3
"""
Convert existing forex data files to include epoch timestamps.
Usage: python3 convert_to_epoch.py input_file.txt output_file.txt
"""

import sys
import datetime
import time

def datetime_to_epoch(datetime_str):
    """Convert 'YYYY-MM-DD HH:MM:SS' to epoch timestamp"""
    try:
        dt = datetime.datetime.strptime(datetime_str, '%Y-%m-%d %H:%M:%S')
        # Assume UTC timezone
        dt = dt.replace(tzinfo=datetime.timezone.utc)
        return int(dt.timestamp())
    except ValueError as e:
        print(f"Error parsing datetime '{datetime_str}': {e}")
        return None

def convert_file(input_file, output_file):
    """Convert forex data file to include epoch timestamps"""
    converted_lines = 0
    error_lines = 0
    
    with open(input_file, 'r') as infile, open(output_file, 'w') as outfile:
        for line_num, line in enumerate(infile, 1):
            line = line.strip()
            if not line or line.startswith('#'):
                continue
                
            parts = line.split(',')
            if len(parts) < 6:
                print(f"Warning: Line {line_num} has insufficient columns: {line}")
                error_lines += 1
                continue
                
            datetime_str = parts[0]
            epoch = datetime_to_epoch(datetime_str)
            
            if epoch is None:
                print(f"Error: Line {line_num} has invalid datetime: {datetime_str}")
                error_lines += 1
                continue
                
            # Write: EPOCH,DATETIME,OPEN,HIGH,LOW,CLOSE,VOLUME
            new_line = f"{epoch},{line}\n"
            outfile.write(new_line)
            converted_lines += 1
            
            if converted_lines % 10000 == 0:
                print(f"Converted {converted_lines} lines...")
    
    print(f"Conversion complete:")
    print(f"  Converted: {converted_lines} lines")
    print(f"  Errors: {error_lines} lines")
    print(f"  Output: {output_file}")

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python3 convert_to_epoch.py input_file.txt output_file.txt")
        sys.exit(1)
        
    input_file = sys.argv[1]
    output_file = sys.argv[2]
    
    print(f"Converting {input_file} to {output_file}...")
    convert_file(input_file, output_file)
