#!/usr/bin/python

import sys

def split_16bit_file(input_filename, even_filename, odd_filename):
    with open(input_filename, 'rb') as f_in:
        full_rom_data = f_in.read()
    
    even_bytes = bytearray()
    odd_bytes = bytearray()
    
    for i in range(0, len(full_rom_data), 2):
        even_bytes.append(full_rom_data[i])
        if i + 1 < len(full_rom_data):
            odd_bytes.append(full_rom_data[i+1])

    with open(even_filename, 'wb') as f_even:
        f_even.write(even_bytes)
        
    with open(odd_filename, 'wb') as f_odd:
        f_odd.write(odd_bytes)

    print(f"Split {input_filename} into {even_filename} (even bytes) and {odd_filename} (odd bytes).")

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: python split_eprom.py <input_file> <even_output_file> <odd_output_file>")
        sys.exit(1)
    
    split_16bit_file(sys.argv[1], sys.argv[2], sys.argv[3])
