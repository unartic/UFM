source_file_path = 'bin/fmrom.prg'
destination_file_path = '../romfm.bin'
position = 0x40000  # 49152 in decimal

# Open the destination file in read+write binary mode
with open(destination_file_path, 'r+b') as dest_file:
    # Seek to the specified position
    dest_file.seek(position)
    
    # Open the source file in binary read mode
    with open(source_file_path, 'rb') as source_file:
        # Read the contents of the source file
      #  source_file.read(2)  #skip header bytes
        content = source_file.read()
        
        # Write those contents into the destination file at the current position
        dest_file.write(content)

print("Content copied successfully.")
