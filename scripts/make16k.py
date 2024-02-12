import os

def enlarge_file_to_16kb(file_path):
    target_size = 16 * 1024  # 16 KB in bytes
    current_size = os.path.getsize(file_path)

    if current_size < target_size:
        bytes_to_add = target_size - current_size
        with open(file_path, 'ab') as file:
            file.write(b'\x00' * bytes_to_add)
        print(f"File enlarged to 16 KB by adding {bytes_to_add} bytes.")
    else:
        print("File is already 16 KB or larger.")

# Replace 'your_file_path_here' with the actual file path you want to enlarge
enlarge_file_to_16kb('bin/fmrom.prg')