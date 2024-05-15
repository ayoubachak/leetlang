import os
import fnmatch

def scan_directory(directory, include_patterns, exclude_patterns):
    files_to_process = []
    for root, dirs, files in os.walk(directory):
        for file in files:
            filepath = os.path.join(root, file)
            if any(fnmatch.fnmatch(filepath, pat) for pat in include_patterns):
                if not any(fnmatch.fnmatch(filepath, pat) for pat in exclude_patterns):
                    print("filepath: ", filepath)
                    files_to_process.append(filepath)
    return files_to_process

def concatenate_files(files, output_file):
    with open(output_file, 'w') as outfile:
        outfile.write("# Code\n")
        for file in files:
            outfile.write(f"## {os.path.basename(file)}\n")
            outfile.write("```\n")
            with open(file, 'r') as infile:
                outfile.write(infile.read())
            outfile.write("\n```\n")

def main():
    directory = "."  # Replace with the directory you want to scan
    include_patterns = ["*vm.c", "*vm.h", "*compiler.c", "*compiler.h", "*object.c", "*object.h"]  # Patterns to include
    exclude_patterns = ["common.h", "debug.c", "debug.h"]  # Patterns to exclude

    files = scan_directory(directory, include_patterns, exclude_patterns)
    concatenate_files(files, "output.md")

if __name__ == "__main__":
    main()
