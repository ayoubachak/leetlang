import os
import fnmatch

def list_files(directory, output_file, ignore_patterns=[]):
    """List files in the specified directory, filter them, and write their contents to an output file."""
    # File patterns to include
    include_patterns = ['*.c', '*.h']
    
    # Prepare to write to the output file
    with open(output_file, 'w', encoding='utf-8') as output:
        # Collect all files that match the include patterns and do not match the ignore patterns
        files_to_print = []
        for root, _, files in os.walk(directory):
            for pattern in include_patterns:
                for filename in fnmatch.filter(files, pattern):
                    if not any(fnmatch.fnmatch(filename, ignore_pattern) for ignore_pattern in ignore_patterns):
                        files_to_print.append(os.path.join(root, filename))

        # Write each file and its contents to the output file
        for filepath in sorted(files_to_print):
            output.write(f"## {os.path.basename(filepath)} :\n")
            output.write("```\n")
            with open(filepath, 'r', encoding='utf-8') as file:
                output.write(file.read())
            output.write("\n```\n\n")  # Extra newline for better separation in output

# Example usage
if __name__ == "__main__":
    current_directory = os.getcwd()  # Get the current working directory
    output_path = os.path.join(current_directory, 'output.md')  # Path for the output file
    ignore_list = ['common.h', 'debug.c', 'debug.h','main.c', 'makerfile','.gitignore', '*.exe']  # Define patterns to ignore
    list_files(current_directory, output_path, ignore_list)
