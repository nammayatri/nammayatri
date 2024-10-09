import re

def convert_string(input_text):
    # Split lines and process each one
    lines = input_text.strip().split('\n')
    
    converted_lines = []
    for line in lines:
        # Match patterns like STRING_NAME arg1 arg2 ... -> "text"
        match = re.match(r'(\S+)\s*((?:\w+\s*)*)->\s*"(.*)"', line.strip())
        
        if match:
            string_name = match.group(1).lower()  # Convert to lowercase
            args = match.group(2).strip()  # Capture the arguments, if any
            text = match.group(3)

            if args:
                # Format the line with all arguments in lambda-like structure
                args_list = args.split()
                args_str = ' '.join(args_list)  # List of arguments in the function signature
                lambda_args = '\\' + ' '.join(args_list)
                formatted_line = f', {string_name} {args_str} : ({lambda_args} -> "{text}")'
            else:
                # No arguments case
                formatted_line = f', {string_name} : "{text}"'
            
            converted_lines.append(formatted_line)
    
    # Join all the converted lines
    return '\n'.join(converted_lines)

def process_file(input_file, output_file):
    with open(input_file, 'r') as infile:
        input_text = infile.read()

    # Convert the input text
    converted_text = convert_string(input_text)

    # Write the converted text to the output file
    with open(output_file, 'w') as outfile:
        outfile.write(converted_text)

# Example usage
input_file = 'input.txt'  # Replace with your input file path
output_file = 'output.txt'  # Replace with your desired output file path
process_file(input_file, output_file)

print(f"Converted content written to {output_file}")
