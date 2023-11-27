require 'open-uri'

# Function to add missing strings to the existing file

def add_missing_strings(missing_strings, existing_file, string_to_append)
  File.open(existing_file, 'a') { |file| file.puts missing_strings.map { |line| "#{string_to_append}#{line}" } }
rescue StandardError => e
  puts "Error while appending missing strings: #{e.message}"
end

# Function to replace existing file content with modified strings
def replace_strings(existing_file_lines, existing_file)
  File.open(existing_file, 'w') { |file| file.puts existing_file_lines }
rescue StandardError => e
  puts "Error while replacing strings in #{existing_file}: #{e.message}"
end

# Function to update strings in the existing file
def update_strings(fetched, mapped_file)
  missing_strings = []
  existing_file = mapped_file
  fetched_file = "#{fetched}.txt"
  file_lines = File.readlines(existing_file)
  fetched_files_lines = File.readlines(fetched_file).map(&:strip)
  fetched_files_lines.each do |search_string|
    file_content = file_lines.map(&:strip)
    constructors = file_content.map { |line| line.split.first }

    if (index_numbers = constructors.each_index.select { |i| constructors[i] == search_string.split.first }).any?
      index_numbers.each do |index|
        file_lines[index] = "        #{search_string}" unless file_content[index] == search_string
      end
    else
      missing_strings << search_string
    end

  end
  replace_strings(file_lines, existing_file)
  add_missing_strings(missing_strings, existing_file, "        ")
  puts "Strings updated successfully in #{existing_file}!"
end

def update_constructors(file_path)
  missing_strings = []
  constructor_file = "CONSTRUCTORS.txt"
  file_content = File.read(constructor_file)
  modified_content = file_content.gsub("(stringForConfig)", "String")
  File.write(constructor_file, modified_content)

  file_lines = File.readlines(file_path)
  fetched_files_lines = File.readlines(constructor_file).map(&:strip)
  fetched_files_lines.each do |search_string|
    unless file_lines.any? { |line| (line.gsub("=", "|").strip) == search_string }
      missing_strings << search_string
    end
  end
  add_missing_strings(missing_strings, file_path, " ")

end


# Main function
def main(arg)
  if arg == "ui-driver"
    resource = "Resource"
    file_url = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTdhwmMEmrZZr_BUdLig1rHWMKRkbHKKHdSXmELemkTCyfxTRtNMIxhdg51O4sl3pmOCvC8Z18FtTgC/pub?gid=1997571669&single=true&output=tsv"
  else 
    resource = "Resources"
    file_url = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTdhwmMEmrZZr_BUdLig1rHWMKRkbHKKHdSXmELemkTCyfxTRtNMIxhdg51O4sl3pmOCvC8Z18FtTgC/pub?gid=0&single=true&output=tsv"
  end 
  proxy_files = []
  file_mapping = {
    ENGLISH: "EN.purs", HINDI: "HI.purs", BENGALI: "BN.purs", TAMIL: "TA.purs", TELUGU: "TE.purs", KANNADA: "KN.purs", MALAYALAM: "ML.purs", FRENCH: "FR.purs"
  }

  begin
    content = open(file_url).read
    lines = content.split("\n")
    headers = lines.first.split("\t")
    proxy_files = headers
    columns = lines[1..].map { |line| line.split("\t") }

    headers.each_with_index do |header, index|
      if header == "CONSTRUCTORS"
        File.write("./#{header}.txt", columns.map { |column| "| #{column[0]}" }.join("\n"))
      else
        column_values = columns.map { |column| "#{column[0]} -> \"#{column[index]}\"" }
        File.write("./#{header}.txt", column_values.join("\n"))
      end
    end

    puts "Files Downloaded successfully!"
    file_mapping.each { |key, value| update_strings(key, "#{arg}/src/#{resource}/Localizable/#{value}") }
    update_constructors("#{arg}/src/#{resource}/Localizable/Types.purs")

  rescue OpenURI::HTTPError => e
    puts "Error downloading file: #{e.message}"
  end

  proxy_files.each { |proxy_file| File.delete("#{proxy_file}.txt") }
end

first_arg = ARGV[0]

main( first_arg ) # calling the main function
