import os
import shutil
import zipfile
import subprocess
import argparse
from typing import Optional


def zip_directory_with_max_compression(
    folder_path: str, zip_path: str
) -> Optional[str]:
    """
    Compress a directory into a zip file with maximum compression.

    Args:
        folder_path: Path to the directory to compress
        zip_path: Path where the zip file should be created

    Returns:
        str: Path to the created zip file, or None if operation failed
    """
    try:
        # Ensure the output directory exists
        os.makedirs(os.path.dirname(zip_path), exist_ok=True)

        if not os.path.exists(folder_path):
            print(f"Warning: Source directory {folder_path} does not exist")
            return None

        # Use ZIP_DEFLATED with maximum compression level (9)
        with zipfile.ZipFile(
            zip_path,
            "w",
            compression=zipfile.ZIP_DEFLATED,
            compresslevel=9,  # Maximum compression level
            allowZip64=True,
        ) as zipf:
            if not os.path.exists(folder_path):
                print(f"Warning: {folder_path} does not exist")
                return None

            # Track original and compressed sizes
            total_original_size = 0
            total_compressed_size = 0
            files_processed = 0

            for root, _, files in os.walk(folder_path):
                for file in files:
                    try:
                        file_path = os.path.join(root, file)
                        arcname = os.path.relpath(file_path, folder_path)

                        # Get original file size
                        original_size = os.path.getsize(file_path)
                        total_original_size += original_size

                        # Add file to zip
                        zipf.write(file_path, arcname)

                        # Get info about the compressed file
                        zip_info = zipf.getinfo(arcname)
                        compressed_size = zip_info.compress_size
                        total_compressed_size += compressed_size

                        files_processed += 1

                    except Exception as e:
                        print(
                            f"Warning: Failed to add file {file_path} to zip: {str(e)}"
                        )

            if files_processed == 0:
                print(f"Warning: No files found in {folder_path}")
                return None

            # Calculate and display compression statistics
            if total_original_size > 0:
                compression_ratio = (
                    1 - (total_compressed_size / total_original_size)
                ) * 100
                print(f"\nCompression Statistics:")
                print(f"Files processed: {files_processed}")
                print(f"Original size: {total_original_size / 1024:.2f} KB")
                print(f"Compressed size: {total_compressed_size / 1024:.2f} KB")
                print(f"Compression ratio: {compression_ratio:.1f}%")

        print(f"Successfully created zip file at {zip_path}")
        return zip_path

    except Exception as e:
        print(f"Error creating zip file: {str(e)}")
        return None
    finally:
        try:
            if os.path.exists(folder_path):
                shutil.rmtree(folder_path)
                print(f"Cleaned up source directory: {folder_path}")
        except Exception as e:
            print(f"Warning: Failed to clean up directory {folder_path}: {str(e)}")


def main():
    parser = argparse.ArgumentParser(
        description="Compress fdep directory into a zip file with maximum compression"
    )
    parser.add_argument(
        "--input-dir",
        default="./tmp/fdep",
        help="Input directory to compress (default: ./tmp/fdep)",
    )
    parser.add_argument(
        "--output-path",
        required=True,
        help="Path where the output zip file should be written",
    )

    args = parser.parse_args()

    # Ensure input directory exists
    if not os.path.exists(args.input_dir):
        print(f"Error: Input directory {args.input_dir} does not exist")
        return 1

    result = zip_directory_with_max_compression(args.input_dir, args.output_path)

    if result is None:
        print("Failed to create zip file")
        return 1

    return 0


if __name__ == "__main__":
    exit(main())
