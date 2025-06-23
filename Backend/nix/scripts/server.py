import os
import socket
import datetime
import gc
import signal
import sys
import threading
from pathlib import Path
import traceback
import time

# Configuration
CHUNK_SIZE = 1024 * 1024  # 1MB chunks for reading
BUFFER_SIZE = 100 * 1024 * 1024  # 1MB socket buffer
CONNECTION_BACKLOG = 300
MAX_PATH_LENGTH = 10240  # 10KB max for path

# Keep GC enabled but tune it for better performance
gc.set_threshold(700, 10, 10)

# Global flag for graceful shutdown
shutdown_flag = threading.Event()


# def find_free_socket_path():
#     """Find a free socket path that doesn't exist yet"""
#     base_path = "/tmp/fdep"
#     os.makedirs(base_path, exist_ok=True)

#     # Try to find a unique socket path
#     for i in range(1000):
#         path = f"{base_path}/fdep_{i}.sock"
#         if not os.path.exists(path):
#             return path

#     # Fallback to a timestamp-based path if all else fails
#     return f"{base_path}/fdep_{int(time.time())}.sock"


def handle_client(client_socket, client_address):
    """Handle a client connection synchronously"""
    # start_time = datetime.datetime.now()
    path = None
    total_bytes = 0

    try:
        # Set socket timeout
        client_socket.settimeout(None)  # 30 second timeout

        # Phase 1: Read the path first
        path_buffer = b""

        while b"\n" not in path_buffer:
            try:
                chunk = client_socket.recv(1024)
                if not chunk:
                    # print(f"Connection closed by {client_address} before path received")
                    return
                path_buffer += chunk

            except socket.timeout:
                # print(f"Timeout reading path from {client_address}")
                return
        # Extract the path (first line)
        path = path_buffer.decode("utf-8").strip()

        # print(f"Received path request: {path} from {client_address}")

        # Send ACK for path
        try:
            client_socket.sendall(b"ACK\n")
        except (ConnectionResetError, ConnectionAbortedError, BrokenPipeError):
            # print(f"Client {client_address} disconnected before receiving path ACK")
            return

        file_dump_patterns = {
            "module_imports",
            "function_code",
            "types_code",
            "class_code",
            "instance_code",
            "fieldUsage",
            "typeUpdates",
            "types.parser.json",
            "module_apis.json",
            "function_instance_mapping.json",
            "type.typechecker.json",
        }

        is_file_dump = any(pattern in path for pattern in file_dump_patterns)

        if is_file_dump:
            # For file dumps, stream directly to file
            output_path = path[1:] if path.startswith("/") else path
            os.makedirs(os.path.dirname(output_path), exist_ok=True)

            # Write to a temporary file first
            temp_path = output_path + ".tmp"

            try:
                # Open file for writing
                with open(temp_path, "wb") as f:
                    while True:
                        try:
                            chunk = client_socket.recv(CHUNK_SIZE)
                            if not chunk:
                                break
                            f.write(chunk)
                            total_bytes += len(chunk)

                        except socket.timeout:
                            # print(f"Timeout reading data from {client_address}")
                            raise

                # Atomically move temp file to final location
                os.replace(temp_path, output_path)

                # print(
                #     f"File dump complete: {output_path} ({total_bytes / 1024 / 1024:.2f}MB)"
                # )

            except Exception as e:
                # Clean up temp file on error
                if os.path.exists(temp_path):
                    os.unlink(temp_path)
                raise e

        else:
            output_path = path[1:] if path.startswith("/") else path
            os.makedirs(os.path.dirname(output_path), exist_ok=True)
            temp_path = output_path + ".tmp"
            try:
                with open(temp_path, "wb") as f:
                    while True:
                        try:
                            chunk = client_socket.recv(CHUNK_SIZE)
                            if not chunk:
                                break
                            f.write(chunk)
                            total_bytes += len(chunk)
                        except socket.timeout:
                            # print(f"Timeout reading data from {client_address}")
                            raise

                    # Atomically replace
                    os.replace(temp_path, output_path)

            except Exception as e:
                # Clean up temp file on error
                if os.path.exists(temp_path):
                    os.unlink(temp_path)
                raise e

            # print(
            #     f"Stream processing complete: {path} ({total_bytes / 1024 / 1024:.2f}MB)"
            # )
    except Exception as e:
        # print(f"Error handling client {client_address}: {e}")
        traceback.print_exc()
        try:
            client_socket.sendall(f"ERROR: {str(e)}\n".encode())
        except:
            pass
    finally:
        try:
            client_socket.close()
        except:
            pass

        # elapsed = datetime.datetime.now() - start_time
        # if path and total_bytes > 0:
        #     print(
        #         f"Completed {path}: {total_bytes / 1024 / 1024:.2f}MB in {elapsed.total_seconds():.3f}s ({total_bytes / elapsed.total_seconds() / 1024 / 1024:.1f}MB/s)"
        #     )


def start_socket_server():
    """Start the Unix Domain Socket server"""
    socket_path = "fdep.sock"

    # Ensure socket file doesn't exist
    if os.path.exists(socket_path):
        os.unlink(socket_path)

    # Write socket path to a file
    with open("fdep_port", "w") as f:
        f.write(socket_path)

    print(f"Run the following command in your shell to set the socket path:")
    print(f"export FDEP_SOCKET_PATH={socket_path}")

    # Create Unix domain socket
    server_socket = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)

    try:
        # Set socket options
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, BUFFER_SIZE)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_SNDBUF, BUFFER_SIZE)

        # Bind and listen
        server_socket.bind(socket_path)
        server_socket.listen(CONNECTION_BACKLOG)

        # Set socket permissions
        os.chmod(socket_path, 0o777)

        print(f"Unix Domain Socket server started on {socket_path}")
        print(f"Configuration:")
        print(f"  - Chunk size: {CHUNK_SIZE / 1024 / 1024:.1f}MB")
        print(f"  - Buffer size: {BUFFER_SIZE / 1024 / 1024:.1f}MB")
        print(f"  - Connection backlog: {CONNECTION_BACKLOG}")

        # Set socket to non-blocking for periodic shutdown checks
        server_socket.settimeout(1.0)

        while not shutdown_flag.is_set():
            try:
                # Accept new connection
                client_socket, client_address = server_socket.accept()

                # Handle client in a new thread
                client_thread = threading.Thread(
                    target=handle_client,
                    args=(client_socket, client_address),
                    daemon=True,
                )
                client_thread.start()

            except socket.timeout:
                # Timeout is expected, just check shutdown flag
                continue
            except Exception as e:
                if not shutdown_flag.is_set():
                    print(f"Error accepting connection: {e}")
                    traceback.print_exc()

        print("Server shutting down...")

    except Exception as e:
        print(f"Server error: {e}")
        traceback.print_exc()
    finally:
        server_socket.close()
        if os.path.exists(socket_path):
            os.unlink(socket_path)
        if os.path.exists("server.pid"):
            os.remove("server.pid")


def signal_handler(sig, frame):
    """Handle shutdown signals"""
    print(f"\nReceived signal {sig}, shutting down gracefully...")
    shutdown_flag.set()


def main():
    """Main entry point"""
    # Setup signal handlers
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)

    try:
        start_socket_server()
    except KeyboardInterrupt:
        print("\nServer stopped by user")
    except Exception as e:
        print(f"Fatal error: {e}")
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
