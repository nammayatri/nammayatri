import asyncio
import os
import json
import websockets
import socket
import datetime

data = dict()

def find_free_port():
    with socket.socket(socket.AF_INET6, socket.SOCK_STREAM) as s:
        s.bind(('', 0))  # Bind to any available port
        s.listen(1)
        port = s.getsockname()[1]
        return port

async def handler(websocket, path):
    try:
        file_dump = False
        for i in [
            "module_imports",
            "function_code",
            "types_code",
            "class_code",
            "instance_code",
            "fieldUsage",
            "typeUpdates",
            ".types.parser.",
            ".module_apis.json"
        ]:
            if i in path:
                file_dump = True

        async for message in websocket:
            try:
                if file_dump:
                    obj = json.loads(message)
                    os.makedirs(path[1:].rsplit("/", 1)[0], exist_ok=True)
                    with open(path[1:], "w") as f:
                        f.write(json.dumps(obj, indent=4))
                        f.close()
                else:
                    obj = json.loads(message)
                    if data.get(path) == None:
                        data[path] = dict()
                    if data[path].get(obj.get("key")) == None:
                        data[path][obj.get("key")] = []
                    data[path][obj.get("key")].append(obj)
            except Exception as e:
                print(e)
    except websockets.exceptions.ConnectionClosed as e:
        a = datetime.datetime.now()
        drain_for_module(path)
        b = datetime.datetime.now()
        delta = b - a
        print("time taken to dump: ", path[1:], delta)
    except Exception as e:
        print(e)

def drain_for_module(path):
    global data
    if data.get(path) != None:
        v = data.get(path)
        try:
            process_fdep_output(path, v)
            del data[path]
        except Exception as e:
            print("draining", e)

def process_fdep_output(k, v):
    if not os.path.isfile(k[1:]):
        os.makedirs(k[1:].rsplit("/", 1)[0], exist_ok=True)
        with open(k[1:], "w") as f:
            json.dump(v, f, indent=4)
    else:
        try:
            with open(k[1:], "r") as f:
                alreadyPresentdict = json.load(f)
                newDict = dict(v, **alreadyPresentdict)
                print(v.keys(), alreadyPresentdict.keys())
                with open(k[1:], "w") as ff:
                    json.dump(newDict, ff, indent=4)
        except Exception as e:
            print(e)
            with open(k[1:], "w") as f:
                json.dump(v, f, indent=4)

async def start_websocket_server():
    port = find_free_port()

    # Write port to a file that Nix can read
    with open("fdep_port", "w") as f:
        f.write(str(port))

    async with websockets.serve(
        handler,
        "::1",
        port,
        ping_interval=None,
        ping_timeout=None,
        close_timeout=None,
        max_queue=1000,
    ):
        print(f"WebSocket server started on ws://::1:{port}")
        await asyncio.Future()

if __name__ == "__main__":
    print("Current working directory:", os.getcwd())
    asyncio.run(start_websocket_server())