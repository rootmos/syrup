#!/usr/bin/env python

import unittest
import SocketServer
import threading
import random
import string
import socket

class MyEchoHandler(SocketServer.BaseRequestHandler):
    def handle(self):
        self.data = self.request.recv(1024)
        self.request.sendall(self.data)

class echoServer(object):
    def __enter__(self):
        self.server = SocketServer.TCPServer(("localhost", 0), MyEchoHandler)
        self.thread = threading.Thread(target=self.server.serve_forever, kwargs={"poll_interval": 0.01})
        self.thread.start()
        return self

    def __exit__(self, type, value, traceback):
        self.server.shutdown()
        self.server.server_close()
        if self.thread.isAlive():
            self.thread.join()

    @property
    def server_address(self):
        return self.server.server_address

class SyrupTests(unittest.TestCase):
    def poke(self, addressTuple):
        salt = ''.join(random.choice(string.ascii_lowercase + string.digits) for _ in range(10))
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.connect(addressTuple)
        try:
            sock.sendall(salt)
            response = sock.recv(1024)
            self.assertEqual(salt, response)
        finally:
            sock.close()

    def test_sanity(self):
        with echoServer() as s:
            self.poke(s.server_address)

if __name__ == '__main__':
    unittest.main()
