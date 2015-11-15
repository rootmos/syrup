#!/usr/bin/env python

import unittest
import SocketServer
import threading
import random
import string
import socket
import time
import os
import httplib
import urllib

serverPollInterval = 0.01
pokeTimeout = 1.0

ECHO=1
DROP=2
TIMEOUT=3

syrup_addr = os.environ["SYRUP_ADDR"]
syrup_target_addr = os.environ["SYRUP_TARGET_ADDR"]
syrup_rest_port = 8080

class syrup(object):
    def __init__(self, client_port, server_port = int(random.uniform(7000,10000))):
        self.client_port = client_port
        self.server_port = server_port

    def __enter__(self):
        conn = httplib.HTTPConnection(syrup_addr, syrup_rest_port)
        while True:
            try:
                conn.connect()
                break
            except socket.error:
                pass

        params = urllib.urlencode({'host': syrup_target_addr, 'port': self.client_port})
        conn.request("PUT","/tcp/%u?%s" % (self.server_port, params))
        response = conn.getresponse()
        assert response.status == 201

        return self

    def __exit__(self, type, value, traceback):
        conn = httplib.HTTPConnection(syrup_addr, syrup_rest_port)
        conn.request("DELETE","/tcp/%u" % self.server_port)
        assert conn.getresponse().status == 204

    @property
    def address(self):
        return (syrup_addr, self.server_port)

class MyEchoHandler(SocketServer.BaseRequestHandler):
    def handle(self):
        self.request.setblocking(1)
        self.request.settimeout(pokeTimeout)

        try:
            while True:
                data = self.request.recv(1024)
                if not data:
                    return

                print "ECHOing '%s': %s -> %s" % (data, self.client_address, self.server.server_address)
                self.request.send(data)
        except socket.timeout:
            pass


class MyDiscardHandler(SocketServer.BaseRequestHandler):
    pass

class MyTimeoutHandler(SocketServer.BaseRequestHandler):
    def handle(self):
        self.request.setblocking(1)
        self.request.settimeout(pokeTimeout)
        sleepTime = pokeTimeout * 1.5

        try:
            while True:
                data = self.request.recv(1024)
                if not data:
                    return

                print "ECHOing (with delay %ss) '%s': %s -> %s" % (sleepTime, data, self.client_address, self.server.server_address)
                time.sleep(sleepTime)
                self.request.send(data)
        except socket.timeout:
            pass

class server(object):
    def __init__(self, action = ECHO):
        if action == ECHO:
            self.server = SocketServer.TCPServer(("", 0), MyEchoHandler)
        elif action == DROP:
            self.server = SocketServer.TCPServer(("", 0), MyDiscardHandler)
        elif action == TIMEOUT:
            self.server = SocketServer.TCPServer(("", 0), MyTimeoutHandler)
        else:
            raise RuntimeError("Unknown action!")

        self.thread = threading.Thread(target=self.server.serve_forever, kwargs={"poll_interval": serverPollInterval})

    def __enter__(self):
        self.thread.start()
        return self

    def __exit__(self, type, value, traceback):
        self.server.shutdown()
        self.server.server_close()
        if self.thread.isAlive():
            self.thread.join()

    @property
    def address(self):
        return self.server.server_address

    @property
    def host(self):
        return self.address[0]

    @property
    def port(self):
        return self.address[1]

def poke(addressTuple, expectLatency = None, N = 100):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.setblocking(1)
    sock.settimeout(pokeTimeout)

    try:
        sock.connect(addressTuple)
    except socket.error:
        print "POKE failed to connect to %s:%u" % addressTuple
        return False

    success = 0
    drops = 0
    timeouts = 0
    latencySum = 0

    for i in range(1, N+1):
        try:
            salt = ''.join(random.choice(string.ascii_lowercase + string.digits) for _ in range(10))
            print "POKE (%u/%u) sending '%s' to %s:%u" % (i, N, salt, addressTuple[0], addressTuple[1])

            start_time = time.time()
            sock.sendall(salt)
            response = sock.recv(1024)
            elapsed_time = time.time() - start_time

            print "POKE (%u/%u) received '%s' from %s:%u after %s seconds" % (i, N, response, addressTuple[0], addressTuple[1], elapsed_time)

            if response == "":
                drops += 1
            else:
                latencySum += elapsed_time
                assert salt == response
                success += 1
        except socket.timeout:
            print "POKE timed-out, was sent to %s:%u" % (addressTuple[0], addressTuple[1])
            timeouts += 1
    sock.close()

    print "POKE N: %u, success: %u, drops: %u, timeouts: %u" % (N, success, drops, timeouts)

    averageLatency = latencySum / N
    print "POKE average latency: %ss" % averageLatency
    if expectLatency:
        assert averageLatency >= expectLatency

    if timeouts or drops:
        return False
    else:
        return True

class SanityChecks(unittest.TestCase):
    def test_sanity_successful(self):
        with server() as s:
            self.assertTrue(poke(s.address))

    def test_sanity_timeout(self):
        with server(action=TIMEOUT) as s:
            self.assertFalse(poke(s.address, N = 1))

    def test_sanity_drop(self):
        with server(action=DROP) as s:
            self.assertFalse(poke(s.address, N = 1))

    def test_sanity_connection_refused(self):
        self.assertFalse(poke( ("localhost", 9) ))

class SyrupTests(unittest.TestCase):
    def test_simple(self):
        with server() as serv, syrup(serv.port) as s:
            self.assertTrue(poke(s.address))

    def test_latency(self):
        with server() as serv, syrup(serv.port) as s:
            self.assertTrue(poke(s.address, expectLatency=0.5))

if __name__ == '__main__':
    unittest.main()
