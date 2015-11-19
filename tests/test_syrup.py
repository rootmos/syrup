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

serverPollInterval = 0.001
pokeTimeout = 0.4

ECHO=1
DROP=2
REFUSE=3

syrup_addr = os.environ["SYRUP_ADDR"]
syrup_target_addr = os.environ["SYRUP_TARGET_ADDR"]
syrup_rest_port = 8080

class syrup(object):
    def __init__(self, client_port, server_port = None, latency = None):
        if not server_port:
            server_port = int(random.uniform(7000,10000))

        self.client_port = client_port
        self.server_port = server_port
        self.latency = latency

    def __enter__(self):
        conn = httplib.HTTPConnection(syrup_addr, syrup_rest_port)
        while True:
            try:
                conn.connect()
                break
            except socket.error:
                pass

        params = {'host': syrup_target_addr, 'port': self.client_port}
        if self.latency:
            params['latency'] = self.latency
        conn.request("PUT","/tcp/%u?%s" % (self.server_port, urllib.urlencode(params)))
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
    def handle(self):
        try:
            while True:
                data = self.request.recv(1024)
                if not data:
                    return

                print "DISCARDing '%s': %s -> %s" % (data, self.client_address, self.server.server_address)
        except socket.timeout:
            print "DISCARD stopping handler due to timeout"

class server(object):
    def __init__(self, action = ECHO):
        if action == ECHO:
            self.server = SocketServer.TCPServer(("", 0), MyEchoHandler)
        elif action == DROP:
            self.server = SocketServer.TCPServer(("", 0), MyDiscardHandler)
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


def connectPoke(addressTuple):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.setblocking(1)
    sock.settimeout(pokeTimeout)

    try:
        sock.connect(addressTuple)
        return sock
    except socket.error:
        return None


def poke(addressTuple, action=ECHO, expectLatency = None, N = 5):

    success = 0
    refused = 0
    empty_responses = 0
    timeouts = 0
    latencySum = 0
    disconnects = 0

    for i in range(1, N+1):
        sock = connectPoke(addressTuple)
        if not sock:
            print "POKE (%u/%u) refused connection to %s:%u" % (i, N, addressTuple[0], addressTuple[1])
            refused += 1
            continue

        try:
            salt = ''.join(random.choice(string.ascii_lowercase + string.digits) for _ in range(10))
            print "POKE (%u/%u) sending '%s' to %s:%u" % (i, N, salt, addressTuple[0], addressTuple[1])
            start_time = time.time()
            sock.send(salt)
            response = sock.recv(1024)
            elapsed_time = time.time() - start_time

            print "POKE (%u/%u) received '%s' from %s:%u after %s seconds" % (i, N, response, addressTuple[0], addressTuple[1], elapsed_time)

            if response == "":
                empty_responses += 1
            else:
                latencySum += elapsed_time
                assert salt == response
                success += 1
        except socket.timeout:
            print "POKE (%u/%u) timed-out, was sent to %s:%u" % (i, N, addressTuple[0], addressTuple[1])
            timeouts += 1
        except socket.error:
            print "POKE (%u/%u) disconnect, was sent to %s:%u" % (i, N, addressTuple[0], addressTuple[1])
            disconnects += 1
        finally:
            sock.close()

    print "POKE N: %u, success: %u, empty_responses: %u, timeouts: %u, disconnects: %u, refused: %u" % (N, success, empty_responses, timeouts, disconnects, refused)

    averageLatency = latencySum / N
    print "POKE average latency: %ss" % averageLatency
    if expectLatency:
        assert averageLatency >= expectLatency

    if action == ECHO:
        test = success
    elif action == DROP:
        test = timeouts
    elif action == REFUSE:
        test = refused

    if test == N:
        return True
    else:
        return False

class SanityChecks(unittest.TestCase):
    def test_sanity_successful(self):
        with server() as s:
            self.assertTrue(poke(s.address))

    def test_sanity_drop(self):
        with server(action=DROP) as s:
            self.assertTrue(poke(s.address, action = DROP))

    def test_sanity_connection_refused(self):
        self.assertTrue(poke(("localhost", 9), action = REFUSE))

class SyrupTests(unittest.TestCase):
    def test_simple(self):
        with server() as serv, syrup(serv.port) as s:
            self.assertTrue(poke(s.address))

    def test_very_many_pokes(self):
        with server() as serv, syrup(serv.port) as s:
            self.assertTrue(poke(s.address, N=10000))

    def test_latency(self):
        with server() as serv, syrup(serv.port, latency=200) as s:
            self.assertTrue(poke(s.address, expectLatency=0.2))

if __name__ == '__main__':
    unittest.main()
