Syrup
=====
[![Build Status](https://travis-ci.org/rootmos/syrup.svg)](https://travis-ci.org/rootmos/syrup)

Syrup is a utility for simulating degraded network conditions such as
latency and flaky connections. Basically, it's just a glorified port-forwarder
with some extra logic to put throw a monkey wrench into the connection.

Syrup is build using [Erlang](http://www.erlang.org) with [Ranch](https://github.com/ninenines/ranch)
and [Cowboy](https://github.com/ninenines/cowboy).
It's distributed using [Docker](http://docker.io), the image can be found
[here](https://hub.docker.com/r/rootmos/syrup).

Installation
============
There's two ways to install Syrup:
* Automagic using [Docker](http://docker.io), or
* Manual building (requires an [Erlang](http://www.erlang.org) environment)

## Installation with Docker
The [syrup](https://github.com/rootmos/syrup/blob/master/syrup) script can
automagically pull the [image](https://hub.docker.com/r/rootmos/syrup) for you,
so all you need to do is:

```bash
$ wget https://raw.githubusercontent.com/rootmos/syrup/master/syrup
$ chmod +x syrup
```

## Manual installation
The manual building requires:
* [Erlang](http://www.erlang.org) and
* [rebar](https://github.com/rebar/rebar)

If these prerequisites are met, then the building should be as easy as:
```bash
$ git pull https://github.com/rootmos/syrup
$ cd syrup
$ make
```
The `syrup` directory should contain a built distribution in the `rel/syrup`
directory, which will be detected by the `syrup` script (located in the
repository root).


Usage
=====
Syrup is controlled by using the [syrup](https://github.com/rootmos/syrup/blob/master/syrup) script.
**Note** that you might need to run it as `./syrup` (unless you put in it your `PATH`) and it might also require `sudo` (if you want to use Docker).

```bash
$ syrup start

$ syrup ping
pong

$syrup stop
```

To add a port-forwarding from localhost:6001 to localhost:5001, call:
```bash
$ syrup add -f 6001 -t 5001
```
If you want to forward to another address use the `-h` option:
```bash
$ syrup add -f 6001 -h 192.168.0.7 -t 5001
```
If you want to add some latency to the connection use the `-l` option:
```bash
$ syrup add -f 6001 -t 5001 -l 200
```

To remove a port-forwarding:
```bash
$ syrup rm 6001
```

Example
=======
Here's an example session using [iperf](http://sourceforge.net/projects/iperf2):

In one shell start iperf's server:
```bash
$ iperf -s -p 5001
------------------------------------------------------------
Server listening on TCP port 5001
TCP window size: 85.3 KByte (default)
------------------------------------------------------------
```

Then in another shell run the client:
```
$ syrup start

$ syrup add -f 6001 -t 5001

$ iperf -c localhost -p 6001
------------------------------------------------------------
Client connecting to localhost, TCP port 6001
TCP window size: 2.50 MByte (default)
------------------------------------------------------------
[  3] local 127.0.0.1 port 57840 connected with 127.0.0.1 port 6001
[ ID] Interval       Transfer     Bandwidth
[  3]  0.0-10.0 sec  25.7 GBytes  22.1 Gbits/sec

$ syrup stop
```
