Syrup
=====
[![Build Status](https://travis-ci.org/rootmos/syrup.svg)](https://travis-ci.org/rootmos/syrup)

Syrup is a utility for simulating degraded network conditions such as
latency and flaky connections.

Installation
============
There's two ways to install Syrup:
* Automagic using [Docker](http://docker.io), or
* Manual building (requires an [Erlang](http://www.erlang.org) environment)

## Installation with Docker
The [syrup](https://github.com/rootmos/syrup/blob/master/syrup) script can automagically pull the [image](https://hub.docker.com/r/rootmos/syrup) for you, so all you need to do is:

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
Then the `syrup` directory should contain a built distribution in the `rel/syrup` directory.

