#!/bin/sh

. ./run.lib.sh

start_syrup

nosetests -v --exe
