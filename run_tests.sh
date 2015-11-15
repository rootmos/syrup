#!/bin/sh

syrup=rel/syrup/bin/syrup

export SYRUP_ADDR=localhost
export SYRUP_TARGET_ADDR=localhost

$syrup start

nosetests -v --exe

cleanup ()
{
    echo "Performing cleanup..."
    $syrup stop
}
trap cleanup INT TERM EXIT
