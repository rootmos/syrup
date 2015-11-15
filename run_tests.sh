#!/bin/sh

syrup=rel/syrup/bin/syrup

export SYRUP_ADDR=localhost
export SYRUP_TARGET_ADDR=localhost

$syrup start

cleanup ()
{
    echo "Performing cleanup..."
    $syrup stop
}
trap cleanup INT TERM EXIT

nosetests -v --exe
