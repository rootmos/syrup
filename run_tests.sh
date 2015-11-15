#!/bin/sh

docker="sudo docker"
syrup=$(${docker} run -d syrup)
${docker} logs -f ${syrup} | logger -t "syrup" &

export SYRUP_ADDR=$(${docker} inspect --format '{{ .NetworkSettings.IPAddress }}' ${syrup})

sleep 1

nosetests -v --exe

cleanup ()
{
    echo "Performing cleanup..."
    ${docker} kill ${syrup}
    ${docker} rm ${syrup}
}
trap cleanup INT TERM EXIT
