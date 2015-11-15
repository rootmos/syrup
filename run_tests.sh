#!/bin/sh

syrup=$(docker run -d syrup)
docker logs -f ${syrup} | logger -t "syrup" &

export SYRUP_ADDR=$(docker inspect --format '{{ .NetworkSettings.IPAddress }}' ${syrup})
export SYRUP_TARGET_ADDR=$(docker inspect --format '{{ .NetworkSettings.Gateway }}' ${syrup})

nosetests -v --exe

cleanup ()
{
    echo "Performing cleanup..."
    docker kill ${syrup}
    docker rm ${syrup}
}
trap cleanup INT TERM EXIT
