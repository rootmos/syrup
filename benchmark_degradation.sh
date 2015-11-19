#!/bin/bash

. ./run.lib.sh

echo "=== Setting up the iperf server"
iperf --server --port 5001 --daemon

echo "=== Start Syrup"
start_syrup

for try in {1..5}; do
    sleep 1
    curl -X PUT -i "127.0.0.1:8080/tcp/6001?host=127.0.0.1&port=5001"
    if [ $? -eq 0 ]; then
        break
    fi
    echo "Failed to configure Syrup, wait a while..."
done

if [ $try -eq 5 ]; then
    echo "Unable to configure Syrup!"
    exit 255
fi

echo "=== Execute Without Syrup"
without=$(iperf --client localhost --port 5001 --reportstyle C)

echo "=== Execute With Syrup"
with=$(iperf --client localhost --port 6001 --reportstyle C)

echo "=== Stop the iperf server"
killall iperf

echo
echo
echo "=== Results:"

rounded_percentage()
{
    echo "$1/$2*100" | bc -l | xargs printf "%.0f"
}

bitrate_without=$(echo $without | cut -d, -f9)
bitrate_with=$(echo $with | cut -d, -f9)
bitrate_expr="$bitrate_with/$bitrate_without"
bitrate_degradation=$(rounded_percentage $bitrate_with $bitrate_without)
echo "Bitrate: $bitrate_expr=$bitrate_degradation%"

data_without=$(echo $without | cut -d, -f8)
data_with=$(echo $with | cut -d, -f8)
data_degradation=$(rounded_percentage $data_with $data_without)
echo "Data: $data_with/$data_without=$data_degradation%"


# Validate results
result=0
acceptable_degradation=25

if [ $bitrate_degradation -lt $acceptable_degradation ];
then
    echo "Bitrate degraded too much!"
    result=3
fi

if [ $data_degradation -lt $acceptable_degradation ];
then
    echo "Data degraded too much!"
    result=3
fi

exit $result
