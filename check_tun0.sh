#!/bin/bash
if [ -d "/proc/sys/net/ipv4/conf/tun0" ]; then
    echo "VPN: ON"
else
    echo "VPN: OFF"
fi