#!/bin/sh

if [ -s /var/local/countupdates/count ] ; then
    updates=$(cat /var/local/countupdates/count)
else
    updates=0
fi

if [ "$updates" -gt 0 ]; then
    echo " System Updates: $updates"
else
    echo ""
fi
