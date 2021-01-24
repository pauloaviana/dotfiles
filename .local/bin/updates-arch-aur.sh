#!/bin/sh

if ! updates=$(paru -Qua | wc -l); then
    updates=0
fi

if [ "$updates" -gt 0 ]; then
        echo "($updates)"
else
    echo ""
fi
