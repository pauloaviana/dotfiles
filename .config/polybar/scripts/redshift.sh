#!/bin/bash
temp=$(redshift -p  | grep temperature | cut -c 20-30)
result=$(ps -C redshift | grep -o redshift)
if [ "$result" == "redshift" ]
then
echo $temp
else
echo OFF
fi
