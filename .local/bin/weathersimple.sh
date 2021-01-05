#!/bin/bash
weather=$(curl -s wttr.in/florina?format=%t)
printf " %s %s \n" "$weather"
