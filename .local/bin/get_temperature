#!/bin/bash
# Other common locations for CPU temperature information
# /proc/acpi/thermal_zone/THRM/temperature
# /sys/class/thermal/thermal_zone*/temp
# /sys/class/thermal/cooling_device*/temp
# /sys/devices/platform/f71882fg.1152/temp*_input
# /sys/devices/platform/coretemp.0/hwmon/hwmon*/temp*_input
paste <(cat /sys/devices/platform/coretemp.0/hwmon/hwmon*/temp*_label) <(cat /sys/devices/platform/coretemp.0/hwmon/hwmon*/temp*_input) | column -s $'\t' -t | sed 's/\(.\)..$/.\1°C/'
