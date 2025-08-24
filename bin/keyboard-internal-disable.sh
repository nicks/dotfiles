#!/bin/bash

echo 1 > /sys/devices/platform/i8042/serio0/input/input3/inhibited
echo 1 > /sys/devices/pci0000:00/0000:00:15.1/i2c_designware.1/i2c-1/i2c-SYNA8004:00/0018:06CB:CD8B.0001/input/input9/inhibited
echo 1 > /sys/devices/pci0000:00/0000:00:15.1/i2c_designware.1/i2c-1/i2c-SYNA8004:00/0018:06CB:CD8B.0001/input/input10/inhibited
