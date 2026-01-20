#!/usr/bin/env bash
iface=$(route get default 2>/dev/null | awk '/interface: / {print $2}')
name=$(networksetup -listallhardwareports | \
       awk -v iface="$iface" '
         $1 == "Hardware" && $2 == "Port:" {port=$3}
         $1 == "Device:" && $2 == iface {print port}
       ')
if [ -z "$name" ]; then
  name="No Internet"
fi
UPDOWN=$(ifstat -i "$iface" -b 0.1 1 | tail -n1)
DOWN=$(echo $UPDOWN | awk "{ print \$1 }" | cut -f1 -d ".")
UP=$(echo $UPDOWN | awk "{ print \$2 }" | cut -f1 -d ".")

DOWN_FORMAT=""
if [ "$DOWN" -gt "999" ]; then
  DOWN_FORMAT=$(echo $DOWN | awk '{ printf "%03.0f Mbps", $1 / 1000}')
else
  DOWN_FORMAT=$(echo $DOWN | awk '{ printf "%03.0f kbps", $1}')
fi

UP_FORMAT=""
if [ "$UP" -gt "999" ]; then
  UP_FORMAT=$(echo $UP | awk '{ printf "%03.0f Mbps", $1 / 1000}')
else
  UP_FORMAT=$(echo $UP | awk '{ printf "%03.0f kbps", $1}')
fi

sketchybar -m --set network_down label="$DOWN_FORMAT dn" icon.highlight=$(if [ "$DOWN" -gt "0" ]; then echo "on"; else echo "off"; fi) \
           --set network_up label="$UP_FORMAT up" icon.highlight=$(if [ "$UP" -gt "0" ]; then echo "on"; else echo "off"; fi) \
              --set network_name label="$name"
