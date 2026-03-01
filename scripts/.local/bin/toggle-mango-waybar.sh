#!/bin/sh
pkill -f 'waybar.*config-mango' || waybar -c ~/.config/waybar/config-mango.jsonc -s ~/.config/waybar/style-mango.css &
