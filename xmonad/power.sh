#!/bin/bash

# Adapted from this gist: https://gist.github.com/adnan360/f86012baeb4c9ca4f1af033550b03033  

CHOSEN=$(echo -e "Cancel\nLock\nSuspend\nReboot\nShutdown\nHibernate" | rofi -dmenu -i -p "Power Menu" )

# CHOSEN=$(echo -e "Cancel\nLock\nSuspend\nReboot\nShutdown\nHibernate" | dmenu -i -p "$1")

if [[ $CHOSEN = "Lock" ]]; then
	sflock
elif [[ $CHOSEN = "Suspend" ]]; then
	sflock && systemctl suspend
elif [[ $CHOSEN = "Reboot" ]]; then
	systemctl reboot
elif [[ $CHOSEN = "Shutdown" ]]; then
	[ $(echo -e "No\nYes" | rofi -dmenu -i -p "Are you sure you want to shut down?" ) == "Yes" ] && systemctl poweroff
elif [[ $CHOSEN = "Hibernate" ]]; then
	systemctl hibernate
fi
