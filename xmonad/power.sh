#!/bin/bash

# Adapted from this gist: https://gist.github.com/adnan360/f86012baeb4c9ca4f1af033550b03033  

chosen=$(echo -e "Cancel\nLock\nSuspend\nReboot\nShutdown\nHibernate" | rofi -dmenu -i -p "Power Menu" )

# chosen=$(echo -e "Cancel\nLock\nSuspend\nReboot\nShutdown\nHibernate" | dmenu -i -p "$1")

if [[ $chosen = "Lock" ]]; then
	sflock
elif [[ $chosen = "Suspend" ]]; then
	sflock && systemctl suspend
elif [[ $chosen = "Reboot" ]]; then
	systemctl reboot
elif [[ $chosen = "Shutdown" ]]; then
	[ $(echo -e "No\nYes" | rofi -dmenu -i -p "Are you sure you want to shut down?" ) == "Yes" ] && systemctl poweroff
elif [[ $chosen = "Hibernate" ]]; then
	systemctl hibernate
fi
