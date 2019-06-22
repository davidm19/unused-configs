#!/bin/bash

# Adapted from this gist: https://gist.github.com/adnan360/f86012baeb4c9ca4f1af033550b03033  

chosen=$(echo -e "Cancel\nLock\nSuspend\nReboot\nShutdown\nHibernate" | rofi -dmenu -i)

if [[ $chosen = "Lock" ]]; then
	sflock
elif [[ $chosen = "Suspend" ]]; then
	systemctl suspend
elif [[ $chosen = "Reboot" ]]; then
	systemctl reboot
elif [[ $chosen = "Shutdown" ]]; then
	systemctl poweroff
elif [[ $chosen = "Hibernate" ]]; then
	systemctl hibernate
fi
