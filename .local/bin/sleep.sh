#! /bin/bash

log_file="/tmp/swaylock.log"

log_message() {
  echo "$(date +"%Y-%m-%d %H:%M:%S") - $1" >> "$log_file"
}

log_message "sleep lock started"

# If sway is running, we are using swaylock
if pgrep "sway" > /dev/null; then
  # Check if swaylock is installed
  if ! hash swaylock >/dev/null 2>&1; then
    log_message "Error: swaylock command not found. Please ensure it is installed."
    exit 1
  fi

  # check if swaylock is already running
  if ! pgrep "swaylock" > /dev/null ; then
    log_message "Swaylock with -s $SWAYSOCK not running, attempting to start it."
    pkill swayidle
    pkill_exit=$?
    log_message "Pkill swayidle: $pkill_exit"
    swaymsg -s "$SWAYSOCK" input type:keyboard xkb_switch_layout 0
    input_exit=$?
    log_message "Exit code of swaymsg input switch: $input_exit"
    log_message "Turn screen on before swaylock"
    swaymsg "output * dpms on"
    swaylock -c 000000 -F -k -l --font 'FiraCode Nerd Font Mono' --font-size 22
    lock_exit=$?
    log_message "Succesfully block screen using swaylock: $lock_exit"
    idle.sh &
    idle_exit=$?
    log_message "Run idle: $idle_exit"
  fi
# We are on i3 X session and using i3lock
else
  # Check if i3lock is installed
  if ! hash i3lock >/dev/null 2>&1; then
    log_message "Error: i3lock command not found. Please ensure it is installed."
    exit 1
  fi

  # Check if i3lock is already running
  if ! pgrep "i3lock" > /dev/null ; then
    dbus-send --type=method_call --dest=ru.gentoo.KbddService /ru/gentoo/KbddService ru.gentoo.kbdd.set_layout uint32:2
    log "Succesfully block screen using i3lock"
    i3lock -c 000000 -n
    log "i3lock was closed"
  fi
fi
