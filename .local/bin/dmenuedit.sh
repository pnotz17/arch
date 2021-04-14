#!/usr/bin/env bash

: "${DMEDITOR:=st -e nvim}"

declare -A options

options[xinitrc]="$HOME/.xinitrc"
options[zsh]="$HOME/.zshrc"

declare -A options_clean
  for i in "${!options[@]}"; do
    [[ -f ${options["${i}"]} ]] && options_clean["${i}"]=${options["${i}"]}
  done

choice=$(printf '%s\n' "${!options_clean[@]}" | sort | dmenu -l 20 -p 'Edit config:' "$@")

if [ "$choice" ]; then
  cfg=$(printf '%s\n' "${options_clean["${choice}"]}")
  $DMEDITOR "$cfg"

else
    echo "Program terminated." && exit 0
fi
