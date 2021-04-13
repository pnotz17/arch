#!/usr/bin/env sh

#!/usr/bin/env sh

# Quickly start creating a new script.

Directory=$(find "$HOME/.local/bin" -maxdepth 2 -type d \
    | dmenu -l 30 -p 'Select directory for new script:')
[ -z "$Directory" ] && exit 1

Name=$(printf '' | dmenu -p 'Name the script:')
[ -z "$Name" ] && exit 1

Script="$Directory/$Name"
touch "$Script"
chmod +x "$Script"
ln -sf "$Script" "$LBIN"
printf '%s\n\n# ' '#!/usr/bin/env sh' > "$Script"

$EDITOR "$Script"
