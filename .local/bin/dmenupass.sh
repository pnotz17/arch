#!/usr/bin/env sh

# Default environment variables - see man pass (1):
[ -n "$PASSWORD_STORE_DIR" ] || export PASSWORD_STORE_DIR="$HOME/.password-store"
[ -n "$PASSWORD_STORE_CLIP_TIME" ] || export PASSWORD_STORE_CLIP_TIME=45

cd "$PASSWORD_STORE_DIR"
passname="$(find * -type f | sed 's/\.gpg$//g' | dmenu "$@")"
[ -n "$passname" ] || exit 1
pass show -c "$passname" >/dev/null
notify-send \
	'Unix pass' \
	"$passname\n(will clear in $PASSWORD_STORE_CLIP_TIME seconds)"
