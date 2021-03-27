notify-send -t 7000 -i audio-x-generic \
"Downloading audio" "`xclip -o -sel clip | xargs youtube-dl -e`"

# downloads audio playlist (if available) in ~/Downloads/
xclip -o -sel clip | xargs youtube-dl --yes-playlist -x -o "~/Downloads/%(title)s.%(ext)s"
