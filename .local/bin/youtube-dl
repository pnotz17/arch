notify-send -t 7000 -i video-x-generic \
"Downloading video" "`xclip -o -sel clip | xargs youtube-dl -e`"

# downloads video in ~/Downloads/
xclip -o -sel clip | xargs youtube-dl -o "~/Downloads/%(title)s.%(ext)s"
