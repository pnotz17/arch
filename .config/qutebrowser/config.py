# Change the argument to True to still load settings configured via autoconfig.yml
config.load_autoconfig(False)

# Settings
c.backend = 'webengine'
c.auto_save.session = False
c.editor.command = ['st', '-e', 'vim', '{}']
# c.content.canvas_reading = False
# c.content.cookies.accept = 'no-unknown-3rdparty'
# c.content.cookies.store = False
# c.content.unknown_url_scheme_policy = 'disallow'
# c.content.desktop_capture = False
# c.content.dns_prefetch = True
# c.content.geolocation = False
# c.content.mouse_lock = False
# c.content.headers.do_not_track = True
# c.content.headers.referer = 'never'
# c.content.blocking.enabled = True
# c.content.blocking.hosts.lists = ['https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts']
# c.content.blocking.method = 'auto'
# c.content.blocking.whitelist = []
# c.content.javascript.alert = False
# c.content.javascript.prompt = False
# c.content.javascript.enabled = True
# c.content.local_content_can_access_file_urls = False
# c.content.media.audio_capture = False
# c.content.media.audio_video_capture = False
# c.content.media.video_capture = False
# c.content.pdfjs = True
# c.content.persistent_storage = False
# c.content.proxy = 'system'
# c.content.register_protocol_handler = False
# c.content.webgl = False
c.content.webrtc_ip_handling_policy = 'default-public-interface-only'
c.downloads.location.directory = '/home/pnotz17/downloads'
c.statusbar.show = 'never'
c.tabs.padding = {'top': 2, 'bottom': 2, 'left': 2, 'right': 2}
c.tabs.show = 'never'
c.zoom.default = '100%'
c.zoom.levels = ['25%', '33%', '50%', '67%', '75%', '90%', '100%', '110%', '125%', '150%', '175%', '200%', '250%', '300%', '400%', '500%']

# Search engines 
c.url.searchengines = {
'DEFAULT': 'https://duckduckgo.com/?q={}',
'aw': 'https://wiki.archlinux.org/?search={}',
'gw': 'https://wiki.gentoo.org/?search={}',
'g': 'https://www.google.com/search?q={}', 
'r': 'https://www.reddit.com/r/{}', 
'p': 'https://thepiratebay.zone/search/{}', 
'k': 'https://kickasstorrents.to/usearch/{}', 
'x': 'https://searx.fmac.xyz/{}', 
'y': 'https://www.youtube.com/results?search_query={}'}

# Dark Mode
c.colors.webpage.darkmode.enabled = True

# Fonts
c.fonts.default_family = 'UbuntuMono Nerd Font'
c.fonts.default_size = '13pt'
c.fonts.web.family.standard = 'UbuntuMono Nerd Font'
c.fonts.web.size.default = 16

# Bindings for normal mode
config.bind('D', 'hint links spawn st -e youtube-dl {hint-url}')
config.bind('M', 'hint links spawn --detach mpv {hint-url}')
config.bind('t', 'set-cmd-text -s :open -t')
config.bind('xb', 'config-cycle statusbar.show always never')
config.bind('xt', 'config-cycle tabs.show always never')
config.bind('xx', 'config-cycle statusbar.show always never;; config-cycle tabs.show always never')

#  Colors
c.colors.completion.fg = ['#9cc4ff', 'white', 'white']
c.colors.completion.odd.bg = '#121212'
c.colors.completion.even.bg = '#191919'
c.colors.completion.category.fg = '#e1acff'
c.colors.completion.category.bg = 'qlineargradient(x1:0, y1:0, x2:0, y2:1, stop:0 #000000, stop:1 #191919)'
c.colors.completion.category.border.top = '#3f4147'
c.colors.completion.category.border.bottom = '#3f4147'
c.colors.completion.item.selected.fg = '#151515'
c.colors.completion.item.selected.bg = '#ecbe7b'
c.colors.completion.item.selected.match.fg = '#c678dd'
c.colors.completion.match.fg = '#c678dd'
c.colors.completion.scrollbar.fg = 'white'
c.colors.downloads.bar.bg = '#151515'
c.colors.downloads.error.bg = '#ff6c6b'
c.colors.hints.fg = '#151515'
c.colors.hints.match.fg = '#98be65'
c.colors.messages.info.bg = '#151515'
c.colors.statusbar.normal.bg = '#151515'
c.colors.statusbar.insert.fg = 'white'
c.colors.statusbar.insert.bg = '#497920'
c.colors.statusbar.passthrough.bg = '#34426f'
c.colors.statusbar.command.bg = '#151515'
c.colors.statusbar.url.warn.fg = 'yellow'
c.colors.tabs.bar.bg = '#1c1f34'
c.colors.tabs.odd.bg = '#151515'
c.colors.tabs.even.bg = '#151515'
c.colors.tabs.selected.odd.bg = '#151515'
c.colors.tabs.selected.even.bg = '#151515'
c.colors.tabs.pinned.odd.bg = 'seagreen'
c.colors.tabs.pinned.even.bg = 'darkseagreen'
c.colors.tabs.pinned.selected.odd.bg = '#151515'
c.colors.tabs.pinned.selected.even.bg = '#151515'

