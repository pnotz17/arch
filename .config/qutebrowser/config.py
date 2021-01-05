# Uncomment this to still load settings configured via autoconfig.yml
# config.load_autoconfig()

# Adjust font and font size
c.fonts.default_family                           = "UbuntuMono Nerd Font"
c.fonts.default_size                             = "12pt"
c.fonts.web.family.standard                      = "UbuntuMono Nerd Font"
c.fonts.web.size.default                         = 16

# your editor of choice
c.editor.command                                 = ['st', '-e', 'vim', '{}']

# Download location
c.downloads.location.directory                   = '/home/panos21/Downloads'

# Tab width & height
c.tabs.padding                                   = {"top": 2, "bottom": 2, "left": 2, "right": 2}

# Hide tabs & status bar by default
c.tabs.show                                      = "never"
c.statusbar.show                                 = "never"

#Other settings
c.content.headers.user_agent                     = 'Mozilla/5.0 ({os_info}) AppleWebKit/{webkit_version} (KHTML, like Gecko) {qt_key}/{qt_version} {upstream_browser_key}/{upstream_browser_version} Safari/{webkit_version}'
c.content.host_blocking.lists                    = ['https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts']
c.content.headers.accept_language                = 'en-US,en;q=0.9'
c.content.host_blocking.enabled                  = True
c.content.headers.do_not_track                   = True
c.content.geolocation                            = False
c.content.pdfjs                                  = True
c.backend                                        = 'webengine'
c.content.host_blocking.whitelist = []

# javascript
c.content.javascript.alert                       = True
c.content.javascript.can_access_clipboard        = False
c.content.javascript.can_close_tabs              = False
c.content.javascript.can_open_tabs_automatically = False
c.content.javascript.enabled                     = True
c.content.javascript.modal_dialog                = False
c.content.javascript.prompt                      = True
c.content.javascript.enabled                     = True

# zoom levels
c.zoom.default                                   = '100%'
c.zoom.levels                                    = ['25%', '33%', '50%', '67%', '75%', '90%', '100%', '110%', '125%', '150%', '175%', '200%', '250%', '300%', '400%', '500%']

# Search engines with keybindings
c.url.searchengines = {
'DEFAULT': 'https://duckduckgo.com/?q={}',
'aw': 'https://wiki.archlinux.org/?search={}',
'gl': 'https://www.google.com/search?q={}', 
're': 'https://www.reddit.com/r/{}', 
'pb': 'https://thepiratebay.zone/search/{}', 
'kt': 'https://kickasstorrents.to/usearch/{}', 
'sx': 'https://searx.fmac.xyz/{}', 
'yt': 'https://www.youtube.com/results?search_query={}'}

# Toggle status bar & tabs
config.bind('xb', 'config-cycle statusbar.show always never')
config.bind('xt', 'config-cycle tabs.show always never')
config.bind('xx', 'config-cycle statusbar.show always never;; config-cycle tabs.show always never')
config.bind('t', 'set-cmd-text -s :open -t')

# Play videos with mpv
config.bind('M', 'hint links spawn --detach mpv {hint-url}')
config.bind('D', 'hint links spawn st -e youtube-dl {hint-url}')

# Dark mode
config.set("colors.webpage.darkmode.enabled", True)

# set qutebrowser colors
c.colors.completion.fg                           = ['#9cc4ff', 'white', 'white']
c.colors.completion.odd.bg                       = '#121212'
c.colors.completion.even.bg                      = '#191919'
c.colors.completion.category.fg                  = '#e1acff'
c.colors.completion.category.bg                  = 'qlineargradient(x1:0, y1:0, x2:0, y2:1, stop:0 #000000, stop:1 #191919)'
c.colors.completion.category.border.top          = '#3f4147'
c.colors.completion.category.border.bottom       = '#3f4147'
c.colors.completion.item.selected.fg             = '#151515'
c.colors.completion.item.selected.bg             = '#ecbe7b'
c.colors.completion.item.selected.match.fg       = '#c678dd'
c.colors.completion.match.fg                     = '#c678dd'
c.colors.completion.scrollbar.fg                 = 'white'
c.colors.downloads.bar.bg                        = '#151515'
c.colors.downloads.error.bg                      = '#ff6c6b'
c.colors.hints.fg                                = '#151515'
c.colors.hints.match.fg                          = '#98be65'
c.colors.messages.info.bg                        = '#151515'
c.colors.statusbar.normal.bg                     = '#151515'
c.colors.statusbar.insert.fg                     = 'white'
c.colors.statusbar.insert.bg                     = '#497920'
c.colors.statusbar.passthrough.bg                = '#34426f'
c.colors.statusbar.command.bg                    = '#151515'
c.colors.statusbar.url.warn.fg                   = 'yellow'
c.colors.tabs.bar.bg                             = '#1c1f34'
c.colors.tabs.odd.bg                             = '#151515'
c.colors.tabs.even.bg                            = '#151515'
c.colors.tabs.selected.odd.bg                    = '#151515'
c.colors.tabs.selected.even.bg                   = '#151515'
c.colors.tabs.pinned.odd.bg                      = 'seagreen'
c.colors.tabs.pinned.even.bg                     = 'darkseagreen'
c.colors.tabs.pinned.selected.odd.bg             = '#151515'
c.colors.tabs.pinned.selected.even.bg            = '#151515'
