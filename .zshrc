# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Set the list of directories that Zsh searches for programs.
export PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin 

# Setting up Defaults
export EDITOR='vim'
export TERMINAL='st'
export BROWSER="/usr/bin/qutebrowser" 

# Aliases
alias sudo='doas'
alias sv='sudo vim'
alias sp='sudo pcmanfm'
alias vim='vim -S ~/.vimrc'
alias ps='sudo pacman -S'
alias pss='sudo pacman -Ss'
alias psyy='sudo pacman -Syy'
alias yss='yay -Ss'
alias ys='yay -S'
alias extract='tar -xvf'
alias build='sudo make clean install'
alias update='sudo pacman -Syu'
alias psy='sudo pacman -Syu'
alias ls='ls --color=auto'
alias rm='sudo pacman -Rscnd'
alias csc='du -sh  ~/.cache'
alias cpc='du -sh /var/cache/pacman/pkg/'
alias rmsc='sudo rm -rf ~/.cache/*'
alias rmpc='sudo pacman -Scc'
alias rmyc='yay -Scc'
alias rmjc='sudo journalctl --rotate --vacuum-time=1s'
alias pkglist='sudo pacman -Qqe > pkglist.txt'
alias pkgs='pacman -Q  |  wc -l'
alias rmo='sudo pacman -Rns $(pacman -Qtdq)'
alias import='gpg --keyserver ha.pool.sks-keyservers.net --recv-keys'
alias srm=' sudo reflector --verbose --country Greece --age 12 --protocol https --sort rate --save /etc/pacman.d/mirrorlist'
alias xr='xmonad --recompile'
alias gu='git config --global user.email "pnotz17@gmail.com"'
alias gn='git config --global user.name "Panagiotis A Natsis"'
alias gau='git add -u'
alias gs='git status'
alias ga='git add'
alias gc='git commit -m "changes in dotfiles"'
alias gp='git push'

# Navigation
alias ..='cd ..'
alias ...='cd ../..'

# History
HISTFILE=~/.zsh/zhistory
HISTSIZE=10000
SAVEHIST=10000

## Key Bindings
bindkey -v

# Colors
# makes color constants available
autoload -U colors zsh/terminfo
colors

# Autocompletion
autoload -Uz compinit
compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION

autoload bashcompinit
bashcompinit

zmodload -i zsh/complist

WORDCHARS=''
unsetopt menu_complete   
unsetopt flowcontrol
setopt auto_menu        
setopt complete_in_word
setopt always_to_end

# Autocompletion with an arrow-key driven interface
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' list-colors ''
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)
([0-9a-z-]#)*=01;34=0=01'

zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"

# Don't complete uninteresting users
zstyle ':completion:*:*:*:users' ignored-patterns \
        adm amanda apache at avahi avahi-autoipd beaglidx bin cacti canna \
        clamav daemon dbus distcache dnsmasq dovecot fax ftp games gdm \
        gkrellmd gopher hacluster haldaemon halt hsqldb ident junkbust kdm \
        ldap lp mail mailman mailnull man messagebus  mldonkey mysql nagios \
        named netdump news nfsnobody nobody nscd ntp nut nx obsrun openvpn \
        operator pcap polkitd postfix postgres privoxy pulse pvm quagga radvd \
        rpc rpcuser rpm rtkit scard shutdown squid sshd statd svn sync tftp \
        usbmux uucp vcsa wwwrun xfs '_*'

zstyle '*' single-ignored show

# Automatically update PATH entries
zstyle ':completion:*' rehash true

# Keep directories and files separated
zstyle ':completion:*' list-dirs-first true

# Compinstall filename
zstyle :compinstall filename '/home/panos21/.zshrc'

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Plugins
source ~/.zsh/powerlevel10k/powerlevel10k.zsh-theme 2>/dev/null
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh 2>/dev/null
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null

## Prompt color scripts
#/opt/shell-color-scripts/colorscripts/pacman
#colorscript random

#typeset -g POWERLEVEL9K_INSTANT_PROMPT=quiet
source ~/.zsh/powerlevel10k/powerlevel10k.zsh-theme
