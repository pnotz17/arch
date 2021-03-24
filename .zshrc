# Set the list of directories that Zsh searches for programs.
export PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin 

# Setting up Defaults
export EDITOR='vim'
export TERMINAL='st'
export BROWSER="/usr/bin/firefox" 

# History
HISTFILE=~/.zsh/zhistory
HISTSIZE=10000
SAVEHIST=10000

# Enable colors and change prompt:
autoload -U colors && colors

# Basic auto/tab complete:
autoload -Uz compinit && compinit
compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION
autoload bashcompinit && bashcompinit
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:*:*:*:*' menu yes select

# Aliases
alias sudo='doas'
alias sv='doas vim'
alias v='doas vim'
alias sp='doas pcmanfm'
alias vim='vim -S ~/.vimrc'
alias ps='doas pacman -S'
alias pss='doas pacman -Ss'
alias psyy='doas pacman -Syy'
alias yss='yay -Ss'
alias ys='yay -S'
alias te='tar -xvf'
alias build='doas make clean install'
alias update='doas pacman -Syu --noconfirm'
alias psy='doas pacman -Syu --noconfirm'
alias ls='ls --color=auto'
alias rm='doas pacman -Rscnd'
alias csc='du -sh  ~/.cache'
alias cpc='du -sh /var/cache/pacman/pkg/'
alias rmsc='doas rm -rf ~/.cache/*'
alias rmpc='doas pacman -Scc --noconfirm'
alias rmyc='yay -Scc --noconfirm'
alias rmjc='doas journalctl --rotate --vacuum-time=1s'
alias pkglist='doas pacman -Qqe > pkglist.txt'
alias pkgs='pacman -Q  |  wc -l'
alias rmo='doas pacman -Rns $(pacman -Qtdq)'
alias import='gpg --keyserver ha.pool.sks-keyservers.net --recv-keys'
alias srm=' doas reflector --verbose --country Greece --age 12 --protocol https --sort rate --save /etc/pacman.d/mirrorlist'
alias xr='xmonad --recompile'
alias gau='git add -u'
alias gs='git status'
alias ga='git add'
alias gc='git commit -m "changes in dotfiles"'
alias gp='git push'
alias gtc='git clone'
alias ..='cd ..'
alias ...='cd ../..'

# Git settings
parse_git_branch() {
    git_status="$(git status 2> /dev/null)"
    pattern="On branch ([^[:space:]]*)"
    if [[ ! ${git_status} =~ "(working (tree|directory) clean)" ]]; then
        state="*"
    fi
    if [[ ${git_status} =~ ${pattern} ]]; then
      branch=${match[1]}
      branch_cut=${branch:0:35}
      if (( ${#branch} > ${#branch_cut} )); then
          echo "(${branch_cut}…${state})"
      else
          echo "(${branch}${state})"
      fi
    fi
}

setopt PROMPT_SUBST
PROMPT='%{%F{magenta}%}%9c%{%F{none}%}$(parse_git_branch)$'

# Plugins
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh 2>/dev/null
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null


