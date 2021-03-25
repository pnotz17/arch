#  Basic stuff 
autoload -U compinit colors vcs_info
colors
compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION

# History
HISTFILE=~/.zsh/zhistory
HISTSIZE=10000
SAVEHIST=10000

# ls-colors
export CLICOLOR=1
ls --color=auto &> /dev/null && alias ls='ls --color=auto'

#  Autocompletion
zstyle ':completion:*' completer _complete _correct _approximate 
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:*:*:*:*' menu yes select

# Display the existence of files not yet known to VCS
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' unstagedstr '!'
zstyle ':vcs_info:*' stagedstr '+'
zstyle ':vcs_info:*' formats "%u%c"

# Prompt
_setup_ps1() {
vcs_info
GLYPH="▲"
[ "x$KEYMAP" = "xvicmd" ] && GLYPH="▼"
PS1=" %(?.%F{blue}.%F{red})$GLYPH%f %(1j.%F{cyan}[%j]%f .)%F{blue}%~%f %(!.%F{red}#%f .)"
RPROMPT="$vcs_info_msg_0_"
}
_setup_ps1

# git: Show marker (T) if there are untracked files in repository # Make sure you have added staged to your 'formats':  %c
zstyle ':vcs_info:git*+set-message:*' hooks git-untracked

+vi-git-untracked(){
    if [[ $(git rev-parse --is-inside-work-tree 2> /dev/null) == 'true' ]] && \
        git status --porcelain | grep '??' &> /dev/null ; then
        # This will show the marker if there are any untracked files in repo.
        # If instead you want to show the marker only if there are untracked
        # files in $PWD, use:
        #[[ -n $(git ls-files --others --exclude-standard) ]] ; then
        hook_com[staged]+='⚠️'
    fi
}

# Vi mode
zle-keymap-select () {
 _setup_ps1
  zle reset-prompt
}
zle -N zle-keymap-select
zle-line-init () {
  zle -K viins
}
zle -N zle-line-init
bindkey -v

# Path
if [ -d "$HOME/.local/bin" ] ;
  then PATH="$HOME/.local/bin:$PATH"
fi

# Plugins
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh 2>/dev/null
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null

# Aliases
alias sudo='doas'
alias v='doas vim'
alias sp='doas pcmanfm'
alias ps='doas pacman -S'
alias pss='doas pacman -Ss'
alias yss='yay -Ss'
alias ys='yay -S'
alias e='tar -xvf'
alias build='doas make clean install'
alias psy='doas pacman -Syu --noconfirm'
alias rm='doas pacman -Rscnd'
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

