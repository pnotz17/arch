#  Basic stuff 
autoload -U colors && colors
autoload -U compinit  vcs_info 
compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION

# Environment variables
export TERM=st-256color
export EDITOR=vim
export Browser=firefox

# Path
if [ -d "$HOME/.local/bin" ] ;
  then PATH="$HOME/.local/bin:$PATH"
fi

# History
HISTFILE=~/.zsh/zhistory
HISTSIZE=10000
SAVEHIST=10000

# Prompt
setopt PROMPT_SUBST
PROMPT='%F{blue}%9c$(git_branch_test_color)%F{none} %# '
RPROMPT='%D{%k:%M:%S}'

# Git settings
git_branch_test_color() {
  local ref=$(git symbolic-ref --short HEAD 2> /dev/null)
  if [ -n "${ref}" ]; then
    if [ -n "$(git status --porcelain)" ]; then
      local gitstatuscolor='%F{red}**M**'
    else
      local gitstatuscolor='%F{green}'
    fi
    echo "${gitstatuscolor} (${ref})"
  else
    echo ""
  fi
}

# Plugins
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh 2>/dev/null
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
source ~/.zsh/.fzf/shell/completion.zsh 2> /dev/null
source ~/.zsh/.fzf/shell/key-bindings.zsh 2> /dev/null

if [[ ! "$PATH" == *~/.zsh/.fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/$HOME/.zsh/.fzf/bin"
fi

# ls - colors
export CLICOLOR=1
ls --color=auto &> /dev/null && alias ls='ls --color=auto'

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


