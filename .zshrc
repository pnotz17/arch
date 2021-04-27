#  Basics 
autoload -U colors && colors
autoload -U compinit  vcs_info 
compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION

# Environment variables
export TERM=st-256color
export EDITOR=nvim
export BROWSER=firefox
export PATH=$HOME/.local/bin:$PATH

# History
HISTFILE=~/.zsh/zhistory
HISTSIZE=10000
SAVEHIST=10000

# Prompt
setopt prompt_subst
PROMPT='%F{none}%n@%F{blue}%m:%15<..<%~%<<$(git_branch_test_color)%F{none}%# '

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

# Autocompletion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

# ls - colors
export CLICOLOR=1
ls --color=auto &> /dev/null && alias ls='ls --color=auto'

# Aliases
alias sudo='doas'
alias v='doas nvim'
alias ss='doas spacefm'
alias ps='doas pacman -S'
alias pss='doas pacman -Ss'
alias yss='yay -Ss'
alias ys='yay -S'
alias un='unzip'
alias ex='tar -xpvf'
alias co='tar -zcvf'
alias build='doas make clean install'
alias up='doas pacman -Syu --noconfirm'
alias rm='doas pacman -Rscnd'
alias sc='doas rm -rf ~/.cache/*'
alias pc='doas pacman -Scc --noconfirm'
alias yc='yay -Scc --noconfirm'
alias jc='doas journalctl --rotate --vacuum-time=1s'
alias list='doas pacman -Qqe > pkglist.txt'
alias pkg='pacman -Q  |  wc -l'
alias ro='doas pacman -Rns $(pacman -Qtdq)'
alias sm=' doas reflector --verbose --country Greece --age 12 --protocol https --sort rate --save /etc/pacman.d/mirrorlist'
alias u='git add -u'
alias s='git status'
alias a='git add'
alias c='git commit -m "changes in dotfiles"'
alias p='git push'
alias gc='git clone'
alias x='sudo chmod +x *'
alias gi=' grep -iE 'installed' /var/log/pacman.log'
alias gu=' grep -iE 'upgraded' /var/log/pacman.log'



