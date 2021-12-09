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
ls --color=auto &> /dev/null 

# Aliases
alias pw='bash -c '"'"'echo `tr -dc $([ $# -gt 1 ] && echo $2 || echo "A-Za-z0-9") < /dev/urandom | head -c $([ $# -gt 0 ] && echo $1 || echo 30)`'"'"' --'
alias sc='doas rm -r ~/.cache/ ~/.local/share/xorg/ ~/.local/share/recently-used.xbel'
alias sm='doas reflector --latest 5 --sort rate --save /etc/pacman.d/mirrorlist'
alias dm='doas mount /dev/sda2 /hdd1 && doas mount /dev/sdb2 /hdd2'
alias jc='doas journalctl --rotate --vacuum-time=1s'
alias gi='grep -iE 'installed' /var/log/pacman.log'
alias gu='grep -iE 'upgraded' /var/log/pacman.log'
alias xr='xmonad --recompile; xmonad --restart'
alias ug='grub-mkconfig -o /boot/grub/grub.cfg'
alias c='git commit -m "changes in dotfiles"'
alias ro='doas pacman -Rns $(pacman -Qtdq)'
alias pl='doas pacman -Qqe > pkglist.txt'
alias up='doas pacman -Syyu --noconfirm'
alias pc='doas pacman -Scc --noconfirm'
alias build='doas make clean install'
alias r='doas chown -R $USER:$USER'
alias mf='fc-list | grep ".local"'
alias lu='ls -l /dev/disk/by-uuid'
alias af='fc-list | grep "fonts"'
alias pk='pacman -Q  |  wc -l'
alias yc='yay -Scc --noconfirm'
alias rm='doas pacman -Rscnd'
alias poweroff='doas poweroff'
alias pss='doas pacman -Ss'
alias sy='doas pacman -Syy'
alias ls='ls --color=auto'
alias reboot='doas reboot'
alias ps='doas pacman -S'
alias x='doas chmod +x'
alias ss='doas spacefm'
alias dg='doas geany'
alias t='doas touch'
alias l='doas ln -s'
alias ex='tar -xpvf'
alias co='tar -zcvf'
alias u='git add -u'
alias s='git status'
alias gc='git clone'
alias vv='doas nvim'
alias yss='yay -Ss'
alias p='git push'
alias a='git add'
alias ys='yay -S'
alias un='unzip'
alias d='doas'
alias v='nvim'

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

# Prompt
setopt prompt_subst
PROMPT='[%F{#FFFFFF}%n@]%F{#FF0000}%m:%15<..<%~%<<$(git_branch_test_color)%F{none}%# '
