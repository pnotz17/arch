#
# ~/.bashrc
#

#exports
export TERM="st-256color"                     
export EDITOR="nvim"  
export BROWSER="firefox"          
export PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\W\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"

#history
HISTFILE="/home/pnotz17/.local/.bash_history"
HISTCONTROL=ignoredups:erasedups  

#aliases
alias pw='bash -c '"'"'echo `tr -dc $([ $# -gt 1 ] && echo $2 || echo "A-Za-z0-9") < /dev/urandom | head -c $([ $# -gt 0 ] && echo $1 || echo 30)`'"'"' --'
alias sc='doas rm -r ~/.cache/ ~/.local/share/xorg/ ~/.local/share/recently-used.xbel'
alias sm='doas reflector --latest 5 --sort rate --save /etc/pacman.d/mirrorlist'
alias dm='doas mount /dev/sda2 /hdd1 && doas mount /dev/sdb2 /hdd2'
alias jc='doas journalctl --rotate --vacuum-time=1s'
alias gi='grep -iE 'installed' /var/log/pacman.log'
alias gu='grep -iE 'upgraded' /var/log/pacman.log'
alias xr='xmonad --recompile; xmonad --restart'
alias c='git commit -m "changes in dotfiles"'
alias ro='doas pacman -Rns $(pacman -Qtdq)'
alias pl='doas pacman -Qqe > pkglist.txt'
alias up='doas pacman -Syu --noconfirm'
alias pc='doas pacman -Scc --noconfirm'
alias build='doas make clean install'
alias mf='fc-list | grep ".local"'
alias lu='ls -l /dev/disk/by-uuid'
alias af='fc-list | grep "fonts"'
alias pk='pacman -Q  |  wc -l'
alias yc='yay -Scc --noconfirm'
alias rm='doas pacman -Rscnd'
alias poweroff='doas poweroff'
alias r='doas chmod -R 777'
alias pss='doas pacman -Ss'
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

#shopt
shopt -s autocd 
shopt -s cdspell 
shopt -s cmdhist 
shopt -s dotglob
shopt -s histappend 
shopt -s expand_aliases 
shopt -s checkwinsize 

#ignore upper and lowercase when TAB completion
bind "set completion-ignore-case on"

#If not running interactively, don't do anything
[[ $- != *i* ]] && return

#syntax highlighting, autosuggestions, menu-completion, abbreviations with Bash Line Editor
source ~/.local/share/blesh/ble.sh     

#enable bash completion in interactive shells
  if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
      . /etc/bash_completion
  fi

#bash insulter
if [ -f /etc/bash.command-not-found ]; then
    . /etc/bash.command-not-found
fi

#neofetch
#pfetch
