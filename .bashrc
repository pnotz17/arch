#
# ~/.bashrc
#

#exports
export TERM="st-256color"                     
export EDITOR="vim"  
export BROWSER="firefox"          
export PATH=$HOME/.local/bin:$PATH
export LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:ex=01;32:*.cmd=01;32:*.exe=01;32:*.com=01;32:*.btm=01;32:*.bat=01;32:*.sh=01;32:*.csh=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tz=01;31:*.rpm=01;31:*.cpio=01;31:*.jpg=01;35:*.gif=01;35:*.bmp=01;35:*.xbm=01;35:*.xpm=01;35:*.png=01;35:*.tif=01;35:'

#history
HISTCONTROL=ignoredups:erasedups  

#aliases
alias pw='bash -c '"'"'echo `tr -dc $([ $# -gt 1 ] && echo $2 || echo "A-Za-z0-9") < /dev/urandom | head -c $([ $# -gt 0 ] && echo $1 || echo 30)`'"'"' --'
alias sm='doas reflector --latest 20 --protocol https --sort rate --save /etc/pacman.d/mirrorlist'
alias sc='doas rm -r ~/.cache/ ~/.local/share/xorg/ ~/.local/share/recently-used.xbel'
alias dm='doas mount /dev/sda2 /hdd1 && doas mount /dev/sdb2 /hdd2'
alias jc='doas journalctl --rotate --vacuum-time=1s'
alias ug='doas grub-mkconfig -o /boot/grub/grub.cfg'
alias gi='grep -iE 'installed' /var/log/pacman.log'
alias gu='grep -iE 'upgraded' /var/log/pacman.log'
alias xr='xmonad --recompile; xmonad --restart'
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
alias reboot='doas reboot'
alias ps='doas pacman -S'
alias x='doas chmod +x'
alias ss='doas spacefm'
alias dg='doas geany'
alias l='doas ln -s'
alias ex='tar -xpvf'
alias co='tar -zcvf'
alias u='git add -u'
alias s='git status'
alias gc='git clone'
alias vv='doas vim'
alias yss='yay -Ss'
alias a='git add *'
alias p='git push'
alias ys='yay -S'
alias un='unzip'
alias d='doas'
alias v='vim'

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

#git prompt
git_prompt() {
  BRANCH=$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/*\(.*\)/\1/')

  if [ ! -z $BRANCH ]; then
    echo -n "$(tput setaf 3)"

    if [ ! -z "$(git status --short)" ]; then
      echo " $(tput setaf 1)✗"
    fi
  fi
}

PS1="[\u@]\e[31m\]\h:\w\[\033[0;36m\]\$(git_prompt) \[\033[0m\]\$ "
#PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\W\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"

