# Enable colors and basic auto/tab complete:
autoload -U colors && colors
autoload -Uz compinit && compinit
autoload bashcompinit && bashcompinit
compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:*:*:*:*' menu yes select

# Prompt
setopt PROMPT_SUBST
PROMPT='%{%F{magenta}%}%9c%{%F{none}%}$(parse_git_branch)$'

# History
HISTFILE=~/.zsh/zhistory
HISTSIZE=10000
SAVEHIST=10000

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
