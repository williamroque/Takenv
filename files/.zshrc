export ZSH="/Users/jetblack/.oh-my-zsh"
ZSH_DISABLE_COMPFIX=true

ZSH_THEME="cdimascio-lambda"

plugins=(git zsh-syntax-highlighting) 

source $ZSH/oh-my-zsh.sh

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='vim'
else
    export EDITOR='nvim'
fi

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"
export HOMEBREW_GITHUB_API_TOKEN=27a1744305cfd7a7d58a16c2c7b9a0bbf90b1b22

# custom paths
export PATH=$HOME/.npm-global/bin:$PATH
export PATH=$PATH:$HOME/cbin

# custom aliases
alias openas='sh ~/.openas.sh'
alias vimrc='sh ~/.vimrc.sh'
alias backup='sh ~/.backup.sh'
alias cbackup='sh ~/.cbackup.sh'
alias rrepl="rustup run nightly-2016-08-01 ~/.cargo/bin/rusti"
alias preview="~/.preview.sh"
alias mds="cp ~/.mdstyle.css ."
alias getip="ifconfig | grep -E -o '([0-999]+\.){3}([0-999]+)' | sed -n 2p"
alias vox="sh ~/.vox.sh"
alias clock="sh ~/.clock.sh"
alias ss="cmatrix -s; exit"
alias cls="python -c 'print(\"\\x1bc\\033[3J\", end=\"\")'"
alias rm="trash"
alias breaks="python ~/.breaks.py"
alias brew="HOMEBREW_NO_AUTO_UPDATE=1 brew"
alias dispov="osascript ~/cbin/dispov.scpt"
alias gest="python ~/gestures.py"
alias sec="python ~/secure.py"
alias imgtxt="python ~/imgtxt.py"

function change() {
    git add .
    git commit -m $1
    git push
}

if [[ "$TERM" != 'eterm-color' ]]; then
    test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
fi

# Vi mode

bindkey -v

bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward

function zle-line-init zle-keymap-select {
    VIM_PROMPT="%{$fg_bold[yellow]%} [% NORMAL]%  %{$reset_color%}"
    RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/} $EPS1"
    zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select
#zle -N self-insert url-quote-magic
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

export KEYTIMEOUT=1

# smart arrow history
bindkey '^[[A' up-line-or-search                                                
bindkey '^[[B' down-line-or-search

export PATH="$PATH:/usr/local/bin"
export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"

export LC_ALL=en_US.UTF-8  
export LANG=en_US.UTF-8

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

if type ag &> /dev/null; then
    export FZF_DEFAULT_COMMAND='ag -p ~/.gitignore -g ""'
fi
