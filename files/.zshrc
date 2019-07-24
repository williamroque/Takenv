export ZSH="/Users/williamroque/.oh-my-zsh"

ZSH_THEME="cdimascio-lambda"

# CASE_SENSITIVE="true"

# HYPHEN_INSENSITIVE="true"

# DISABLE_AUTO_UPDATE="true"

# export UPDATE_ZSH_DAYS=13

# DISABLE_LS_COLORS="true"

# DISABLE_AUTO_TITLE="true"

# ENABLE_CORRECTION="true"

COMPLETION_WAITING_DOTS="true"

# DISABLE_UNTRACKED_FILES_DIRTY="true"

# HIST_STAMPS="mm/dd/yyyy"

# ZSH_CUSTOM=/path/to/new-custom-folder

plugins=(git zsh-autosuggestions zsh-syntax-highlighting) 

source $ZSH/oh-my-zsh.sh

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='vim'
else
    export EDITOR='nvim'
fi

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"
export HOMEBREW_GITHUB_API_TOKEN=27a1744305cfd7a7d58a16c2c7b9a0bbf90b1b22

# Python path
export PATH=$PATH:/Library/Frameworks/Python.framework/Versions/3.7/bin

# Custom aliases
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

function change() {
    git add .
    git commit -m $1
    git push
}

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

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
export KEYTIMEOUT=1

# Smart arrow history
bindkey '^[[A' up-line-or-search                                                
bindkey '^[[B' down-line-or-search

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
