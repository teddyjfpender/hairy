# .devcontainer/.bashrc
# Sourced by /root/.bashrc in the dev container

# Enable colorized ls
export LS_OPTIONS='--color=auto'
eval "`dircolors`"
alias ls='ls $LS_OPTIONS'
alias ll='ls $LS_OPTIONS -l'
alias l='ls $LS_OPTIONS -lA'

# Common aliases to avoid mistakes
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

# Alias for cabal with grc
# We'll uncomment this once grc is installed and configured
alias cabal='grc -c /root/.grc/cabal cabal'

# Custom PS1 (optional, example)
PS1="\\[\\033[01;32m\\]\\u@\\h\\[\\033[00m\\]:\\[\\033[01;34m\\]\\w\\[\\033[00m\\]\\$ "

# Source GRC bash completion/setup if available
if [ -f /etc/bash_completion.d/grc ]; then
    . /etc/bash_completion.d/grc
elif [ -f /usr/share/bash-completion/completions/grc ]; then
    . /usr/share/bash-completion/completions/grc
fi 