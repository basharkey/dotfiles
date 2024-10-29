if [ -f "$(brew --prefix)/etc/bash_completion" ]; then
    . "$(brew --prefix)/etc/bash_completion"
fi

source <(kubectl completion bash)
alias k=kubectl
complete -o default -F __start_kubectl k
