autoload -Uz compinit
compinit

alias k=kubectl
[[ $commands[kubectl] ]] && source <(kubectl completion zsh) # add autocomplete permanently to your zsh shell

# Don't cycle through tab completion entries just list them
setopt NO_AUTO_MENU

# Fix Emacs term dirtrack
# https://stackoverflow.com/questions/3508387/how-can-i-have-term-el-ansi-term-track-directories-if-using-anyhting-other-tha
if [ -n "$INSIDE_EMACS" ]; then
  chpwd() { print -P "\033AnSiTc %d" }
  print -P "\033AnSiTu %n"
  print -P "\033AnSiTc %d"
fi

# Fix Emacs term creating extra prompts
# https://superuser.com/questions/645599/why-is-a-percent-sign-appearing-before-each-prompt-on-zsh-in-windows
unsetopt PROMPT_SP
