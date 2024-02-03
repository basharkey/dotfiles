# Don't cycle through tab completion entries just list them
setopt NO_AUTO_MENU

# Fix dirtrack in emacs
# https://stackoverflow.com/questions/3508387/how-can-i-have-term-el-ansi-term-track-directories-if-using-anyhting-other-tha
if [ -n "$INSIDE_EMACS" ]; then
  chpwd() { print -P "\033AnSiTc %d" }
  print -P "\033AnSiTu %n"
  print -P "\033AnSiTc %d"
fi
