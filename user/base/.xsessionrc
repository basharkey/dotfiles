# Allow use of virsh if user is a part of libvirt group
export LIBVIRT_DEFAULT_URI=qemu:///system

# Fix dissapearing mouse cursor for VMs (systems without GPU acceleration)
# export WLR_NO_HARDWARE_CURSORS=1

# GTK4 theme fix
export GTK_THEME=Adwaita-dark

# Fix Java apps such as OWASP ZAP only showing a blank screen
export _JAVA_AWT_WM_NONREPARENTING=1

# Set SSH agent socket env var for both the user session and systemd user session
# must be set for both as ssh-agent is run via systemd user service
export SSH_AUTH_SOCK=/run/user/$(id -u)/ssh-agent.socket
if [ -x "/usr/bin/dbus-update-activation-environment" ]; then
   dbus-update-activation-environment --systemd SSH_AUTH_SOCK
fi

# Disable X screensaver and powersaving
xset -dpms
xset s off

exec  i3
