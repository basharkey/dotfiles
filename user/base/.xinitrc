# Allow use of virsh if user is a part of libvirt group
export LIBVIRT_DEFAULT_URI=qemu:///system

# Set Qt theme to follow qt5ct
export QT_QPA_PLATFORMTHEME=qt5ct

# Fix dissapearing mouse cursor for VMs (systems without GPU acceleration)
# export WLR_NO_HARDWARE_CURSORS=1

# GTK4 theme fix
export GTK_THEME=Adwaita-dark

# Fix Java apps such as OWASP ZAP only showing a blank screen
export _JAVA_AWT_WM_NONREPARENTING=1

# SSH agent
export SSH_AUTH_SOCK=/run/user/$(id -u)/ssh-agent.socket

# Manually set dbus as startx unsets it
export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/$(id -u)/bus

if command -v dbus-update-activation-environment >/dev/null 2>&1; then
   dbus-update-activation-environment DISPLAY XAUTHORITY SSH_AUTH_SOCK
fi

# Disable X screensaver and powersaving
xset -dpms
xset s off

exec  i3
