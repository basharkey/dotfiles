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

exec dbus-launch --exit-with-session i3
