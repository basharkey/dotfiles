# This file exists as startx does not source bash aliases or functions
# https://www.reddit.com/r/archlinux/comments/8vklgh/how_to_startx_as_a_graphical_login_shell_to/

# User specific aliases and functions
if [ -d ~/.bashrc.d ]; then
    for rc in ~/.bashrc.d/*; do
	if [ -f "$rc" ]; then
	    . "$rc"
	fi
    done
    unset rc
fi

# System wide aliases and functions
for i in /etc/profile.d/*.sh /etc/profile.d/sh.local ; do
    if [ -r "$i" ]; then
        if [ "${-#*i}" != "$-" ]; then
            . "$i"
        else
            . "$i" >/dev/null
        fi
    fi
    unset i
done
