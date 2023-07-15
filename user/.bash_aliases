# This file exists as startx does not source bash aliases or functions
# https://www.reddit.com/r/archlinux/comments/8vklgh/how_to_startx_as_a_graphical_login_shell_to/

# User specific aliases and functions
if [ -d ~/.bashrc.d ]; then
    for rc in ~/.bashrc.d/*; do
	if [ -f "$rc" ]; then
	    . "$rc"
	fi
    done
fi

# System wide aliases and functions
if [ -d /etc/profile.d ]; then
    for i in /etc/profile.d/*.sh; do
	if [ -r $i ]; then
	    . $i
	fi
    done
    unset i
fi
