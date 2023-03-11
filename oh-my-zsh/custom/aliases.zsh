# shortcuts to projects I often work on.
aa=~/git/hub/williamdemeo/agda-algebras  # (use, e.g., `cd $aa`)
h=~/git/hub/HASKELL/UDEMY/Haskell
org=~/git/hub/williamdemeo/org-projects/journal
fl=~/git/hub/williamdemeo/formal-ledger-specifications  # (use, e.g., `cd $fl`)


# Add an "alert" command to let us know when a long running command finished.
# Use it like this: `sleep 10; alert`
alias finished='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# For changing to frequently used directories:
# For identifying large sub directories
alias dumd='du --max-depth=1 -h'

# For removing extraneous files produced by LaTeX:
alias rmtex='\rm -rf *.out(N) *.aux(N) *.blg(N) *.log(N) *.toc(N) *.idx(N) *.lof(N) *.ilg(N) *.fdb_latexmk(N) *.fls(N) *.synctex.gz(N) *.blg(N) ./auto'

# For removing files produced by emacs:
alias rmt='\rm *~'

# For turning off/on trackpad on laptops
alias padoff='xinput set-prop 13 "Device Enabled" 0'
alias padon='xinput set-prop 13 "Device Enabled" 1'


# Safe delete, move, copy
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"
set -o noclobber

# Listing, directories, and motion
# some more ls aliases
alias lss='ls -lSha'
alias dir='ls --color=auto --format=vertical'
alias vdir='ls --color=auto --format=long'
alias cl='clear'
alias treeacl='tree -A -C -L 2'

# Text and editor commands
alias em='emacs -nw'     # No X11 windows
alias eqq='emacs -nw -Q' # No config and no X11

function setzoom() { gsettings set org.gnome.desktop.interface text-scaling-factor "$@"; }

# For mounting a Nexus 4; see http://youtu.be/fw2MKGIgyF4
# and http://www.omgubuntu.co.uk/2013/06/mount-nexus-4-on-ubuntu
#    alias mountnexus="sudo mtpfs -o allow_other /media/nexus4"
#    alias umountnexus="sudo umount /media/nexus4"
# No longer using this method. Instead, see:
# http://blog.sukria.net/2013/03/09/support-for-nexus-4-in-ubuntu-12-10/

alias play=paplay
alias tomtom="/usr/share/sounds/ubuntu/stereo/dialog-warning.ogg"
alias can="/usr/share/sounds/ubuntu/stereo/bell.ogg"
alias bongos="/usr/share/sounds/ubuntu/stereo/dialog-question.ogg"
alias popsound="/usr/share/sounds/ubuntu/stereo/message.ogg"
alias cupsound="/usr/share/sounds/ubuntu/stereo/dialog-information.ogg"

