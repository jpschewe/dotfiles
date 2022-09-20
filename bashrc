#!/bin/bash      # Hey EMACS, this should be in -*- shell-script -*- mode
#
# FILE		.bashrc 
# OVERVIEW	This startup is executed every time a bash is executed.
#

#FIXME: this isn't working yet
#if [ -n "$BASHRCREAD" ]; then
#  return 0
#fi

#echo "Top of bashrc"

test -z "$UID"  &&  UID=`id -ur 2> /dev/null`
test -z "$EUID" && EUID=`id -u  2> /dev/null`
test -z "$USER" && USER=`id -un 2> /dev/null`
test -z "$HOST" && HOST=`hostname 2> /dev/null`
test -z "$CPU"  &&  CPU=`uname -m 2> /dev/null`
test -z "$HOSTNAME" && HOSTNAME=${HOST}
test -z "$LOGNAME"  && LOGNAME=${USER}
if [ -z "$CPU" ]; then
  case "$CPU" in
    i?86) HOSTTYPE=i386   ;;
    *)    HOSTTYPE=${CPU} ;;
  esac
fi

if [ -z "$OSTYPE" ]; then
  OSTYPE=`uname -s 2> /dev/null | tr A-Z a-z`
fi
# I added OSDIST since suse hardcoded MACHTYPE to be ${CPU}-suse-${OSTYPE}
if [ -z "$OSDIST" ]; then
  if [ -f /etc/fedora-release -o -f /etc/redhat-release ]; then
    OSDIST="redhat"
  elif [ -f /etc/SuSE-release ]; then 
    OSDIST="suse"
  elif [ -f /etc/debian_version ]; then 
    OSDIST="debian"
  else 
    OSDIST="$OSTYPE"
  fi
fi
if [ -z "$MACHTYPE" ]; then
  if [ "$OSDIST" = "$OSTYPE" ]; then 
    MACHTYPE=${CPU}-${OSTYPE}
  else
    MACHTYPE=${CPU}-${OSDIST}-${OSTYPE}
  fi
fi
# Do NOT export UID, EUID, USER, MAIL, and LOGNAME
export HOST CPU HOSTNAME HOSTTYPE OSTYPE MACHTYPE OSDIST

if [ $OSTYPE = "cygwin" ]; then
v  # set in NT directly export CYGWIN="tty nosmbntsec"
  # title doesn't seem to help either, binmode will break getenv in emacs, ntea will break permissions for ls
  # set to server to run postgres 
  #alias uptime='finger @localhost | grep UpTime'
  export SHELL="/bin/bash"
  export TMP="/tmp"
  export TEMP="/tmp"
fi


#now actually build my path
# then load packages locations
source "${HOME}/.addpath"
source "${HOME}/.packages"

#------------------------------
# BASH Environment options
#------------------------------
# Set umask to group-write IFF home directory is in my default group
# This might work for places where I DO and DON'T want group-write.
if [ -G "${HOME}" ]; then
  umask 022 # u+rw, og+r
else
  umask 002 # ug+rw, o+r
fi

export IGNOREEOF=0
export FIGNORE=.o:\~:.bak:.class
export HISTFILE=~/.bash_history
export HISTSIZE=500
export command_oriented_history=1
export history_control=ignoredups

notify=
unset MAILPATH MAILCHECK
unset MAIL

if [ -n "$(command -v vim)" ]; then
  #export EDITOR='vim -u ~/.vimrc'
    export EDITOR=vim
else
    export EDITOR=vi
fi


# turn on core files
ulimit -c unlimited

#-------------------------------
# GENERAL ENVIRONMENT VARIALBLES
#-------------------------------
#export GNU_HOST='grinch.htc.honeywell.com'
#export GNU_SECURE="${HOME}/.gnu_hosts"

#if [ -x /usr/share/vim/vim72/macros/less.sh ]; then
#  PAGER=/usr/share/vim/vim72/macros/less.sh
#  alias less=${PAGER}
#elif [ -x "$(command -v less)" ]; then
if [ -x "$(command -v less)" ]; then
  PAGER="less"
  alias less='less -is'
else
  PAGER="more"
fi

#export DoMEPrintFormats=${HOME}/.domePrintFormats
#export DoMEUserFunctions=${HOME}/.domeUserFunctions

#------------------------------
# ALIASES & one-line FUNCTIONS
#------------------------------

BRC="$HOME/.bashrc"	# also used for test in .profile - don't export this
#alias brc=". '$BRC'"
alias brc='unset BASHRCREAD ; . "${HOME}/.bashrc"'

# other stuff here
alias restart-tomcat='(cd / && sudo /etc/init.d/tomcat restart)'
#alias BG='(exec $* &) &'
alias cwd='builtin pwd'
alias clean='/bin/rm -f *~ *.~* .*~ *% core &>/dev/null'
alias eclean='/bin/rm -f *~ *.~* .*~ *% \#* .?E?m?A?c?S?l?O?c?K?.* &>/dev/null'
alias fmclean='/bin/rm -f *.{auto,backup,lck,recover} &>/dev/null'
alias texclean='/bin/rm -f *.{aux,dvi,log} &>/dev/null'
alias h='history 15'
#alias less='less -is'
#alias lf='ls -F'
#alias la='ls -Fa'
#alias ll='ls -Fl'
alias cp='cp -i'
alias mv='mv -i'
#alias w='w -s'
alias which='type'
alias where='type -all'

#alias rdesktop='\rdesktop -g 1024x768 -K'
#alias mn65-rsconsole='rdesktop -u getreal mn65-rsconsole'

#my aliases
alias ls='ls -F'

ls -hl > /dev/null 2>&1
if [ $? == 0 ]; then
  alias ll='ls -hl'
else
  alias ll='ls -l'
fi

# check if -h is supported by df
df_opts=""
command df -h / > /dev/null 2>&1
if [ $? == 0 ]; then
  df_opts="-h"
fi

# remove some things from df output
df() {
    command df ${df_opts} $* \
        | grep -v " /snap" \
        | grep -v " /run" \
        | grep -v " /sys" \
        | grep -v " /dev"
}

alias v='ll'
alias d='ls -s'
alias bc='bc -l'
alias lsdir='ls -lA | grep "^d"'
alias lsfile='ls -lA | egrep -v "^d|^l"'
alias lslink='ls -lA | grep "^l"'
alias en='enscript -2r -b"- jschewe -" -G'
#alias en='enscript -2rG'
#alias en1='enscript -1rG'

alias faketop='ps auwx | sort -n -k 3'

#alias = 'echo $TOTAL;'
#alias == 'set TOTAL=\!$;'
#alias + '@ TOTAL+=\!$;'
#alias - '@ TOTAL-=\!$;'
#alias / '@ TOTAL/=\!$;'
#alias * '@ TOTAL*=\!$;'

# Sort on last alpha field
# - see http://www.perl.org/CPAN/doc/FMTEYEWTK/sort.html
#alias psort='perl -0000 -ne \'print ((join "\n", map { $_->[0] } sort { $a->[1] cmp $b->[1] } map {[$_, (split)[-1]] } (split /\n/, $_)) . "\n")\''


#------------------------------
# Multi-line/complex FUNCTIONS
#------------------------------

# CVS functions
cvsstat() { 
  cvs -n update $* 2>&1 | egrep -v '^cvs update:'; 
}

keepalive() {
  echo "Going into keepalive mode"
  while `/bin/true`; do
    sleep 300
    echo "Keepalive :)"
  done
}

# Modified from SuSE
#startx ()  {
#  /usr/X11R6/bin/startx ${1+"$@"} 2>&1 | tee "${HOME}/.X-${HOST}.err"
#}


# directory management
pd() {
  case "$1" in
  0) ;;
  [1-9]*) pushd +$1 >/dev/null ;;
  *) pushd "$1" >/dev/null ;;
  esac
}

pdirs() {
    dirs -l -v
}

sd() {
  local num
  case "$1" in
  "") pdirs; return ;;
  [0-9]*) pd $1; pdirs; return $?;;
  esac

  num=$(pdirs | grep -E "${1}" | awk '{print $1}')
  case "$num" in
  0) ;;
  "")
    if pushd "$1" >/dev/null 2>&1; then
      pdirs
      return
    fi
    echo "Directory $1 not found" >&2
    pdirs
    return 1
    ;;
  *)
    pd $num
    pdirs
    ;;
  esac
}


# figure out by which name netcat goes by
if [ -n "$(command -v nc)" -a -z "$(command -v netcat)" ]; then
  alias netcat=nc
elif [ -n "$(command -v netcat)" -a -z "$(command -v nc)" ]; then
  alias nc=netcat
fi


# determine which open to use
if [ -z "$(command -v open)" ]; then
    if [ -n "$(command -v xdg-open)" ]; then
        alias open=xdg-open
    fi
fi

# aliases for rm to use trash, if available
#if [ -n "$(command -v trash-put)" ]; then
#  alias rm=trash-put
#elif [ -n "$(command -v trash)" ]; then
#  alias rm=trash
#else 
#  alias rm='rm -i'
#fi

# ------------------
# some terminal stuff
# ------------------
if [ $TERM = "linux" ]; then
   export TERM=vt102
fi

if [ $TERM = xterm ]; then
  BASE_PS1="\u@\h:\w\[\033]0;\u@\h\007\]";
elif [ $TERM = xterm-color ]; then
  BASE_PS1="\u@\h:\w\[\033]0;\u@\h\007\]";
elif [ $TERM = "sun-nic" ]; then
  alias ls='ls -F'
  BASE_PS1="\u@\h:\w";
elif [ $TERM = "sun" ]; then
  alias ls='ls -F'
  BASE_PS1="\u@\h:\w";
elif [ $TERM = "screen" ]; then
  # do screen stuff here
  BASE_PS1="\u@\h:\w\[\033]0;\u@\h[$WINDOW]\007\]";
else 
  BASE_PS1="\u@\h:\w";
fi
BASE_PS1='\n$(dirs)\n'${BASE_PS1}

# custom completion methods
. ${HOME}/dotfiles/git-completion.sh
if [ -n "$(command -v git)" ]; then
  # TODO figure out a more robust way to find this script
  # This one really slows things down
  #GIT_PS1_SHOWDIRTYSTATE=true
  #export GIT_PS1_SHOWDIRTYSTATE

  # not really worth it without the above one
  #GIT_PS1_SHOWSTASHSTATE=true
  #export GIT_PS1_SHOWSTASHSTATE
  #GIT_PS1_SHOWUNTRACKEDFILES=true
  #export GIT_PS1_SHOWUNTRACKEDFILES
  
  PS1=${BASE_PS1}'\n$(__git_ps1 "[%s] ")>'
else
  PS1="${BASE_PS1}\n>"
fi
export PS1

if [ -n "$EMACS" -o "$TERM" = "emacs" ]; then
  export PAGER=cat
  alias ls='ls -F'
  alias more='cat'
  alias less='cat'
  PS1="\u@\h:\w\n>";
  export EDITOR=gnuclient
fi

# perl - this may be wrong - need to play with it for a while.
if [ $OSTYPE = "cygwin" ]; then
  # could also want this to be "raw"
  export PERLIO="perlio"
fi

MANPATH1=`substpath PATH bin man`
MANPATH2=`substpath PATH bin share/man`
if [ -z "${MANPATH1}" ]; then
  MANPATH=${MANPATH2}
else
  if [ -z "${MANPATH2}" ]; then
    MANPATH=${MANPATH1}
  else
    MANPATH=${MANPATH1}:${MANPATH2}
  fi
fi

export MANPATH
case $OSTYPE in
  linux | darwin*) 
     append MANPATH /usr/share/man
     ;;
  sunos4 | solaris2)
     ### gnu-misc
     append MANPATH ${GNU_MISC_DIR}/share/man
     ;;
  *)
     ;;
esac

INFOPATH=`substpath PATH bin info`
export INFOPATH
case $OSTYPE in
  linux) 
     append INFOPATH /usr/share/info
     ;;
  *)
     ;;
esac

# X-Windows
if [ -d "$X11DIR" ]; then
  export X11INC_DIR=${X11DIR}/include
  export X11LIB_DIR=${X11DIR}/lib
  if [ -z "$DISPLAY" ]; then
    export DISPLAY="`who -m --lookup | sed -e 's/.*(\(.*\))/\1/'`:0.0"
    if [ -z "$DISPLAY" ]; then
      export DISPLAY=":0.0"
    fi
  fi
fi

# put times in the history log
export HISTTIMEFORMAT="%h/%d - %H:%M:%S "

# favorite is listed last
for ask_pass in \
    /usr/lib64/ssh/ssh-askpass \
    /usr/lib/ssh/ssh-askpass \
    /usr/bin/ssh-askpass \
    /usr/lib64/ssh/gnome-ssh-askpass \
    /usr/lib/ssh/gnome-ssh-askpass \
    /usr/lib64/ssh/ksshaskpass
  do
  if [ -e "${ask_pass}" ]; then
        SSH_ASKPASS=${ask_pass}
    fi
done
if [ -n "${SSH_ASKPASS}" ]; then
    export SSH_ASKPASS
fi


if [ -e "${HOME}/.ssh/setup-ssh-agent" ]; then
    #log_debug=true
    . "${HOME}/.ssh/setup-ssh-agent"
fi

# from http://wiki.tcl.tk/1373
function music () {
  case $1 in
    on)
        if [ -e "${HOME}/.music" ]; then
          if [ `pgrep -U ${UID} -f musicbox` ]; then
            #echo "Musicbox already running, ignoring command"
            music cont
          else
            #No musicbox running, start it.
            mkdir -p "${HOME}/.musicbox"
            musicbox > /dev/null &
          fi
        else
          echo "Musicbox won't work without ${HOME}/.music existing and pointing to a directory of music"
        fi

        ;;
    off)
         pkill -U $UID -f musicbox
         pkill -U $UID -f ogg123
         ;;
    pause)
           pkill -STOP -U $UID musicbox
           pkill -STOP -U $UID ogg123
           ;;
    cont)
          if [ `pgrep -U ${UID} -f musicbox` ]; then
            pkill -CONT -U $UID -f musicbox
            pkill -CONT -U $UID -f ogg123
          else
            music on
          fi
          ;;
    #    stat)   here=`pwd`
    #          cd "${HOME}/.mydata/CDs
    #          find . -name '*.m3u' -print | wc -l | xargs echo '#Albums = '
    #          find . -name '*.ogg' -print | wc -l | xargs echo '#Songs  = '
    #          cd $here
    #          ;;
    played)
            if [ -r $HOME/.musicboxrc ]; then
              echo 'source ~/.musicboxrc ;puts "Played: [array size played]"; exit' | tclsh
            else
              echo "Nothing played"
            fi
            ;;
#    vol)    rexima -v | grep pcm
#         ;;
    clear)
           rm -f $HOME/.musicboxrc
           rm -f $HOME/.musicboxrc.new
           rm -f $HOME/.musicboxrc.old
           ;;
    *)
       # Assume a number and use it to control the mixer
       #rexima pcm $1
       echo "Usage: music {on|off|pause|cont|played|clear}"
       ;;
  esac
}

# bash completion
if [ -f /opt/local/etc/bash_completion ]; then
    . /opt/local/etc/bash_completion
fi

# get our path to launched applications
case $OSTYPE in
    darwin*)
        if [ $TERM = "xterm" ]; then
            launchctl setenv PATH "${PATH}"
        fi
        ;;
esac

# virtualenvwrapper setup
VENV_WRAPPER_SCRIPT=""
if [ -e /usr/local/brew/bin/virtualenvwrapper.sh ]; then
    VENV_WRAPPER_SCRIPT=/usr/local/brew/bin/virtualenvwrapper.sh

    # mac needs to use python3
    VIRTUALENVWRAPPER_PYTHON=/usr/local/brew/bin/python3
    export VIRTUALENVWRAPPER_PYTHON
elif [ -e /usr/share/virtualenvwrapper/virtualenvwrapper.sh ]; then
    VENV_WRAPPER_SCRIPT=/usr/share/virtualenvwrapper/virtualenvwrapper.sh
fi

if [ -n "${VENV_WRAPPER_SCRIPT}" ]; then
  WORKON_HOME=${HOME}/py-envs
  export WORKON_HOME
  . "${VENV_WRAPPER_SCRIPT}"
fi

## tmux helpers

# Change the current directory for a tmux session, which determines
# the starting dir for new windows/panes:
function tmux-cwd {
    tmux command-prompt -I $PWD -p "New session dir:" "attach -c %1"
}

#
# Avoid loops and such
#
BASHRCREAD=true
export BASHRCREAD

