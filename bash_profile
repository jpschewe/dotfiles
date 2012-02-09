# Hey EMACS, this should be in -*- shell-script -*- mode
#
# FILE		.bash_login
# OVERVIEW	This startup is executed ONLY on bash _login_ shells
#

#echo "Top of bash_profile"

# From SUSE's default profile
test -z "$PROFILEREAD" && test -f /etc/profile && . /etc/profile

# Always load the interactive bash stuff first.
test -z "$BRC" && test -f "${HOME}/.bashrc" && . "${HOME}/.bashrc"

# be informative
echo -n "`hostname`($TERM):"; uptime

# setup terminal
case "$TERM" in 
  emacs)
  	# This session is connected to an emacs "shell buffer"; set the
	# DISPLAY and GNU_HOST environment vars to something useful
	case $HOSTTYPE in
	  sparc ) e_host=$(who am i | sed 's/.*(\([^\.)]*\).*)/\1/');;
	  hp*   ) e_host=$(who am i -R | sed 's/.*(\([^\.)]*\).*)/\1/');;
	  *     ) e_host=$(hostname);;
	esac
	#export DISPLAY="${e_host}:0"
	export GNU_HOST=$e_host
	unset e_host
	term="NO_CHANGE";;
  sun*)
  	# emacs works better with no insert characters on the console.
	term=sun-nic;;
  sparcbook)
	term="NO_CHANGE";;
  vt100)
	term=vt100;;
esac

term=$TERM
if [ -z "$term" ]; then
  # Hmmm... Must be a terminal or network connection.
  # Send the string "Terminal Type?" to the terminal, followed by a control-E
  # which triggers the terminal answer-back response, if any.
  echo ""
  echo "Unknown terminal type: ($TERM)"
  echo ""
  echo "(sun, xterm, wyse, vt100, other)"
#  echo -ne "Terminal Type: \005"
  echo -n "Terminal Type: "
  read term
  term=$(echo "$term" | tr A-Z a-z)
  case "$term" in
    s*)	term=sun-nic;;
    x*)	term=xterm;;
    v*)	term=vt100;;
    w*)	term=wyse-85;;
  esac
fi

#if [ "$term" != "NO_CHANGE" ]; then
#  export TERM=`tset - $term`	#-- setup TERMCAP/TERM entries
#  stty sane >/dev/null 2>&1
#fi

#-- If I'm on the console, do some special stuff
#if [ "$DT" ]; then
#  source ${HOME}/.bash_console
#  logout
#fi
#if [ "`tty`" = "/dev/console" -o "$DT" ]; then
#  source ${HOME}/.bash_console
#fi


#ssh specific stuff
#figure out where we're coming in from
#CLIENT=`echo ${SSH_CLIENT} | awk '{print $1}'`
#if [ "${CLIENT}" = "137.192.66.146" ]; then
#  #Do some fun keep alive stuff so the redirector stays up
#  echo "Coming in from eggplant.mtu.net"
#  echo "Keep alive?"
#  read prompt
#  if [ "${prompt}x" = "yx" ]; then
#    echo "Going into keepalive mode"
#	 while `/bin/true`; do
#	   sleep 300
#		echo "Keepalive :)"
#    done
#  fi
#fi

#eof .bash_login
