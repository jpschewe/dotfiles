# .profile is read for all login shells
# all other interactive shells will read .bashrc
# So read .bashrc also from .profile and make all changes to .bashrc.
# Then you should always have your correct setup.

if [ -z "$PROFILEREAD" ]; then
  . /etc/profile

  if [ -f ~/.bashrc -a -z "$BASHRCREAD" ]; then
    . ~/.bashrc
  fi
  
  #
  # some people don't like fortune.  If you have humor, please enable it by
  # uncommenting the following lines.
  #
  
  if [ -x /usr/bin/fortune ] ; then
      echo
      /usr/bin/fortune
      echo
  fi
  
  #
  # Avoid loops and such
  #
  readonly PROFILEREAD=true
  export PROFILEREAD
fi