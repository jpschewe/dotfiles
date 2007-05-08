#!/bin/sh

mypath="`echo $0 | /bin/sed -e 's,[^/]*$,,;s,/$,,;s,^$,.,'`"
cd ${mypath}
mypath=$PWD

/bin/ln -sf "${mypath}/addpath" "${HOME}/.addpath"
/bin/ln -sf "${mypath}/bash_login" "${HOME}/.bash_login"
/bin/ln -sf "${mypath}/bash_logout" "${HOME}/.bash_logout"
/bin/ln -sf "${mypath}/bashrc" "${HOME}/.bashrc"
/bin/ln -sf "${mypath}/packages" "${HOME}/.packages"
/bin/ln -sf "${mypath}/profile" "${HOME}/.profile"

# ask about ssh
echo -n "Would you like to have ssh-agent start up on login? (y/N) "
answer=''
read answer
if [ "x${answer}" = "xy" ]; then
  /bin/mkdir -p "${HOME}/.ssh"
  /bin/ln -sf "${mypath}/sssha" "${HOME}/.ssh/sssha"
else
  /bin/rm -f "${HOME}/.ssh/sssha"
fi

# subversion
/bin/mkdir -p "${HOME}/.subversion"
if [ -f "${HOME}/.subversion/config" ]; then
  /bin/mv "${HOME}/.subversion/config" "${HOME}/.subversion/config.old"
fi
/bin/ln -fs "${mypath}/subversion-config" "${HOME}/.subversion/config"
