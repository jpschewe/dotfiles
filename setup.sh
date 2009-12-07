#!/bin/sh

mypath="`echo $0 | sed -e 's,[^/]*$,,;s,/$,,;s,^$,.,'`"
cd ${mypath}
mypath=$PWD

# setup bin
mkdir -p "${HOME}/bin"
for script in musicbox; do
  ln -sf "${mypath}/${script}" "${HOME}/bin"
done

# bash
ln -sf "${mypath}/addpath" "${HOME}/.addpath"
ln -sf "${mypath}/bash_profile" "${HOME}/.bash_profile"
ln -sf "${mypath}/bash_logout" "${HOME}/.bash_logout"
ln -sf "${mypath}/bashrc" "${HOME}/.bashrc"
ln -sf "${mypath}/packages" "${HOME}/.packages"

# vim
ln -sf "${mypath}/vimrc" "${HOME}/.vimrc"
if [ -L "${HOME}/.vim" ]; then
  rm -f "${HOME}/.vim"
  ln -sf "${mypath}/vim" "${HOME}/.vim"
elif [ -f "${HOME}/.vim" ]; then
  echo "Expecting ${HOME}/.vim to be a symlink or a directory, not a file, skipping"
elif [ -d "${HOME}/.vim" ]; then
  echo "${HOME}/.vim already exists as a directory, moving to the side"
  mv "${HOME}/.vim" "${HOME}/.vim.old"
  ln -sf "${mypath}/vim" "${HOME}/.vim"
else
  ln -sf "${mypath}/vim" "${HOME}/.vim"
fi

# screen
ln -sf "${mypath}/screenrc" "${HOME}/.screenrc"

# subversion
mkdir -p "${HOME}/.subversion"
if [ -f "${HOME}/.subversion/config" ]; then
  /bin/mv "${HOME}/.subversion/config" "${HOME}/.subversion/config.old"
fi
ln -fs "${mypath}/subversion-config" "${HOME}/.subversion/config"



# ask about ssh
echo -n "Would you like to have ssh-agent start up on login? (y/N) "
answer=''
read answer
if [ "x${answer}" = "xy" ]; then
  mkdir -p "${HOME}/.ssh"
  ln -sf "${mypath}/sssha" "${HOME}/.ssh/sssha"
  ln -sf "${mypath}/sssha-helper" "${HOME}/.ssh/sssha-helper"
else
  rm -f "${HOME}/.ssh/sssha"
  rm -f "${HOME}/.ssh/sssha-helper"
fi
