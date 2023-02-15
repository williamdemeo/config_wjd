#!/bin/bash
# FILE: setup.sh
# BLAME: <williamdemeo@gmail.com>
# DESCR: script for configuring Ubuntu 18.04 LTS EC2 instance (e.g., for a headless setup).
# DATE: 14 Jul 2013
# UPDATES: 24 Mar 2020, 27 Jun 2014, 14 Jul 2013
echo
echo 'This script will install/configure a Ubuntu Linux box the way I like it.'
echo " "
SCRIPT_HOME="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
echo " "
echo "(This script was called from the directory " $SCRIPT_HOME " )"
echo " "
echo 'Here is a summary of what will be installed:'
echo
echo "1. Miscellaneous libraries/utilities (e.g., git ghc cabal-install alex happy"
echo "                                      xz-utils libgtk-3-dev libxpm-dev libjpeg-dev"
echo "                                      libgif-dev libtiff-dev libgnutls28-dev)"
echo "2. Emacs text editor/ide"
echo "3. Optionally, the following may also be installed:"
echo "   3.1. Custom dot files (.profile, .bashrc, .bash\_aliases, .bash\_profile, .bashrc\_custom, .screenrc, .emacs)."
echo "   3.2. Java"
echo "   3.3. Scala"
echo "   3.4. IntelliJ"
echo "   3.5. Proof General (for doing type theory and constructive math in emacs)"
echo "   3.6. A popular proof assistant"
echo " "
echo "Here are one-line descriptions of packages that will be installed if you continue:"
apt-cache show git ghc cabal-install alex happy emacs??-el emacs??-common emacs-goodies-el emacs-window-layout emacs-common-non-dfsg rlwrap | egrep 'Package|Description-en'
apt-cache show xz-utils libgtk-3-dev libxpm-dev libjpeg-dev libgif-dev libtiff-dev libgnutls28-dev gnutls-doc gnutls-bin  | egrep 'Package|Description-en'
read -p 'Abort this setup script? [Y/n]' -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]];then
  echo
  echo 'Setup aborted.'
  echo
  exit
fi
echo
echo "Okay, here we go..."
echo "###############################################################################"
echo
echo '    1.  Miscellaneous utilities'
echo
sudo apt install -y libxpm-dev libjpeg-dev libgif-dev libtiff-dev libgnutls28-dev gnutls-doc gnutls-bin
sudo apt install -y xz-utils libgtk-3-dev git ghc cabal-install alex happy
echo

echo '###############################################################################'
echo
echo '    2.  emacs editor and some add-ons'
echo
sudo apt install -y emacs emacs??-el emacs??-common emacs-goodies-el emacs-window-layout emacs-common-non-dfsg
echo
EMACS_HOME=$SCRIPT_HOME'/emacs.d'
sudo apt install -y rlwrap
ln -sb --suffix='.orig' $EMACS_HOME $HOME/.emacs.d
ln -sb --suffix='.orig' $EMACS_HOME/init.el $HOME/.emacs

echo '###############################################################################'
echo
echo '    3.  spacemacs editor'
echo
read -p '     Would you like to download/install spacemacs? [Y/n]' -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]; then
  SPACEMACS_HOME=$HOME/.spacemacs.d
  echo
  echo '         Installing spacemacs...'
  echo
  git clone https://github.com/syl20bnr/spacemacs $SPACEMACS_HOME
  ln -sb --suffix='.orig' $EMACS_HOME/lisp/spacemacs.el $HOME/.spacemacs
  echo
else
	echo
	echo '      ...ok, skipping spacemacs setup.'
	echo
fi
echo ' '
echo
echo '###############################################################################'
echo
echo '    3.  custom_dotfiles (configuration files)'
echo
DOT_HOME=$SCRIPT_HOME'/bash.d'
ln -sb --suffix='.orig' $DOT_HOME/screenrc ~/.screenrc
ln -sb --suffix='.orig' $DOT_HOME/bashrc ~/.bashrc
ln -sb --suffix='.orig' $DOT_HOME/bashrc_custom ~/.bashrc_custom
ln -sb --suffix='.orig' $DOT_HOME/bash_profile ~/.bash_profile
ln -sb --suffix='.orig' $DOT_HOME/bash_aliases ~/.bash_aliases
ln -sb --suffix='.orig' $DOT_HOME/profile ~/.profile

echo
echo '###############################################################################'
echo ' '
echo '    4. Java JDK, Scala Build Tool (sbt), Eclipse (Optional)'
echo
echo
echo "       4.1 Java JDK..."
echo
source
echo
echo "       4.3 IntelliJ..."
echo
if type -p idea; then
  echo '         Found idea executable in PATH.'
  _idea=idea
else
  echo
  read -p '         No IntelliJ found. Install it? [Y/n]' -n 1 -r
  if [[ $REPLY =~ ^[Yy]$ ]]; then
    mkdir -p $HOME/opt
    mkdir -p $HOME/opt/JETBRAINS
    cd $HOME/opt/JETBRAINS
    wget -N https://download.jetbrains.com/idea/ideaIC-2017.3.3.tar.gz
	  tar xvzf ideaIC-2017.3.3.tar.gz
	  ln -sb --suffix='.orig' $HOME/opt/JETBRAINS/idea-IC-*/bin/idea.sh $HOME/bin/idea
	  echo
	  echo "     --------------------------------------------------------"
	  echo "     |   RUN IntelliJ IDE with the command $HOME/bin/idea   |"
	  echo "     --------------------------------------------------------"
  else
	echo
	echo '      ...ok, skipping IntelliJ setup.'
	echo
  fi
fi
echo
echo '###############################################################################'
echo
echo '    6. Proof General, Lean, Coq'
echo ' '
read -p '       6.1 Would you like to install Proof General? [Y/n]' -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]; then
  echo
  echo '         Installing Proof General...'
  echo
  sudo apt install -y proofgeneral
  echo
else
	echo
	echo '      ...ok, skipping Proof General setup.'
	echo
fi
echo ' '
read -p '       6.2 Would you like to install the Lean proof assistant? [Y/n]' -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]; then
  echo
  echo '         Installing Lean...'
  echo
  mkdir -p $HOME/opt
  mkdir -p $HOME/opt/Lean
  cd $HOME/opt/Lean
  wget -N https://leanprover.github.io/lean-nightly/build/lean-nightly-linux.tar.gz
  tar xvzf lean-nightly-linux.tar.gz
  ln -sb --suffix='.orig' $HOME/opt/Lean/lean-nightly-linux/bin/lean $HOME/bin/lean
  echo
  echo "     ------------------------------------------------"
  echo "     |   RUN Lean with the command $HOME/bin/lean   |"
  echo "     ------------------------------------------------"
else
	echo
	echo '      ...ok, skipping Lean setup.'
	echo
fi
echo ' '
read -p '       6.3 Would you like to install the Coq proof assistant? [Y/n]' -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]; then
  echo
  echo '         Installing Coq...'
  echo
  sudo apt install -y coq
  echo
  echo
else
	echo
	echo '      ...ok, skipping Coq setup.'
	echo
fi
echo
echo '    Configuration is complete.'
echo
echo '    You can post comments, questions, or feedback in a comment box at:'
echo '    http://williamdemeo.org.'
echo

#### OLD OBSOLETE STUFF ##################################################################
#
# If you want this script to install node, jshint, and rlwrap, uncomment the lines below.
#
#
###############################################################################
# echo
# echo '    1.  node (server side JavaScript), and the node version manager (nvm)'
# echo
# # Install nvm: node-version manager
# # https://github.com/creationix/nvm
# git clone https://github.com/creationix/nvm.git ~/.nvm
# echo
# # Load nvm and install latest production node
# source $HOME/.nvm/nvm.sh
# nvm install v0.10.12
# nvm use v0.10.12

###############################################################################
# echo
# echo '    2.  jshint (allows checking JS code within emacs; see http://jshint.com/)'
# echo
# # Install jshint to allow checking of JS code within emacs
# # http://jshint.com/
# npm install -g jshint

###############################################################################
# echo
# echo '    3.  rlwrap (libreadline features for node; see: http://nodejs.org/api/repl.html)'
# echo
# # Install rlwrap to provide libreadline features with node
# # See: http://nodejs.org/api/repl.html#repl_repl
# sudo apt install -y rlwrap
