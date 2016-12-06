#!/bin/bash

# Installs sysconfcpus so we can limit the cpus that elm uses to compile. 
# Surprisingly, this makes elm compile much faster. 
# See: https://github.com/elm-lang/elm-compiler/issues/1473#issuecomment-245704142 

export INSTALL_PATH="$HOME/dependencies"

if [ ! -d $INSTALL_PATH/sysconfcpus/bin ]; then
  git clone https://github.com/obmarg/libsysconfcpus.git
  cd libsysconfcpus
  ./configure --prefix=$INSTALL_PATH/sysconfcpus
  make && make install
  cd ..
fi
