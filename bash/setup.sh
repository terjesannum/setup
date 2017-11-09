#!/usr/bin/env bash

INSTALL_DIR=$HOME
DIR=`cd $(dirname $0); pwd -P`
. $DIR/../lib/functions.sh

cp $DIR/profile $HOME/.profile
cp $DIR/bash_profile $HOME/.bash_profile
cp $DIR/bashrc $HOME/.bashrc
cp $DIR/inputrc $HOME/.inputrc

github_clone_or_pull $INSTALL_DIR/lib/bash/github.com \
                     https://github.com/cykerway/complete-alias.git

