#!/usr/bin/env bash

DIR=`cd $(dirname $0); pwd -P`
. $DIR/../lib/functions.sh

mkdir -p $HOME/.emacs.d/github.com
ln -fs $DIR/init.el $HOME/.emacs.d/init.el

github_clone_or_pull $HOME/.emacs.d/github.com \
    https://github.com/terjesannum/cyberpunk-theme.el.git \
    https://github.com/terjesannum/emacs-gcloud-mode.git \
    https://github.com/terjesannum/emacs-kubectx-mode.git \
    https://github.com/terjesannum/emacs-influxdb-mode.git \
    https://github.com/terjesannum/emacs-shell.git \
    https://github.com/jwiegley/use-package.git
