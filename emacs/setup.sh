#!/usr/bin/env bash

INSTALL_DIR=$HOME
DIR=`cd $(dirname $0); pwd -P`

if type tic >/dev/null 2>&1; then
  tic $DIR/dumb-emacs-ansi.ti
fi

. $DIR/../lib/functions.sh
mkdir -p $INSTALL_DIR/el
cp $DIR/*.el $INSTALL_DIR/el/
cat > $INSTALL_DIR/.emacs <<EOF
(defvar ts-emacs-dir "$INSTALL_DIR/el" "Directory with emacs setup")
(load-file (concat ts-emacs-dir "/init.el"))
EOF
github_clone_or_pull $INSTALL_DIR/el/github.com \
    https://github.com/magnars/dash.el.git \
    https://github.com/spotify/dockerfile-mode.git \
    https://github.com/dominikh/go-mode.el.git \
    https://github.com/mooz/js2-mode.git \
    https://github.com/Wilfred/logstash-conf.el.git \
    https://github.com/jrblevin/markdown-mode.git \
    https://github.com/puppetlabs/puppet-syntax-emacs.git \
    https://github.com/pashky/restclient.el.git \
    https://github.com/yoshiki/yaml-mode.git \
    https://github.com/nikolas/window-number \
    https://github.com/szermatt/emacs-bash-completion.git \
    https://github.com/purcell/exec-path-from-shell.git \
    https://github.com/terjesannum/cyberpunk-theme.el.git \
    https://github.com/terjesannum/emacs-kubectx-mode-line.git
