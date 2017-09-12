#!/bin/sh

INSTALL_DIR=$HOME
DIR=`cd $(dirname $0); pwd -P`

mkdir -p $INSTALL_DIR/el
ln -fs $DIR/init.el $INSTALL_DIR/el/init.el
cat > $INSTALL_DIR/.emacs <<EOF
(defvar ts-emacs-dir "$INSTALL_DIR/el" "Directory with emacs setup")
(load-file (concat ts-emacs-dir "/init.el"))
EOF
mkdir -p $INSTALL_DIR/el/github.com
for repo in \
    https://github.com/magnars/dash.el.git \
    https://github.com/spotify/dockerfile-mode.git \
    https://github.com/dominikh/go-mode.el.git \
    https://github.com/mooz/js2-mode.git \
    https://github.com/gongo/json-reformat.git \
    https://github.com/Wilfred/logstash-conf.el.git \
    https://github.com/jrblevin/markdown-mode.git \
    https://github.com/puppetlabs/puppet-syntax-emacs.git \
    https://github.com/pashky/restclient.el.git \
    https://github.com/yoshiki/yaml-mode.git \
    ; do
    repodir=$INSTALL_DIR/el/github.com/$(basename $repo .git)
    if test -d $repodir; then
        (cd $repodir; git pull)
    else
        (cd $INSTALL_DIR/el/github.com; git clone $repo)
    fi
done



    
