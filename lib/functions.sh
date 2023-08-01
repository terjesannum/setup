function github_clone_or_pull {
    repo=$1
    commit=$2
    dir=$HOME/.emacs.d/github.com
    mkdir -p $dir
    repodir=$dir/$(basename $repo .git)
    if test -d $repodir; then
        if test -z "$commit"; then
            (cd $repodir; git pull)
        fi
    else
        (cd $dir;
         git clone $repo;
         if test -n "$commit"; then cd $repodir && git checkout $commit; fi
        )
    fi
}
