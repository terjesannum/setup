function github_clone_or_pull {
    dir=$1
    repo=$2
    commit=$3
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
