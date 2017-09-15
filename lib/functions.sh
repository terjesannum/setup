function github_clone_or_pull {
    dir=$1
    mkdir -p $dir
    shift
    for repo in $*; do
        repodir=$dir/$(basename $repo .git)
        if test -d $repodir; then
            (cd $repodir; git pull)
        else
            (cd $dir; git clone $repo)
        fi
    done
}
