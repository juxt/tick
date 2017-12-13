#!/bin/bash

# Keep a separate branch of generated API docs.
#
# This script generates API documentation, commits it to a separate branch, and
# pushes it upstream. It does this without actually checking out the branch,
# using a separate working tree directory, so without any disruption to your
# current working tree. You can have local file modifications, but the git index
# (staging area) must be clean.

############################################################
# These variables can all be overridden from the command line,
# e.g. AUTODOC_REMOTE=plexus ./generate_docs

# The git remote to fetch and push to. Also used to find the parent commit.
AUTODOC_REMOTE=${AUTODOC_REMOTE:-"origin"}

# Branch name to commit and push to
AUTODOC_BRANCH=${AUTODOC_BRANCH:-"gh-pages"}

# Command that generates the API docs
#AUTODOC_CMD=${AUTODOC_CMD:-"lein with-profile +codox codox"}
#AUTODOC_CMD=${AUTODOC_CMD:-"boot codox -s src -n my-project -o gh-pages target"}

# Working tree directory. The output of $AUTODOC_CMD must end up in this directory.
AUTODOC_DIR=${AUTODOC_DIR:-"gh-pages"}

############################################################

function echo_info() {
   echo -en "[\033[0;32mautodoc\033[0m] "
   echo $*
}

function echo_error() {
   echo -en "[\033[0;31mautodoc\033[0m] "
   echo $*
}

if [[ -z "$AUTODOC_CMD" ]]; then
    echo_error "Please specify a AUTODOC_CMD, e.g. lein codox"
    exit 1
fi

if ! git diff-index --quiet --cached HEAD ; then
    echo_error "Git index isn't clean. Make sure you have no staged changes. (try 'git reset .')"
    exit 1
fi

VERSION=0019

echo "//======================================\\\\"
echo "||          AUTODOC v${VERSION}               ||"
echo "\\\\======================================//"

MESSAGE="Updating docs based on $(git rev-parse --abbrev-ref HEAD) $(git rev-parse HEAD)

Ran: $AUTODOC_CMD
"

if [[ ! -z "$(git status --porcelain)" ]]; then
  MESSAGE="$MESSAGE
Repo not clean.

    Status:
$(git status --short)

    Diff:
$(git diff)"
fi

# Fetch the remote, we don't care about local branches, only about what's
# currently on the remote
git fetch $AUTODOC_REMOTE

# Start from a clean slate, we only commit the new output of AUTODOC_CMD, nothing else.
rm -rf $AUTODOC_DIR
mkdir -p $AUTODOC_DIR

echo_info "Generating docs"
echo $AUTODOC_CMD | bash

AUTODOC_RESULT=$?

if [[ ! $AUTODOC_RESULT -eq 0 ]]; then
    echo_error "The command '${AUTODOC_CMD}' returned a non-zero exit status (${AUTODOC_RESULT}), giving up."
    exit $AUTODOC_RESULT
fi

if [[ $(find $AUTODOC_DIR -maxdepth 0 -type d -empty 2>/dev/null) ]]; then
    echo_error "The command '$AUTODOC_CMD' created no output in '$AUTODOC_DIR', giving up"
    exit 1
fi

# The full output of AUTODOC_CMD is added to the git index, staged to become a new
# file tree+commit
echo_info "Adding file to git index"
git --work-tree=$AUTODOC_DIR add -A

# Create a git tree object with the exact contents of $AUTODOC_DIR (the output of
# the AUTODOC_CMD), this will be file tree of the new commit that's being created.
TREE=`git write-tree`
echo_info "Created git tree $TREE"

# Create the new commit, either with the previous remote HEAD as parent, or as a
# new orphan commit
if git show-ref --quiet --verify "refs/remotes/${AUTODOC_REMOTE}/${AUTODOC_BRANCH}" ; then
    PARENT=`git rev-parse ${AUTODOC_REMOTE}/${AUTODOC_BRANCH}`
    echo "Creating commit with parent refs/remotes/${AUTODOC_REMOTE}/${AUTODOC_BRANCH} ${PARENT}"
    COMMIT=$(git commit-tree -p $PARENT $TREE -m "$MESSAGE")
else
    echo "Creating first commit of the branch"
    COMMIT=$(git commit-tree $TREE -m "$MESSAGE")
fi

echo_info "Pushing $COMMIT to $AUTODOC_BRANCH"

# Rest the index, commit-tree doesn't do that by itself. If we don't do this
# `git status` or `git diff` will look *very* weird.
git reset .

# Push the newly created commit to remote
if [[ ! -z "$PARENT" ]] && [[ $(git rev-parse ${COMMIT}^{tree}) == $(git rev-parse refs/remotes/$AUTODOC_REMOTE/$AUTODOC_BRANCH^{tree} ) ]] ; then
    echo_error "WARNING: No changes in documentation output from previous commit. Not pushing to ${AUTODOC_BRANCH}"
else
    git push $AUTODOC_REMOTE $COMMIT:refs/heads/$AUTODOC_BRANCH
    # Make sure our local remotes are up to date.
    git fetch
    # Show what happened, you should see a little stat diff here of the changes
    echo
    git log -1 --stat $AUTODOC_REMOTE/$AUTODOC_BRANCH
fi
