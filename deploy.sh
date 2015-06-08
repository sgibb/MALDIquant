#!/bin/bash
# taken from https://github.com/csgillespie/dratTravis/blob/master/deploy.sh

set -o errexit -o nounset

addToDrat() {
  PKG_REPO=$PWD
  GIT_REPO=${PKG_REPO##*/}

  cd ..; mkdir drat; cd drat

  ## Set up Repo parameters
  git init
  git config user.name "Sebastian Gibb"
  git config user.email "mail@sebastiangibb.de"
  git config --global push.default simple

  ## Get drat repo
  git remote add upstream "https://$GH_TOKEN@github.com/sgibb/drat.git"
  git fetch upstream
  git checkout gh-pages

  Rscript -e "drat::insertPackage('$PKG_REPO/$PKG_TARBALL', \
    repodir = '.', \
    commit = '$PKG_TARBALL (sgibb/$GIT_REPO@$TRAVIS_COMMIT; travis $TRAVIS_BUILD_NUMBER)')"
  git push
}

addToDrat
