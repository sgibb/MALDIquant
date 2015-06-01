#!/bin/bash
# taken from https://github.com/csgillespie/dratTravis/blob/master/deploy.sh

set -o errexit -o nounset

addToDrat() {
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

  Rscript -e "drat::insertPackage(devtools::build('../MALDIquant'), \
    repodir = '.', \
    commit='Travis update: build $TRAVIS_BUILD_NUMBER')"
  git push
}

addToDrat
