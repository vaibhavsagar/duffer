#!/usr/bin/env bash
BRANCH=gh-pages
TARGET_REPO=vaibhavsagar/duffer.hs.git

echo -e "Starting to deploy to Github Pages\n"
if [ "$TRAVIS" == "true" ]; then
    git config --global user.email "travis@travis-ci.org"
    git config --global user.name "Travis"
fi
# Using token, clone gh-pages branch
git clone --quiet --branch=$BRANCH https://${GH_TOKEN}@github.com/$TARGET_REPO build > /dev/null
# Generate presentation
pandoc --standalone -t revealjs -V theme:simple presentation/presentation.md -o index.html
# Go into directory and copy data we're interested in to that directory
cd build
cp ../index.html .
# Download a fresh copy of reveal.js
rm -rf reveal.js
curl -L https://github.com/hakimel/reveal.js/archive/3.3.0.tar.gz | tar xz
mv reveal.js* reveal.js
# Add, commit and push files
git add -f .
git commit -m "Travis build $TRAVIS_BUILD_NUMBER pushed to Github Pages"
git push -fq origin $BRANCH > /dev/null
echo -e "Deploy completed\n"
