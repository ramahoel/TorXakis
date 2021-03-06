# TorXakis - Model Based Testing
# Copyright (c) 2015-2017 TNO and Radboud University
# See LICENSE at root directory of this repository.

git remote remove origin;
git remote add origin https://github.com/TorXakis/TorXakis.git;
git fetch;
git checkout gh-pages;
git rebase develop;
rm ./doc/* -rf
cp -r $CACHE_DIR/.stack-work/install/x86_64-linux/lts-9.1/8.0.2/doc/. ./doc/;
git add .;
commitMsg="Haddock @ $(date +%Y%m%d_%H%M%S)";
git commit -m "$(echo $commitMsg)";
git push -f "https://keremispirli:$GITHUB_TOKEN@github.com/TorXakis/TorXakis.git";
