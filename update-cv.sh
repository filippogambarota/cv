#!/usr/bin/env bash

cd "$HOME/work/filippogambarota/cv"
git pull
Rscript -e "renv::activate();devtools::load_all();render_cv()"
