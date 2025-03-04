#!/usr/bin/env bash
cd ~/work/filippogambarota/cv
git pull
Rscript -e "devtools::load_all();render_cv()"
