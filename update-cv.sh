#!/usr/bin/env bash
git pull
Rscript -e "devtools::load_all();render_cv()"