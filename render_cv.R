# This script builds both the HTML and PDF versions of your CV

# If you want to speed up rendering for googlesheets driven CVs you can cache a
# version of your data This avoids having to fetch from google sheets twice and
# will speed up rendering. It will also make things nicer if you have a
# non-public sheet and want to take care of the authentication in an interactive
# mode.
# To use, simply uncomment the following lines and run them once.
# If you need to update your data delete the "ddcv_cache.rds" file and re-run

# source("CV_printing_functions.R")
# cv_data <- create_CV_object(
#   data_location = "https://docs.google.com/spreadsheets/d/14MQICF2F8-vf8CKPF1m4lyGKO6_thG-4aSwat1e2TWc",
#   cache_data = TRUE
# )
# cache_data <- TRUE

which_cv <- commandArgs(trailingOnly = TRUE)

#which_cv <- readline(prompt="Compile CV? (1 = ITA, 2 = ENG, 3 = both):  ")


if (which_cv == 1| which_cv == tolower("ita")){
    
    ## Ita Version
    
    # Knit the HTML version
    rmarkdown::render("ita/cv_gambarota_ita.Rmd",
                      params = list(pdf_mode = FALSE,
                                    html_mode = TRUE),
                      output_file = "cv_gambarota_ita.html",
                      quiet = T)
    # Knit the PDF version to temporary html location
    tmp_html_cv_loc <- fs::file_temp(ext = ".html")
    rmarkdown::render("ita/cv_gambarota_ita.Rmd",
                      params = list(pdf_mode = TRUE,
                                    html_mode = FALSE),
                      output_file = tmp_html_cv_loc,
                      quiet = T)
    
    # Convert to PDF using Pagedown
    pagedown::chrome_print(input = tmp_html_cv_loc,
                           output = "ita/cv_gambarota_ita.pdf")
} else if (which_cv == 2|which_cv == tolower("eng")){
    
    ## Eng Version
    
    # Knit the HTML version
    rmarkdown::render("cv_gambarota_eng.Rmd",
                      params = list(pdf_mode = FALSE,
                                    html_mode = TRUE),
                      output_file = "index.html",
                      quiet = T)
    # Knit the PDF version to temporary html location
    tmp_html_cv_loc <- fs::file_temp(ext = ".html")
    rmarkdown::render("cv_gambarota_eng.Rmd",
                      params = list(pdf_mode = TRUE,
                                    html_mode = FALSE),
                      output_file = tmp_html_cv_loc,
                      quiet = T)
    # Convert to PDF using Pagedown
    pagedown::chrome_print(input = tmp_html_cv_loc,
                           output = "eng/cv_gambarota_eng.pdf")
} else if (which_cv == 3|which_cv == tolower("both")){
    
    ## Ita Version
    
    # Knit the HTML version
    rmarkdown::render("ita/cv_gambarota_ita.Rmd",
                      params = list(pdf_mode = FALSE,
                                    html_mode = TRUE),
                      output_file = "cv_gambarota_ita.html",
                      quiet = T)
    # Knit the PDF version to temporary html location
    tmp_html_cv_loc <- fs::file_temp(ext = ".html")
    rmarkdown::render("ita/cv_gambarota_ita.Rmd",
                      params = list(pdf_mode = TRUE,
                                    html_mode = FALSE),
                      output_file = tmp_html_cv_loc,
                      quiet = T)
    
    # Convert to PDF using Pagedown
    pagedown::chrome_print(input = tmp_html_cv_loc,
                           output = "ita/cv_gambarota_ita.pdf")
    
    ## Eng Version
    
    # Knit the HTML version
    rmarkdown::render("cv_gambarota_eng.Rmd",
                      params = list(pdf_mode = FALSE,
                                    html_mode = TRUE),
                      output_file = "index.html",
                      quiet = T)
    # Knit the PDF version to temporary html location
    tmp_html_cv_loc <- fs::file_temp(ext = ".html")
    rmarkdown::render("cv_gambarota_eng.Rmd",
                      params = list(pdf_mode = TRUE,
                                    html_mode = FALSE),
                      output_file = tmp_html_cv_loc,
                      quiet = T)
    # Convert to PDF using Pagedown
    pagedown::chrome_print(input = tmp_html_cv_loc,
                           output = "eng/cv_gambarota_eng.pdf")
}