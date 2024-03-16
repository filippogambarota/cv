# download the online cv using the googledrive package

get_online_cv <- function(){
  file <- here::here("data/data.xlsx")
  googledrive::drive_download(info()$cv, path = file, overwrite = TRUE)
  import_cv(file)
}

clean_cv_data <- function(data){
    data %>%
        mutate(across(where(is.character), ~str_remove_all(.x, "\\.0")))
}

download_sign <- function(){
    googledrive::drive_download(info()$signature, path = "files/signature.png", overwrite = TRUE)
}

# reading the cv into a list

import_cv <- function(cv){
    cv_sec_names <- readxl::excel_sheets(cv)
    cv_secs <- lapply(cv_sec_names, function(sec) readxl::read_xlsx(cv, sec))
    cv_secs <- setNames(cv_secs, cv_sec_names)
    return(cv_secs)
}

# download the bib file with my publications

get_publications <- function(){
  bib <- "files/ref.bib"
  download.file(info()$bib, destfile = "files/ref.bib", quiet = TRUE)
  RefManageR::ReadBib(bib)
}

get_author_surname <- function(author){
    if(grepl("\\}", author)){
        temp <- unlist(str_split(author, " \\{"))
        temp <- str_remove(temp, "\\}")
    }else{
        temp <- unlist(str_split(author, " "))
    }
    temp[length(temp)]
}

bib_to_df <- function(bib){
    bib <- RefManageR::ReadBib(bib, check = "warn", .Encoding = "UTF-8")
    as.data.frame(bib)
}

prepare_bib <- function(bib, metrics){
    bib <- bib_to_df(bib)
    title <- paste("###", bib$title)
    title <- str_remove_all(title, "\n")
    title <- str_replace_all(title, "`", "'")
    title <- str_replace_all(title, "''", "'")
    date <- sapply(stringr::str_split(bib$date, "-"), function(x) x[1])
    authors <- stringr::str_split(bib$author, " and ")
    authors <- purrr::map(authors, ~purrr::map(.x, get_author_surname))
    authors <- sapply(authors, function(x) paste(unlist(x), collapse = ", "))
    authors <- str_replace_all(authors, "Gambarota", "**Gambarota**")
    ref <- ifelse(!is.na(bib$doi),
                  sprintf("%s [DOI: %s](%s)", bib$journal, bib$doi, bib$doi),
                  bib$journal)
    key <- rownames(bib)
    journal <- bib$journaltitle

    out <- tibble(
        type = bib$bibtype, title, authors, date, ref, key, journal
    )

    out |>
        left_join(select(metrics, key, IF, Quartile), by = "key")
}

add_css_class <- function(x, class){
  sprintf("[%s]{%s}", x, class)
}

update_cv <- function(which = "cv", upload = FALSE){
    gdrive <- "https://drive.google.com/drive/u/0/folders/1wEXXzfboKaJg0pI-ewfFmmxZBX2CK2lI"
    if(!file.exists("img/signature.png")){
        download_sign()
    }

    if(which == "cv"){
        outname <- "index.html"
        outname_pdf <- "docs/cv.pdf"
        msg <- "CV updated! :)"
    }else{
        outname_pdf <- "docs/temp.pdf"
        msg <- "temporary CV updated! :)"
    }

    if(which == "cv"){
        out_html <- rmarkdown::render("cv.Rmd",
                                      output_dir = "docs",
                                      output_file = outname,
                                      quiet = TRUE,
                                      params = list(pdf_mode = FALSE,
                                                    html_mode = TRUE,
                                                    which = which))
    }

    out_html_pdf <- rmarkdown::render("cv.Rmd",
                                  output_dir = "docs",
                                  output_file = "temp",
                                  quiet = TRUE,
                                  params = list(pdf_mode = TRUE,
                                                html_mode = FALSE,
                                                which = which))

    pagedown::chrome_print(out_html_pdf, output = outname_pdf)
    fs::file_delete(out_html_pdf)

    cli::cli_alert_success(msg)

    if(upload & which == "cv"){
        gert::git_add(c("cv.Rmd", "docs/", "data/")) # adding
        gert::git_commit("updating cv")
        gert::git_push()
        cli::cli_alert_success("CV uploaded on Github! :)")
        gdrive <- googledrive::as_dribble(gdrive)
        cv_gdrive <- googledrive::drive_upload(outname_pdf,
                                               gdrive,
                                               overwrite = TRUE,
                                               verbose = FALSE)
        googledrive::drive_share_anyone(cv_gdrive)
        cli::cli_alert_success("CV shared and uploaded on Google Drive! :)")
    }
}
