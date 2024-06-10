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

render_cv <- function(){
  options(googledrive_quiet = TRUE)
  quarto::quarto_render("cv.qmd",
                        output_format = "html",
                        output_file = "index.html",
                        quiet = TRUE)
  pagedown::chrome_print("index.html", output = "cv.pdf")
  googledrive::drive_auth("filippo.gambarota@gmail.com")
  upload <- googledrive::as_dribble(info()$drive_folder)
  googledrive::drive_upload("cv.pdf",
                            path = upload,
                            name = "cv-gambarota.pdf",
                            overwrite = TRUE)
}
