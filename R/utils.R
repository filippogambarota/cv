# download the online cv using the googledrive package

get_online_cv <- function(){
  file <- here::here("data/data.xlsx")
  googledrive::drive_download(info()$cv, path = file, overwrite = TRUE)
  import_cv(file)
}

today <- function(to = "plain"){
  date <- Sys.Date()
  day <- as.numeric(format(date, "%d"))

  # Determine the suffix for the day
  suffix <- ifelse(day %% 10 == 1 & day != 11, "st",
                   ifelse(day %% 10 == 2 & day != 12, "nd",
                          ifelse(day %% 10 == 3 & day != 13, "rd", "th")))
  if(to == "html"){
    suffix <- sprintf("<sup>%s</sup>", suffix)
  }
  mon <- as.character(lubridate::month(date, label = TRUE, abbr = FALSE))
  sprintf("%s %s%s %s", mon, format(date, "%d"), suffix, format(date, "%Y") )
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
  bibl <- readLines(bib)
  bibl_temp <- bibl
  bibl_temp <- gsub("Gambarota", "Gambarota", bibl_temp)
  bibl_temp[grepl("^@", bibl_temp)] <- bibl[grepl("^@", bibl)]
  writeLines(bibl_temp, bib)
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
  options(googledrive_quiet = TRUE, pagedown.remote.maxattempts=40, pagedown.remote.sleeptime=2) # number of attempt in total
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
  system("git add .")
  system("git commit -m 'updating'")
  system("git push")
}

put_before <- function(x, where, what){
  subb <- paste(what, where)
  gsub(where, subb, x)
}

bold_pattern <- function(x, pattern){
  gsub(pattern, bold(pattern), x)
}

wrap_text <- function(text, width) {
    words <- strsplit(text, "\\s+")[[1]]  # Split text into words
    line_length <- 0
    wrapped_text <- character()

    for (word in words) {
      if (line_length + nchar(word) <= width) {
        wrapped_text <- paste0(wrapped_text, word, " ")
        line_length <- line_length + nchar(word) + 1
      } else {
        wrapped_text <- paste0(wrapped_text, " <br> ", word, " ")
        line_length <- nchar(word) + 1
      }
    }

  return(wrapped_text)
}

get_first_name <- function(x){
  strsplit(x, ",")[[1]][[1]]
}

clean_for_name <- function(x){
  x |>
    gsub("-", "_", x = _) |>
    gsub(":", "", x = _) |>
    gsub(";", "", x = _) |>
    gsub("\\s", "", x = _) |>
    gsub("\\(", "", x = _) |>
    gsub("\\)", "", x = _) |>
    gsub(",", "", x = _)
}

get_bib_keys <- function(x){
  keys <- x[grepl("^@", x)]
  stringr::str_extract(keys, "(?<=@ARTICLE\\{)[^,]+")
}

icon_from_file <- function(x){
  dplyr::case_when(
    grepl("html$|docs.google.com/presentation", x) ~ "{{< fa globe >}}",
    is.na(x) ~ NA,
    xfun::file_ext(x) == "pdf" ~ "{{< fa file-pdf >}}",
    #grepl("github\\.com", x) & xfun::file_ext(x) == "" ~ "{{< fa github >}}",
    grepl("osf\\.io", x) ~ "{{< ai osf >}}",
    TRUE ~ "{{< fa globe >}}"
  )
}

link_with_icon <- function(x){
  ifelse(!is.na(icon_from_file(x)),
         sprintf("[%s](%s)", icon_from_file(x), x),
         "")
}
