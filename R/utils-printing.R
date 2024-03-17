format_edu <- function(data){
    title <- ifelse(!is.na(data$duration),
                    sprintf("%s (%s)", data$title, data$duration),
                    data$title)
    cat("### ", title, "\n\n")
    cat(data$place, "\n\n")
    cat("N/A", "\n\n")
    cat(data$date, "\n\n")
    if(!is.na(data$details)){
        cat(data$details, "\n\n")
    }
}

format_social <- function(data, size){
    pattern <- "<a href='%s'> <i class='%s-%sx'></i></a>"
    sprintf(pattern, data$link, data$icon, size)
}

format_pub <- function(data, metrics = TRUE){
    cat(data$title, "\n\n")
    if(metrics){
        if(!is.na(data$IF) & !is.na(data$Quartile)){
            cat(sprintf("%s **IF: %s (%s)**", data$ref, data$IF, data$Quartile), "\n\n")
        }else{
            cat(data$ref, "\n\n")
        }
    }else{
        cat(data$ref, "\n\n")
    }
    cat("N/A", "\n\n")
    cat(data$date, "\n\n")
    cat(data$authors, "\n\n")
}

format_teaching <- function(data){
  title <- ifelse(!is.na(data$hours) & !is.na(data$cfu),
                  sprintf("%s (%s hrs., %s CFU)", data$title, data$hours, data$cfu),
                  ifelse(!is.na(data$hours) & is.na(data$cfu),
                         sprintf("%s (%s hrs.)", data$title, data$hours),
                         data$title))

  if(!is.na(data$details)){
    if(!is.na(data$materials)){
      details <- sprintf("%s [[Materials](%s)]", data$details, data$materials)
    }else{
      details <- data$details
    }
  }else{
    if(!is.na(data$materials)){
      details <- sprintf("[[Materials](%s)]", data$details, data$materials)
    }
  }

  make_entry(
    what = title,
    date = data$date,
    place = data$place,
    details = details
  )
  br(2)
}

format_conf <- function(data){
  authors <- str_replace_all(data$authors, "Gambarota", "**Gambarota**")
  authors <- ifelse(startsWith(authors, "**Gambarota**"),
                    str_replace_all(authors[startsWith(authors, "**Gambarota**")],
                                    pattern = "\\*\\*Gambarota\\*\\*",
                                    "\\*\\*Gambarota\\*\\* [presenter]"),
                    authors)

  video <- ifelse(!is.na(data$link_talk),
                  sprintf("[Video](%s)", data$link_talk),
                  NA)
  materials <- ifelse(!is.na(data$link_materials),
                      sprintf("[%s](%s)", data$type_material, data$link_materials),
                      NA)
  video_materials <- c(video, materials)
  video_materials <- video_materials[!is.na(video_materials)]
  if(length(video_materials) > 0){
    video_materials <- paste(video_materials, collapse = ", ")
    data$title <- sprintf("%s [*%s*] - %s",
                               data$title,
                               data$type,
                               video_materials)
  }else{
    data$title <- sprintf("%s [*%s*]",
                               data$title,
                               data$type)
  }

  place <- sprintf("%s - %s", data$conference, data$place)
  what <- sprintf("%s </br> %s", authors, data$title)
  make_entry(what, data$date, place)
  br(2)
}

format_memb <- function(data){
  cat(sprintf("%s (%s) %s", add_css_class(data$society, ".what"), data$abbr, add_css_class(paste("from:", data$date), ".cvdate")))
  cat("\n\n")
}

format_rev <- function(data){
    cat("###", data$journal, "\n\n")
    cat("N/A", "\n\n")
    cat("N/A", "\n\n")
    cat("N/A", "\n\n")
}

format_awards <- function(data){
  cat(sprintf("%s %s </br> %s",
          add_css_class(data$date, ".cvdate"),
          add_css_class(data$title, ".what"),
          data$details)
  )
  cat("\n")
}

format_all <- function(data, format_fun, ...){
    data_by_row <- split(data, 1:nrow(data))
    purrr::walk(data_by_row, format_fun, ...)
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

format_other_pubs <- function(data, bibdata){
    data$authors <- bold_my_name(data$authors)
    data$IF <- bibdata$IF[bibdata$journal %in% data$journal]
    data$Quartile <- bibdata$Quartile[bibdata$journal %in% data$journal]
    metrics <- sprintf("**IF: %s (%s)**", data$IF, data$Quartile)
    journal_status <- sprintf("%s (**Status: %s**)", data$journal, data$status)
    cat("###", data$title, "\n\n")
    cat(sprintf("%s - %s", journal_status, metrics), "\n\n")
    cat("N/A", "\n\n")
    cat(data$date, "\n\n")
    cat(data$authors, "\n\n")
}

format_under_review <- function(data){
    data$authors <- gsub("Gambarota", "**Gambarota**", data$authors)
    cat("###", data$title, "\n\n")
    if(!is.na(data$preprint) & !is.na(data$link)){
        cat(sprintf("[%s](%s)", data$preprint, data$link), "\n\n")
    }else{
        cat("N/A", "\n\n")
    }
    cat("N/A", "\n\n")
    cat("N/A", "\n\n")
    cat(data$authors, "\n\n")
}

bold <- function(x){
    sprintf("**%s**", x)
}

bold_my_name <- function(x){
    gsub("Gambarota", "**Gambarota**", x)
}


br <- function(n = 1){
  brs <- rep("<br>", n)
  cat(brs, sep = "\n")
}


make_entry <- function(what = NA, date = NA, place = NA, details = NA){

  what <- if(!is.na(what)) add_css_class(what, ".what") else what
  date <- if(!is.na(date)) add_css_class(date, ".cvdate") else date
  place <- if(!is.na(place)) add_css_class(place, ".where") else place
  details <- if(!is.na(details)) add_css_class(details, ".details") else details

  entry <- sprintf("%s </br> {{< fa location-dot >}} %s", what, place)
  cat(date, "\n")
  cat(entry, "\n")
  if(!is.na(details)){
    br()
    cat(details)
  }
}
