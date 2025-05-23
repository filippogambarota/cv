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

format_terza_missione <- function(data){

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

# format_teaching <- function(data){
#   title <- ifelse(!is.na(data$hours) & !is.na(data$cfu),
#                   sprintf("%s (%s hrs., %s CFU)", data$title, data$hours, data$cfu),
#                   ifelse(!is.na(data$hours) & is.na(data$cfu),
#                          sprintf("%s (%s hrs.)", data$title, data$hours),
#                          data$title))
#
#   if(!is.na(data$details)){
#     if(!is.na(data$materials)){
#       details <- sprintf("%s [[Materials](%s)]", data$details, data$materials)
#     }else{
#       details <- data$details
#     }
#   }else{
#     if(!is.na(data$materials)){
#       details <- sprintf("[[Materials](%s)]", data$details, data$materials)
#     }
#   }
#
#   make_entry(
#     what = title,
#     date = data$date,
#     place = data$place,
#     details = details
#   )
#   br(1)
# }

format_teaching <- function(data){
  if(nrow(data) > 1){
    title <- unique(data$title)
    details <- ifelse(!is.na(data$details),
                      ifelse(!is.na(data$materials),
                             sprintf("%s </br> [%s](%s)", data$details, icon_from_file(data$materials), data$materials),
                             data$details),
                      sprintf("</br> [%s](%s)", icon_from_file(data$materials), data$materials))
    details <- unique(details)
    date <- sort(data$date)
    if(tail(date, 1) < format(Sys.Date(), format = "%Y")){
      date <- paste0(date[1], " - ", tail(date, 1))
    }else{
      date <- paste0(date[1], " - ", "ongoing")
    }
    place <- unique(data$place)
    date_details <- sort(data$month, decreasing = TRUE)
    details_events <- sprintf("- %s (%s hrs.)",
                              format(date_details, format = "%B %Y"),
                              data$hours
    )
    details_events <- paste0(details_events, collapse = "\n")
  } else{
    title <- ifelse(!is.na(data$hours) & !is.na(data$cfu),
                    sprintf("%s (%s hrs., %s CFU)", data$title, data$hours, data$cfu),
                    ifelse(!is.na(data$hours) & is.na(data$cfu),
                           sprintf("%s (%s hrs.)", data$title, data$hours),
                           data$title))

    if(!is.na(data$details)){
      if(!is.na(data$materials)){
        details <- sprintf("%s <br/> [%s](%s)", data$details, icon_from_file(data$materials), data$materials)
      }else{
        details <- data$details
      }
    }else{
      if(!is.na(data$materials)){
        details <- sprintf("<br/> [%s](%s)", icon_from_file(data$materials), data$materials)
      }
    }
    details_events <- NA
    date <- data$date
    place <- data$place
  }
  make_entry(
    what = title,
    date = date,
    place = place,
    details = details,
    bullets = details_events
  )
  cat("\n\n")
}

format_conf <- function(data){
  authors <- str_replace_all(data$authors, "Gambarota", "**Gambarota**")
  authors <- ifelse(startsWith(authors, "**Gambarota**"),
                    str_replace_all(authors[startsWith(authors, "**Gambarota**")],
                                    pattern = "\\*\\*Gambarota\\*\\*",
                                    "\\*\\*Gambarota\\*\\* [P]"),
                    authors)

  video <- ifelse(!is.na(data$link_talk),
                  sprintf("[{{< fa video >}}](%s)", data$link_talk),
                  NA)
  # materials <- ifelse(!is.na(data$link_materials),
  #                     sprintf("[[%s](%s)]", data$type_material, data$link_materials),
  #                     NA)
  materials <- ifelse(!is.na(data$link_materials),
                      sprintf("[%s](%s)", icon_from_file(data$link_materials), data$link_materials),
                      NA)
  video_materials <- c(video, materials)
  video_materials <- video_materials[!is.na(video_materials)]
  if(length(video_materials) > 0){
    video_materials <- paste(video_materials, collapse = " ")
    data$title <- sprintf("%s </br> %s",
                               data$title,
                               video_materials)
  }else{
    data$title <- sprintf("%s",
                               data$title)
  }

  place <- sprintf("%s - %s", data$conference, data$place)
  # what <- sprintf("%s </br> %s", authors, data$title)
  make_entry(what = data$title, date = data$date, place = place, what2 = authors)
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
  cat("\n\n")
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


make_entry <- function(what = NA, date = NA, place = NA, details = NA, bullets = NA, what2 = NA){

  what <- if(!is.na(what)) add_css_class(what, ".what") else what
  what2 <- if(!is.na(what2)) add_css_class(what2, ".what2") else what2
  date <- if(!is.na(date)) add_css_class(date, ".cvdate") else date
  place <- if(!is.na(place)) add_css_class(place, ".where") else place
  details <- if(!is.na(details)) add_css_class(details, ".details") else details

  # entry <- sprintf("%s </br> {{< fa location-dot >}} %s", what, place)

  if(!is.na(what2)){
    entry <- sprintf("%s </br> %s </br> *%s*", what2, what, place)
  }else{
    entry <- sprintf("%s </br> *%s*", what, place)
  }

  cat(date, "\n")
  cat(entry, "\n")
  if(!is.na(details)){
    br()
    cat(details)
  }
  if(!is.na(bullets)){
    cat("\n\n:::{.details}", "\n\n")
    cat("\n\n", bullets, "\n")
    cat(":::", "\n")
  }
}

pb_skill_old <- function(x, max = 5){
  skill <- paste0(rep("🔵", x), collapse = "")
  noskill <- paste0(rep("⚪", max - x), collapse = "")
  out <- paste0(skill, noskill, collapse = "")
  add_css_class(out, ".pbskill")
}

pb_skill <- function(x, max = 5){
  step <- 100/max
  pat <- '<span class="progress-bar"><span class="progress" style="width: %s%%;"></span></span>'
  sprintf(pat, step * x)
}

flag <- function(x){
  sprintf('<span class="fi fi-%s"></span>', x)
}
