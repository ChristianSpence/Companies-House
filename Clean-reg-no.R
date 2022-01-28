ch_reg_no_fix <- function(col) {
  col <- stringr::str_remove_all(col, "\\s")
  col <- toupper(col)
  col <- stringr::str_remove_all(col, "[^[:alnum:]]")
  col <- purrr::map_chr(col, function(x) {
    if (is.na(x)) {
      x
    } else if (nchar(x) < 8) {
      paste0(stringr::str_sub(x, 1, regexpr("[0-9]", x) - 1),
             strrep("0", 8 - nchar(x)),
             stringr::str_sub(x, regexpr("[0-9]", x)))
    } else {
      x
    }
    }
  )
  col <- ifelse(grepl("^[ACFGILNOPRSZ0-9]{1}[ACEFGILOPRSZ0-9]{1}[0-9]{6}", col),
              col,
              NA
  )
  return(col)
}
