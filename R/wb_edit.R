wb_edit <- function(img, actions) {
  for (i in seq_along(actions)) {
    img <- apply_function_to_img(img, names(actions[i]), unlist(actions[i]))
  }
  img
}

wit <- function(img, y) {
  print(y)
}

apply_function_to_img <- function(img, fn_name, fn_val) {
  f <- match.fun(fn_name)
  if (!is.null(fn_val)) {
    img <- f(img, fn_val)
  } else {
    img <- f(img)
  }

  img

}

edit_wb <- function(ml, path,
                    flop = FALSE, rot = FALSE, crop = FALSE, negate = FALSE,
                    entry = FALSE,
                    width = NULL,
                    font_size = 10) {
  if (is.character(crop)) {
    crop <- parse_crop(crop)
  }

  if (entry) {
    footer <- make_wb_footer_from_entry(ml, path)
    #header <- make_wb_header_from_entry(ml, path)
  }

  path |>
    image_read() |>
    wb_flop(flop) |>
    wb_rotate(rot) |>
    wb_crop(crop) |>
    wb_scale(width) |>
    wb_negate(negate) |>
    wb_footer(footer, font_size)

}

wb_flop <- function(img) {
  stopifnot(is.logical(flop))
  if (flop) {
    img <- image_flop(img)
  }
  img
}

wb_rotate <- function(img, rot) {
  stopifnot(isFALSE(rot) | is.numeric(rot))
  if (!rot) {
    return(img)
  }
  image_rotate(img, rot)
}

wb_crop <- function(img, crop) {
  if (isFALSE(crop)) {
    return(img)
  }
  image_crop(
    img,
    geometry_area(crop$width, crop$height, crop$x_off, crop$y_off)
  )
}

parse_crop <- function(geometry) {
  g <- unlist(strsplit(geometry, "[x+]"))
  l <- list(width = g[1], height = g[2], x_off = g[3], y_off = g[4]) |>
    lapply(as.integer)

  if (is.na(l$width)) {
    l$width <- NULL
  }
  if (is.na(l$height)) {
    l$height <- NULL
  }
  if (is.na(l$x_off)) {
    l$x_off <- 0
  }
  if (is.na(l$y_off)) {
    l$y_off <- 0
  }
  l
}

wb_negate <- function(img, negate) {
  if (negate) {
    img <- image_negate(img)
  }
  img
}

wb_footer <- function(img, footer, size = 10) {

  if (isFALSE(footer)) {
    return("")
  }

  max_chars <- image_info(img)$width/(size/1.5)

  wrap_string <- function(string, max_chars) {
    stringi::stri_wrap(string, width = max_chars, cost_exponent = 0) |>
      paste0(collapse = "\n")
  }

  footer <- sapply(footer, wrap_string, max_chars) |>
    paste0(collapse = "\n")

  n_newlines <- str_count(footer, "\n")

  height = (n_newlines + 1) * size * 1.2

  geo <- geometry_area(height = height)

  image_border(img, "white", geo) |>
    image_crop(geometry_area(y_off = height)) |>
    image_annotate(
      footer,
      color = "black",
      font = "mono",
      size = size,
      gravity = "SouthWest"
    )
}

wb_scale <- function(img, width) {
  if (is.null(width)) {
    return(img)
  }
  image_scale(img, width)
}

make_wb_footer_from_entry <- function(ml, path) {
  entry <- ml |>
    group_by(path_local) |>
    filter(path_local == path) |>
    summarize(
      date = unique(Date),
      ab1 = unique(last_ab1),
      ab2 = unique(last_ab2)
    )

  c(entry$date, entry$ab1, entry$ab2)
}

make_wb_header_from_entry <- function(ml, path) {
  entry <- ml |>
    group_by(path_local) |>
    filter(path_local == path) |>
    mutate(header = "")
}

get_most_recent_ab <- function(abs) {

}

get_wb_path_from_link <- function(link, wb_dir, local = FALSE) {
  path <- link |>
    str_extract("Raw%20Data.*\\.tif") |>
    str_replace_all("%20", " ")

  if (local) {
    path <- str_match(path, "aragaki-kai/(.*)$")[,2]
    path <- path(wb_dir, path)
  }
  path
}

wrangle_wb_ml <- function(ml, wb_dir) {
  ml |>
    mutate(
      path_sharepoint = get_wb_path_from_link(link_to_img_folder, wb_dir),
      path_local = get_wb_path_from_link(link_to_img_folder, wb_dir, TRUE)
    ) |>
    rowwise() |>
    mutate(
      last_ab1 = strsplit(`1* Ab(s) (Cat#), Dil. Factor`, ";")[[1]] |> tail(1),
      last_ab2 = strsplit(`2* Ab(s)(Cat#), Dil. Factor`, ";")[[1]] |> tail(1)
    ) |>
    ungroup()
}

make_wb_label <- function(x) {
  x <- x |>
    select(drug, conc_nM, time, cell_line, seeding_density_mL, lane_num, ab_name)

  unique_vals <- apply(x, 2, \(x) length(unique(x)))

  footer <- make_wb_footer(x, unique_vals)
  header <- make_wb_header(x, unique_vals)

  tibble(footer = footer) |> bind_cols(header)
}

make_wb_header <- function(x, unique_vals) {
  x[unique_vals > 1] |>
    mutate(across(everything(), as.character)) |>
    pivot_longer(-lane_num) |>
    group_by(lane_num) |>
    mutate(
      value = reduce(value, paste, sep = "\n"),
      name = reduce(name, paste, sep = "\n")
    ) |>
    distinct()
}

annotate_lane_loop <- function(img, header_data) {
  header_data <- pivot_wider(header_data, names_from = lane_num, values_from = value) |>
    relocate(name, .after = last_col())
  x <- 10
  for (i in 1:ncol(header_data)) {
    pos <- paste0("+", x)
    img <- image_annotate(img, header_data[[i]], location = pos, size = 7)
    x <- x + 40
  }
  img
}
