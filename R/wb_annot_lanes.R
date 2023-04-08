# TODO
# Add coloring
# Add linebreaking?


#' Add a header annotating Western blot lanes
#'
#' @param img A `magick-image`
#' @param annot A `data.frame`, where each *row* represents a lane and each
#'   *column* represents a variable. The first row will annotate the first lane
#'   (left to right)
#' @param n_lanes Numeric. The number of lanes in the image
#' @param text_size Size of text. Will be automatically downscalled if too big
#'   to fit.
#'
#' @return A `magick-image`
#' @export
wb_annot_lanes <- function(img, annot, n_lanes = 8, text_size = 20) {

  for (i in 1:ncol(annot)) {
    img <- make_blocks(
      img = img,
      labels = annot[[i]],
      n_lanes = n_lanes,
      text_size
    )
  }

  add_block_titles(img, annot, text_size)
}

get_new_text_size <- function(img, labels, block_sizes, text_size) {
  img <- image_draw(img, res = 30)

  # THIS IS SLOW! A binary search for optimal size would probably be better.
  while (any((block_sizes - strwidth(labels, cex = text_size)) < 0) & text_size > 1) {
    text_size = text_size - 1
  }
  dev.off()
  text_size
}

get_text_height <- function(img, labels, text_size) {
  img <- image_draw(img, res = 30)
  height <- max(strheight(labels, cex = text_size, units = "figure")) * image_info(img)$height
  dev.off()
  height
}

make_blocks <- function(img, n_lanes, labels, text_size) {

  block_sizes <- get_block_sizes(
    img,
    n_lanes,
    labels
  )

  block_labels <- rle(labels)$values

  text_size <- get_new_text_size(
    img = img,
    labels = block_labels,
    block_sizes = block_sizes,
    text_size = text_size
  )

  text_height <- get_text_height(
    img = img,
    labels = labels,
    text_size = text_size
  )

  img <- add_side(img, text_height, "top")

  block_borders <- c(0, cumsum(block_sizes))

  img <- image_draw(img, res = 30)


  for (i in 1:(length(block_borders)-1)) {
    rect(block_borders[i], 0, block_borders[i+1], text_height)
    text(
      x = (block_borders[i] + block_borders[i+1])/2,
      y = text_height/2,
      labels = block_labels[i],
      cex = text_size
    )
  }
  dev.off()

  img
}

get_block_sizes <- function(img, n_lanes, labels) {
  width <- get_lane_width(img, n_lanes)
  rle(labels)$lengths * get_lane_width(img, n_lanes)
}

get_lane_width <- function(img, n_lanes) {
  info <- img |> image_info()
  info$width/n_lanes
}

make_block_title_space <- function(img, annotation, text_size) {
  titles <- colnames(annotation)
  img <- image_draw(img, res = 30)
  width <- max(strwidth(titles, cex = text_size))
  dev.off()
  add_side(img, width, side = "right")
}

add_block_titles <- function(img, annotation, text_size) {
  pre_width <- image_info(img)$width
  img <- make_block_title_space(img, annotation, text_size)
  height <- get_text_height(img, unlist(annotation), text_size)
  img <- image_draw(img, res = 30)
  annotation <- rev(annotation)
  for (i in 1:ncol(annotation)) {
    text(
      x = pre_width,
      y = height * (i-1),
      labels = colnames(annotation)[i],
      cex = text_size,
      adj = c(0, 0.9)
    )
  }
  dev.off()

  img
}

add_side <- function(img, size, side = c("left", "right", "top", "bottom")) {
  side <- rlang::arg_match(side)

  if (side %in% c("top", "bottom")) {
    side_geom <- geometry_area(height = size)
    img_dim <- image_info(img)$height
  } else {
    side_geom <- geometry_area(width = size)
    img_dim <- image_info(img)$width
  }

  img <- image_border(img, "white", side_geom)

  crop_dim <- ifelse(side %in% c("top", "left"), img_dim + size, size)

  if (side == "top") img <- image_crop(img, geometry_area(height = crop_dim))
  if (side == "right") img <- image_crop(img, geometry_area(x_off = crop_dim))
  if (side == "bottom") img <- image_crop(img, geometry_area(y_off = crop_dim))
  if (side == "left") img <- image_crop(img, geometry_area(width = crop_dim))

  img
}
