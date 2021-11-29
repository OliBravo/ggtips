library(ggplot2)
library(ggtips)
library(dplyr)

# where do you want the resulting json files to be stored
SAVE_DIR <- system.file(file.path("barplot_tooltips", "json_output"), package = "ggtips")

singlelayers <- c("gg1.rds", "gg2.rds", "gg3.rds")

tootips_for_charts <- lapply(seq_along(singlelayers), function(i) {
  p <- readRDS(singlelayers[i])
  p <- p$plot
  varDict <- list(Species = "Species")

  gt <- gridExtra::grid.arrange(p)[[1]][[1]]
  panel_idx <- grep(pattern = "panel", x = gt$grobs)

  fills <- lapply(panel_idx, function(p) {
    grob_children <- gt$grobs[[p]]$children
    rect_idx <- grep(pattern = "\\.rect\\.", x = names(grob_children))
    grob_children[[rect_idx]]$gp$fill
  })
  fills <- unlist(fills)

  print(fills)

  tooltips <- ggtips:::getTooltips(
    plot = p,
    varDict = varDict,
    plotScales = NULL,
    g = gt,
    callback = NULL,
    addAttributes = TRUE
  )

  nms <- names(tooltips)
  nms <- gsub(pattern = "rect", x = nms, replacement = "bars")
  names(tooltips) <- nms

  tooltips <- lapply(nms, function(n) {
    d <- tooltips[[n]][[1]]
    if (n == "bars") {
      tooltips[[n]]$colors <- fills
      tooltips[[n]]$data <- d
      tooltips[[n]][[1]] <- NULL
      return(tooltips[[n]])
    }
    d
  })
  names(tooltips) <- nms

  jsonlite::write_json(
    tooltips,
    path = file.path("output", paste0("tooltips", i, ".json")),
    auto_unbox = TRUE
  )
  ggplot2::ggsave(file=file.path("output", paste0("tooltips", i, ".svg")), plot=p)

  tooltips
})
