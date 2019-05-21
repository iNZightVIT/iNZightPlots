required_arguments <- list(
  pie = c("fill"),
  donut = c("fill"),
  column = c("x")
)

iNZightPlotGG_decide <- function(data, varnames, type) {
  nullVars <- vapply(data[, varnames, drop = FALSE], is.null, FUN.VALUE = logical(1))
  varnames[which(nullVars)] <- NULL
  varnames[!varnames %in% colnames(data)] <- NULL
  
  if (type %in% c("gg_pie", "gg_donut")) {
    names(varnames) <- replace(names(varnames), names(varnames) == "x", "fill")
  } else if (type %in% c("gg_violin", "gg_barcode")) {
    if (!("y" %in% names(varnames))) {
      names(varnames) <- replace(names(varnames), names(varnames) == "x", "y")
    } else if (is.numeric(data[[varnames["x"]]])) {
      orig_x <- varnames["x"]
      varnames["x"] <- varnames["y"]
      varnames["y"] <- orig_x
    }
  } else if (type %in% c("gg_stackedbar", "gg_stackedcolumn")) {
    names(varnames) <- replace(names(varnames), names(varnames) == "x", "fill")
    if ("y" %in% names(varnames)) {
      names(varnames) <- replace(names(varnames), names(varnames) == "y", "x")
    }
  }
  
  print(varnames)
  
  varnames
}

iNZightPlotGG_extraargs <- function(extra_args) {
  to.keep <- c("shape" = "pch", "colour" = "col.pt", "size" = "cex", "alpha" = "alpha", "bg" = "bg")
  extra_args <- extra_args[to.keep]
  
  changed_args <- Filter(function(x) extra_args[[x]] != inzpar()[[x]], names(extra_args))
  
  return_args <- extra_args[changed_args]
  names(return_args) <- names(to.keep)[match(names(return_args), to.keep)]
  
  return_args
}

##' @importFrom magrittr "%>%"
iNZightPlotGG <- function(data, type, data_name = "data", main, xlab, ylab, extra_args = c(), ...) {
  extra_args <- iNZightPlotGG_extraargs(extra_args)
  plot_args <- iNZightPlotGG_decide(data, unlist(list(...)), type)
  plot_exprs <- do.call(
    sprintf("iNZightPlotGG_%s", gsub("^gg_", "", type)), 
    c(rlang::sym(data_name), main = main, xlab = xlab, ylab = ylab, plot_args)
  )
  
  if (isTRUE(extra_args$size != 1)) {
    plot_exprs$plot <- rlang::expr(!!plot_exprs$plot + ggplot2::theme(text = ggplot2::element_text(size = !!(extra_args$size * 11))))
  }
  
  if (isTRUE(extra_args$bg != "lightgrey")) {
    plot_exprs$plot <- rlang::expr(!!plot_exprs$plot + ggplot2::theme(panel.background = ggplot2::element_rect(fill = !!extra_args$bg)))
  }
  
  eval_env <- rlang::env(!!rlang::sym(data_name) := data)

  eval_results <- lapply(plot_exprs, eval, envir = eval_env)
  
  plot_object <- eval_results[[length(eval_results)]]
  
  print(plot_object)
  
  attr(plot_object, "code") <- unname(unlist(lapply(plot_exprs, rlang::expr_text)))
  attr(plot_object, "plottype") <- c(type)
  
  cat(attr(plot_object, "code"), sep = "\n\n")
  
  plot_object
}

iNZightPlotGG_pie <- function(data, fill, main = "Pie Chart", ...) {
  fill = rlang::sym(fill)

  plot_expr <- rlang::expr(
    ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = factor(1), fill = !!fill)) + 
      ggplot2::geom_bar(ggplot2::aes(y = ..count../sum(..count..))) +
      ggplot2::coord_polar(theta = "y") + 
      ggplot2::xlab("") + 
      ggplot2::ylab("") + 
      ggplot2::scale_y_continuous(labels = scales::percent) + 
      ggplot2::scale_x_discrete(breaks = NULL) + 
      ggplot2::ggtitle(!!main)
  )

  list(
    plot = plot_expr
  )
}

iNZightPlotGG_donut <- function(data, fill, main = "Donut Chart", ...) {
  fill <- rlang::sym(fill)

  data_expr <- rlang::expr(
    plot_data <- !!rlang::enexpr(data) %>% 
      dplyr::group_by(!!fill) %>% 
      dplyr::summarise(Count = dplyr::n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(Fraction = Count / sum(Count)) %>% 
      dplyr::arrange(dplyr::desc(Fraction)) %>% 
      dplyr::mutate(ymax = cumsum(Fraction)) %>% 
      dplyr::mutate(ymin = dplyr::lag(ymax, default = 0))
  )
  
  plot_expr <- rlang::expr(
    ggplot2::ggplot(plot_data, ggplot2::aes(fill = !!fill, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) + 
      ggplot2::geom_rect() +
      ggplot2::coord_polar(theta = "y") + 
      ggplot2::xlab("") + 
      ggplot2::ylab("") + 
      ggplot2::scale_x_continuous(breaks = NULL, limits = c(0, 4)) +
      ggplot2::scale_y_continuous(labels = scales::percent) + 
      ggplot2::ggtitle(!!main)
  )

  list(
    data = data_expr,
    plot = plot_expr
  )
}

iNZightPlotGG_column <- function(data, x, main = "Column chart", xlab = as.character(x), ylab = "Percent", ...) {
  x <- rlang::sym(x)
  
  plot_expr <- rlang::expr(
    ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, fill = !!x)) + 
      ggplot2::geom_bar() + 
      ggplot2::xlab(!!xlab) + 
      ggplot2::ylab(!!ylab)
  )
  
  list(
    plot = plot_expr
  )
}

rotate <- function(plot_expr) {
  rlang::expr(!!plot_expr + ggplot2::coord_flip())
}

iNZightPlotGG_bar <- function(data, x, main = "Bar chart", ...) {
  column_plot <- iNZightPlotGG_column(data, x, main, ...)
  
  column_plot$plot <- rotate(column_plot$plot)
  
  column_plot
}

iNZightPlotGG_heatmap <- function(data, x, y, main = "XY Heatmap", ...) {
  x <- rlang::sym(x)
  y <- rlang::sym(y)
  
  data_expr <- rlang::expr(
    plot_data <- !!rlang::enexpr(data) %>% 
      dplyr::group_by(!!x, !!y) %>% 
      dplyr::summarise(Count = dplyr::n())
  )
  
  plot_expr <- rlang::expr(
    ggplot2::ggplot(plot_data, ggplot2::aes(x = !!x, y = !!y)) +
      ggplot2::geom_tile(ggplot2::aes(fill = Count)) +
      ggplot2::ggtitle(!!main)
  )

  list(
    data = data_expr,
    plot = plot_expr
  )
}

iNZightPlotGG_stackedcolumn <- function(data, fill, main = "Stacked column", x, xlab = as.character(x), ylab = "Percent", ...) {
  fill = rlang::sym(fill)
  
  if (missing(x)) {
    x <- rlang::expr(factor(1))
    was_missing <- TRUE
  } else {
    x <- rlang::sym(x)
    was_missing <- FALSE
  }
  
  plot_expr <- rlang::expr(
    ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, fill = !!fill)) + 
      ggplot2::geom_bar(ggplot2::aes(y = ..count../sum(..count..)), position = "fill") +
      ggplot2::xlab(!!xlab) + 
      ggplot2::ylab(!!ylab) + 
      ggplot2::scale_y_continuous(labels = scales::percent) + 
      ggplot2::ggtitle(!!main)
  )
  
  if (isTRUE(was_missing)) {
    plot_expr <- rlang::expr(
      !!plot_expr + 
        ggplot2::scale_x_discrete(breaks = NULL) + 
        ggplot2::xlab("")
    )
  } else {
    plot_expr <- rlang::expr(
      !!plot_expr +
        ggplot2::xlab(!!as.character(x))
    )
  }
  
  list(
    plot = plot_expr
  )
}

iNZightPlotGG_stackedbar <- function(data, fill, main = "Stacked bar", x, ...) {
  column_plot <- iNZightPlotGG_stackedcolumn(!!rlang::enexpr(data), fill, main, x, ...)
  
  column_plot$plot <- rotate(column_plot$plot)
  
  column_plot
}

iNZightPlotGG_violin <- function(data, x, y, main = "Violin chart", xlab = as.character(x), ylab = as.character(y), ...) {
  if (missing(x)) {
    x <- rlang::expr(factor(1))
    fill <- NULL
  } else {
    x <- rlang::sym(x)
    fill <- rlang::sym(x)
  }
  
  y <- rlang::sym(y)
  
  plot_expr <- rlang::expr(
    ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, y = !!y, fill = !!fill)) + 
      ggplot2::geom_violin() + 
      ggplot2::xlab(!!xlab) + 
      ggplot2::ylab(!!ylab)
  )
  
  list(
    plot = plot_expr
  )
}

iNZightPlotGG_barcode <- function(data, x, y, main = "Barcode chart", ...) {
  if (missing(x)) {
    x <- rlang::expr(factor(1))
  } else {
    x <- rlang::sym(x)
  }
  
  y <- rlang::sym(y)
  
  plot_expr <- rlang::expr(
    ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, y = !!y)) + 
      ggplot2::geom_point(shape = "|", size = 16, alpha = 0.2) + 
      ggplot2::coord_flip()
  )
  
  list(
    plot = plot_expr
  )
}

iNZightPlotGG_boxplot <- function(data, x, y, main = "Barchart", ...) {
  x <- rlang::sym(x)
  y <- rlang::sym(y)
  
  plot_expr <- rlang::expr(
    ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, y = !!y)) +
      ggplot2::geom_boxplot()
  )
  
  list(
    plot = plot_expr
  )
}
