##' @importFrom magrittr "%>%"
iNZightPlotGG <- function(data, type, data_name = "data", ...) {
  plot_exprs <- eval(call(sprintf("iNZightPlotGG_%s", gsub("^gg_", "", type)), rlang::sym(data_name), quote(...)))
  
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
      ggplot2::scale_x_discrete(labels = "") + 
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
      ggplot2::xlim(c(0, 4)) +
      ggplot2::scale_y_continuous(labels = scales::percent) + 
      ggplot2::ggtitle(!!main)
  )

  list(
    data = data_expr,
    plot = plot_expr
  )
}

iNZightPlotGG_column <- function(data, x, main = "Column chart", ...) {
  x <- rlang::sym(x)
  
  plot_expr <- rlang::expr(
    ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x)) + 
      ggplot2::geom_bar()
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

iNZightPlotGG_stackedcolumn <- function(data, fill, main = "Stacked column", ...) {
  fill = rlang::sym(fill)
  
  plot_expr <- rlang::expr(
    ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = factor(1), fill = !!fill)) + 
      ggplot2::geom_bar(ggplot2::aes(y = ..count../sum(..count..))) +
      ggplot2::xlab("") + 
      ggplot2::ylab("") + 
      ggplot2::scale_y_continuous(labels = scales::percent) + 
      ggplot2::scale_x_discrete(labels = "") + 
      ggplot2::ggtitle(!!main)
  )
  
  list(
    plot = plot_expr
  )
}

iNZightPlotGG_stackedbar <- function(data, fill, main = "Stacked bar", ...) {
  column_plot <- iNZightPlotGG_stackedcolumn(!!rlang::enexpr(data), fill, main, ...)
  
  column_plot$plot <- rotate(column_plot$plot)
  
  column_plot
}

iNZightPlotGG_violin <- function(data, x, y, main = "Violin chart", ...) {
  if (missing(x)) {
    x <- rlang::expr(factor(1))
  } else {
    x <- rlang::sym(x)
  }
  
  y <- rlang::sym(y)
  
  plot_expr <- rlang::expr(
    ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, y = !!y)) + 
      ggplot2::geom_violin()
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

