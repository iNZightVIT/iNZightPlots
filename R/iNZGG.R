required_arguments <- list(
  pie = c("fill"),
  donut = c("fill"),
  column = c("x")
)

replace_data_name <- function(expr, new_name) {
  if (is.name(expr[[2]])) {
    expr[[2]] <- rlang::sym(new_name)
  } else {
    expr[[2]] <- replace_data_name(expr[[2]], new_name)
  }
  
  expr
}

insert_into_first_place <- function(expr, insert_expr) {
  if (is.name(expr[[2]])) {
    expr[[2]] <- rlang::expr(!!expr[[2]] %>% !!insert_expr)
  } else {
    expr[[2]] <- insert_into_first_place(expr[[2]], insert_expr)
  }
  
  expr
}

add_to_group <- function(expr, vars) {
  if (expr[[3]][[1]] == "dplyr::group_by") {
    expr[[3]] <- as.call(c(as.list(expr[[3]]), vars))
  } else if (expr[[3]][[1]] == "dplyr::ungroup") {
    expr[[3]] <- as.call(list(rlang::expr(dplyr::group_by), vars))
  }
  
  if (is.name(expr[[2]])) {
    expr
  } else {
    expr[[2]] <- add_to_group(expr[[2]], vars)
    expr
  }
}

apply_palette <- function(expr, palette, type) {
  viridis_names <- unname(unlist(viridis_palette_names()))
  colour_plots <- c("gg_cumcurve", "gg_lollipop", "gg_freqpolygon", "gg_barcode", "gg_dotstrip")
  
  if (palette %in% viridis_names) {
    if (type %in% colour_plots) {
      rlang::expr(!!expr + ggplot2::scale_colour_viridis_d(option = !!palette))
    } else {
      if (type != "gg_heatmap") {
        rlang::expr(!!expr + ggplot2::scale_fill_viridis_d(option = !!palette))
      } else {
        rlang::expr(!!expr + ggplot2::scale_fill_viridis_c(option = !!palette))
      }
    }
  } else {
    if (type %in% colour_plots) {
      rlang::expr(!!expr + ggplot2::scale_colour_brewer(palette = !!palette))
    } else {
      if (type != "gg_heatmap") {
        rlang::expr(!!expr + ggplot2::scale_fill_brewer(palette = !!palette))
      } else {
        rlang::expr(!!expr + ggplot2::scale_fill_distiller(palette = !!palette))
      }
    }
  }
}

remove_nas <- function(exprs) {
  
}

iNZightPlotGG_facet <- function(data, data_name, exprs, g1, g2, g1.level, g2.level) {
  if (!is.null(g1)) {
    if (g1.level != "_MULTI") {
      if (is.null(exprs$data)) {
        exprs <- list(
          data = rlang::expr(plot_data <- !!rlang::sym(data_name) %>% dplyr::filter(!!rlang::sym(g1) == !!g1.level)),
          plot = replace_data_name(exprs$plot, "plot_data")
        )
      } else {
        exprs$data[[3]] <- insert_into_first_place(exprs$data[[3]], rlang::expr(dplyr::filter(!!rlang::sym(g1) == !!g1.level)))
        exprs$data[[3]] <- add_to_group(exprs$data[[3]], rlang::sym(g1))
      }
    } else {
      if (!is.null(exprs$data)) {
        exprs$data[[3]] <- add_to_group(exprs$data[[3]], rlang::sym(g1))
      }
    }
  }
  
  if (!is.null(g2)) {
    if (g2.level != "_MULTI" && g2.level != "_ALL") {
      if (is.null(exprs$data)) {
        exprs <- list(
          data = rlang::expr(plot_data <- !!rlang::sym(data_name) %>% dplyr::filter(!!rlang::sym(g2) == !!g2.level)),
          plot = replace_data_name(exprs$plot, "plot_data")
        )
        
        exprs$data[[3]] <- add_to_group(exprs$data[[3]], rlang::sym(g2))
      } else {
        exprs$data[[3]] <- insert_into_first_place(exprs$data[[3]], rlang::expr(dplyr::filter(!!rlang::sym(g2) == !!g2.level)))
        exprs$data[[3]] <- add_to_group(exprs$data[[3]], rlang::sym(g2))
        
      }
      
    } else {
      if (!is.null(exprs$data)) {
        exprs$data[[3]] <- add_to_group(exprs$data[[3]], rlang::sym(g2))
      }
    }
    
  }
  
  if (isTRUE(is.null(g2))) {
    exprs$plot <- rlang::expr(!!exprs$plot + ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym(g1)), labeller = ggplot2::label_both))
  } else {
    exprs$plot <- rlang::expr(!!exprs$plot + ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym(g1)), cols = ggplot2::vars(!!rlang::sym(g2)), labeller = ggplot2::label_both))
  }
  
  # print(exprs)
  
  exprs
}

iNZightPlotGG_decide <- function(data, varnames, type, extra_vars) {
  varnames <- varnames[grep("\\.level$", names(varnames), invert = TRUE)]
  varnames <- varnames[grep("g1", names(varnames), invert = TRUE)]
  varnames <- varnames[grep("g2", names(varnames), invert = TRUE)]
  nullVars <- vapply(data[, varnames, drop = FALSE], is.null, FUN.VALUE = logical(1))
  varnames[which(nullVars)] <- NULL
  varnames[!varnames %in% colnames(data)] <- NULL

  if (type %in% c("gg_pie", "gg_donut")) {
    names(varnames) <- replace(names(varnames), names(varnames) == "x", "fill")
  } else if (type %in% c("gg_violin", "gg_barcode", "gg_boxplot", "gg_cumcurve", "gg_column2", "gg_lollipop", "gg_dotstrip", "gg_density")) {
    if (!("y" %in% names(varnames))) {
      names(varnames) <- replace(names(varnames), names(varnames) == "x", "y")
      if (isTRUE(!is.null(extra_vars$fill_colour) && extra_vars$fill_colour != "")) {
        varnames["fill"] <- extra_vars$fill_colour
      } else {
        varnames["fill"] <- "darkgreen"
      }
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
  } else if (type == "gg_poppyramid") {
    if (is.numeric(data[[varnames["x"]]])) {
      names(varnames) <- replace(names(varnames), names(varnames) == "y", "fill")
    } else {
      names(varnames) <- replace(names(varnames), names(varnames) == "x", "fill")
      names(varnames) <- replace(names(varnames), names(varnames) == "y", "x")
    }
  } else if (type == "gg_spine") {
    names(varnames) <- replace(names(varnames), names(varnames) == "y", "fill")
  } else if (type == "gg_freqpolygon") {
    names(varnames) <- replace(names(varnames), names(varnames) == "y", "colour")
  } else if (type == "gg_column") {
    if ("y" %in% names(varnames)) {
      names(varnames) <- replace(names(varnames), names(varnames) == "y", "group")
    }
  }
  
  if (type %in% c("gg_lollipop", "gg_column2") && !is.null(extra_vars$desc)) {
    varnames <- c(as.list(varnames), desc = extra_vars$desc)
  }
  
  if (type %in% c("gg_lollipop", "gg_column2") && !is.null(extra_vars$labels)) {
    varnames <- c(as.list(varnames), labels = extra_vars$labels)
  }

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
iNZightPlotGG <- function(
  data, 
  type, 
  data_name = "data", 
  ...,
  main = NULL, 
  xlab = NULL, 
  ylab = NULL, 
  extra_args = c(), 
  palette = "default"
) {
  dots <- list(...)

  if (length(extra_args) > 0) {
    print(extra_args)
    rotate <- extra_args$rotation
    desc <- extra_args$desc
    # percent <- extra_args$percent
    extra_args <- c(
      iNZightPlotGG_extraargs(extra_args), 
      desc = desc, 
      labels = extra_args$labelVar,
      fill_colour = extra_args$fill_colour
    )
    
    
  }
  
  plot_args <- iNZightPlotGG_decide(data, unlist(dots), type, extra_args)

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
  
  if (isTRUE(!is.null(dots$g1))) {
    plot_exprs <- iNZightPlotGG_facet(data, data_name, plot_exprs, dots$g1, dots$g2, dots$g1.level, dots$g2.level)
  }
  
  if (isTRUE(rotate) && !(type %in% c("gg_pie", "gg_donut"))) {
    plot_exprs$plot <- rotate(plot_exprs$plot)
  }
  
  if (isTRUE(!missing(palette) && !is.null(palette) && palette != "default")) {
    plot_exprs$plot <- apply_palette(plot_exprs$plot, palette, type)
  }

  # if (isTRUE(percent) && type %in% c("gg_column")) {
  #   plot_exprs$plot <- rlang::expr(!!plot_exprs$plot + ggplot2::scale_y_continuous(labels = scales::percent))
  # }
  
  cat(unname(unlist(lapply(plot_exprs, rlang::expr_text))), sep = "\n\n")
  
  eval_env <- rlang::env(!!rlang::sym(data_name) := data)

  eval_results <- lapply(plot_exprs, eval, envir = eval_env)
  
  plot_object <- eval_results[[length(eval_results)]]
  
  dev.hold()
  print(plot_object)
  dev.flush()
  
  attr(plot_object, "code") <- unname(unlist(lapply(plot_exprs, rlang::expr_text)))
  attr(plot_object, "plottype") <- c(type)
  attr(plot_object, "varnames") <- unlist(dots)
  
  plot_object
}

iNZightPlotGG_pie <- function(data, fill, main = sprintf("Pie Chart of %s", as.character(fill)), ...) {
  fill <- rlang::sym(fill)

  plot_expr <- rlang::expr(
    ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = factor(1), fill = !!fill)) + 
      ggplot2::geom_bar(ggplot2::aes(y = ..count../sum(..count..)), position = "fill") +
      ggplot2::coord_polar(theta = "y") + 
      ggplot2::xlab("") + 
      ggplot2::ylab("") + 
      ggplot2::scale_y_continuous(labels = scales::percent) + 
      ggplot2::scale_x_discrete(breaks = NULL) + 
      ggplot2::ggtitle(!!main)  +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x      = ggplot2::element_blank()
      )
  )

  list(
    plot = plot_expr
  )
}

iNZightPlotGG_donut <- function(data, fill, main = sprintf("Donut Chart of %s", as.character(fill)), ...) {
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
      ggplot2::ggtitle(!!main)  +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x      = ggplot2::element_blank()
      )
  )

  list(
    data = data_expr,
    plot = plot_expr
  )
}

iNZightPlotGG_column <- function(data, x, group, main = sprintf("Column chart of %s", as.character(x)), xlab = as.character(x), ylab = "Count", ...) {
  x <- rlang::sym(x)
  
  if (missing(group)) {
    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, fill = !!x)) + 
        ggplot2::geom_bar() + 
        ggplot2::labs(title = !!main) + 
        ggplot2::xlab(!!xlab) + 
        ggplot2::ylab(!!ylab)
    )
  } else {
    group <- rlang::sym(group)
    
    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, fill = !!group)) + 
        ggplot2::geom_bar(position = "dodge") + 
        ggplot2::labs(title = !!main) + 
        ggplot2::xlab(!!xlab) + 
        ggplot2::ylab(!!ylab)
    )
  }
  

  
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

iNZightPlotGG_heatmap <- function(data, x, y, main = sprintf("Heatmap of %s and %s", as.character(x), as.character(y)), xlab = as.character(x), ylab = as.character(y), ...) {
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
      ggplot2::labs(title = !!main) + 
      ggplot2::xlab(!!xlab) + 
      ggplot2::ylab(!!ylab)
  )

  list(
    data = data_expr,
    plot = plot_expr
  )
}

iNZightPlotGG_stackedcolumn <- function(data, fill, main = sprintf("Stacked column of %s", as.character(fill)), x, xlab = as.character(x), ylab = "Percent", ...) {
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
      ggplot2::scale_y_continuous(labels = scales::percent) + 
      ggplot2::labs(title = !!main) + 
      ggplot2::xlab(!!xlab) + 
      ggplot2::ylab(!!ylab)
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
        ggplot2::xlab(!!xlab)
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

iNZightPlotGG_violin <- function(data, x, y, fill = "darkgreen", main = sprintf("Distribution of %s", as.character(y)), xlab = as.character(x), ylab = as.character(y), ...) {
  y <- rlang::sym(y)
  
  if (missing(x)) {
    x <- rlang::expr(factor(1))
    # fill <- rlang::expr(fill)
    
    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, y = !!y)) + 
        ggplot2::geom_violin(fill = !!fill) + 
        ggplot2::labs(title = !!main) + 
        ggplot2::xlab(!!xlab) + 
        ggplot2::ylab(!!ylab)
    )
  } else {
    x <- rlang::sym(x)
    fill <- rlang::sym(x)
    
    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, y = !!y, fill = !!fill)) + 
        ggplot2::geom_violin() + 
        ggplot2::labs(title = !!main) + 
        ggplot2::xlab(!!xlab) + 
        ggplot2::ylab(!!ylab)
    )
  }

  list(
    plot = plot_expr
  )
}

iNZightPlotGG_barcode <- function(data, x, y, fill = "darkgreen", main = sprintf("Distribution of %s", as.character(y)), xlab = as.character(y), ylab = as.character(x), ...) {
  y <- rlang::sym(y)
  
  if (missing(x)) {
    x <- rlang::expr(factor(1))

    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!y, y = !!x)) + 
        ggplot2::geom_point(shape = "|", size = 16, alpha = 0.2, colour = !!fill) + 
        ggplot2::labs(title = !!main) + 
        ggplot2::xlab(!!xlab) + 
        ggplot2::ylab(!!ylab)
    )
  } else {
    x <- rlang::sym(x)
    colour <- rlang::sym(x)
    
    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!y, y = !!x, colour = !!colour)) + 
        ggplot2::geom_point(shape = "|", size = 16, alpha = 0.2) + 
        ggplot2::labs(title = !!main) + 
        ggplot2::xlab(!!xlab) + 
        ggplot2::ylab(!!ylab)
    )
  }
  
  list(
    plot = plot_expr
  )
}

iNZightPlotGG_boxplot <- function(data, x, y, fill = "darkgreen", main = sprintf("Distribution of %s", as.character(y)), xlab = as.character(x), ylab = as.character(y), ...) {
  y <- rlang::sym(y)
  
  if (missing(x)) {
    x <- rlang::expr(factor(1))
    
    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, y = !!y)) +
        ggplot2::geom_boxplot(fill = !!fill) + 
        ggplot2::labs(title = !!main) + 
        ggplot2::xlab(!!xlab) + 
        ggplot2::ylab(!!ylab)
    )
  } else {
    x <- rlang::sym(x)
    fill <- rlang::sym(x)
    
    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, y = !!y, fill = !!fill)) +
        ggplot2::geom_boxplot() + 
        ggplot2::labs(title = !!main) + 
        ggplot2::xlab(!!xlab) + 
        ggplot2::ylab(!!ylab)
    )
  }
  
  list(
    plot = plot_expr
  )
}

iNZightPlotGG_column2 <- function(data, x, y, main = sprintf("Distribution of %s", as.character(y)), xlab = "Index", ylab = as.character(y), desc = FALSE, labels, ...) {
  if (missing(x)) {
    x <- rlang::expr(1:nrow(!!rlang::enexpr(data)))
  } else {
    x <- rlang::sym(x)
  }
  
  y <- rlang::sym(y)
  
  if (desc) {
    data_expr <- rlang::expr(
      plot_data <- !!rlang::enexpr(data) %>% 
        dplyr::arrange(dplyr::desc(!!y))
    )
  } else {
    data_expr <- rlang::expr(
      plot_data <- !!rlang::enexpr(data) %>% 
        dplyr::arrange(!!y)
    )
  }
  
  plot_expr <- rlang::expr(
    ggplot2::ggplot(plot_data, ggplot2::aes(x = !!x, y = !!y)) + 
      ggplot2::geom_col() + 
      ggplot2::labs(title = !!main) + 
      ggplot2::xlab(!!xlab) + 
      ggplot2::ylab(!!ylab)
  )
  
  list(
    data = data_expr,
    plot = plot_expr
  )
}

iNZightPlotGG_lollipop <- function(data, x, y, main = sprintf("Distribution of %s", as.character(y)), xlab = "Index", ylab = as.character(y), desc = FALSE, labels, ...) {
  if (missing(x)) {
    if (missing(labels) || labels == "") {
      x <- rlang::expr(1:nrow(!!rlang::enexpr(data)))
    } else {
      x <- rlang::sym(labels)
    }
    
  } else {
    x <- rlang::sym(x)
  }
  
  y <- rlang::sym(y)
  
  if (desc) {
    data_expr <- rlang::expr(
      plot_data <- !!rlang::enexpr(data) %>% 
        dplyr::arrange(dplyr::desc(!!y))
    )
  } else {
    data_expr <- rlang::expr(
      plot_data <- !!rlang::enexpr(data) %>% 
        dplyr::arrange(!!y)
    )
  }

  
  plot_expr <- rlang::expr(
    ggplot2::ggplot(plot_data, ggplot2::aes(x = !!x, y = !!y)) + 
      ggplot2::geom_segment(ggplot2::aes(xend = !!x, yend = 0)) + 
      ggplot2::geom_point() + 
      ggplot2::labs(title = !!main) +
      ggplot2::xlab(!!xlab) + 
      ggplot2::ylab(!!ylab)
  )

  list(
    data = data_expr,
    plot = plot_expr
  )
}

iNZightPlotGG_cumcurve <- function(data, x, y, main = sprintf("Cumulative Count of %s", as.character(y)), xlab = as.character(y), ylab = "Cumulative Frequency", ...) {
  y <- rlang::sym(y)
  
  if (missing(x)) {
    data_expr <- rlang::expr(
      plot_data <- !!rlang::enexpr(data) %>% 
        dplyr::arrange(!!y) %>% 
        dplyr::mutate(Observation = 1:dplyr::n())
    )
    
    plot_expr <- rlang::expr(
      ggplot2::ggplot(plot_data, ggplot2::aes(x = !!y, y = Observation)) + 
        ggplot2::geom_step() + 
        ggplot2::labs(title = !!main) +
        ggplot2::xlab(!!xlab) + 
        ggplot2::ylab(!!ylab)
    )
  } else {
    x <- rlang::sym(x)
    
    data_expr <- rlang::expr(
      plot_data <- !!rlang::enexpr(data) %>% 
        dplyr::group_by(!!x) %>% 
        dplyr::arrange(!!x, !!y) %>% 
        dplyr::mutate(Observation = 1:dplyr::n())
    )
    
    plot_expr <- rlang::expr(
      ggplot2::ggplot(plot_data, ggplot2::aes(x = !!y, y = Observation, colour = !!x)) + 
        ggplot2::geom_step() + 
        ggplot2::labs(title = !!main) + 
        ggplot2::xlab(!!xlab) + 
        ggplot2::ylab(!!ylab)
    )
  }

  list(
    data = data_expr,
    plot = plot_expr
  )
  
}

iNZightPlotGG_poppyramid <- function(data, x, fill, main = sprintf("Count of %s by %s", as.character(x), as.character(fill)), xlab = as.character(x), ylab = "Count", ...) {
  x <- rlang::sym(x)
  fill <- rlang::sym(fill)
  
  plot_expr <- rlang::expr(
    ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, fill = !!fill)) + 
      ggplot2::geom_bar(data = subset(!!rlang::enexpr(data), !!fill == levels(!!fill)[1])) + 
      ggplot2::geom_bar(data = subset(!!rlang::enexpr(data), !!fill == levels(!!fill)[2]), ggplot2::aes(y = stat(count * -1))) + 
      ggplot2::coord_flip() + 
      ggplot2::labs(title = !!main) + 
      ggplot2::xlab(!!xlab) + 
      ggplot2::ylab(!!ylab)
  )
  
  list(
    plot = plot_expr
  )
  
}

iNZightPlotGG_spine <- iNZightPlotGG_poppyramid

iNZightPlotGG_freqpolygon <- function(data, x, colour, main = sprintf("Count of %s by %s", as.character(x), as.character(colour)), xlab = as.character(x), ylab = "Count", ...) {
  x <- rlang::sym(x)
  colour <- rlang::sym(colour)
  
  plot_expr <- rlang::expr(
    ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!x, colour = !!colour, group = !!colour)) + 
      ggplot2::geom_line(stat = "count") + 
      ggplot2::geom_point(stat = "count", size = 4) + 
      ggplot2::labs(title = !!main) + 
      ggplot2::xlab(!!xlab) + 
      ggplot2::ylab(!!ylab)
  )
  
  list(
    plot = plot_expr
  )
}

iNZightPlotGG_dotstrip <- function(data, x, y, fill = "darkgreen", main = sprintf("Distribution of %s", as.character(y)), xlab = as.character(y), ylab = as.character(x), ...) {
  y <- rlang::sym(y)
  
  if (missing(x)) {
    x <- rlang::expr(factor(1))

    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!y, y = !!x)) + 
        ggplot2::geom_point(alpha = 0.2, colour = !!fill) + 
        ggplot2::labs(title = !!main) + 
        ggplot2::xlab(!!xlab) + 
        ggplot2::ylab(!!ylab)
    )
    
  } else {
    x <- rlang::sym(x)
    colour <- rlang::sym(x)
    
    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!y, y = !!x, colour = !!colour)) + 
        ggplot2::geom_point(alpha = 0.2) + 
        ggplot2::labs(title = !!main) + 
        ggplot2::xlab(!!xlab) + 
        ggplot2::ylab(!!ylab)
    )
  }

  list(
    plot = plot_expr
  )
}

iNZightPlotGG_density <- function(data, x, y, fill = "darkgreen", main = sprintf("Distribution of %s", as.character(y)), xlab = as.character(y), ylab = "Density", ...) {
  y <- rlang::sym(y)
  
  if (missing(x)) {
    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!y)) + 
        ggplot2::geom_density(fill = !!fill, alpha = 0.2) + 
        ggplot2::labs(title = !!main) + 
        ggplot2::xlab(!!xlab) + 
        ggplot2::ylab(!!ylab)
    )
  } else {
    fill <- rlang::sym(x)
    
    plot_expr <- rlang::expr(
      ggplot2::ggplot(!!rlang::enexpr(data), ggplot2::aes(x = !!y, fill = !!fill)) + 
        ggplot2::geom_density(alpha = 0.4) + 
        ggplot2::labs(title = !!main) + 
        ggplot2::xlab(!!xlab) + 
        ggplot2::ylab(!!ylab)
    )
  }

  list(
    plot = plot_expr
  )
}

