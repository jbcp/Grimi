graph_source_set <- function(x_value, y_value, fill_value, facet_value){

  if(x_value != ""){
    if(y_value != ""){
      if(fill_value != ""){
        pkg.globals$graph_source$Fill <- ggplot(data = pkg.globals$filter_data, aes_string(x = x_value, y = y_value, fill=fill_value))
        max_color <- length(unique(pkg.globals$filter_data[,fill_value]))
        pkg.globals$graph_source$Color <- scale_fill_manual(values=getPalette(pkg.globals$max_color))
      }
      else{
        pkg.globals$graph_source$Source <- ggplot(data = pkg.globals$filter_data, aes_string(x = x_value, y=y_value))
        pkg.globals$graph_source$Fill <- ""
        pkg.globals$max_color <- 0
      }
    }
    else{
      if(fill_value != ""){
        pkg.globals$graph_source$Fill <- ggplot(data = pkg.globals$filter_data, aes_string(x = x_value, fill=fill_value))
        pkg.globals$max_color <- length(unique(pkg.globals$filter_data[,fill_value]))
        pkg.globals$graph_source$Color <- scale_fill_manual(values=getPalette(pkg.globals$max_color))

      }
      else{
        pkg.globals$graph_source$Source <- ggplot(data = pkg.globals$filter_data, aes_string(x = x_value))
        pkg.globals$graph_source$Fill <- ""
        pkg.globals$max_color <- 0
      }
    }

  }
  else{
    pkg.globals$graph_source$Source <- ""
  }

  if(facet_value != ""){
    pkg.globals$graph_source$Facet <- facet_wrap(eval(parse(text = paste0("~", facet_value))))
  }
  else{
    pkg.globals$graph_source$Facet <- ""
  }
}
