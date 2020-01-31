graph_type_set <- function(data, x_value="", y_value="", fill_value=""){

  if(x_value == "") return (c(""))
  else{
    if(y_value == "") return (c("Histogram"))
    else{
      if(typeof(data[, y_value]) != "character") return (c("Bar", "Line", "Area", "Point", "Density", "Boxplot", "Violin", "DotPlot"))
      else return (c("Histogram"))
    }
  }
  
}