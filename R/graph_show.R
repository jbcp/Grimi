graph_show <- function(pkg.globals){
  if(typeof(pkg.globals$graph_source$Fill) == "character") pkg.globals$graph <- pkg.globals$graph_source$Source
  else pkg.globals$graph <- pkg.globals$graph_source$Fill
  
  # print(typeof(graph))
  if(typeof(pkg.globals$graph) != "character"){
    if(typeof(pkg.globals$graph_source$Type) != "character") pkg.globals$graph <- pkg.globals$graph + pkg.globals$graph_source$Type
    if(typeof(pkg.globals$graph_source$Label) != "character") pkg.globals$graph <- pkg.globals$graph + pkg.globals$graph_source$Label
    if(typeof(pkg.globals$graph_source$Color) != "character") pkg.globals$graph <- pkg.globals$graph + pkg.globals$graph_source$Color
    if(typeof(pkg.globals$graph_source$Theme) != "character") pkg.globals$graph <- pkg.globals$graph + pkg.globals$graph_source$Theme
    if(typeof(pkg.globals$graph_source$Smooth) != "character") pkg.globals$graph <- pkg.globals$graph + pkg.globals$graph_source$Smooth
    if(typeof(pkg.globals$graph_source$Source)!= "character"){
      if(typeof(pkg.globals$graph_source$Title) != "character") pkg.globals$graph <- pkg.globals$graph + pkg.globals$graph_source$Title
      if(typeof(pkg.globals$graph_source$X_lab) != "character") pkg.globals$graph <- pkg.globals$graph + pkg.globals$graph_source$X_lab
      if(typeof(pkg.globals$graph_source$Y_lab) != "character") pkg.globals$graph <- pkg.globals$graph + pkg.globals$graph_source$Y_lab
      if(pkg.globals$graph_source$Flip == TRUE) pkg.globals$graph <-  pkg.globals$graph + coord_flip()
      pkg.globals$graph <- pkg.globals$graph + theme(
        plot.title = element_text(hjust = 0.5, size = pkg.globals$graph_source$Title_size, face="bold.italic", color = pkg.globals$graph_source$Title_color),
        axis.title.x = element_text(size = pkg.globals$graph_source$x_label_size, color = pkg.globals$graph_source$x_label_color),
        axis.title.y = element_text(size = pkg.globals$graph_source$y_label_size, color = pkg.globals$graph_source$y_label_color)
      )
    }
    if(typeof(pkg.globals$graph_source$Facet) != "character") pkg.globals$graph <- pkg.globals$graph + pkg.globals$graph_source$Facet
  }
}
