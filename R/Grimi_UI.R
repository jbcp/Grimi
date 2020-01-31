Grimi_UI <-  shinyUI({
    pkg.globals <- new.env()
    # ##Color

    color_name <- data.frame(
      color = c("Set1", "Set2", "Set3", "Pastel1", "Pastel2", "Paired", "Dark2", "Accent",
                "YlOrRd", "YlOrBr", "YlGnBu", "YlGn", "Reds", "RdPu", "Purples", "PuRd", "PuBuGn", "OrRd", "Oranges",
                "Greys", "Greens", "GnBu", "BnPu", "BuGn", "Blues",
                "Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG"),
      count = c(9, 8, 12, 9, 8, 12, 8, 8,
                9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
                9, 9, 9, 9, 9, 9,
                11, 11, 11, 11, 11, 11, 11, 11, 11)
    )

    graph_type <- data.frame(
      val = c("Histogram", "Bar", "Line", "Area", "Point", "Density", "Boxplot", "Violin")
    )

    graph_type$img = c(
      sprintf("<img src = 'Grimi/img/bar-graph.png' width = '20px' height = '20px'>"),
      sprintf("<img src = 'Grimi/img/bar-graph.png' width = '20px' height = '20px'>"),
      sprintf("<img src = 'Grimi/img/line-graph.png' width = '20px' height = '20px'>"),
      sprintf("<img src = 'Grimi/img/area-graph.png' width = '20px' height = '20px'>"),
      sprintf("<img src = 'Grimi/img/dot-graph.png' width = '20px' height = '20px'>"),
      sprintf("<img src = 'Grimi/img/density-graph.png' width = '20px' height = '20px'>"),
      sprintf("<img src = 'Grimi//boxplot-graph.png' width = '15px' height = '20px'>"),
      sprintf("<img src = 'Grimi/img/violin-graph.png' width = '15px' height = '20px'>")
    )

    line_type <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "blank")
    getPalette <- colorRampPalette(brewer.pal(9,"Set1"))

    ##Theme
    theme_name <- data.frame(
      theme_name = c("grey", "gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void", "test")
    )

    options <- c("width", "line_size", "option_dodge", "option_add_point", "option_arrow", "option_linetype", "option_color", "option_size", "option_shape", "option_adjust", "option_lineend", "option_notch", "line_size", 'outlier_shape', 'outlier_size', 'option_add_average', 'option_alpha' )

    usedata <- iris
    filtered_data <- usedata
    pkg.globals$filter_data <- usedata


    pkg.globals$graph_source <- list(Source = "", Fill = "", Type = "", Label = "", Color = "", Theme = "", Title = "", X_lab = "", Y_lab="", Flip ="", Title_size = 30, x_label_size = 10, y_label_size = 10, Title_color = "#000000", x_label_color ="#000000", y_label_color = "#000000")
    pkg.globals$graph <- ""
    pkg.globals$x_value <- ""
    pkg.globals$y_value <- ""
    pkg.globals$fill_value <- ""
    pkg.globals$theme_value <- ""
    pkg.globals$facet_value <- ""
    pkg.globals$type_value <- ""
    pkg.globals$color_value <- ""
    pkg.globals$single_color_value <- "#F8766D"
    pkg.globals$size_value <- ""

    ##Variable for colorbrewer
    pkg.globals$max_color <- 1
    pkg.globals$label_pos <- 0.5
    pkg.globals$label_check <- FALSE
    pkg.globals$legend_pos <- "left"
    pkg.globals$f_name <- ""
    pkg.globals$font_select <- "title"
    pkg.globals$save_select <- ""
    pkg.globals$filter <- c()
    pkg.globals$option_setting <- list(width = 0.09, line_size = "", dodge = "", add_point = "", linetype = "", color = "", size = "", shape = "", arrow = "", lineend = "", alpha = "", notch="", adjust="", single_color="")

    fluidPage(
      htmlTemplate(system.file("www", "main.html", package = "Grimi"),
                   button = fluidRow(
                     column(12, id="source", class="drag-source", style="text-align: center",
                            lapply(1:ncol(usedata), function(i){
                              if(class(usedata[0, i]) == "integer"){
                                tags$button(class="btn btn-sm btn-rounded btn-primary", style="width:90px; margin-top:2px; margin-left:4px", colnames(usedata)[i], id=colnames(usedata)[i])
                              }
                              else if(class(usedata[0, i]) == "numeric"){
                                tags$button(class="btn btn-sm btn-rounded btn-primary", style="width:90px; margin-top:2px; margin-left:4px", colnames(usedata)[i], id=colnames(usedata)[i])
                              }
                              else{
                                tags$button(class="btn btn-sm btn-rounded btn-success", style="width:90px; margin-top:2px; margin-left:4px", colnames(usedata)[i], id=colnames(usedata)[i])
                              }
                            })
                     )
                   ),
                   variable_x = tags$div(class="drag-container", id="variable_x", style="height:30px"),
                   variable_y = tags$div(class="drag-container", id="variable_y", style="height:30px"),
                   variable_type = selectInput(inputId = "variable_type", label="", choices = graph_type$val, selected="Histogram"),
                   variable_fill = tags$div(class="drag-container", id="variable_fill", style="height:30px"),
                   variable_facet = tags$div(class="drag-container", id="variable_facet", style="height:30px"),
                   variable_color = tags$div(class="drag-container", id="variable_color", style="height:30px"),
                   variable_size = tags$div(class="drag-container", id="variable_size", style="height:30px"),
                   variable_color_set = selectInput(inputId = "variable_color_set", label="", choices = color_name$color, selected = "Set1"),
                   variable_single_color = tags$div(id="color-picker-container"),
                   variable_theme = selectInput(inputId = "variable_theme", label= "Select Theme", choices = theme_name$theme_name),
                   variable_label = checkboxInput("variable_label", label="Label"),
                   variable_flip = checkboxInput("variable_flip", label="Flip"),
                   variable_filter = dropdownButton(
                     tags$h3("List of Input"),
                     tags$div(style='max-height: 80vh; overflow-y: auto; ',
                              lapply(1:ncol(usedata), function(i){
                                if(class(usedata[0, i]) == "integer"){
                                  selectizeInput(inputId=paste0(colnames(usedata)[i], "_filter"), choices=unique(usedata[,colnames(usedata)[i]]), selected = unique(usedata[,colnames(usedata)[i]]), label=colnames(usedata)[i], multiple=TRUE, options=NULL)
                                }
                                else if(class(usedata[0, i]) == "numeric"){
                                  selectizeInput(inputId=paste0(colnames(usedata)[i], "_filter"), choices=unique(usedata[,colnames(usedata)[i]]), selected = unique(usedata[,colnames(usedata)[i]]), label=colnames(usedata)[i], multiple=TRUE, options=NULL)
                                }
                                else{
                                  selectizeInput(inputId=paste0(colnames(usedata)[i], "_filter"), choices=unique(usedata[,colnames(usedata)[i]]), selected = unique(usedata[,colnames(usedata)[i]]), label=colnames(usedata)[i], multiple=TRUE, options=NULL)
                                }
                              })),
                     circle = FALSE, status = "secondary", icon = icon("filter"), label="Filter"),
                   variable_option = dropdownButton(
                     tags$div(style="max-heigh: 50vh; overflow-y: auto;",
                              shinyjs::hidden(
                                sliderInput("width", "Width", min = 0, max = 1.0, value = 0.09),
                                sliderInput("line_size", "Size", min = 1, max = 10, value = 1),
                                sliderInput("outlier_size", "Outlier Size", min = 1, max = 10, value = 1),
                                sliderInput('option_alpha', 'Alpha', min = 0, max = 1, value = 1),
                                sliderInput('option_adjust', 'Adjust', min = 0, max = 10, value = 1, step = 0.5),
                                sliderInput('label_position', 'Label Position', min = 0, max = 1, value = 0.5),
                                checkboxInput("option_dodge", label="Dodge"),
                                checkboxInput("option_add_point", label="Add Point"),
                                checkboxInput("option_arrow", label="Arrow"),
                                checkboxInput("option_notch", label="Notch"),
                                selectInput(inputId = "option_linetype", label="Linetype", choices = list(Variable = colnames(filtered_data), LineType = line_type), selected = "solid" ),
                                selectInput(inputId = "option_shape", label="Shape", choices = list(Variable = colnames(filtered_data), Shape = c('0', '1', '2')) ),
                                selectInput(inputId = "outlier_shape", label="Outlier Shape", choices = list( Shape = c('0', '1', '2')) ),
                                selectInput(inputId = "option_lineend", label="Line End", choices = c('butt', 'square', 'round') )
                              )
                     ),
                     circle = FALSE, status = "secondary", icon = icon("gear"), label= "Options"
                   ),
                   variable_title = tags$div(style="max-heigh: 50vh; overflow-y: auto;",
                                             textInput("graph_title", "Title"),
                                             textInput("graph_x_label", "X Label"),
                                             textInput("graph_y_label", "Y Label"),
                                             selectInput(inputId = "legend_position", label="Legend Position", choices = list(Position = c('top', 'bottom', 'right', 'left')), selected = 'right' ),
                                             radioButtons(inputId = "select_title_axis", "Select", c("Title" = "title", "X Label" = "x_label", "Y Label" = "y_label"), inline = TRUE),
                                             sliderInput(inputId = "font_size", "Font Size", min = 1, max = 100, value = 18),
                                             tags$div(id="font-color-container")
                   ),
                   download_plot = downloadButton(outputId = "downloadPlot"),
                   graph = plotOutput("result_graph", height="100%")
      )
    )
  })



