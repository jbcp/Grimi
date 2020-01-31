Grimi <- function(datas = iris, viewer="dialog"){
  
  if(viewer == "browser") grimi_viewer <- browserViewer(browser = getOption("browser"))
  else if(viewer == "pane") grimi_viewer <- paneViewer(minHeight = "maximize")
  else grimi_viewer <- shiny::dialogViewer("GRIMI", width = 1200, height = 840)
  
  addResourcePath("griminew", system.file("www", package="griminew"))
  
  pkg.globals <- new.env()
  
  pkg.globals$color_name <- data.frame(
    color = c("Set1", "Set2", "Set3", "Pastel1", "Pastel2", "Paired", "Dark2", "Accent",
              "YlOrRd", "YlOrBr", "YlGnBu", "YlGn", "Reds", "RdPu", "Purples", "PuRd", "PuBuGn", "OrRd", "Oranges",
              "Greys", "Greens", "GnBu", "BnPu", "BuGn", "Blues",
              "Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG"),
    count = c(9, 8, 12, 9, 8, 12, 8, 8,
              9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
              9, 9, 9, 9, 9, 9,
              11, 11, 11, 11, 11, 11, 11, 11, 11)
  )
  
  pkg.globals$graph_type <- data.frame(
    val = c("Histogram", "Bar", "Line", "Area", "Point", "Density", "Boxplot", "Violin")
  )
  
  pkg.globals$graph_type$img = c(
    sprintf("<img src = 'griminew/img/bar-graph.png' width = '20px' height = '20px'>"),
    sprintf("<img src = 'griminew/img/bar-graph.png' width = '20px' height = '20px'>"),
    sprintf("<img src = 'griminew/img/line-graph.png' width = '20px' height = '20px'>"),
    sprintf("<img src = 'griminew/img/area-graph.png' width = '20px' height = '20px'>"),
    sprintf("<img src = 'griminew/img/dot-graph.png' width = '20px' height = '20px'>"),
    sprintf("<img src = 'griminew/img/density-graph.png' width = '20px' height = '20px'>"),
    sprintf("<img src = 'griminew//boxplot-graph.png' width = '15px' height = '20px'>"),
    sprintf("<img src = 'griminew/img/violin-graph.png' width = '15px' height = '20px'>")
  )
  
  pkg.globals$line_type <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "blank")
  pkg.globals$getPalette <- colorRampPalette(brewer.pal(9,"Set1"))
  
  ##Theme
  pkg.globals$theme_name <- data.frame(
    theme_name = c("grey", "gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void", "test")
  )
  
  pkg.globals$options <- c("width", "line_size", "option_dodge", "option_add_point", "option_arrow", "option_linetype", "option_color", "option_size", "option_shape", "option_adjust", "option_lineend", "option_notch", "line_size", 'outlier_shape', 'outlier_size', 'option_add_average', 'option_alpha' )
  
  usedata <- datas
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
  
  Grimi_UI <- fluidPage(
    htmlTemplate(system.file("www", "main.html", package = "griminew"),
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
                 variable_type = selectInput(inputId = "variable_type", label="", choices = pkg.globals$graph_type$val, selected="Histogram"),
                 variable_fill = tags$div(class="drag-container", id="variable_fill", style="height:30px"),
                 variable_facet = tags$div(class="drag-container", id="variable_facet", style="height:30px"),
                 variable_color = tags$div(class="drag-container", id="variable_color", style="height:30px"),
                 variable_size = tags$div(class="drag-container", id="variable_size", style="height:30px"),
                 variable_color_set = selectInput(inputId = "variable_color_set", label="", choices = pkg.globals$color_name$color, selected = "Set1"),
                 variable_single_color = tags$div(id="color-picker-container"),
                 variable_theme = selectInput(inputId = "variable_theme", label= "Select Theme", choices = pkg.globals$theme_name$theme_name),
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
                              selectInput(inputId = "option_linetype", label="Linetype", choices = list(Variable = colnames(filtered_data), LineType = pkg.globals$line_type), selected = "solid" ),
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
                                           radioButtons(inputId = "select_title_axis", "Select", c("Title" = "title", "X Label" = "x_label", "Y Label" = "y_label"), selected = 'title',inline = TRUE),
                                           sliderInput(inputId = "font_size", "Font Size", min = 1, max = 100, value = 18),
                                           tags$div(id="font-color-container")
                 ),
                 download_plot = downloadButton(outputId = "downloadPlot"),
                 graph = plotOutput("result_graph", height="100%")
    )
  )
  
  Grimi_Server <- function(input, output, session){
    
    shinyjs::useShinyjs()
    
    
    for(i in 1:length(pkg.globals$filter_data)){
      tmp_array <- ""
      for(j in 1:length(unique(eval(parse(text=paste0("pkg.globals$filter_data$", names(pkg.globals$filter_data)[i])))))){
        tmp = list(list(id = j, data = unique(eval(parse(text=paste0("pkg.globals$filter_data$", names(pkg.globals$filter_data)[i]))))[j]))
        tmp_array <- c(tmp_array,tmp)
      }
      tmp_data = list(list(name = paste0(names(pkg.globals$filter_data)[i], "_filter"),  typeof = class(eval(parse(text=paste0("pkg.globals$filter_data$", names(pkg.globals$filter_data)[i])))), data = tmp_array))
      pkg.globals$filter <- c(pkg.globals$filter, tmp_data)
    }
    session$sendCustomMessage(type = "filter_data", message = pkg.globals$filter)
    
    
    output$downloadPlot <- downloadHandler(
      filename = function() { paste('plot_', Sys.Date(), '.png', sep='') },
      content = function(file) {
        png(file, width = input$shiny_width, height = input$shiny_height)
        dev.off()
      }
    )
    
    observeEvent(input$graph_title, {
      if(input$graph_title != "") pkg.globals$graph_source$Title <- ggtitle(input$graph_title)
      else pkg.globals$graph_source$Title <- ""
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)
      
    })
    
    observeEvent(input$graph_x_label, {
      if(input$graph_x_label != "") pkg.globals$graph_source$X_lab <- xlab(input$graph_x_label)
      else pkg.globals$graph_source$X_lab <- ""
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)
    })
    
    observeEvent(input$graph_y_label, {
      if(input$graph_y_label != "") pkg.globals$graph_source$Y_lab <- ylab(input$graph_y_label)
      else pkg.globals$graph_source$Y_lab <- ""
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)
    })
    
    observeEvent(input$single_color_value, {
      if(pkg.globals$fill_value == "") {
        if(pkg.globals$single_color_value != input$single_color_value){
          pkg.globals$single_color_value <- input$single_color_value
          pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value, filtered_data)
          try({
            graph_show(pkg.globals)
            output$result_graph <- renderPlot({pkg.globals$graph})
          },silent = TRUE)       }
      }
      
    })
    
    observeEvent(input$font_color_value, {
      switch(pkg.globals$font_select,
             "title" = {
               if(pkg.globals$graph_source$Title_color != input$font_color_value){
                 pkg.globals$graph_source$Title_color <- input$font_color_value
                 try({
                   graph_show(pkg.globals)
                   output$result_graph <- renderPlot({pkg.globals$graph})
                 },silent = TRUE)              }
             },
             "x_label" ={
               if(pkg.globals$graph_source$x_label_color != input$font_color_value) {
                 pkg.globals$graph_source$x_label_color <- input$font_color_value
                 try({
                   graph_show(pkg.globals)
                   output$result_graph <- renderPlot({pkg.globals$graph})
                 },silent = TRUE)              }
             },
             "y_label" ={
               if(pkg.globals$graph_source$y_label_color != input$font_color_value) {
                 pkg.globals$graph_source$y_label_color <- input$font_color_value
                 try({
                   graph_show(pkg.globals)
                   output$result_graph <- renderPlot({pkg.globals$graph})
                 },silent = TRUE)              }
             }
      )
      
    })
    
    observeEvent(input$select_title_axis, {
      pkg.globals$font_select <- input$select_title_axis
      switch(pkg.globals$font_select,
             "title" = {updateSliderInput(session, "font_size", value = pkg.globals$graph_source$Title_size)},
             "x_label" ={updateSliderInput(session, "font_size", value = pkg.globals$graph_source$x_label_size)},
             "y_label" ={updateSliderInput(session, "font_size", value = pkg.globals$graph_source$y_label_size)}
      )
    })
    
    
    observeEvent(input$data_x,{
      pkg.globals$x_value <- input$data_x
      graph_source_set(pkg.globals$x_value, pkg.globals$y_value, pkg.globals$fill_value, pkg.globals$facet_value, pkg.globals)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)   })
    
    observeEvent(input$data_y,{
      pkg.globals$y_value <- input$data_y
      graph_source_set(pkg.globals$x_value, pkg.globals$y_value, pkg.globals$fill_value, pkg.globals$facet_value, pkg.globals)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)   })
    
    observeEvent(input$data_facet, {
      pkg.globals$facet_value <- input$data_facet
      graph_source_set(pkg.globals$x_value, pkg.globals$y_value, pkg.globals$fill_value, pkg.globals$facet_value, pkg.globals)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)
    })
    
    observeEvent(input$variable_type, {
      pkg.globals$type_value <- input$variable_type
      if(pkg.globals$type_value != "") pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value, filtered_data)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)   })
    
    observeEvent(input$data_fill,{
      pkg.globals$fill_value <- input$data_fill
      graph_source_set(pkg.globals$x_value, pkg.globals$y_value, pkg.globals$fill_value, pkg.globals$facet_value, pkg.globals)
      pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value, filtered_data)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)   })
    
    observeEvent(input$data_color, {
      pkg.globals$color_value <- input$data_color
      pkg.globals$option_setting$color <- pkg.globals$color_value
      
      pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value, filtered_data)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)   })
    
    observeEvent(input$data_size, {
      pkg.globals$size_value <- input$data_size
      pkg.globals$option_setting$size <- pkg.globals$size_value
      
      pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value, filtered_data)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)   })
    
    observeEvent(input$variable_color_set, {
      getPalette <- colorRampPalette(brewer.pal(pkg.globals$color_name[pkg.globals$color_name$color == input$variable_color_set,]$count, input$variable_color_set))
      if(pkg.globals$max_color == 0) pkg.globals$graph_source$Color <- scale_fill_manual(values=getPalette(19))
      else pkg.globals$graph_source$Color <- scale_fill_manual(values=getPalette(pkg.globals$max_color))
      
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)   })
    
    observeEvent(input$variable_theme, {
      pkg.globals$theme_value <- input$variable_theme
      switch (input$variable_theme,
              "grey" = {pkg.globals$graph_source$Theme<-theme_grey() + theme(legend.position = pkg.globals$legend_pos)},
              "gray" = {pkg.globals$graph_source$Theme<-theme_gray() + theme(legend.position = pkg.globals$legend_pos)},
              "bw" = {pkg.globals$graph_source$Theme<-theme_bw() + theme(legend.position = pkg.globals$legend_pos)},
              "linedraw" = {pkg.globals$graph_source$Theme<-theme_linedraw() + theme(legend.position = pkg.globals$legend_pos)},
              "light" = {pkg.globals$graph_source$Theme<-theme_light() + theme(legend.position = pkg.globals$legend_pos)},
              "dark" = {pkg.globals$graph_source$Theme<-theme_dark() + theme(legend.position = pkg.globals$legend_pos)},
              "minimal" = {pkg.globals$graph_source$Theme<-theme_minimal() + theme(legend.position = pkg.globals$legend_pos)},
              "classic" = {pkg.globals$graph_source$Theme<-theme_classic() + theme(legend.position = pkg.globals$legend_pos)},
              "void" = {pkg.globals$graph_source$Theme<-theme_void() + theme(legend.position = pkg.globals$legend_pos)},
              "test" = {pkg.globals$graph_source$Theme<-theme_test() + theme(legend.position = pkg.globals$legend_pos)}
      )
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)
    })
    
    observeEvent(input$option_dodge, {
      if(input$option_dodge == TRUE) pkg.globals$option_setting$dodge <- "dodge"
      else pkg.globals$option_setting$dodge <- ""
      
      pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value, filtered_data)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)   })
    
    observeEvent(input$option_arrow, {
      if(input$option_arrow == TRUE) pkg.globals$option_setting$arrow <- "arrow"
      else pkg.globals$option_setting$dodge <- ""
      
      pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value, filtered_data)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)   })
    
    observeEvent(input$option_notch, {
      if(input$option_notch == TRUE) pkg.globals$option_setting$notch <- "notch"
      else pkg.globals$option_setting$notch <- ""
      
      pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value, filtered_data)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)   })
    
    observeEvent(input$variable_flip, {
      if(input$variable_flip == TRUE) pkg.globals$graph_source$Flip <- TRUE
      else pkg.globals$graph_source$Flip <- FALSE
      
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)
    })
    
    observeEvent(input$font_size, {
      switch(pkg.globals$font_select,
             "title" = {pkg.globals$graph_source$Title_size <- input$font_size},
             "x_label" = {pkg.globals$graph_source$x_label_size <- input$font_size},
             "y_label" = {pkg.globals$graph_source$y_label_size <- input$font_size})
      
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)
    })
    
    observeEvent(input$width, {
      pkg.globals$option_setting$width <- input$width
      
      pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value, filtered_data)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)   })
    
    observeEvent(input$option_adjust, {
      pkg.globals$option_setting$adjust <- input$option_adjust
      
      pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value, filtered_data)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)   })
    
    observeEvent(input$line_size, {
      pkg.globals$option_setting$line_size <- input$line_size
      
      pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value, filtered_data)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)   })
    
    observeEvent(input$outlier_size, {
      pkg.globals$option_setting$line_size <- input$outlier_size
      
      pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value, filtered_data)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)   })
    
    observeEvent(input$option_alpha, {
      pkg.globals$option_setting$alpha <- input$option_alpha
      
      pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value, filtered_data)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)   })
    
    observeEvent(input$label_position, {
      pkg.globals$label_pos <- input$label_position
      
      if(pkg.globals$label_check == TRUE) {
        pkg.globals$graph_source$Label <- stat_count(geom="text", aes(label=..count..), position = position_stack(pkg.globals$label_pos))
        try({
          graph_show(pkg.globals)
          output$result_graph <- renderPlot({pkg.globals$graph})
        },silent = TRUE)
      }
    })
    
    observeEvent(input$option_linetype, {
      pkg.globals$option_setting$linetype <- input$option_linetype
      pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value, filtered_data)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)
    })
    
    observeEvent(input$option_lineend, {
      pkg.globals$option_setting$lineend <- input$option_lineend
      pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value, filtered_data)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)
    })
    
    observeEvent(input$option_shape, {
      pkg.globals$option_setting$shape <- input$option_shape
      pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value, filtered_data)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)   })
    
    observeEvent(input$outlier_shape, {
      pkg.globals$option_setting$shape <- input$outlier_shape
      pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value, filtered_data)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)   })
    
    observeEvent(input$legend_position, {
      pkg.globals$legend_pos <- input$legend_position
      pkg.globals$graph_source$Theme <- pkg.globals$graph_source$Theme + theme(legend.position = pkg.globals$legend_pos)
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)
    })
    
    observeEvent(input$variable_label, {
      if(input$variable_label == TRUE) {
        pkg.globals$label_check <- TRUE
        shinyjs::show('label_position')
        pkg.globals$graph_source$Label <- stat_count(geom="text", aes(label=..count..), position = position_stack(pkg.globals$label_pos))
      }
      else {
        pkg.globals$label_check <- FALSE
        shinyjs::hide('label_position')
        pkg.globals$graph_source$Label <- ""
      }
      
      try({
        graph_show(pkg.globals)
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = TRUE)
    })
    
    lapply(1:ncol(pkg.globals$filter_data), function(i){
      inputId <-paste0("input$",names(pkg.globals$filter_data)[i], "_filter")
      observeEvent(eval(parse(text = inputId)),{
        test <- fromJSON(eval(parse(text=inputId)))
        test <- str_sub(test, start=2)
        pkg.globals$filter_data <- usedata[eval(parse(text = paste0("usedata$", colnames(usedata)[i]))) %in% test,]
        graph_source_set(pkg.globals$x_value, pkg.globals$y_value, pkg.globals$fill_value, pkg.globals$facet_value, pkg.globals)
        try({
          graph_show(pkg.globals)
          output$result_graph <- renderPlot({pkg.globals$graph})
        },silent = TRUE)     })
      
    })
    
    session$onSessionEnded(function(){
      stopApp()
    })
  }
  
  runGadget(app=Grimi_UI, server=Grimi_Server, viewer = grimi_viewer)
}

