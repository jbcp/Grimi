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
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)

  })

  observeEvent(input$graph_x_label, {
    if(input$graph_x_label != "") pkg.globals$graph_source$X_lab <- xlab(input$graph_x_label)
    else pkg.globals$graph_source$X_lab <- ""
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)
  })

  observeEvent(input$graph_y_label, {
    if(input$graph_y_label != "") pkg.globals$graph_source$Y_lab <- ylab(input$graph_y_label)
    else pkg.globals$graph_source$Y_lab <- ""
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)
  })

  observeEvent(input$single_color_value, {
    if(pkg.globals$fill_value == "") {
      if(pkg.globals$single_color_value != input$single_color_value){
        pkg.globals$single_color_value <- input$single_color_value
        pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value)
        try({
          graph_show()
          output$result_graph <- renderPlot({pkg.globals$graph})
        },silent = FALSE)       }
    }

  })

  observeEvent(input$font_color_value, {
    switch(pkg.globals$font_select,
           "title" = {
             if(pkg.globals$graph_source$Title_color != input$font_color_value){
               pkg.globals$graph_source$Title_color <- input$font_color_value
               try({
                 graph_show()
                 output$result_graph <- renderPlot({pkg.globals$graph})
               },silent = FALSE)              }
           },
           "x_label" ={
             if(pkg.globals$graph_source$x_label_color != input$font_color_value) {
               pkg.globals$graph_source$x_label_color <- input$font_color_value
               try({
                 graph_show()
                 output$result_graph <- renderPlot({pkg.globals$graph})
               },silent = FALSE)              }
           },
           "y_label" ={
             if(pkg.globals$graph_source$y_label_color != input$font_color_value) {
               pkg.globals$graph_source$y_label_color <- input$font_color_value
               try({
                 graph_show()
                 output$result_graph <- renderPlot({pkg.globals$graph})
               },silent = FALSE)              }
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
    graph_source_set(pkg.globals$x_value, pkg.globals$y_value, pkg.globals$fill_value, pkg.globals$facet_value)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)   })

  observeEvent(input$data_y,{
    pkg.globals$y_value <- input$data_y
    graph_source_set(pkg.globals$x_value, pkg.globals$y_value, pkg.globals$fill_value, pkg.globals$facet_value)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)   })

  observeEvent(input$data_facet, {
    pkg.globals$facet_value <- input$data_facet
    graph_source_set(pkg.globals$x_value, pkg.globals$y_value, pkg.globals$fill_value, pkg.globals$facet_value)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)
  })

  observeEvent(input$variable_type, {
    pkg.globals$type_value <- input$variable_type
    if(pkg.globals$type_value != "") pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)   })

  observeEvent(input$data_fill,{
    pkg.globals$fill_value <- input$data_fill
    graph_source_set(pkg.globals$x_value, pkg.globals$y_value, pkg.globals$fill_value, pkg.globals$facet_value)
    pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)   })

  observeEvent(input$data_color, {
    pkg.globals$color_value <- input$data_color
    pkg.globals$option_setting$color <- pkg.globals$color_value

    pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)   })

  observeEvent(input$data_size, {
    pkg.globals$size_value <- input$data_size
    pkg.globals$option_setting$size <- pkg.globals$size_value

    pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)   })

  observeEvent(input$variable_color_set, {
    getPalette <- colorRampPalette(brewer.pal(color_name[color_name$color == input$variable_color_set,]$count, input$variable_color_set))
    if(pkg.globals$max_color == 0) pkg.globals$graph_source$Color <- scale_fill_manual(values=getPalette(19))
    else pkg.globals$graph_source$Color <- scale_fill_manual(values=getPalette(pkg.globals$max_color))

    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)   })

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
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)
  })

  observeEvent(input$option_dodge, {
    if(input$option_dodge == TRUE) pkg.globals$option_setting$dodge <- "dodge"
    else pkg.globals$option_setting$dodge <- ""

    pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)   })

  observeEvent(input$option_arrow, {
    if(input$option_arrow == TRUE) pkg.globals$option_setting$arrow <- "arrow"
    else pkg.globals$option_setting$dodge <- ""

    pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)   })

  observeEvent(input$option_notch, {
    if(input$option_notch == TRUE) pkg.globals$option_setting$notch <- "notch"
    else pkg.globals$option_setting$notch <- ""

    pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)   })

  observeEvent(input$variable_flip, {
    if(input$variable_flip == TRUE) pkg.globals$graph_source$Flip <- TRUE
    else pkg.globals$graph_source$Flip <- FALSE

    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)
  })

  observeEvent(input$font_size, {
    switch(pkg.globals$font_select,
           "title" = {pkg.globals$graph_source$Title_size <- input$font_size},
           "x_label" = {pkg.globals$graph_source$x_label_size <- input$font_size},
           "y_label" = {pkg.globals$graph_source$y_label_size <- input$font_size})

    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)
  })

  observeEvent(input$width, {
    pkg.globals$option_setting$width <- input$width

    pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)   })

  observeEvent(input$option_adjust, {
    pkg.globals$option_setting$adjust <- input$option_adjust

    pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)   })

  observeEvent(input$line_size, {
    pkg.globals$option_setting$line_size <- input$line_size

    pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)   })

  observeEvent(input$outlier_size, {
    pkg.globals$option_setting$line_size <- input$outlier_size

    pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)   })

  observeEvent(input$option_alpha, {
    pkg.globals$option_setting$alpha <- input$option_alpha

    pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)   })

  observeEvent(input$label_position, {
    pkg.globals$label_pos <- input$label_position

    if(pkg.globals$label_check == TRUE) {
      pkg.globals$graph_source$Label <- stat_count(geom="text", aes(label=..count..), position = position_stack(pkg.globals$label_pos))
      try({
        graph_show()
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = FALSE)
    }
  })

  observeEvent(input$option_linetype, {
    pkg.globals$option_setting$linetype <- input$option_linetype
    pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)
  })

  observeEvent(input$option_lineend, {
    pkg.globals$option_setting$lineend <- input$option_lineend
    pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)
  })

  observeEvent(input$option_shape, {
    pkg.globals$option_setting$shape <- input$option_shape
    pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)   })

  observeEvent(input$outlier_shape, {
    pkg.globals$option_setting$shape <- input$outlier_shape
    pkg.globals$graph_source$Type <- graph_set(pkg.globals$type_value, pkg.globals$fill_value, pkg.globals$option_setting, pkg.globals$single_color_value)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)   })

  observeEvent(input$legend_position, {
    pkg.globals$legend_pos <- input$legend_position
    pkg.globals$graph_source$Theme <- pkg.globals$graph_source$Theme + theme(legend.position = pkg.globals$legend_pos)
    try({
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)
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
      graph_show()
      output$result_graph <- renderPlot({pkg.globals$graph})
    },silent = FALSE)
  })

  lapply(1:ncol(pkg.globals$filter_data), function(i){
    inputId <-paste0("input$",names(pkg.globals$filter_data)[i], "_filter")
    observeEvent(eval(parse(text = inputId)),{
      test <- fromJSON(eval(parse(text=inputId)))
      test <- str_sub(test, start=2)
      pkg.globals$filter_data <- usedata[eval(parse(text = paste0("usedata$", colnames(usedata)[i]))) %in% test,]
      graph_source_set(pkg.globals$x_value, pkg.globals$y_value, pkg.globals$fill_value, pkg.globals$facet_value)
      try({
        graph_show()
        output$result_graph <- renderPlot({pkg.globals$graph})
      },silent = FALSE)     })

  })

  session$onSessionEnded(function(){
    
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
    
    stopApp()
  })
}
