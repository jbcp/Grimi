Grimi <- function(datas = iris, viewer="dialog"){

  if(viewer == "browser") grimi_viewer <- browserViewer(browser = getOption("browser"))
  else if(viewer == "pane") grimi_viewer <- paneViewer(minHeight = "maximize")
  else grimi_viewer <- shiny::dialogViewer("GRIMI", width = 1200, height = 840)

  addResourcePath("Grimi", system.file("www", package="Grimi"))

  runGadget(app=Grimi_UI, server=Grimi_Server, viewer = grimi_viewer)
}

