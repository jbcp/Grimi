graph_set <- function(type = "", fill_value = "", option_setting, single_color = "#F8766D", filtered_data){
  # print(type)
  
  switch (type,
          "Bar" = {
            if(option_setting$dodge != ""){
              if(fill_value != "") return (geom_bar(stat = "identity", position = "dodge", width = option_setting$width))
              else return (geom_bar(stat = "identity", position = "dodge", width = option_setting$width, fill = single_color))
            }
            
            else{
              if(fill_value != "") return (geom_bar(stat="identity", width = option_setting$width))
              else return (geom_bar(stat="identity", width = option_setting$width, fill = single_color))
            }
          },
          "Line" = {
            
            lineoption = list(aes_c = "", aes_l ="", color = "", linetype = "", arrow = "")
            lineoption_value = ""
            
            if(option_setting$color != "") lineoption$aes_c = option_setting$color
            else lineoption$color = single_color
            
            
            if(option_setting$linetype %in% colnames(filtered_data)) lineoption$aes_l = option_setting$linetype
            else lineoption$linetype = option_setting$linetype
            
            
            if(option_setting$arrow != "") lineoption$arrow = "arrow()"
            else lineoption$arrow = ""
            
            
            if(lineoption$aes_c != ""){
              if(lineoption$aes_l != "") lineoption_value = paste(lineoption_value, paste0("aes(", lineoption$aes_c, ", ", lineoption$aes_l, ")"), sep = "")
              else lineoption_value = paste(lineoption_value,  paste0("aes(", lineoption$aes_c, ")"), sep = "")
            }
            else{
              if(lineoption$aes_l != "") lineoption_value = paste0("aes(", lineoption$aes_l, ")")
            }
            
            if(lineoption$color != ""){
              if(lineoption$linetype != ""){
                if(lineoption$arrow != "")
                  return (geom_line(color = lineoption$color, linetype = lineoption$linetype, lineend = option_setting$lineend, arrow = arrow(), size = option_setting$line_size))
                else
                  return (geom_line(color = lineoption$color, linetype = lineoption$linetype, lineend = option_setting$lineend, size = option_setting$line_size))
              }
              else{
                if(lineoption$arrow != "")
                  return (geom_line(aes(linetype = eval(parse(text = lineoption$aes_l))), color = lineoption$color, lineend = option_setting$lineend, arrow = arrow(), size = option_setting$line_size))
                else
                  return (geom_line(aes(linetype = eval(parse(text = lineoption$aes_l))), color = lineoption$color, lineend = option_setting$lineend, size = option_setting$line_size))
              }
            }
            else{
              if(lineoption$linetype != ""){
                if(lineoption$arrow != "")
                  return (geom_line(aes(color = eval(parse(text = lineoption$aes_c))), linetype = lineoption$linetype, lineend = option_setting$lineend, arrow = arrow(), size = option_setting$line_size))
                else
                  return (geom_line(aes(color = eval(parse(text = lineoption$aes_c))), linetype = lineoption$linetype, lineend = option_setting$lineend, size = option_setting$line_size))
              }
              else{
                if(lineoption$arrow != "")
                  return (geom_line(aes(color = eval(parse(text = lineoption$aes_c)), linetype = eval(parse(text = lineoption$aes_l))), lineend = option_setting$lineend, arrow = arrow(), size = option_setting$line_size))
                else
                  return (geom_line(aes(color = eval(parse(text = lineoption$aes_c)), linetype = eval(parse(text = lineoption$aes_l))), lineend = option_setting$lineend, size = option_setting$line_size))
              }
            }
          },
          "Area" = {
            areaoption = list(color ="", size ="", alpha = "")
            
            areaoption$color = single_color
            
            if(option_setting$line_size != "") areaoption$size = option_setting$line_size
            else areaoption$size = ""
            
            if(option_setting$alpha != "") areaoption$alpha = option_setting$alpha
            else areaoption$alpha = ""
            
            return (geom_area(color = areaoption$color, size = areaoption$size, alpha = areaoption$alpha))
          },
          "Point" = {
            pointoption = list(aes_c = "", aes_s ="", color = "", shape = "", size = "")
            
            if(option_setting$color != "") pointoption$aes_c = option_setting$color
            else pointoption$color = single_color
            
            if(option_setting$shape %in% colnames(filtered_data)) pointoption$aes_s = option_setting$shape
            else pointoption$shape = option_setting$shape
            
            if(option_setting$line_size != "") pointoption$size = option_setting$line_size
            else pointoption$size = ""
            
            if(pointoption$aes_c != ""){
              if(pointoption$aes_s != ""){
                # return (geom_point(aes(color = eval(parse(text = pointoption$aes_c)), shape = eval(parse(text = pointoption$aes_s))), size = pointoption$size))
                return (aes(color = eval(parse(text = pointoption$aes_c)), shape = eval(parse(text = pointoption$aes_s))) + geom_point(size = pointoption$size))
              }
              else {
                # return (geom_point(aes(color = eval(parse(text = pointoption$aes_c))), size = pointoption$size, shape = eval(parse(text = pointoption$shape))))
                return (aes(color = eval(parse(text = pointoption$aes_c))) + geom_point(size = pointoption$size, shape = eval(parse(text = pointoption$shape))))
              }
            }
            else{
              if(pointoption$aes_s != ""){
                # return (geom_point(aes(shape = eval(parse(text = pointoption$aes_s))), color = pointoption$color , size = pointoption$size))
                return (aes(shape = eval(parse(text = pointoption$aes_s))) + geom_point(size = pointoption$size, shape = eval(parse(text = pointoption$shape))))
                
              }
              else {
                return (geom_point(color = pointoption$color, size = pointoption$size, shape = eval(parse(text = pointoption$shape))))
              }
            }
          },
          "Histogram" = {
            if(fill_value == "") return (geom_bar(stat="count", fill=single_color, width = option_setting$width))
            else return(geom_bar(stat="count", width = option_setting$width))
          },
          "Density" = {return (geom_density())},
          "Boxplot" = {
            boxplotoption = list(size = "", shape = "", notch = "")
            
            if(option_setting$size != "") boxplotoption$size = option_setting$size
            else boxplotoption$size = ""
            
            if(option_setting$shape != "") boxplotoption$shape = option_setting$shape
            else boxplotoption$shape = ""
            
            if(option_setting$notch != "") boxplotoption$notch = option_setting$notch
            else boxplotoption$notch = ""
            
            if(boxplotoption$notch != ""){
              return (geom_boxplot(outlier.size = boxplotoption$size, outlier.shape = eval(parse(text = boxplotoption$shape)), notch = TRUE))
              
            }
            else{
              return (geom_boxplot(outlier.size = boxplotoption$size, outlier.shape = eval(parse(text = boxplotoption$shape))))
            }
          },
          "Violin" = {
            violinoption = list(width = "", adjust = "")
            
            if(option_setting$width != "") violinoption$width = option_setting$width
            else violinoption$width = ""
            
            if(option_setting$adjust != "") violinoption$adjust = option_setting$adjust
            else violinoption$adjust = ""
            
            return (geom_violin(adjust = eval(parse(text = violinoption$adjust))))
          },
          "DotPlot" = {return (geom_dotplot())}
  )
  
}
