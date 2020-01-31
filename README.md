# Grimi
데이터 시각화를 손쉽게 할 수 있도록 도와주는 R Package입니다.

## 주요 기능
+ Drag&Drop을 이용한 그래프 구조 설정
+ 8종의 그래프 제공
+ 그래프 커스터마이징 기능

## SW Prerequisites
+ R (version 3.6.2) 
+ shiny (version 1.3.2) 
+ shinyjs (version 1.0)
+ ggplot2 (version 3.2.0)
+ rjson (version 0.2.20)
+ stringr (version 1.4.0)
+ RColorBrewer (version 1.1-2)
+ shinyWidgets (version 0.4.8)
+ shinydashboard (version 0.7.1)

## Installation
+ Installation
```
install.packages("devtools")
libray(devtools)
devtools::install_github("minjikim0927/Grimi")
```

+ Execution
```
library(Grimi)
Grimi()
```

+ If you want to use browser
```
Grimi(viewer="browser")
```

+ If you want to use your data (The type of data you want to use must be data.frame)
```
Grimi(datas = your_data_name)
```

## Contact to developer(s)
 [MINJI KIM](https://github.com/minjikim0927)  - mjkim@jbcp.kr
