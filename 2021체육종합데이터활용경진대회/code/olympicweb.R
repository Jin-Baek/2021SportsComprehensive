####### 올림픽공원 각 문 분포도 + 전체 차량수 예측 #######
#######            R shiny Web Project             #######
####### Producer. Jin Baek Lee, Ki Uk Kim          #######

# 경로 설정
# setwd("C:\\Users\\KIUK\\Desktop\\map_workspace\\r_web")
 setwd("C:/Users/KIUK/Desktop/공모전")
# path ="C:\\Users\\KIUK\\Desktop\\공모전\\"
# setRepositories(ind = 1:7)
# 필요 패키지 설치
#install.packages("ggmap")
# install.packages("shiny")

# ggmap 2.7 설치
# install.packages("devtools")
# devtools::install_github('dkahle/ggmap')

# 필요한 패키지 다운로드
library(shiny)
library(ggmap)
library(ggplot2)
library(tidyr)
library(tidyverse) # 시간 가공
library('lubridate') #시간 관련
library(dplyr)
library(data.table)
library(stringr)
library(reshape)
library(plotly)
library(lubridate)
##셀레니움 패키지
library("RSelenium")
library("rJava")
library("XML")
library("rvest")
#구글 드라이브 연동
# library("googledrive")
# drive_auth()
# driveDF <- drive_find()
# drive_update("weather.csv",file = driveDF$name[1])

# 구글 지도 API 인증키 등록
register_google(key = 'AIzaSyB0hz2PKlXN5hZGSQSf-s3zUxg-NgQ1bDA')

# 데이터 전처리 R script import

###################### 지도 좌표 ######################

# 남2문 lon = 127.12269175840116 con = 37.51434223590718
# 남3문 con, lon - 37.515826432931874, 127.11848071277946
# 남4문 con, lon - 37.51667795259269, 127.11614058425518
# 동2문 con, lon - 37.51588148167364, 127.13037185783178
# 서2문 con, lon - 37.51998850944507, 127.11447719937004
# 북2문 con, lon - 37.52356986500902, 127.12778052948829
setwd("C:\\Users\\KIUK\\Desktop\\공모전\\2021체육종합데이터활용경진대회\\")
Sys.getlocale()
Sys.setlocale("LC_CTYPE", ".1251")
Sys.setlocale("LC_ALL")
source('./ttayl.R', encoding = 'utf-8')
buildmodel()

map <- get_map(location=c(127.12081265253084, 37.52045297279662), 
        zoom = 15,
        size = c(1500,1000),
        maptype = 'roadmap') 

# 지도 깔끔하게 정리
my_theme <- theme(panel.background = element_blank(),
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  )

# 남2문
s2_point <- geom_point(aes(x=127.12269175840116, y=37.51434223590718),
                       size = 7,
                       color = "#FF0000",
                       alpha = 0.2)


# 남3문
s3_point <- geom_point(aes(x=127.11848071277946, y=37.515826432931874),
                       size = 7,
                       color = "#FF0000",
                       alpha = 0.2,)
# 남4문
s4_point <- geom_point(aes(x=127.11614058425518, y=37.51667795259269),
                       size = 7,
                       color = "#FF0000",
                       alpha = 0.2)

# 동2문
e2_point <- geom_point(aes(x=127.13037185783178, y=37.51588148167364),
                       size = 7,
                       color = "#FF0000",
                       alpha = 0.2)

# 서2문
w2_point <- geom_point(aes(x=127.11447719937004, y=37.51998850944507),
                       size = 7,
                       color = "#FF0000",
                       alpha = 0.2)

# 북2문
n2_point <- geom_point(aes(x=127.12778052948829, y=37.52356986500902),
                       size = 7,
                       color = "#FF0000",
                       alpha = 0.2)

olympic_gate <- ggmap(map) + s2_point + s3_point + s4_point + e2_point + w2_point + n2_point

# ui.R
ui <- shinyUI(fluidPage(
  includeCSS("style.css"),
  sidebarLayout(
    sidebarPanel(
      titlePanel(
        h1(textOutput("to_time"))
      ),
    verticalLayout(
      wellPanel(
        h6("기온"),
        h4(textOutput("today_t"))
      ),
      wellPanel(
        h6("강수량"),
        h4(textOutput("today_r"))
      ),
      wellPanel(
        h6("적설량"),
        h4(textOutput("today_sn"))
      ),
      wellPanel(
        h6("풍속"),
        h4(textOutput("today_cl"))
      )
    )
    ),
    mainPanel(
      plotlyOutput("map", width = "100%", height = "100%") 
    )
  )
))
# ui <- secure_app(ui)

## server.R
server <- shinyServer(function(input, output){
  autoInvalidate <- reactiveTimer()
  autoInvalidate2 <- reactiveTimer()
  
  output$map <- renderPlotly({
    plot <- ggmap(map, extent="device") + s2_point + s3_point + s4_point + e2_point + w2_point + n2_point
  })

  cu_hour <- format(Sys.time(), "%H")
  cu_hour <- as.numeric(cu_hour)
  
  
  # 실시간 시간 처리(2초마다 갱신)
  autoInvalidate <- reactiveTimer(2 * 1000)
  autoInvalidate2 <- reactiveTimer(6 * 10000) # 1분마다 실시간 크롤링
  
  observe({
    autoInvalidate()
    autoInvalidate2()
  })
  
  output$to_time <- renderText({
    autoInvalidate()
    current_timer <- paste0("현재 " , Sys.Date(), " ", format(Sys.time(), "%H:%M"))
  })
  
  df2 <- reactiveFileReader(60000, Null, 'weather.csv', read.csv)
  
  df2 <- read.csv("weather.csv", fileEncoding = "euc-kr")
  
  output$to_date <- renderText({
    autoInvalidate2()
    crawling()
  })
  
  output$today_t <- renderPrint({
    autoInvalidate2()
    tt <- df2$tmp
    cat(tt)
  })
  
  output$today_r <- renderPrint({
    autoInvalidate2()
    rr <- df2$rain
    cat(rr)
  })
  
  output$today_sn <- renderPrint({
    autoInvalidate2()
    sn <- df2$sn
    cat(sn)
  })
  
  output$today_cl <- renderPrint({
    autoInvalidate2()
    cl <- df2$wind_speed
    cat(cl)
  })
})

shinyApp(ui = ui, server = server)
#=====================================================================================================
setwd("C:\\Users\\Administrator\\Google Drive(baekpower98@gmail.com)\\Compitition\\2021체육종합데이터활용경진대회\\")
Sys.setlocale("LC_ALL","Korean")
source('ttayl.R',encoding = "UTF-8")
mymodel <- buildmodel()
summary(mymodel$dayMod)

predict.glm(object = mymodel$dayMod,newdata = test,type = "response")
predict.glm(object = mymodel$endMod,newdata = test,type = "response")