####### 올림픽공원 각 문 분포도 + 전체 차량수 예측 #######
#######            R shiny Web Project             #######
####### Producer. Jin Baek Lee, Ki Uk Kim          #######
#######              크롤링 파일                   #######

setwd("C:\\Users\\KIUK\\Desktop\\공모전\\2021체육종합데이터활용경진대회\\")

##셀레니움 패키지
library("RSelenium")
library("rJava")
library("XML")
library("rvest")

# 셀레니움 설정
# 자신의 JAVA가 설치된 경로 설정
Sys.setenv(JAVA_HOME = 'C:\\Program Files\\Java\\jre1.8.0_311\\')
# 포트 사용 불가시 3214가 적힌 번호 아무 4자리로 변경 ex)1513L
# 원래 포트 유지하고 싶으면, 마지막 줄 참고
driver <- rsDriver(port=1425L,chromever="95.0.4638.69")
    while(TRUE){
    remDR<- driver$client
    remDR$open()
    url <- ("https://www.kma.go.kr/weather/observation/currentweather.jsp")
    remDR$navigate(url)
    
    # 기온 크롤링
    tpr <- remDR$findElement(using = "xpath", value = '//*[@id="content_weather"]/table/tbody/tr[42]/td[6]')
    tpr <- tpr$getElementText()
    tpr <- unlist(tpr)
    
    # 강수
    rain <- remDR$findElement(using = "xpath", value = '//*[@id="content_weather"]/table/tbody/tr[42]/td[9]')
    rain <- rain$getElementText()
    rain <- unlist(rain)
    
    # 적설
    sn <- remDR$findElement(using = "xpath", value = '//*[@id="content_weather"]/table/tbody/tr[42]/td[10]')
    sn <- sn$getElementText()
    sn <- unlist(sn)
    
    # 습도
    hm <- remDR$findElement(using = "xpath", value = '//*[@id="content_weather"]/table/tbody/tr[42]/td[11]')
    hm <- hm$getElementText()
    hm <- unlist(hm)
    
    # 풍속
    ws <- remDR$findElement(using = "xpath", value = '//*[@id="content_weather"]/table/tbody/tr[42]/td[13]')
    ws <- ws$getElementText()
    ws <- unlist(ws)
    
    # 날짜 시간
    rt <- remDR$findElement(using = "class", value = 'table_topinfo')
    rt <- rt$getElementText()
    
    rt <- gsub("기상실황표", "", rt)
    date <- rt
    date <- ymd_hm(date)
    if(wday(date) == 1 || wday(date) == 7){
        date <- "Y"
    }
    else{
        date <- "N"
    }
    rt <- lubridate::hour(ymd_hm(rt))
    
    
    rain <- as.numeric(str_replace(rain, " " , '0'))
    sn <- as.numeric(str_replace(sn, " ", '0'))
    
    # 행사 일정 크롤링
    url_event <- ("https://olympicpark.kspo.or.kr:441/jsp/homepage/contents/culture/schedule_dselect_action.do#today")
    remDR$navigate(url_event)
    
    event_bt <- remDR$findElement(using = 'xpath', value = '//*[@id="board"]/div/fieldset/ul/li[1]/div/p[2]/a[1]/img')
    remDR$mouseMoveToLocation(webElement = event_bt)
    remDR$click(buttonId = 'LEFT')
    
    event_date <- remDR$findElement(using = 'xpath', value = '//*[@id="spanYmd"]')
    event_date <- event_date$getElementText()
    
    event_name <- remDR$findElement(using = 'xpath', value = '//*[@id="listBody"]/tr/td[1]/dl/dt/a')
    event_name <- event_name$getElementText()
    event_name <- unlist(event_name)
    
    event_cnt <- length(event_name)
    
    we_csv <- data.frame(time=rt, tp = tpr,precip = rain, humid = hm, snow = sn, wspeed = ws, eventp = event_cnt, weekn = date)
    write.csv(we_csv, "./crawling/weather.csv", row.names = FALSE)  
    
    remDR$close()
    
    Sys.sleep(120)
}

# 해당 포트 사용 불가시 포트 꺼주기
# remDR$quit()
