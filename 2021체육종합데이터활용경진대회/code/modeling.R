library(tidyr)
library(dplyr)
library(data.table)
library(stringr)
library(reshape)
library(MASS)
library(caret)
library(pscl)
# Path는 본인의 컴퓨터에 알맞게 수정 되어야 한다.

buildmodel <- function(data){
  
  result <- list()
  path="C:\\Users\\Administrator\\Google Drive(baekpower98@gmail.com)\\Compitition\\2021체육종합데이터활용경진대회\\"
  #path="H:\\내 드라이브\\Compitition\\2021체육종합데이터활용경진대회\\"
  data.list <- list.files(path=paste0(path,"\\data"))
  
  for(i in 1:length(data.list)){
    data <- fread(paste0(path,"data\\",data.list[i]),encoding="UTF-8")
    
    data <- data %>% 
      mutate_all(as.character)
    
    data <- data[!is.na(data$OUTDATE),]
    
    data <- data%>% 
      separate(col = INDATE,sep=" |-|:",into = c("In_year","In_month","In_day","In_time","In_minute","In_second")) %>%
      separate(col = OUTDATE, sep=" |-|:",into = c("Out_year","Out_month","Out_day","Out_time","Out_minute","Out_second"))
    
    data <- data %>% 
      mutate(
        INGATE = paste0("In",INGATE),
        OUTGATE =paste0("Out",OUTGATE),
        In_time = as.integer(In_time),
        Out_time = as.integer(Out_time),
        INGATE = as.factor(INGATE),
        OUTGATE = as.factor(OUTGATE)
      )
    
    in_data <- data%>%
      dplyr::select(c(TRANSDATE,In_time,INGATE))
    
    out_data <- data %>%
      dplyr::select(c(TRANSDATE,Out_time,OUTGATE))
    
    in_data<-in_data%>%
      group_by(TRANSDATE,In_time,INGATE)%>%
      summarise(count=n())
    
    out_data<-out_data%>%
      group_by(TRANSDATE,Out_time,OUTGATE)%>%
      summarise(count=n())
    
    in_data<-cast(data=in_data,TRANSDATE+In_time~INGATE,fun=sum)
    out_data<-cast(data=out_data,TRANSDATE+Out_time~OUTGATE,fun=sum)
    
    colnames(in_data)[2] <- "time"
    colnames(out_data)[2] <- "time"
    
    data<-merge(in_data,out_data,by=c("TRANSDATE","time"),all = TRUE)
    data[is.na(data)] <- 0
    
    result[[i]] <- data
  }
  
  #print(result)
  total <- Reduce(function(x,y) merge(x,y,all=TRUE),result)
  total[is.na(total)] <- 0
  
  #================================ 환경 변수 전처리 (기상 데이터)
  env<- fread(paste0(path,"sub\\OBS_ASOS_TIM_20211110172352.csv"))
  env[is.na(env)] <- 0
  env <- env%>% 
    separate(col = 일시,sep=" |:",into = c("TRANSDATE","time",NA)) 
  env <- env[,c(3,4,5,6,7,8,9)]
  
  env <- env %>%
    mutate(time=as.numeric(time))
  
  total <- merge(total,env,by=c("TRANSDATE","time"),all.x = TRUE)
  
  #================================ New column "시간대 별 올림픽공원 내 차량 수"
  total <- total %>%
    mutate(allGATE=(In남2+In남3+In남4+In동2+In북2+In서2)-(Out남2+Out남3+Out남4+Out동2+Out북2+Out서2))
  
  cum <- c()
  for(i in 1:dim(total)[1]){
    if(i==1){
      cum <- append(cum,total$allGATE[i])
    }else{
      cum <- append(cum,cum[i-1]+total$allGATE[i])
    }
  }
  
  total <- cbind(total,cumulative=cum)
  
  #============================================= New column "주말 여부"
  weekn <- c()
  for(i in 1:dim(total)[1]){
    if(weekdays(as.Date(total$TRANSDATE))[i] %in% c("월요일","화요일","수요일","목요일","금요일")){
      weekn <- append(weekn,"N")
    }
    else{
      weekn <- append(weekn,"Y")
    }
  }
  
  total <- cbind(total,weekn=weekn)
  
  #================================================== New column "Event"
  event <- fread(paste0(path,"sub\\KS_OLPARK_RENT_INFO_202110.csv"),encoding="UTF-8")
  event <- event %>%
    dplyr::select(c("RENT_START_YMD","RENT_END_YMD","DK_DAY","JS_PERSON"))%>%
    mutate(RENT_START_YMD=as.character(RENT_START_YMD)) %>%
    mutate(RENT_END_YMD=as.character(RENT_END_YMD)) %>%
    dplyr::filter(!is.na(JS_PERSON)) %>%
    dplyr::filter(!is.na(DK_DAY))%>%
    mutate(FP = floor(JS_PERSON/DK_DAY)) %>%
    dplyr::select(c("RENT_START_YMD","RENT_END_YMD","FP"))
  
  # 수동으로 21년 시작 기준으로 컷함 
  event <- event[841:dim(event)[1],]
  
  # total 데이터에 영벡터 추가하기 
  total <- total %>%
    mutate(eventp = 0)
  
  # 모든 날짜 중복 없이 뽑아내기 
  dateseq <- unique(total$TRANSDATE) 
  
  for(i in 1:dim(event)[1]){
    # 각 이벤트 기간 날짜 모두 뽑아내기 
    start.d <- which(event$RENT_START_YMD[i]==dateseq)
    end.d <- which(event$RENT_END_YMD[i]==dateseq)
    duration <- start.d:end.d
    
    # 각 이벤트 기간이 total의 몇 번째 인덱스들인지 
    ind <- which(total$TRANSDATE %in% dateseq[duration]==TRUE)
    
    # 해당하는 이벤트의 일별 인구 수 count() FP를 total 데이터 영벡터에 넣기 
    total[ind,"eventp"] <- total[ind,"eventp"]+1
  }
  
  #================================================== More preprocessing
  total <- total[-1,]

  total <- total %>%
    mutate(time=as.factor(time))
  
  total <- total %>%
    dplyr::rename(tp=`기온(°C)`) %>%
    dplyr::rename(precip=`강수량(mm)`)%>%
    dplyr::rename(wspeed=`풍속(m/s)`) %>%
    dplyr::rename(humid=`습도(%)`)%>%
    dplyr::rename(snow=`적설(cm)`)
  
  total <- total[,-c(3:14)]
  #================================== Need to check which distribution follows
  # hist(total$cumulative)
  # mean(total$cumulative)/var(total$cumulative)
  
  #================================== Divide into two dataset (weekday & weekend)
  data.day <- total[which((total$weekn=="N")==TRUE),]
  data.end <- total[which((total$weekn=="Y")==TRUE),]
  
  model.day <- glm.nb(cumulative~0+time+tp+precip+wspeed+snow+eventp,data=data.day)
  model.end <- glm.nb(cumulative~0+time+tp+wspeed+humid+snow+eventp,data=data.end)
  
  return(list(dayMod=model.day,endMod=model.end))
}
#final <- buildmodel()

# Jan <- myReshape(fread(paste0(path,"올림픽공원 주차장 이용현황(1월).csv"),encoding="UTF-8"))
# Feb <- myReshape(fread(paste0(path,"올림픽공원 주차장 이용현황(2월).csv"),encoding="UTF-8"))
# March <- myReshape(fread(paste0(path,"올림픽공원 주차장 이용현황(3월).csv"),encoding="UTF-8"))
# Apr <- myReshape(fread(paste0(path,"올림픽공원 주차장 이용현황(4월).csv"),encoding="UTF-8"))
# May <- myReshape(fread(paste0(path,"올림픽공원 주차장 이용현황(5월).csv"),encoding="UTF-8"))
# June <- myReshape(fread(paste0(path,"올림픽공원 주차장 이용현황(6월).csv"),encoding="UTF-8"))
# July <- myReshape(fread(paste0(path,"올림픽공원 주차장 이용현황(7월).csv"),encoding="UTF-8"))
# Aug <- myReshape(fread(paste0(path,"올림픽공원 주차장 이용현황(8월).csv"),encoding="UTF-8"))
# Sep <- myReshape(fread(paste0(path,"올림픽공원 주차장 이용현황(9월).csv"),encoding="UTF-8"))
# Oct <- myReshape(fread(paste0(path,"올림픽공원 주차장 이용현황(10월).csv"),encoding="UTF-8"))
# 
# datalist <- list(Jan,Feb,March,Apr,May,June,July,Aug,Sep,Oct)
# total <- Reduce(function(x,y) merge(x,y,all=TRUE),datalist)
# total[is.na(total)] <- 0
# View(total)
# 
# #==========================================================================================================
# 
# ## 환경 변수 전처리 (기상 데이터)
# env<- fread(paste0(path,"sub\\OBS_ASOS_TIM_20211110172352.csv"))
# env[is.na(env)] <- 0
# env <- env%>% 
#   separate(col = 일시,sep=" |:",into = c("TRANSDATE","time",NA)) 
# env <- env[,c(3,4,5,6,7,8,9)]
# 
# env <- env %>%
#   mutate(time=as.numeric(time))
# 
# total <- merge(total,env,by=c("TRANSDATE","time"),all.x = TRUE)
# #==========================================================================================================
# 
# ## New column "시간대 별 올림픽공원 내 차량 수"
# total <- total %>%
#   mutate(allGATE=(In남2+In남3+In남4+In동2+In북2+In서2)-(Out남2+Out남3+Out남4+Out동2+Out북2+Out서2))
# 
# cum <- c()
# for(i in 1:dim(total)[1]){
#   if(i==1){
#     cum <- append(cum,total$allGATE[i])
#   }else{
#     cum <- append(cum,cum[i-1]+total$allGATE[i])
#   }
# }
# 
# total <- cbind(total,cumulative=cum)
# 
# ## New column "주말 여부"
# weekn <- c()
# for(i in 1:dim(total)[1]){
#   if(weekdays(as.Date(total$TRANSDATE))[i] %in% c("월요일","화요일","수요일","목요일","금요일")){
#     weekn <- append(weekn,"N")
#   }
#   else{
#     weekn <- append(weekn,"Y")
#   }
# }
# 
# total <- cbind(total,weekn=weekn)
# 
# #=========================================================================================================
# event <- fread(paste0(path,"KS_OLPARK_RENT_INFO_202110.csv"),encoding="UTF-8")
# event <- event %>%
#   dplyr::select(c("RENT_START_YMD","RENT_END_YMD","DK_DAY","JS_PERSON"))%>%
#   mutate(RENT_START_YMD=as.character(RENT_START_YMD)) %>%
#   mutate(RENT_END_YMD=as.character(RENT_END_YMD)) %>%
#   dplyr::filter(!is.na(JS_PERSON)) %>%
#   dplyr::filter(!is.na(DK_DAY))%>%
#   mutate(FP = floor(JS_PERSON/DK_DAY)) %>%
#   dplyr::select(c("RENT_START_YMD","RENT_END_YMD","FP"))
# 
# # 수동으로 21년 시작 기준으로 컷함 
# event <- event[841:dim(event)[1],]
# 
# # total 데이터에 영벡터 추가하기 
# total <- total %>%
#   mutate(eventp = 0)
# 
# # 모든 날짜 중복 없이 뽑아내기 
# dateseq <- unique(total$TRANSDATE) 
# 
# for(i in 1:dim(event)[1]){
#   # 각 이벤트 기간 날짜 모두 뽑아내기 
#   start.d <- which(event$RENT_START_YMD[i]==dateseq)
#   end.d <- which(event$RENT_END_YMD[i]==dateseq)
#   duration <- start.d:end.d
#   
#   # 각 이벤트 기간이 total의 몇 번째 인덱스들인지 
#   ind <- which(total$TRANSDATE %in% dateseq[duration]==TRUE)
#   
#   # 해당하는 이벤트의 일별 인구 수 count() FP를 total 데이터 영벡터에 넣기 
#   total[ind,"eventp"] <- total[ind,"eventp"]+1
# }
# 
# #===========================================================================================================
# total <- total[-1,]
# 
# # total<-total %>%
# #   mutate(weekn=as.factor(ifelse(weekn==0,"N","Y")))
# 
# total <- total %>%
#   mutate(time=as.factor(time))
# 
# total <- total %>%
#   dplyr::rename(tp=`기온(°C)`) %>%
#   dplyr::rename(precip=`강수량(mm)`)%>%
#   dplyr::rename(wspeed=`풍속(m/s)`) %>%
#   dplyr::rename(humid=`습도(%)`)%>%
#   dplyr::rename(snow=`적설(cm)`)
# 
# hist(total$cumulative)
# mean(total$cumulative)/var(total$cumulative)
# 
# 
# data.day <- total[which((total$weekn=="N")==TRUE),]
# data.end <- total[which((total$weekn=="Y")==TRUE),]
# 
# model.day <- glm.nb(cumulative~time+tp+precip+wspeed+humid+snow+eventp,data=data.day)
# summary(model.day)
# residuals(model.day,type = "deviance")
# 
# # Residual deviance 
# anova(model.day,test="Chisq")
# 
# # R squared
# SSR <- sum(abs(data.day$cumulative - model.day$fitted.values))
# SST <- sum(abs(data.day$cumulative - mean(data.day$cumulative)))
# r2 <- 1-(SSR/SST)
# 
# # Adjust squared
# n <- length(data.day)
# k <- 6
# adjr2 <- 1-(SSR/(n-k-1))/(SST/(n-1))
# 
# # McFadden R squared
# pR2(model.day)
# 
# 
# 
# 
# model.end <- glm.nb(cumulative~time+tp+precip+wspeed+humid+snow+eventp,data=data.end)
# summary(model.end)
# residuals(model.end,type = "deviance")


# ##### Example
# time <-as.factor(c(14,18,7,5))
# tp<-c(10,-5,25,-4.6)
# precip <- c(0.0,0.1,0.01,0.0)
# wspeed <- c(1.5,3.3,5.1,4.4)
# humid <- c(55,45,45,33)
# snow <- c(0,0.01,0.05,0.0)
# eventp <- c(2,3,0,0)
# 
# test<-data.frame(time,tp,precip,wspeed,humid,snow,eventp)
# predict.glm(object = model.day,newdata = test,type = "response")

