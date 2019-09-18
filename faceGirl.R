library(magick)
library(image.libfacedetection)
library(rvest)
library(ggplot2)
library(timeDate)
Flike<-c()
UFlike<-c()
UFurl<-c()
Furl<-c()
Uweek<-c()
Fweek<-c()

JFuml<-c("https://www.instastalker.net/user/jojoxdaily/476252092/2059567848943956180_476252092",
       "https://www.instastalker.net/user/jojoxdaily/476252092/2051323586951503192_476252092",
       "https://www.instastalker.net/user/jojoxdaily/476252092/2045808623944128277_476252092",
       "https://www.instastalker.net/user/jojoxdaily/476252092/2040624651299523043_476252092",
       "https://www.instastalker.net/user/jojoxdaily/476252092/2036097981523838306_476252092",
       "https://www.instastalker.net/user/jojoxdaily/476252092/2031029312150200193_476252092",
       "https://www.instastalker.net/user/jojoxdaily/476252092/2025946202081454102_476252092",
       "https://www.instastalker.net/user/jojoxdaily/476252092/2022578394215925489_476252092",
       "https://www.instastalker.net/user/jojoxdaily/476252092/2017434870583396704_476252092",
       "https://www.instastalker.net/user/jojoxdaily/476252092/2013083356679329898_476252092",
       "https://www.instastalker.net/user/jojoxdaily/476252092/2008835945198831104_476252092",
       "https://www.instastalker.net/user/jojoxdaily/476252092/2003769686090578922_476252092")

for(a in 1:length(JFuml)){
  href<-read_html(JFuml[a])%>%html_nodes(".posts-box__image a")%>%html_attr("href")
  for(i in 1:length(href)){
    photourl<-read_html(href[i])%>%html_nodes(".posts-box__image img")%>%html_attr("src")
    if(identical(photourl, character(0))){next}
    photourl<-photourl[1]
    like<-read_html(href[i])%>%html_nodes("span~ span+ span")%>%html_text()
    like<-strsplit(like,"\n                            |\n                       ")[[1]][2]
    like<-as.numeric(like)
    time<-read_html(href[i])%>%html_nodes(".posts-author__footer-info span:nth-child(1)")%>%html_text()
    time<-strsplit(time,"\n                             | \n                         ")[[1]][2]
    time<-strsplit(time," ")[[1]][2]
    time<-strsplit(time,"/")
    time<-paste(time[[1]][3],"-",time[[1]][1],"-",time[[1]][2])
    time<-gsub(" ","",time)
    weekday<-isWeekday(time, wday = 1:5)
    if(weekday){
      weekD<-"1"
    }else{
      weekD<-"0"
    }
    image<- image_read(photourl)
    faces <- image_detect_faces(image)
    #plot(faces, image, border = "red", lwd = 7, col = "white")
    ifelse(length(faces$detections$x)==0,UFlike<-c(UFlike,like),"0")
    ifelse(length(faces$detections$x)==0,UFurl<-c(UFurl,photourl),"0")
    ifelse(length(faces$detections$x)==0,Uweek<-c(Uweek,weekD),"0")
    ifelse(faces$detections$neighbours[1]>=as.numeric("80"),Flike<-c(Flike,like),UFlike<-c(UFlike,like))
    ifelse(faces$detections$neighbours[1]>=as.numeric("80"),Furl<-c(Furl,photourl),UFurl<-c(UFurl,photourl))
    ifelse(faces$detections$neighbours[1]>=as.numeric("80"),Fweek<-c(Fweek,weekD),Uweek<-c(Uweek,weekD))
  }
}

follow<-read_html(JFuml[1])%>%html_nodes("a:nth-child(2) span span")%>%html_text()
follow<-strsplit(follow,"\n                                document.write")[[1]][2]
follow<-strsplit(follow,"\\(|\\)")[[1]][3]
JFHFace<-data.frame(like=Flike,url=Furl,Face="1",Followers=follow,Weekday=Fweek,Gender="0",stringsAsFactors = F)
JFUFace<-data.frame(like=UFlike,url=UFurl,Face="0",Followers=follow,Weekday=Uweek,Gender="0",stringsAsFactors = F)
JFBindFace<-rbind(JFHFace,JFUFace)
#BindFace$Face<-as.numeric(BindFace$Face)
AllBind1<-rbind(AllBind,JFBindFace)
BindFace$Followers<-as.numeric(BindFace$Followers)
#BindFace$Weekday<-as.numeric(BindFace$Weekday)