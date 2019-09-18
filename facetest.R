# install.packages("magick")
# install.packages("image.libfacedetection", repos = "https://bnosac.github.io/drat")

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

uml<-c("https://www.instastalker.net/user/raymond.hou/879221194/2055133088221382894_879221194",
       "https://www.instastalker.net/user/raymond.hou/879221194/2041359199188079632_879221194",
       "https://www.instastalker.net/user/raymond.hou/879221194/2026691084676315155_879221194",
       "https://www.instastalker.net/user/raymond.hou/879221194/2012374537279567455_879221194",
       "https://www.instastalker.net/user/raymond.hou/879221194/1997236859177384895_879221194",
       "https://www.instastalker.net/user/raymond.hou/879221194/1982651505422303740_879221194",
       "https://www.instastalker.net/user/raymond.hou/879221194/1968985313843092012_879221194",
       "https://www.instastalker.net/user/raymond.hou/879221194/1944223713869495068_879221194",
       "https://www.instastalker.net/user/raymond.hou/879221194/1932717953402758286_879221194",
       "https://www.instastalker.net/user/raymond.hou/879221194/1905827159563643659_879221194",
       "https://www.instastalker.net/user/raymond.hou/879221194/1891334484559622256_879221194",
       "https://www.instastalker.net/user/raymond.hou/879221194/1878313565041866732_879221194")

for(a in 1:length(uml)){
  href<-read_html(uml[a])%>%html_nodes(".posts-box__image a")%>%html_attr("href")
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

follow<-read_html(uml[1])%>%html_nodes("a:nth-child(2) span span")%>%html_text()
follow<-strsplit(follow,"\n                                document.write")[[1]][2]
follow<-strsplit(follow,"\\(|\\)")[[1]][3]

HFace<-data.frame(like=Flike,url=Furl,Face="1",Followers=follow,Weekday=Fweek,Gender="1",stringsAsFactors = F)
UFace<-data.frame(like=UFlike,url=UFurl,Face="0",Followers=follow,Weekday=Uweek,Gender="1",stringsAsFactors = F)
BindFace<-rbind(HFace,UFace)
#BindFace$Gender<-as.character("1")
#BindFace$Face<-as.numeric(BindFace$Face)
BindFace$Followers<-as.numeric(BindFace$Followers)
#BindFace$Weekday<-as.numeric(BindFace$Weekday)



Flike<-c()
UFlike<-c()
UFurl<-c()
Furl<-c()
Uweek<-c()
Fweek<-c()

Fuml<-c("https://www.instastalker.net/user/duffy_lifediary/17171598/2059932675226953541_17171598",
       "https://www.instastalker.net/user/duffy_lifediary/17171598/2052100479224938311_17171598",
       "https://www.instastalker.net/user/duffy_lifediary/17171598/2041185347854215973_17171598",
       "https://www.instastalker.net/user/duffy_lifediary/17171598/2030530065662804703_17171598",
       "https://www.instastalker.net/user/duffy_lifediary/17171598/2023183000712653961_17171598",
       "https://www.instastalker.net/user/duffy_lifediary/17171598/2011567423443417593_17171598",
       "https://www.instastalker.net/user/duffy_lifediary/17171598/2005721868481716420_17171598",
       "https://www.instastalker.net/user/duffy_lifediary/17171598/2000606338976381492_17171598",
       "https://www.instastalker.net/user/duffy_lifediary/17171598/1994328639488374098_17171598",
       "https://www.instastalker.net/user/duffy_lifediary/17171598/1986735185954578348_17171598",
       "https://www.instastalker.net/user/duffy_lifediary/17171598/1980513203852344702_17171598",
       "https://www.instastalker.net/user/duffy_lifediary/17171598/1974593574646810267_17171598")

for(a in 1:length(Fuml)){
  href<-read_html(Fuml[a])%>%html_nodes(".posts-box__image a")%>%html_attr("href")
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

follow<-read_html(Fuml[1])%>%html_nodes("a:nth-child(2) span span")%>%html_text()
follow<-strsplit(follow,"\n                                document.write")[[1]][2]
follow<-strsplit(follow,"\\(|\\)")[[1]][3]
FHFace<-data.frame(like=Flike,url=Furl,Face="1",Followers=follow,Weekday=Fweek,Gender="0",stringsAsFactors = F)
FUFace<-data.frame(like=UFlike,url=UFurl,Face="0",Followers=follow,Weekday=Uweek,Gender="0",stringsAsFactors = F)

FBindFace<-rbind(FHFace,FUFace)
AllBind<-rbind(FBindFace,BindFace)
#BindFace$Face<-as.numeric(BindFace$Face)
BindFace$Followers<-as.numeric(BindFace$Followers)
#BindFace$Weekday<-as.numeric(BindFace$Weekday)

