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

RBuml<-c("https://www.instastalker.net/user/ryan.food/442026038/2052042785666946289_442026038",
         "https://www.instastalker.net/user/ryan.food/442026038/2035564376933919859_442026038",
         "https://www.instastalker.net/user/ryan.food/442026038/2024505281808185985_442026038",
         "https://www.instastalker.net/user/ryan.food/442026038/2014540028903382665_442026038",
         "https://www.instastalker.net/user/ryan.food/442026038/2002943960415609046_442026038",
         "https://www.instastalker.net/user/ryan.food/442026038/1979752319592909337_442026038",
         "https://www.instastalker.net/user/ryan.food/442026038/1967243573431946847_442026038",
         "https://www.instastalker.net/user/ryan.food/442026038/1956559344838663145_442026038",
         "https://www.instastalker.net/user/ryan.food/442026038/1947848285097335274_442026038",
         "https://www.instastalker.net/user/ryan.food/442026038/1938439607684556608_442026038",
         "https://www.instastalker.net/user/ryan.food/442026038/1920305954735781076_442026038",
         "https://www.instastalker.net/user/ryan.food/442026038/1894041974261076260_442026038")

for(a in 1:length(RBuml)){
  href<-read_html(RBuml[a])%>%html_nodes(".posts-box__image a")%>%html_attr("href")
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

follow<-read_html(RBuml[1])%>%html_nodes("a:nth-child(2) span span")%>%html_text()
follow<-strsplit(follow,"\n                                document.write")[[1]][2]
follow<-strsplit(follow,"\\(|\\)")[[1]][3]
RBHFace<-data.frame(like=Flike,url=Furl,Face="1",Followers=follow,Weekday=Fweek,Gender="1",stringsAsFactors = F)
RBUFace<-data.frame(like=UFlike,url=UFurl,Face="0",Followers=follow,Weekday=Uweek,Gender="1",stringsAsFactors = F)
RBBindFace<-rbind(RBHFace,RBUFace)
#BindFace$Face<-as.numeric(BindFace$Face)
AllBind2<-rbind(AllBind1,RBBindFace)
BindFace$Followers<-as.numeric(BindFace$Followers)
#BindFace$Weekday<-as.numeric(BindFace$Weekday)

# save(AllBind2,file="AllBind2.Rda")
# save(AllBind1,file="AllBind1.Rda")
# save(AllBind,file="AllBind.Rda")
# save(BindFace,file="BindFace.Rda")
