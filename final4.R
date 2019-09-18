library(rvest)
library(ggplot2)

all<-data.frame(hashtag=c(),like=c())
uml<-c("https://www.instastalker.net/user/liyu_foodmap/3214542666/2050754675472565498_3214542666",
       "https://www.instastalker.net/user/liyu_foodmap/3214542666/2021620902592201948_3214542666",
       "https://www.instastalker.net/user/liyu_foodmap/3214542666/2000018712547281112_3214542666",
       "https://www.instastalker.net/user/liyu_foodmap/3214542666/1982627834891391610_3214542666",
       "https://www.instastalker.net/user/liyu_foodmap/3214542666/1968140194200844438_3214542666",
       "https://www.instastalker.net/user/liyu_foodmap/3214542666/1956568160652038768_3214542666")
for(a in 1:length(uml)){
  href<-read_html(uml[a])%>%html_nodes(".posts-box__image a")%>%html_attr("href")
  for(i in 1:length(href)){
    hashtag<-read_html(href[i])%>%html_nodes(".posts-author__post-content a")%>%html_text()
    ifelse(grepl("^#抽獎",hashtag),next,count<-"1")
    hashtag<-hashtag[grep("^#liyu",hashtag)]
    count<-as.numeric(count)
    if(length(hashtag)>0){
      like<-read_html(href[i])%>%html_nodes("span~ span+ span")%>%html_text()
      like<-strsplit(like,"\n                            |\n                       ")[[1]][2]
      like<-as.numeric(like)
      hashtagN<-hashtag
      if(length(all)>0){
        ifelse(hashtagN%in%all$hashtag,
               all[all$hashtag%in%hashtagN,]$count<-all[all$hashtag%in%hashtagN,]$count+1,"0")
        ifelse(hashtagN%in%all$hashtag,
               all[all$hashtag%in%hashtagN,]$like<-all[all$hashtag%in%hashtagN,]$like+like,
               temp<-data.frame(hashtag=hashtag[!hashtagN%in%all$hashtag],like,count,stringsAsFactors = F))
        ifelse(hashtagN%in%all$hashtag,"0",all<-rbind(all,temp))
        ifelse()
      }else{
        temp<-data.frame(hashtag,like,count,stringsAsFactors = F)
        all<-rbind(all,temp)
      }
    }
  }
}

all$MeanLike<-all$like/all$count
ggplot()+geom_bar(data=all,aes(x=hashtag,y=MeanLike),stat = "identity")+ theme(axis.text.x = element_text(angle = 45))

country<-all[grep("#liyu台北$|#liyu新北$|#liyu桃園$|#liyu高雄$|#liyu台中$|#liyu台南$",all$hashtag),]
ggplot()+geom_bar(data=country,aes(x=hashtag,y=MeanLike),stat = "identity")+ theme(axis.text.x = element_text(angle = 45))


#all<-all[all$count>1,]

# TagCount<-all[all$count>1,]
# TagCount<-TagCount[order(TagCount$MeanLike,decreasing = T),]
