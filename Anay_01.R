
# start ##############################
# setwd('D:\\RProject\\DBun2403')
# setwd('C:\\Users\\Jbs\\Desktop\\jbs\\DBun243')
# setwd("D:\\EBS\\02 업무\\900 업무진행 기타\\320 와우자 재기동(반디외국어2)\\서버로그\\wowzalog_5")
setwd("D:\\EBS\\02 업무\\900 업무진행 기타\\320 와우자 재기동(반디외국어2)\\서버로그")

# install.packages("data.table")
library(data.table)
library(dplyr)

rm(list=ls())
gc()

v_server <- "svr_5"
f_list <- list.files()
# f_list <- grep("2018-10", f_list, value = T)
# f_list.access <- f_list %>% 
#   filter()
#  access : date	time	tz	x-event	x-category	x-severity	x-status	x-ctx	x-comment	x-vhost	x-app	x-appinst	x-duration	s-ip	s-port	s-uri	c-ip	c-proto	c-referrer	c-user-agent	c-client-id	cs-bytes	sc-bytes	x-stream-id	x-spos	cs-stream-bytes	sc-stream-bytes	x-sname	x-sname-query	x-file-name	x-file-ext	x-file-size	x-file-length	x-suri	x-suri-stem	x-suri-query	cs-uri-stem	cs-uri-query
#  error : x-severity	x-category	x-event	date	time	c-client-id	c-ip	c-port	cs-bytes	sc-bytes	x-duration	x-sname	x-stream-id	x-spos	sc-stream-bytes	cs-stream-bytes	x-file-size	x-file-length	x-ctx	x-comment


max_i <- 10

for (i in 1:max_i){

  cat("loop: i",i, "/", max_i,"\n")
  a <- ifelse(nchar(i)==1,paste0("0",i),i) ## ok
  # a <- paste0("0",i); a <- substr(a,nchar(a)-1,nchar(a)) ## ok
  f_list_t <- grep(paste0("2018-",a),f_list, value = T)
  cat("file: ", length(f_list_t),"/", length(f_list))
  out_acc <- NULL
  out_err <- NULL
  if (length(f_list_t)!=0){
    for (j in 1:length(f_list_t)){
      f_name <- f_list_t[j]
      cat("loop: j",j, "/", length(f_list_t),"\n")
      if (f_name %like% "access") {
        log1.f <- fread(f_name, drop=c(9), skip=5)
        out_acc <- rbind(out_acc, log1.f)
      }else{
        log2.f <- fread(f_name, skip=5)
        out_err <- rbind(out_err, log2.f)
      }
    }
    write.csv(out_acc, paste0("../",v_server,"_acc_log.2018-",a,".csv"), row.names=FALSE)
    write.csv(out_err, paste0("../",v_server,"_err_log.2018-",a,".csv"), row.names=FALSE)
    # write.table(out_acc, paste0("../",v_server,"_acc_log.2018-",a,".csv"), row.names=FALSE,sep = "\t")
    # write.table(out_err, paste0("../",v_server,"_err_log.2018-",a,".csv"), row.names=FALSE,sep = "\t")
  }
}

setwd("D:\\EBS\\02 업무\\900 업무진행 기타\\320 와우자 재기동(반디외국어2)\\서버로그\\data")
f_list <- list.files(pattern = "svr")
out_acc <- NULL; out_err <- NULL

for (i in 1:length(f_list)){
  
  cat(i,"/",length(f_list),"\n")
  f_name <- f_list[i]
  if (f_name %like% "acc") {
    log1.f <- fread(f_name)
    # log1.f <- read.csv(f_name, stringsAsFactors = F)
    out_acc <- rbind(out_acc, log1.f)
  }else{
    log2.f <- fread(f_name)
    # log2.f <- read.csv(f_name, stringsAsFactors = F)
    out_err <- rbind(out_err, log2.f)
  }
}

# read.csv("../data/_acc_log.2018-",a,".csv"), row.names=FALSE)
write.csv(out_acc,"../tot_acc_log2.csv", row.names=FALSE)
write.csv(out_err,"../tot_err_log.csv", row.names=FALSE)
rm(out_acc)
rm(list=ls())

out_acc <- fread("../tot_acc_log1.csv")
out_acc0 <- fread("../tot_acc_log.csv")
out_acc1 <- fread("../tot_acc_log2.csv")
out_acc <- rbind(out_acc, out_acc0)
out_acc <- rbind(out_acc, out_acc1)

table(substr(out_acc$V1,1,7))


acc.col <- c("date","time","tz","x.event","x.category","x.severity","x.status","x.ctx","x.vhost","x.app",
             "x.appinst","x.duration","s.ip","s.port","s.uri","c.ip","c.proto","c.referrer","c.user.agent","c.client.id",
             "cs.bytes","sc.bytes","x.stream.id","x.spos","cs.stream.bytes","sc.stream.bytes","x.sname","x.sname.query","x.file.name","x.file.ext",
             "x.file.size","x.file.length","x.suri","x.suri.stem","x.suri.query","cs.uri.stem","cs.uri.query")
err.col <- c("x.severity","x.category","x.event","date","time","c.client.id","c.ip","c.port","cs.bytes","sc.bytes","x.duration","x.sname","x.stream.id","x.spos","sc.stream.bytes","cs.stream.bytes","x.file.size","x.file.length","x.ctx","x.comment")


colnames(out_acc) <- c(acc.col)
# colnames(out_acc0) <- c(acc.col)
# colnames(out_acc1) <- c(acc.col)

colnames(out_err) <- c(err.col)

# colnames(tot_acc_log.df) <- c(acc.col)
# colnames(tot_err_log.df) <- c(err.col)

col_list = c("date","time","tz","x.event","x.category",
             "x.severity","x.status","x.duration","s.uri","c.proto","c.ip")

tot_acc <- out_acc %>% 
  select (col_list)
tot_acc1 <- out_acc1 %>% 
  select (col_list)
tot_acc <- rbind(tot_acc,tot_acc1)

tot_acc0 <- out_acc0 %>% 
  select (col_list)


save(tot_acc.clean,file="./tot_access_all.Rdata")
# save(tot_acc,file="./tot_access.Rdata")
# save(tot_acc.clean,file="./tot_access_clean.Rdata")
# save(tot_acc0,file="./tot_access0.Rdata")
# save(tot_acc0.clean,file="./tot_access0_clean.Rdata")

rm(list=ls())
gc()

load(file="./data/tot_access_all.Rdata")
# load(file="./tot_access.Rdata")
# load(file="./tot_access_clean.Rdata")
# load(file="./tot_access0.Rdata")
# load(file="./tot_access0_clean.Rdata")
summary(tot_acc.clean)

tot_acc.clean <- tot_acc %>% mutate(
  tz = as.factor(tz),
  x.event = as.factor(x.event),
  x.category = as.factor(x.category),
  x.severity = as.factor(x.severity),
  x.status = as.factor(x.status),
  c.proto = as.factor(c.proto),
  c.ip = as.factor(c.ip)
  # dt = strptime(paste0(date,time), format="%Y-%m-%d %H:%M:%S")
  # dt = as.Date(as.POSIXct(paste0(date,time)))
)

tot_acc.clean$c.proto = as.factor(tot_acc.clean$c.proto)
tot_acc.clean$c.ip = as.factor(tot_acc.clean$c.ip)
tot_acc.clean$dt = strptime(paste0(tot_acc.clean$date,tot_acc.clean$time), format="%Y-%m-%d %H:%M:%S")


a <- as.Date(as.POSIXct(paste0(aa,a1)))
a <- as.Date((paste0(aa,a1)), format="%Y-%m-%dH24:MI:SS")
a <- as.Date("2018-01-01", format="%Y-%m-%d")
a <- strptime("2018-01-0101:00:00", format="%Y-%m-%d %H:%M:%S")
a <- strptime(paste0(aa,a1), format="%Y-%m-%d %H:%M:%S")

table(substr(tot_acc.clean$date,1,7))

tot_acc.clean <- rbind(tot_acc.clean,tot_acc0.clean)

rm(tot_acc)
rm(tot_acc0)

# tot_acc[tot_acc$dt <= as.Date(as.POSIXct("2018-01-01","00:00:00")),]
tot_acc[tot_acc$dt <= strptime("2018-01-01 00:00:00", format="%Y-%m-%d %H:%M:%S"),]

# ---------------------------------------------
## load 이후 작업
tot_acc.clean$x.duration <- as.double(tot_acc.clean$x.duration)

# 
# tot_acc.clean$tz <- as.factor(tot_acc.clean$tz)
# tot_acc.clean$x.event <- as.factor(tot_acc.clean$x.event)
# tot_acc.clean$x.category <- as.factor(tot_acc.clean$x.category)
# tot_acc.clean$x.severity <- as.factor(tot_acc.clean$x.severity)
# tot_acc.clean$x.status <- as.factor(tot_acc.clean$x.status)


t_a_ip_event <- table(tot_acc.clean$c.ip,tot_acc.clean$x.event)
t_a_ip_status <- table(tot_acc.clean$c.ip,tot_acc.clean$x.status)
t_c_uri <- table(tot_acc.clean$s.uri)
t_c_proto <- table(tot_acc.clean$c.proto)
t_c_ip <- table(tot_acc.clean$c.ip)
t_c_event <- table(tot_acc.clean$x.event)
t_c_status <- table(tot_acc.clean$x.status)

save_list <- c("t_a_ip_event","t_a_ip_status","t_c_uri","t_c_proto","t_c_ip","t_c_event","t_c_status")
save("t_a_ip_event","t_a_ip_status","t_c_uri","t_c_proto","t_c_ip","t_c_event","t_c_status", file="./table1.Rdata")
# save(c(save_list), file="./table2.Rdata")
# load(file="./data/table.Rdata")

t_a_ip_event

plot(t_c_uri[-1])
plot(t_c_proto[-1])
plot(t_c_ip[-1])
plot(t_c_event[-3])
plot(t_c_status)
plot(t_a_ip_event[-3])


getwd()
# write.csv(t_a_ip_event,"../data/ip_event.csv")

df.a <- tot_acc_log.df[`c-ip` == "110.10.122.10",]
df.b <- tot_acc_log.df[`c-ip` == "127.0.0.1",]
head(df.a[,c(1,2,4,7,17)],100) 
View(df.a[,c(1,2,4,7,15,17)])
View(tot_acc_log.df)
#---------------------

df.a <- tot_acc_log.df[c-ip== "110.10.122.10",]
select(c-ip ==  "110.10.122.10")

df.a <- tot_acc_log.df %>% 
  filter(c-tot_acc_log.df$`c-ip`=="110.10.122.10")

tot_acc_log.df %>% 
  group_by(c-ip) %>% 
  summarise(n = n())

tot_acc_log.df %>% 
  group_by("x-event") %>% 
  summarise(n = n())


# CDN IP & duration time
ip_duration <- 
  tot_acc.clean %>% 
  select(c.ip, x.duration) %>% 
  group_by(c.ip) %>% 
  summarise(sum(x.duration))

ip_duration_1 <- 
  tot_acc.clean %>% 
  select(date, c.ip, x.duration) %>% 
  group_by(c.ip, substr(date,1,7)) %>% 
  summarise(sum(x.duration))

colnames(ip_duration) <- c("ip","s_duration")
colnames(ip_duration_1) <- c("ip","date_ym","duration")
ip_duration_1.d <- na.omit(ip_duration_1)
ip_duration_1.d$date_ym <- as.factor(ip_duration_1.d$date_ym)

summary(ip_duration_1.d)

write.csv(ip_duration_1,file="./ip_duration_1.csv", row.names = F)


plot(ip_duration_1.d$ip, ip_duration_1.d$duration)
plot(ip_duration_1.d$date_ym, ip_duration_1.d$duration, ip_duration_1.d$ip )


# install.packages("plotly")
library(plotly)
library(ggplot2)

plot(ip_duration_1$duration, ip_duration_1$ip, ip_duration_1$date_ym)

plot(ip_duration_1$duration, ip_duration_1$ip, col=ip_duration_1$date_ym)
plot(ip_duration_1$duration, ip_duration_1$date_ym, col=ip_duration_1$ip)

ip_duration_1.p <- ip_duration_1.d[ip_duration_1.d$ip != "-",]
ip_duration_1.p <- ip_duration_1.p[ip_duration_1.p$ip != "127.0.0.1",]
# ip_duration_1.p <- ip_duration_1.p[ip_duration_1.d$ip != "-",]

ggplot(data=ip_duration_1.p, aes(x=date_ym, y=duration, group=ip, shape=ip)) +
  # ggplot(data=ip_duration_1.p, aes(x=date_ym, y=duration, group=ip, colour=ip)) +
  geom_line() +
  geom_point()


pie(ip_duration_1.d$duration, labels=ip_duration_1.d$ip)

ip_duration <- na.omit(ip_duration)

pie(ip_duration$s_duration, ip_duration$ip)

# # Basic line graph with points
# ggplot(data=dat1, aes(x=time, y=total_bill, group=sex)) +
#   geom_line() +
#   geom_point()
# 
# # Map sex to color
# ggplot(data=dat1, aes(x=time, y=total_bill, group=sex, colour=sex)) +
#   geom_line() +
#   geom_point()
# 
# # Map sex to different point shape, and use larger points
# ggplot(data=dat1, aes(x=time, y=total_bill, group=sex, shape=sex)) +
#   geom_line() +
#   geom_point()
# 
# 
# # Use thicker lines and larger points, and hollow white-filled points
# ggplot(data=dat1, aes(x=time, y=total_bill, group=sex, shape=sex)) + 
#   geom_line(size=1.5) + 
#   geom_point(size=3, fill="white") +
#   scale_shape_manual(values=c(22,21))
# 


# 
#   
# log1 <- read.table("wowzastreamingengine_access.log.2018-01-01_01", 
#                    header=T, 
#                    sep="\t", 
#                    comment.char = "",
#                    skip=5,
#                    # allowEscapes = T,
#                    stringsAsFactors = F, 
#                    na.strings = "")
# 
# log1.f <- fread("wowzastreamingengine_access.log.2018-01-01", drop=c(9), skip=5)
# log2.f <- fread("wowzastreamingengine_error.log.2018-01-01", skip=5)
# 


# 
# # 존재하는 파일
# file_list <- list.files()
# 
# total_air_df <- NULL
# 
# for(i in 1:length(file_list)){
#   file_name <- file_list[i]
#   cat(i, '/', length(file_list), '\n')
#   
#   air_df <- read.csv(file_name, stringsAsFactors = F)
#   
#   total_air_df <- rbind(total_air_df, air_df)
#   
# }
# 
# total_air_df$A <- ifelse(total_air_df$X10분.최다.강수량.mm. > 5, 1, 0)
# 
# 
# 
# write.csv(total_air_df, "C:\\Users\\Jbs\\Desktop\\jbs\\DBun243\\data\\기상자료\\기상자료total\\total_air_df.csv", row.names = F)
# 
# 



