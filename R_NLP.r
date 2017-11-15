rm(list = ls())

library(tidyr)
library(dplyr)
library(data.table)
library(readr)
library(lubridate)
library(ggplot2)
library(readxl)
library(rJava)
library(xlsxjars)
library(xlsx)
library(devtools)
library(recharts)
library(REmap)
library(plotly)
library(stringr)
library(corrplot)
library(psych)
library(lars)
library(car)
library(GGally)
library(MASS)
library(ridge)

getwd()
setwd("C:\\Users\\Administrator\\Desktop\\外访地址有效性判定")
dir()

wfdata <- read_excel('waifang.xlsx')

wfpp <- read_excel('匹配数据.xlsx')

dzhi <- fread('dizhi.txt',encoding = 'UTF-8')

visit0 <- fread('visit.txt',encoding = 'UTF-8')


#110101195801082086,重复例子
bind1 <- left_join(visit0,dzhi,by=c('address'='address','id_no'='id_no'))

#地址类型匹配
test1 <- bind1 %>% group_by(address_type) %>% tally()

test2 <- dzhi %>% group_by(address_type) %>% tally()

#grepl使用，进行模糊赋值
test1$address_type[which(grepl(pattern = '户籍',test1$address_type) == TRUE)] <- '户籍地址'

test1$address_type[which(grepl(pattern = '家庭|住宅|居住|新地址|住址',test1$address_type) == TRUE)] <- '家庭地址'

test1$address_type[which(grepl(pattern = '公司|单位|工作',test1$address_type) == TRUE)] <- '单位地址'

test1$address_type[which(grepl(pattern = '通讯|邮寄',test1$address_type) == TRUE)] <- '通讯地址'

test1$address_type[which(!(test1$address_type %in% c('户籍地址','家庭地址','单位地址','通讯地址')))] <- '其他'

test3 <- test1 %>% group_by(address_type) %>% summarise(sum1 = sum(n))

test1 <- bind1 %>% group_by(address_type) %>% tally()

test1$人行 <-  grepl(pattern = '人行',test1$address_type)
test1$新  <-  grepl(pattern = '新',test1$address_type)

#bind1处理
bind1$newaddress_type <- bind1$address_type

bind1$newaddress_type[which(grepl(pattern = '户籍',bind1$newaddress_type) == TRUE)] <- '户籍地址'

bind1$newaddress_type[which(grepl(pattern = '家庭|住宅|居住|新地址|住址',bind1$newaddress_type) == TRUE)] <- '家庭地址'

bind1$newaddress_type[which(grepl(pattern = '公司|单位|工作',bind1$newaddress_type) == TRUE)] <- '单位地址'

bind1$newaddress_type[which(grepl(pattern = '通讯|邮寄',bind1$newaddress_type) == TRUE)] <- '通讯地址'

bind1$newaddress_type[which(!(bind1$newaddress_type %in% c('户籍地址','家庭地址','单位地址','通讯地址')))] <- '其他'

bind1$bz <- bind1$address_type

bind1$bz1 <-  grepl(pattern = '人行',bind1$address_type)
bind1$bz2 <-  grepl(pattern = '新',bind1$address_type)

bind1$bz1[which(bind1$bz1 == TRUE)] <- '人行'
bind1$bz1[which(bind1$bz1 == FALSE)] <-''

bind1$bz2[which(bind1$bz2 == TRUE)] <- '新'
bind1$bz2[which(bind1$bz2 == FALSE)] <-''

#paste合并
bind1$bz <- paste(bind1$bz1,bind1$bz2,sep = '')

bind1$bz[which(bind1$bz == '')] <- '无'

bind1 <- bind1[,1:73]

a1 <- bind1 %>% filter(comment_a1 == '外访地址存在')

#gsub进行替换
bind1$address <- gsub(pattern="中国", replacement="", bind1$address)

#str_extract进行正则匹配提取
bind1$province <- str_extract(bind1$address,"^\\w{2,3}省")

bind1$city <- str_extract(bind1$address,"\\w.市")

bind1$city <- gsub(pattern=".呼和浩特", replacement="呼和浩特", bind1$city)

bind1$zhen <- str_extract(bind1$address,"\\w.{1,2}[镇|区|乡]")

bind1$detail <- str_extract(bind1$address,"\\.*[号|房|室|户|部|村|路|1]")

bind1$address <- gsub(pattern=",|)|）| |-|\\.|\\*", replacement="", bind1$address)

#substr进行阶段获取
bind1$detail1 <- substr(bind1$address,nchar(bind1$address),nchar(bind1$address))

mowei <- substr(bind1$address,nchar(bind1$address),nchar(bind1$address))

dizhilxtj <- bind1 %>% group_by(comment_a1) %>% tally()

test11 <- bind1 %>% group_by(comment_a1,detail1) %>% tally()

test11 <- left_join(test11,dizhilxtj,by="comment_a1")

test11$zhb <- test11$n.x/test11$n.y

test11_1 <- bind1 %>% group_by(comment_a1,newaddress_type) %>% tally()

mowei1 <- data.frame(table(mowei))

ggplot(test11_1, aes(x=newaddress_type, y=n, fill=comment_a1)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge(0.6))

test11_2 <- bind1 %>% group_by(comment_a1,comment_c1) %>% tally()

tbnum <- bind1 %>% group_by(case_no) %>% tally()

bind1 <- bind1 %>% filter(nchar(bind1$id_no)==18)

bind1$year <- 2017-as.numeric(substr(bind1$id_no,7,10))

bind1$sex <- as.numeric(substr(bind1$id_no,17,17))%%2

colnames(tbnum) <- c('case_no','countnum')

bind1 <- left_join(bind1,tbnum,by="case_no")

test11_3 <- bind1 %>% filter(comment_a1 == "外访地址存在") %>% filter(!(comment_c1 %in% c('确认持卡人不在此','确认持卡人在此','请选择') ))


#####特征x7家庭地址与户籍地址是否相同#####1:表示地址相同,0:表示地址不同
dzhi$address_type[which(grepl(pattern = '户籍',dzhi$address_type) == TRUE)] <- '户籍地址'

dzhi$address_type[which(grepl(pattern = '家庭|住宅|居住|新地址|住址',dzhi$address_type) == TRUE)] <- '家庭地址'

dzhi$address_type[which(grepl(pattern = '公司|单位|工作',dzhi$address_type) == TRUE)] <- '单位地址'

dzhi$address_type[which(grepl(pattern = '通讯|邮寄',dzhi$address_type) == TRUE)] <- '通讯地址'

dzhi$address_type[which(!(dzhi$address_type %in% c('户籍地址','家庭地址','单位地址','通讯地址')))] <- '其他'

x7_1 <- dzhi %>% filter(address_type == '家庭地址')

x7_2 <- dzhi %>% filter(address_type == '户籍地址')

x7 <- left_join(x7_1,x7_2,by="id_no")

x7$pd <- x7$address.x==x7$address.y

x7$pd[which(x7$pd==TRUE)] <- 1

x7$pd[which(x7$pd==FALSE)] <- 0

x7$pd[is.na(x7$pd)] <- 0

x7result <- x7 %>% group_by(id_no) %>% summarise(bz = sum(pd))

x7result$bz[which(x7result$bz>0)] <- 1

x7result <- distinct(x7result)

colnames(x7result) <- c('id_no','hjshf')

bind1 <- left_join(bind1,x7result,by='id_no')

bind1$hjshf[is.na(bind1$hjshf)] <- 0

#####x11地址是否含有数字#####1:表示含有数字,0:表示不含有数字
#grepl使用，进行模糊匹配
bind1$numyx <-  grepl(pattern = '[0-9]|[０-９]',bind1$address)

bind1$numyx[which(bind1$numyx==TRUE)] <- 1

bind1$numyx[which(bind1$numyx==FALSE)] <- 0

#####x9地址是城市或者农村#####0:表示城市,1:表示农村

bind1$cityornot <-  grepl(pattern = '村|屯|队|组|庄',bind1$address)

bind1$cityornot[which(bind1$cityornot == TRUE)] <- 1

bind1$cityornot[which(bind1$cityornot == FALSE)] <- 0

#####x10身份证与户籍地址是否相同#####1:表示相同,0:表示不同

bind1$shfzh4 <- as.numeric(substr(bind1$id_no,1,4))

datashfzh <- read_excel('datashfzh.xlsx')

bind1 <- left_join(bind1,datashfzh,by=c('shfzh4'='shfzh4'))

bind1$city.x <- gsub(pattern="市", replacement="", bind1$city.x)

bind1$province.y <- gsub(pattern="省", replacement="", bind1$province.y)

bind1$province.y <- gsub(pattern="[0-9]", replacement="", bind1$province.y)

unique(bind1$city.x)

bind1$shhjpd <- bind1$city.x == bind1$city.y

bind1$shhjpd[which(bind1$shhjpd == TRUE)] <- 1

bind1$shhjpd[which(bind1$shhjpd == FALSE)] <- 0

bind1$shhjpd[is.na(bind1$shhjpd)] <- 0

#####x8地址是否具体#####
bind1$dizhidetail <- 1

#正则匹配含有某值
bind1$dizhidetail[which(bind1$detail1 %in% c('村','组','楼','屯','队'))] <- 0

bind1$dizhidetail[nchar(bind1$address) <= 15 ] <- 0

dataresult <- cbind(bind1$newaddress_type,bind1$year,bind1$sex,bind1$customer_name,bind1$bz,bind1$case_money,
                    bind1$hjshf,bind1$dizhidetail,bind1$cityornot,bind1$shhjpd,bind1$numyx)

dataresult <- data.frame(dataresult)

dataresult$address <- bind1$address

colnames(dataresult) <- c('地址类型','年龄','性别','客户类型','地址来源','欠款金额','家庭地址与户籍地址是否相同',
                          '地址是否具体','城市或者农村','身份证与户籍地址是否相同','地址是否含有数字','外访地址')

write.csv(dataresult,'特征数据.csv')