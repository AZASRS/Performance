## @knitr setup
library(PerformanceAnalytics)
library(lubridate)
library(scales)
library(RColorBrewer)
library(zoo)
library(ggplot2)
library(reshape2)
library(knitr)
library(tidyr)
library(dplyr)
library(xts)
library(magrittr)
library(xtable)
library(gridExtra)
library(ggrepel)
library(ggpubr)
library(asrsMethods)
library(asrsPalettes)
### Load custom functions and Priv Markets latest Data
load('P:/IMD/Karl/R Projects/private investment performance/pmedata.rdata')
source(file = "P:/IMD/Karl/R projects/Public Performance/Scripts/Functions.R")
pme.df$`Fund IRR`= pme.df$`Fund IRR`/100
effective.date = valdate #explore moving this and next 2 lines to lyx file
tomillions = function(x) x/1000000
label.d = "as of 3/31/18"

#Because of data issues, fix the start AND end date for the attribution analysis
end.date <- as.Date("2018-03-31") #need to update

#Ten year window 
start.date <- as.Date("2008-03-31") #need to update
xts.start <- as.Date("2008-04-30") #need to update
xts.range <- paste0(xts.start, "/", end.date)

## @knitr tot.equity
nepc.dat <- read.csv("P:/IMD/Karl/R Projects/Public Performance/Data/CSVs/rNEPC.csv", stringsAsFactors = FALSE) %>%
  mutate(ID = ShortName) %>%
  mutate(Date = as.Date(AsOf, format = "%m/%d/%Y")) %>%
  mutate(NetReturn = NetReturn/100) %>%
  filter(between(Date, start.date, end.date) & Period == "Monthly")

#nepc.map is the mapping file for nepc.dat
nepc.map <- read.csv("P:/IMD/Karl/R Projects/Public Performance/Data/Mapping/NEPC.map.csv", stringsAsFactors = FALSE)

saa.equity <- read.csv("P:/IMD/JohnD/Attribution/csv/saa.equity.csv", stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

equity.id <- c("RAY0003", "RAY0005", "RAY0006", "RAY0007", "RAY0039", "RAY0038", "RAY0010", "RAYE9", "RAY00041")

b.tf <- subset(nepc.dat, nepc.dat$ID == "RAY0001", select = c('Date','NetReturn'))
b.tf.xts <- xts(b.tf[ ,-1], b.tf[ ,1])
b.tf.xts <- b.tf.xts[xts.range, ]
tf.index <- as.vector(cumprod(1 + apply.quarterly(b.tf.xts, FUN = Return.cumulative)))
tf.multiplier <- tf.index[length(tf.index)] / tf.index

#Total Equity Benchmark
b.eq <- subset(nepc.dat, nepc.dat$ID == "ASRSTEBM", select = c('Date','NetReturn'))
b.eq.xts <- xts(b.eq[ ,-1], b.eq[ ,1])
b.eq.xts <- b.eq.xts[xts.range,]

#compile TWR data for total public, domestic, international,
  #private & opportunistic equity
t.eq.id <- c("RAY0003","RAY0004","RAY0008","RAY0025", "RAYOPPPE")
r.teq.list <- list()
b.teq.list <- list()
mv.teq.list <- list()
dva.all.list <- list()

for(c in t.eq.id) {
  #get return, name, & inception date
  name <- nepc.map[which(nepc.map$ID == c), 'Short.Name']
  r <- subset(nepc.dat, nepc.dat$ID == c, select = c('Date','NetReturn'))
  r.xts <- xts(r[,-1], r[,1])
  r.xts <- r.xts[!is.na(r.xts),]
  r.xts <- r.xts[!duplicated(index(r.xts)), ]
  r.xts <- r.xts[xts.range,]
  i.d <- as.character(time(r.xts)[1])
  r.teq.list[[name]] <- r.xts
  #benchmarks for each asset class
  b.id <- nepc.map[which(nepc.map$ID == c), 'BM.ID']
  b <- subset(nepc.dat, nepc.dat$ID == b.id, select = c('Date','NetReturn'))
  b.xts <- xts(b[ ,-1], b[ ,1])
  b.xts <- b.xts[paste0(i.d,"/"), ]
  b.xts = b.xts[xts.range, ]
  b.teq.list[[name]] <- b.xts
  #get market values
  mv = subset(nepc.dat, nepc.dat$ID == c, select = c('Date','MthEndMV'))
  mv.xts <- xts(mv[ ,-1], mv[ ,1])
  mv.xts <- lag.xts(mv.xts, 1)
  mv.xts <- mv.xts[!duplicated(index(mv.xts)), ]
  mv.xts <- mv.xts[paste0(i.d,"/"), ]
  mv.xts <- mv.xts[xts.range, ]
  mv.teq.list[[name]] <- mv.xts
  #calculate DVA
  mv.monthly <- lag.xts(mv.xts,1)
  #ex.all.list[[name]] <- r.xts - b.xts
  dva.all.list[[name]] <- mv.monthly * (r.xts - b.xts)
}

r.teq <- do.call(merge, r.teq.list)
r.teq <- na.fill(r.teq, 0)
colnames(r.teq) <- names(r.teq.list)
b.teq <- do.call(merge, b.teq.list)
b.teq <- na.fill(b.teq, 0)
colnames(r.teq) <- names(b.teq.list)


#create table of returns
range.qtr <- paste0(end.date - months(3) + days(1), "/", end.date)
range.1 <- paste0(end.date - years(1) + days(1), "/", end.date)
range.3 <- paste0(end.date - years(3) + days(1), "/", end.date)
range.5 <- paste0(end.date - years(5) + days(1), "/", end.date)
range.10 <- paste0(end.date - years(10) + days(1), "/", end.date)

peq.trailing <- list()
for(i in names(r.teq.list[-1])) {
  ret <- merge(r.teq.list[[i]], b.teq.list[[i]])
  if(dim(ret)[1] <= 12) next
  return.1 <- t(Return.annualized(ret[((dim(ret)[1]-(12-1)):dim(ret)[1]), ], scale = 12))
  if(dim(ret)[1] >=36) {
    return.3 <- t(Return.annualized(ret[((dim(ret)[1]-(36-1)):dim(ret)[1]), ], scale = 12))}
  else {return.3 <- NA}
  if(dim(ret)[1] >=60){
    return.5 <- t(Return.annualized(ret[((dim(ret)[1]-(60-1)):dim(ret)[1]), ], scale = 12))}
  else {return.5 <- NA}
  if(dim(ret)[1] >= 120)  {
    return.10 <- t(Return.annualized(ret[((dim(ret)[1]-(120-1)):dim(ret)[1]), ], scale = 12))}
  else {return.10 <- NA}
  tr.ret <- cbind(return.1, return.3, return.5, return.10)
  desc <- c("One Year", "Three Year", "Five Year", "Ten Year")
  excess = as.data.frame(t(tr.ret))
  Excess = (excess[, 1] - excess[,2])
  tr.ret = data.frame(rbind(tr.ret, Excess))
  colnames(tr.ret) = desc
  tr.ret$Composite = c(i, "Benchmark", "Excess")
  peq.trailing[[i]] = tr.ret
}
all.peq = do.call(rbind, peq.trailing)
rownames(all.peq) = NULL
all.peq = subset(all.peq, select = c("Composite", desc))
all.peq$`One Year` = paste0(round(all.peq$`One Year`*100,2),"%")
all.peq$`Three Year` = paste0(round(all.peq$`Three Year`*100,2),"%")
all.peq$`Five Year` = paste0(round(all.peq$`Five Year`*100,2),"%")
all.peq$`Ten Year` = paste0(round(all.peq$`Ten Year`*100,2),"%")
hlines = c(-1,seq(0,nrow(all.peq), by=(+3)))
print(xtable(all.peq, align = rep("r",6), digits = 2), hline.after = hlines,
      scalebox=.4, include.rownames = FALSE)

## @knitr tot.eq.mvs
#Market Values for publics
teq.mvs <- do.call(merge, mv.teq.list)
teq.mvs = na.fill(teq.mvs, 0)
colnames(teq.mvs) <- names(mv.teq.list)
teq.mvs = teq.mvs[endpoints(teq.mvs, "quarters"), ]

teq.mvs.df <- data.frame("Date" = index(teq.mvs), coredata(teq.mvs[ ,-1]))
colnames(teq.mvs.df)[-1] <- c("Dom Eq", "Intl Eq", "Priv Eq",
                              "Opp Eq")
teq.tidy <- gather(teq.mvs.df, Composite, `Market Value`, -Date)
teq.tidy$`Market Value` = tomillions(teq.tidy$`Market Value`)/1000
teq.tidy$Composite = factor(teq.tidy$Composite, levels = 
                              c("Dom Eq", "Intl Eq", "Priv Eq",
                                "Opp Eq"))
#plot of just the market values
mvs.teq <- ggplot()+
  geom_bar(data = teq.tidy,aes(x = Date,y = `Market Value`, fill = Composite), 
           stat="identity")+ ggtitle("Market Values") + 
  scale_fill_manual(values = IMD.palette()) + xlab("") + ylab("in Billions")+
  scale_y_continuous(labels = scales::dollar)+
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 8), 
        legend.key.size = unit(.25, "cm"))
print(mvs.teq)
#might need to add footnote that total excludes RFP portfolio

## @knitr tot.eq.rolling
r.teq.roll <- merge(
  rollapply(na.trim(r.teq[ ,1]), width = 12, FUN = "Return.annualized", scale = 12),
  rollapply(na.trim(r.teq[ ,2]), width = 12, FUN = "Return.annualized", scale = 12),
  rollapply(na.trim(r.teq[ ,3]), width = 12, FUN = "Return.annualized", scale = 12),
  rollapply(na.trim(r.teq[ ,4]), width = 12, FUN = "Return.annualized", scale = 12),
  rollapply(na.trim(r.teq[ ,5]), width = 12, FUN = "Return.annualized", scale = 12))

plot.teq.roll <- ggplot(
  data = fortify.zoo(r.teq.roll, melt = TRUE), 
  aes(x = Index, y = Value, stat = Series, colour = Series)) +
  geom_line(size = .5) +
  xlab("") + scale_y_continuous(name = "Annualized Return", labels = scales::percent) +
  ggtitle("Total Equity Rolling 1 Year Returns") +
  scale_color_manual(values = IMD.palette(), name = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size = 8, face = "bold"),
        axis.title.y = element_text(size = 6, face = "italic"), 
        legend.text = element_text(size = 7), legend.key.size = unit(.25, "cm"),
        axis.text.x = element_text(size = 6, face = "bold"), axis.text.y = element_text(size = 6, face = "bold"))
print(plot.teq.roll)

## @knitr tot.eq.dva
dva.all <- do.call(merge, dva.all.list[c(1, 4:5)]) %>%
  apply.quarterly(., FUN = colSums) %>%
  tomillions(.) %>%
  set_colnames(names(dva.all.list)[c(1, 4:5)])

plot.teq.dva <- ggplot(
  data = fortify.zoo(dva.all[range.5, ], melt = TRUE),
  aes(x = Index, y = Value, fill = Series)) +
  geom_col(position = "stack") +
  ggtitle("") +
  ylab("in Millions") + xlab("") + scale_y_continuous(labels = scales::dollar) +
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
  scale_fill_manual(values=IMD.palette(), name = "Asset Class") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 10, face = "italic"),
        legend.background = element_rect(fill="gray92", size=4.5, linetype="dotted"))

end.qtr <- dim(dva.all)[1]
teq.dva.summary.long <- data.frame(
  colSums(dva.all[(end.qtr- 3):end.qtr,], na.rm = TRUE),
  colSums(dva.all[(end.qtr-11):end.qtr,], na.rm = TRUE),
  colSums(dva.all[(end.qtr-19):end.qtr,], na.rm = TRUE)
) %>%
  set_colnames(c("One Year","Three Year","Five Year")) %>%
  mutate(Asset.Class = rownames(.)) %>%
  gather(Period, DVA, -Asset.Class) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year"))) %>%
  mutate(Asset.Class = factor(Asset.Class, levels = c(names(r.teq.list[1]),
                                                      names(r.teq.list[4]),
                                                      names(r.teq.list[5]))))

plot.teq.dva.tp <- ggplot(teq.dva.summary.long, aes(x = Period, y = DVA, fill = Asset.Class)) +
  geom_bar(stat = "identity") +
  ggtitle("") +
  ylab("in Millions") + xlab("") + scale_y_continuous(labels = scales::dollar) +
  scale_fill_manual(values=IMD.palette(), name = "Asset Class:") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.title.y = element_text(size = 10, face = "italic"),
        legend.background = element_rect(fill="gray92", size=4.5, linetype="dotted"))

teq.dva.plot<- ggarrange(plot.teq.dva, plot.teq.dva.tp, nrow = 1, ncol = 2, 
                         common.legend = T, legend = "bottom")
annotate_figure(teq.dva.plot, top  = 
                  text_grob("Total Equity Dollar Value Added \n Relative to Sub-Asset Class Benchmarks", 
                                 face = "bold", size = 16))

## @knitr pub.equity.dva
#compile data for public market returns
r.eq.list = list()   
b.eq.list = list()
mv.eq.list = list()
dva.eq.list = list()
sel.eq.list = list()

for(c in equity.id) {
  #get return, name, & inception date
  name <- nepc.map[which(nepc.map$ID == c), 'Short.Name']
  r <- subset(nepc.dat, nepc.dat$ID == c, select = c('Date','NetReturn'))
  r.xts <- xts(r[,-1], r[,1])
  r.xts <- r.xts[!is.na(r.xts),]
  r.xts <- r.xts[!duplicated(index(r.xts)), ]
  r.xts <- r.xts[xts.range,]
  i.d <- as.character(time(r.xts)[1])
  r.eq.list[[name]] <- r.xts
  
  #benchmarks for each asset class and convert Private Equity & Private Debt to quarterly
  b.id <- nepc.map[which(nepc.map$ID == c), 'BM.ID']
  #b.n <- nepc.map[which(nepc.map$ID == c), 'BM.Shortname']
  b <- subset(nepc.dat, nepc.dat$ID == b.id, select = c('Date','NetReturn'))
  b.xts <- xts(b[ ,-1], b[ ,1])
  b.xts <- b.xts[paste0(i.d,"/"), ]
  b.xts = b.xts[xts.range, ]
  b.eq.list[[name]] <- b.xts
  
  #get market values
  mv = subset(nepc.dat, nepc.dat$ID == c, select = c('Date','MthEndMV'))
  mv.xts <- xts(mv[ ,-1], mv[ ,1])
  mv.xts <- lag.xts(mv.xts, 1)
  mv.xts <- mv.xts[!duplicated(index(mv.xts)), ]
  mv.xts <- mv.xts[paste0(i.d,"/"), ]
  mv.xts <- mv.xts[xts.range, ]
  mv.eq.list[[name]] <- mv.xts
  
  #calculate selection effect
  sel.eq.list[[name]] <- (r.xts - b.xts) * mv.xts
  
  #calculate total DVA
  dva.xts <- (r.xts - b.eq.xts) * mv.xts
  dva.eq.list[[name]] <- dva.xts
}
#Portfolio Returns
r.eq <- do.call(merge, r.eq.list)
r.eq <- na.fill(r.eq, 0)
colnames(r.eq) <- names(r.eq.list)

#Benchmark Returns
b.eq <- do.call(merge, b.eq.list)
#b.eq = na.fill(b.eq, 0)
colnames(b.eq) <- names(b.eq.list)
#Test:   identical(b.eq.xts, b.eq.list[[1]])

#Equity DVA
eq.dva <- do.call(merge, dva.eq.list)
eq.dva <- na.fill(eq.dva, 0)
eq.dva <- apply.quarterly(eq.dva, FUN = colSums)
colnames(eq.dva) <- names(dva.eq.list)
#Test: sum(do.call(merge, dva.eq.list) - eq.mvs * (r.eq - as.vector(b.eq.xts)), na.rm = T) == 0    

#Equity MVS
eq.mvs <- do.call(merge, mv.eq.list)
eq.mvs <- na.fill(eq.mvs, 0)
colnames(eq.mvs) <- names(mv.eq.list)

#Pub Eq Total Return and BM
eq.data <- merge(r.eq[,1], b.eq[ ,1], eq.mvs[ ,1])
colnames(eq.data) <- c("Public Equity","Composite Benchmark", "BegMV")

#calculate active weights versus SAA
saa.eq <- xts(saa.equity[ ,-1], saa.equity[ ,1])
saa.eq <- saa.eq[xts.range, -10]
#Test:  sum(rowSums(saa.eq[,-1])) == dim(saa.eq)[1]

#active exposure
aw.eq.percent <- eq.mvs/rowSums(eq.mvs[,-1]) - saa.eq
aw.eq.dollar <- as.vector(eq.mvs[,1]) * aw.eq.percent
#Test:  sum(rowSums(trunc(aw.eq.percent[,-1], digits = 8))) == 0
aw.eq.quart <- apply.quarterly(aw.eq.percent[,-1], FUN = mean)
aw.eq.quart = aw.eq.quart[(end.qtr-19):end.qtr, ]
aw.eq.quart <- data.frame("Date"= index(aw.eq.quart), coredata(aw.eq.quart))
aw.eq.lf <- gather(aw.eq.quart, 'Composite', 'Active Weight', -Date)

#Allocation effect
eq.allocation <- aw.eq.dollar * (b.eq - as.vector(b.eq[,1]))
eq.allocation <- na.fill(eq.allocation, 0)
eq.allocation <- apply.quarterly(eq.allocation, FUN = colSums)
#Test:  sum(eq.allocation[,1]) == 0

#Selection effect
eq.selection <- do.call(merge, sel.eq.list)
eq.selection <- na.fill(eq.selection, 0)
eq.selection <- apply.quarterly(eq.selection, FUN = colSums)
colnames(eq.selection) = names(sel.eq.list)
#Test:  identical(eq.dva[,1], eq.selection[,1])

#Other
eq.other = eq.dva[,1] - rowSums(eq.allocation[,-1]) - rowSums(eq.selection[,-1])
end.mth <- dim(eq.data)[1]

eq.return.long <- data.frame(
  t(Return.annualized(eq.data[(end.mth-11):end.mth, 1:2])),
  t(Return.annualized(eq.data[(end.mth-35):end.mth, 1:2])),
  t(Return.annualized(eq.data[(end.mth-59):end.mth, 1:2])),
  t(Return.annualized(eq.data[(end.mth-119):end.mth, 1:2]))
) %>%
  set_colnames(c("One Year","Three Year","Five Year", "Ten Year")) %>%
  mutate(Portfolio = rownames(.)) %>%
  gather(Period, Return, - Portfolio) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year", "Ten Year"))) %>%
  mutate(Portfolio = factor(Portfolio, levels = c("Public Equity", "Composite Benchmark")))

eq.return.plot <- ggplot(eq.return.long, aes(x = Period, y = Return, fill = Portfolio)) +
  geom_bar(stat = "identity", position = "Dodge") + 
  geom_text(aes(label = round(Return*100, 1)), position = position_dodge(width=0.9), vjust=-0.25)+
  xlab("") + scale_y_continuous(name = "Annualized Return", labels = scales::percent) +
  ggtitle("Public Equity and Composite Benchmark",
          subtitle = "Trailing Period Returns") +
  scale_fill_manual(values = IMD.palette(), name = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 10, face = "italic"), legend.position = "bottom",
        legend.background = element_rect(fill="gray92", size=4.5, linetype="dotted"))

eq.dva.summary <- merge(eq.dva[, 1], rowSums(eq.allocation[,-1]), rowSums(eq.selection[,-1]), eq.other[,1]) %>%
  set_colnames(c("Public Equity DVA", "Allocation", "Selection", "Other")) %>%
  multiply_by(tf.multiplier) %>%
  tomillions(.)
#Test:   sum(eq.dva.summary[,1]) == sum(selection.summary[,2])

end.qtr <- dim(eq.dva.summary)[1]
eq.dva.summary.long <- data.frame(
  colSums(coredata(eq.dva.summary[(end.qtr- 3):end.qtr, -1])),
  colSums(coredata(eq.dva.summary[(end.qtr-11):end.qtr, -1])),
  colSums(coredata(eq.dva.summary[(end.qtr-19):end.qtr, -1]))
) %>%
  set_colnames(c("One Year","Three Year","Five Year")) %>%
  mutate(Effect = rownames(.)) %>%
  gather(Period, DVA, -Effect) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year"))) %>%
  mutate(Effect = factor(Effect, levels = c("Allocation", "Selection", "Other")))

eq.dva.plot <- ggplot(eq.dva.summary.long, aes(x = Period, y = DVA, fill = Effect)) +
  geom_bar(stat = "identity") +
  ggtitle("Public Equity Dollar Value Add", 
          subtitle = "Relative to Composite Benchmark") +
  ylab("in Millions") + xlab("") + scale_y_continuous(labels = scales::dollar) +
  scale_fill_manual(values=IMD.palette()) + 
  guides(fill = guide_legend(title = "Effect:")) +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10), legend.title = element_blank(),
        axis.title.y = element_text(size = 10, face = "italic"), legend.position = 'bottom',
        legend.background = element_rect(fill="gray92", size=4.5, linetype="dotted"))

grid.arrange(eq.return.plot, eq.dva.plot, ncol = 2)

## @knitr eq.allocation
end.qtr <- dim(eq.allocation)[1]

eq.allocation.summary <- tomillions(eq.allocation[,-1] * tf.multiplier)

eq.allocation.long <- data.frame(
  colSums(coredata(eq.allocation.summary[(end.qtr -  3):end.qtr, ])),
  colSums(coredata(eq.allocation.summary[(end.qtr - 11):end.qtr, ])),
  colSums(coredata(eq.allocation.summary[(end.qtr - 19):end.qtr, ]))
) %>%
  set_colnames(c("One Year","Three Year","Five Year")) %>%
  mutate(Portfolio = rownames(.)) %>%
  gather(Period, DVA, -Portfolio) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year"))) %>%
  mutate(Portfolio = factor(Portfolio, levels = unique(.$Portfolio)))

eq.allocation.plot = ggplot(eq.allocation.long, aes(x = Period, y = DVA, fill = Portfolio)) +
  geom_bar(stat='identity', position=position_dodge()) + 
  xlab("") + scale_y_continuous(name = "in Millions", labels = scales::dollar) +
  ggtitle("Public Equity Allocation Effect by Sub Asset Class",
          subtitle = "Relative to Composite Benchmark") +
  scale_fill_manual(values = IMD.palette())+ labs(fill = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title.y = element_text(size = 10, face = "italic"), legend.position = 'none',
        legend.background = element_rect(fill="gray92", size=4.5, linetype="dotted"))

aw.eq.lf$Composite = factor(aw.eq.lf$Composite, levels = unique(aw.eq.lf$Composite))
allpos=subset(aw.eq.lf,`Active Weight`>0)
allneg=subset(aw.eq.lf,`Active Weight`<0)
eq.aw.plot <- ggplot()+
  geom_bar(data=allpos,aes(x=Date,y=`Active Weight`,fill=Composite),stat="identity")+
  geom_bar(data=allneg,aes(x=Date,y=`Active Weight`,fill=Composite),stat="identity")+
  ylab("Active Weight")+
  scale_y_continuous(labels=percent)+
  scale_fill_manual(values = IMD.palette(), name = "Composite") +
  ggtitle("Average Quarterly Active Weight")+
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(eq.allocation.plot, eq.aw.plot, nrow = 1, ncol = 2, common.legend = T, legend = "bottom")

## @knitr eq.selection
eq.selection.summary <- tomillions(eq.selection[,-1] * tf.multiplier)

eq.selection.long <- data.frame(
  colSums(coredata(eq.selection.summary[(end.qtr -  3):end.qtr, ])),
  colSums(coredata(eq.selection.summary[(end.qtr - 11):end.qtr, ])),
  colSums(coredata(eq.selection.summary[(end.qtr - 19):end.qtr, ]))
) %>%
  set_colnames(c("One Year","Three Year","Five Year")) %>%
  mutate(Portfolio = c("US Large", "US Mid", "US Small", "Intl Dev LC", "Intl Dev SC","EM", "Risk Factors","Pub Opp")) %>%
  gather(Period, DVA, -Portfolio) %>%
  mutate(Period = factor(Period, levels = c("One Year","Three Year","Five Year"))) %>%
  mutate(Portfolio = factor(Portfolio, levels = unique(.$Portfolio)))

eq.selection.plot = ggplot(eq.selection.long, aes(x = Period, y = DVA, fill = Portfolio)) +
  geom_bar(stat='identity', position=position_dodge()) + 
  xlab("") + scale_y_continuous(name = "in Millions", labels = scales::dollar) +
  ggtitle("Public Equity Selection Effect",
          subtitle = "Relative to Composite Benchmark") +
  scale_fill_manual(values = IMD.palette())+ labs(fill = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 9),
        axis.title.y = element_text(size = 10, face = "italic"),
        legend.title = element_blank(), legend.position = 'bottom')

print(eq.selection.plot)

## @knitr eq.trailing
peq.trailing <- list()
for(i in names(r.eq.list[-1])) {
  if(i == "Risk Factors") next
  ret <- merge(r.eq.list[[i]], b.eq.list[[i]])
  if(dim(ret)[1] <= 12) next
  return.1 <- t(Return.annualized(ret[((dim(ret)[1]-(12-1)):dim(ret)[1]), ], scale = 12))
  if(dim(ret)[1] >=36) {
    return.3 <- t(Return.annualized(ret[((dim(ret)[1]-(36-1)):dim(ret)[1]), ], scale = 12))}
  else {return.3 <- NA}
  if(dim(ret)[1] >=60){
    return.5 <- t(Return.annualized(ret[((dim(ret)[1]-(60-1)):dim(ret)[1]), ], scale = 12))}
  else {return.5 <- NA}
  if(dim(ret)[1] >= 120)  {
    return.10 <- t(Return.annualized(ret[((dim(ret)[1]-(120-1)):dim(ret)[1]), ], scale = 12))}
  else {return.10 <- NA}
  tr.ret <- cbind(return.1, return.3, return.5, return.10)
  desc <- c("One Year", "Three Year", "Five Year", "Ten Year")
  excess = as.data.frame(t(tr.ret))
  Excess = (excess[, 1] - excess[,2])
  tr.ret = data.frame(rbind(tr.ret, Excess))
  colnames(tr.ret) = desc
  tr.ret$Composite = c(i, "Benchmark", "Excess")
  peq.trailing[[i]] = tr.ret
}
all.peq = do.call(rbind, peq.trailing)
rownames(all.peq) = NULL
all.peq = subset(all.peq, select = c("Composite", desc))
all.peq$`One Year` = paste0(round(all.peq$`One Year`*100,2),"%")
all.peq$`Three Year` = paste0(round(all.peq$`Three Year`*100,2),"%")
all.peq$`Five Year` = paste0(round(all.peq$`Five Year`*100,2),"%")
all.peq$`Ten Year` = paste0(round(all.peq$`Ten Year`*100,2),"%")
hlines = c(-1,seq(0,nrow(all.peq), by=(+3)))
print(xtable(all.peq, align = rep("r",6), digits = 2), hline.after = hlines,
      scalebox=.4, include.rownames = FALSE)

## @knitr pub.eq.msci

t <- read.csv(paste0("P:/IMD/Karl/R projects/Public Performance/Barra/",'EQ', '.barra.total.csv'), skip=1)
t$Date <- as.Date(t$Date,format='%m/%d/%Y')
colnames(t)[6:9] <- c("Sector", "Cash Drag", "Style Factors", "Stock Specific")
b.excess <- zoo(t[,4], t[,1])
timeline <- as.yearmon(time(b.excess))
b <- coredata(b.excess)
b.excess <- zoo(b, timeline)
gd.be <- gd(b.excess)-1
total <- zoo(t[ ,5:11], timeline)
gdminus1.tot <- gdminus1(total)
barra.df=gg(gdminus1.tot,"Total","Contribution")
allpos=subset(barra.df,Contribution>0)
allneg=subset(barra.df,Contribution<0)
plot1 <- ggplot()+
  geom_bar(data=allpos,aes(x=Date,y=Contribution,fill=Total),stat="identity")+
  geom_bar(data=allneg,aes(x=Date,y=Contribution,fill=Total),stat="identity")+
  geom_line(data = gd.be, aes(x=Index, y=gd.be[,2]), colour=IMD.palette()[7], size=.5)+
  ylab("Cumulative Excess Return (Line)")+
  scale_y_continuous(labels=percent)+
  scale_fill_manual(values = IMD.palette(), name = "Attribution") +
  ggtitle("Public Equity Barra Factor Attribution")+
  theme(plot.title = element_text(hjust = 0.5))

print(plot1)

## @knitr e.ports

internal <- c("RAY0104", "RAYE7", "RAYE8", "RAY0203", "RAY0204", "RAY0142", "RAY0305")
#e3 (RAY0203) & e4 (RAY0204) closed in March.  e7 (RAYE7) & e8 (RAYE8) closed in july
e.mv.list <- list()
ret.list <- list()
bm.list <- list()
for(i in internal){
  d = subset(nepc.dat, nepc.dat$ID == i, select = c('Date','NetReturn','MthEndMV'))
  d = d[!duplicated(d$Date), ]
  s.name <- nepc.map[which(nepc.map$ID==i), 'Short.Name']
  open <- nepc.map[which(nepc.map$ID==i), 'Open']
  if(open == "N") {
    defund.date <- nepc.map[which(nepc.map$ID == i), "Defunding"]
    defund.date <- as.Date(defund.date, format = "%m/%d/%Y")
    d = subset(d, d$Date <= defund.date)
  }
  mv = xts(d[ ,'MthEndMV'],d[ ,1])
  e.mv.list[[s.name]] = mv
  d.xts <- xts(d[ ,"NetReturn"], d[ ,1])
  ret.list[[s.name]] = d.xts
  i.d <- as.character(time(d.xts)[1])
  b.code <- nepc.map[which(nepc.map$ID == i),'BM.ID']
  bm=subset(nepc.dat, nepc.dat$ID == b.code, select = c('Date','NetReturn'))
  bench <- subset(bm, bm$Date >= i.d)
  bench = xts(bench[ ,2], bench[ ,1])
  bm.list[[s.name]] <- bench
}
ret <- do.call(merge, ret.list)
colnames(ret) <- names(ret.list)
ret = na.fill(ret, 0)
ret = ret[((dim(ret)[1]-(84-1)):dim(ret)[1]), ]
mv.data <- do.call(merge,e.mv.list)
mv.data = na.fill(mv.data,0)
mv.data = mv.data[((dim(mv.data)[1]-(84-1)):dim(mv.data)[1]), ]
colnames(mv.data) = names(e.mv.list)
total <- rowSums(mv.data)
weights <- mv.data/total
wa.ret <- rowSums(weights * ret)
bm.data <- do.call(merge, bm.list)
bm.data = na.fill(bm.data, 0)
bm.data = bm.data[((dim(bm.data)[1]-(84-1)):dim(bm.data)[1]), ]
wa.bm <- rowSums(weights * bm.data)
excess <- xts((wa.ret - wa.bm), index(bm.data))
colnames(excess) <- "E Portfolio Composite"
total.gd = round((gd(excess[((dim(excess)[1]-35):dim(excess)[1]), ])-1)*10000,2)
wa.e <- ggplot(total.gd, aes(x=Index, y=`E Portfolio Composite`))+
  geom_point(colour=IMD.palette()[1])+ 
  stat_smooth(method="loess", colour=IMD.palette()[2])+
  ggtitle("E Portfolios Composite",  subtitle = "Monthly Cumulative Excess Performance")+
  ylab("Excess Return in bps")+xlab("")+
  scale_x_date(date_labels = "%m/%Y")+ 
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 9))
wa.d = data.frame("Date" = index(bm.data), "Composite"=wa.ret, "Benchmark"=wa.bm)
wa.d = xts(wa.d[,-1], wa.d[,1])
wa.c.roll = apply.rolling(wa.d[,1], width = 12, FUN = "Return.annualized")
wa.b.roll = apply.rolling(wa.d[,2], width = 12, FUN = "Return.annualized")
wa.7.roll = wa.c.roll - wa.b.roll
wa.7.roll$calcs.inbp = wa.7.roll$calcs * 10000
wa.roll <-  ggplot(wa.7.roll, aes(x=Index, y=calcs.inbp))+geom_line(colour=IMD.palette()[1])+
  ggtitle("E Portfolios", subtitle = "Rolling 1 Year Excess Performance")+ylab("Excess Return in bps")+xlab("")+
  scale_x_date(date_labels = "%m/%Y")+ 
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5),
        plot.subtitle = 
          element_text(hjust = 0.5, size = 9))
grid.arrange(wa.e, wa.roll, ncol=2)

## @knitr dom.ret.table
#trailing period return table
eq.ind <- names(r.eq.list)[c(2:4, 9)]
dom.trailing <- list()
for(i in eq.ind) {
  ret <- merge(r.eq.list[[i]], b.eq.list[[i]])
  if(dim(ret)[1] <= 12) next
  return.1 <- t(Return.annualized(ret[((dim(ret)[1]-(12-1)):dim(ret)[1]), ], scale = 12))
  if(dim(ret)[1] >=36) {
    return.3 <- t(Return.annualized(ret[((dim(ret)[1]-(36-1)):dim(ret)[1]), ], scale = 12))}
  else {return.3 <- NA}
  if(dim(ret)[1] >=60){
    return.5 <- t(Return.annualized(ret[((dim(ret)[1]-(60-1)):dim(ret)[1]), ], scale = 12))}
  else {return.5 <- NA}
  if(dim(ret)[1] >= 120)  {
    return.10 <- t(Return.annualized(ret[((dim(ret)[1]-(120-1)):dim(ret)[1]), ], scale = 12))}
  else {return.10 <- NA}
  tr.ret <- cbind(return.1, return.3, return.5, return.10)
  desc <- c("One Year", "Three Year", "Five Year", "Ten Year")
  excess = as.data.frame(t(tr.ret))
  Excess = (excess[, 1] - excess[,2])
  tr.ret = data.frame(rbind(tr.ret, Excess))
  colnames(tr.ret) = desc
  tr.ret$Composite = c(i, "Blended Benchmark", "Excess")
  dom.trailing[[i]] = tr.ret
}
dom.peq = do.call(rbind, dom.trailing)
rownames(dom.peq) = NULL
dom.peq = subset(dom.peq, select = c("Composite", desc))
dom.peq$`One Year` = paste0(round(dom.peq$`One Year`*100,2),"%")
dom.peq$`Three Year` = paste0(round(dom.peq$`Three Year`*100,2),"%")
dom.peq$`Five Year` = paste0(round(dom.peq$`Five Year`*100,2),"%")
dom.peq$`Ten Year` = paste0(round(dom.peq$`Ten Year`*100,2),"%")
hlines = c(-1,seq(0,nrow(dom.peq), by=(+3)))
print(xtable(dom.peq, align = rep("r",6), digits = 2), hline.after = hlines,
      scalebox=.4, include.rownames = FALSE)

## @knitr dom.allocation
dom.mvs <- do.call(merge, mv.eq.list[eq.ind])
dom.mvs = na.fill(dom.mvs, 0)
colnames(dom.mvs) <- names(mv.eq.list[eq.ind])
dom.mvs = dom.mvs[endpoints(dom.mvs, "quarters"), ]

dom.mvs.df <- data.frame("Date" = index(dom.mvs), coredata(dom.mvs))
colnames(dom.mvs.df)[-1] <- c("US LC", "US MC", "US SC", "Opp")
dom.tidy <- gather(dom.mvs.df, Composite, `Market Value`, -Date)
dom.tidy$`Market Value` = tomillions(dom.tidy$`Market Value`)/1000
dom.tidy$Composite = factor(dom.tidy$Composite, levels = 
                              c("US LC", "US MC", "US SC", "Opp"))
#plot of just the market values
mvs.dom <- ggplot()+
  geom_bar(data = dom.tidy,aes(x = Date,y = `Market Value`, fill = Composite), 
           stat="identity")+ ggtitle("Market Values") + 
  scale_fill_manual(values = IMD.palette()) + xlab("") + ylab("in Billions")+
  scale_y_continuous(labels = scales::dollar)+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
        legend.text = element_text(size = 8), 
        legend.key.size = unit(.25, "cm"))
print(mvs.dom)

## @knitr dom.eq.rolling
dom.ret <- do.call(merge, r.eq.list[eq.ind])
r.dom.roll <- merge(
  rollapply(na.trim(dom.ret[ ,1]), width = 12, FUN = "Return.annualized", scale = 12),
  rollapply(na.trim(dom.ret[ ,2]), width = 12, FUN = "Return.annualized", scale = 12),
  rollapply(na.trim(dom.ret[ ,3]), width = 12, FUN = "Return.annualized", scale = 12),
  rollapply(na.trim(dom.ret[ ,4]), width = 12, FUN = "Return.annualized", scale = 12))

plot.dom.roll <- ggplot(
  data = fortify.zoo(r.dom.roll, melt = TRUE), 
  aes(x = Index, y = Value, stat = Series, colour = Series)) +
  geom_line(size = .5) +
  xlab("") + scale_y_continuous(name = "Annualized Return", labels = scales::percent) +
  ggtitle("Domestic Equity Rolling 1 Year Returns") +
  scale_color_manual(values = IMD.palette(), name = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size = 8, face = "bold"),
        axis.title.y = element_text(size = 6, face = "italic"), 
        legend.text = element_text(size = 7), legend.key.size = unit(.25, "cm"),
        axis.text.x = element_text(size = 6, face = "bold"), axis.text.y = element_text(size = 6, face = "bold"))
print(plot.dom.roll)

## @knitr dom.msci.total
t <- read.csv(paste0("P:/IMD/Karl/R projects/Public Performance/Barra/",'DOM', '.barra.total.csv'), skip=1)
t$Date <- as.Date(t$Date,format='%m/%d/%Y')
colnames(t)[6:9] <- c("Sector", "Cash Drag", "Style Factors", "Stock Specific")
b.excess <- zoo(t[,4], t[,1])
timeline <- as.yearmon(time(b.excess))
b <- coredata(b.excess)
b.excess <- zoo(b, timeline)
gd.be <- gd(b.excess)-1
total <- zoo(t[ ,5:11], timeline)
gdminus1.tot <- gdminus1(total)
barra.df=gg(gdminus1.tot,"Total","Contribution")
allpos=subset(barra.df,Contribution>0)
allneg=subset(barra.df,Contribution<0)
plot1 <- ggplot()+
  geom_bar(data=allpos,aes(x=Date,y=Contribution,fill=Total),stat="identity")+
  geom_bar(data=allneg,aes(x=Date,y=Contribution,fill=Total),stat="identity")+
  geom_line(data = gd.be, aes(x=Index, y=gd.be[,2]), colour=IMD.palette()[7], size=.5)+
  ylab("Cumulative Excess Return (Line)")+
  scale_y_continuous(labels=percent)+
  scale_fill_manual(values = IMD.palette(), name = "Attribution") +
  ggtitle("Domestic Equity Barra Factor Attribution")+
  theme(plot.title = element_text(hjust = 0.5))

print(plot1)

## @knitr dom.msci.factors

#Barra Style Factor Breakdown
s=read.csv(
  paste0("P:/IMD/Karl/R projects/Public Performance/Barra/","DOM", '.style.csv'),
  skip=1,
  stringsAsFactors = F
)
s$Date <- as.Date(s$Date, format='%m/%d/%Y')
style <- zoo(s[,-1], timeline)
style.gd <- gd(total[,4])-1
gdminus1.style <- gdminus1(style)
style.df=gg(gdminus1.style,"Factor","Contribution")

s.allpos=subset(style.df,Contribution>0)
s.allneg=subset(style.df,Contribution<0)
plot2 <- ggplot()+
  geom_bar(data=s.allpos,aes(x=Date,y=Contribution,fill=Factor),stat="identity")+
  geom_bar(data=s.allneg,aes(x=Date,y=Contribution,fill=Factor),stat="identity")+
  geom_line(data = style.gd, aes(x=Index, y=style.gd[,2]), colour=IMD.palette()[9], size=.5)+
  ylab("Cumulative Style Factors / Excess Return")+
  scale_y_continuous(labels=percent)+
  scale_fill_manual(values = IMD.palette(), name = "Factor") +
  ggtitle("Domestic Equity Style Factor Breakout")+
  theme(plot.title = element_text(hjust = 0.5))
print(plot2)

## @knitr e11

nepc.id <- "RAYE11"
i.d <- as.Date(nepc.map[nepc.map$ID == nepc.id, "Inception"], format = "%m/%d/%Y")
d=subset(x = nepc.dat, subset = nepc.dat$ShortName == nepc.id,
         select = c('Date','NetReturn')
)
d = d[!duplicated(d$Date), ]
d.xts <- xts(d[,-1], d[,1])
d.xts <- d.xts[paste0(i.d, "/"), ]
b <- subset(x = nepc.dat, subset = nepc.dat$ShortName == 'RAYDOEQ',
            select = c('Date','NetReturn')
)
b <- xts(b[,-1],b[,1])
bm.dat <- b[paste0(i.d,"/"),]
data <- merge(d.xts, bm.dat)
name <- nepc.map[which(nepc.map$ID == nepc.id), 'Short.Name']
data.gd <- gd(data)
data.gd = data.frame("Date" = index(data.gd), coredata(data.gd))
colnames(data.gd)[-1] <- c(name, 'S&P 500')
d.gd.lf <- gather(data.gd, 'Portfolio', 'Return', -Date)

plot1 <- ggplot(d.gd.lf, aes(x=Date, y=Return, colour = Portfolio))+
  geom_line()+ ggtitle("E 11 Performance")+
  scale_color_manual(values = IMD.palette()[1:2])+
  ylab("Cumulative Return")+xlab("")+
  scale_x_date(date_labels = "%m/%Y")+ 
  theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5))

act.ex <- data[ ,1] - data[ ,2]
exp.ex <- nepc.map[which(nepc.map$ID == nepc.id), 'ExpExcess'] #expected excess return
exp.sd <- as.numeric(nepc.map[which(nepc.map$ID == nepc.id), 'ExpTE'])
exp.ir <- exp.ex/exp.sd
theplot <- bw.cone(act.ex, exp.ex, exp.sd)
plot2 <- theplot+ggtitle("Actual Excess vs Expected Excess +/-1 & 2 Std Dev")

grid.arrange(plot1, plot2, ncol=2)

## @knitr intl.ret.table
eq.ind <- names(r.eq.list)[5:7]
intl.trailing <- list()
for(i in eq.ind) {
  ret <- merge(r.eq.list[[i]], b.eq.list[[i]])
  if(dim(ret)[1] <= 12) next
  return.1 <- t(Return.annualized(ret[((dim(ret)[1]-(12-1)):dim(ret)[1]), ], scale = 12))
  if(dim(ret)[1] >=36) {
    return.3 <- t(Return.annualized(ret[((dim(ret)[1]-(36-1)):dim(ret)[1]), ], scale = 12))}
  else {return.3 <- NA}
  if(dim(ret)[1] >=60){
    return.5 <- t(Return.annualized(ret[((dim(ret)[1]-(60-1)):dim(ret)[1]), ], scale = 12))}
  else {return.5 <- NA}
  tr.ret <- cbind(return.1, return.3, return.5)
  desc <- c("One Year", "Three Year", "Five Year")
  excess = as.data.frame(t(tr.ret))
  Excess = (excess[, 1] - excess[,2])
  tr.ret = data.frame(rbind(tr.ret, Excess))
  colnames(tr.ret) = desc
  tr.ret$Composite = c(i, "Blended Benchmark", "Excess")
  intl.trailing[[i]] = tr.ret
}
intl.peq = do.call(rbind, intl.trailing)
rownames(intl.peq) = NULL
intl.peq = subset(intl.peq, select = c("Composite", desc))
intl.peq$`One Year` = paste0(round(intl.peq$`One Year`*100,2),"%")
intl.peq$`Three Year` = paste0(round(intl.peq$`Three Year`*100,2),"%")
intl.peq$`Five Year` = paste0(round(intl.peq$`Five Year`*100,2),"%")
hlines = c(-1,seq(0,nrow(intl.peq), by=(+3)))
print(xtable(intl.peq, align = rep("r",5), digits = 2), hline.after = hlines,
      scalebox=.4, include.rownames = FALSE)

## @knitr intl.allocation
intl.mvs <- do.call(merge, mv.eq.list[eq.ind])
intl.mvs = na.fill(intl.mvs, 0)
colnames(intl.mvs) <- names(mv.eq.list[eq.ind])
intl.mvs = intl.mvs[endpoints(intl.mvs, "quarters"), ]

intl.mvs.df <- data.frame("Date" = index(intl.mvs), coredata(intl.mvs))
colnames(intl.mvs.df)[-1] <- c("Intl LC", "Intl SC", "EM")
intl.tidy <- gather(intl.mvs.df, Composite, `Market Value`, -Date)
intl.tidy$`Market Value` = tomillions(intl.tidy$`Market Value`)/1000
intl.tidy$Composite = factor(intl.tidy$Composite, levels = 
                               c("Intl LC", "Intl SC", "EM"))
#plot of just the market values
mvs.intl <- ggplot()+
  geom_bar(data = intl.tidy,aes(x = Date,y = `Market Value`, fill = Composite), 
           stat="identity")+ ggtitle("Market Values") + 
  scale_fill_manual(values = IMD.palette()) + xlab("") + ylab("in Billions")+
  scale_y_continuous(labels = scales::dollar)+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
        legend.text = element_text(size = 8), 
        legend.key.size = unit(.25, "cm"))
print(mvs.intl)

## @knitr intl.eq.rolling
intl.ret <- do.call(merge, r.eq.list[eq.ind])
r.intl.roll <- merge(
  rollapply(na.trim(intl.ret[ ,1]), width = 12, FUN = "Return.annualized", scale = 12),
  rollapply(na.trim(intl.ret[ ,2]), width = 12, FUN = "Return.annualized", scale = 12),
  rollapply(na.trim(intl.ret[ ,3]), width = 12, FUN = "Return.annualized", scale = 12))

plot.intl.roll <- ggplot(
  data = fortify.zoo(r.intl.roll, melt = TRUE), 
  aes(x = Index, y = Value, stat = Series, colour = Series)) +
  geom_line(size = .5) +
  xlab("") + scale_y_continuous(name = "Annualized Return", labels = scales::percent) +
  ggtitle("Domestic Equity Rolling 1 Year Returns") +
  scale_color_manual(values = IMD.palette(), name = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size = 8, face = "bold"),
        axis.title.y = element_text(size = 6, face = "italic"), 
        legend.text = element_text(size = 7), legend.key.size = unit(.25, "cm"),
        axis.text.x = element_text(size = 6, face = "bold"), axis.text.y = element_text(size = 6, face = "bold"))
print(plot.intl.roll)

## @knitr intl.msci.total

t <- read.csv(paste0("P:/IMD/Karl/R projects/Public Performance/Barra/",'INTL', '.barra.total.csv'), skip=1)
t$Date <- as.Date(t$Date,format='%m/%d/%Y')
colnames(t)[6:9] <- c("Sector", "Cash Drag", "Style Factors", "Stock Specific")
b.excess <- zoo(t[,4], t[,1])
timeline <- as.yearmon(time(b.excess))
b <- coredata(b.excess)
b.excess <- zoo(b, timeline)
gd.be <- gd(b.excess)-1
total <- zoo(t[ ,5:11], timeline)
gdminus1.tot <- gdminus1(total)
barra.df=gg(gdminus1.tot,"Total","Contribution")
allpos=subset(barra.df,Contribution>0)
allneg=subset(barra.df,Contribution<0)
plot1 <- ggplot()+
  geom_bar(data=allpos,aes(x=Date,y=Contribution,fill=Total),stat="identity")+
  geom_bar(data=allneg,aes(x=Date,y=Contribution,fill=Total),stat="identity")+
  geom_line(data = gd.be, aes(x=Index, y=gd.be[,2]), colour=IMD.palette()[7], size=.5)+
  ylab("Cumulative Excess Return (Line)")+
  scale_y_continuous(labels=percent)+
  scale_fill_manual(values = IMD.palette(), name = "Attribution") +
  ggtitle("International Equity Barra Factor Attribution")+
  theme(plot.title = element_text(hjust = 0.5))

print(plot1)

## @knitr intl.msci.factors

#Barra Style Factor Breakdown
s=read.csv(
  paste0("P:/IMD/Karl/R projects/Public Performance/Barra/","INTL", '.style.csv'),
  skip=1,
  stringsAsFactors = F
)
s$Date <- as.Date(s$Date, format='%m/%d/%Y')
style <- zoo(s[,-1], timeline)
style.gd <- gd(total[,4])-1
gdminus1.style <- gdminus1(style)
style.df=gg(gdminus1.style,"Factor","Contribution")

s.allpos=subset(style.df,Contribution>0)
s.allneg=subset(style.df,Contribution<0)
plot2 <- ggplot()+
  geom_bar(data=s.allpos,aes(x=Date,y=Contribution,fill=Factor),stat="identity")+
  geom_bar(data=s.allneg,aes(x=Date,y=Contribution,fill=Factor),stat="identity")+
  geom_line(data = style.gd, aes(x=Index, y=style.gd[,2]), colour=IMD.palette()[9], size=.5)+
  ylab("Cumulative Style Factors / Excess Return")+
  scale_y_continuous(labels=percent)+
  scale_fill_manual(values = IMD.palette(), name = "Factor") +
  ggtitle("International Equity Style Factor Breakout")+
  theme(plot.title = element_text(hjust = 0.5))

print(plot2)

## @knitr pe.irr.table
port <- "PE"
dates <- valdate + days(1) -months(3) - days(1)
dates = c(dates, valdate-years(c(1,3,5))) 
pe.tot.v <- y.v[["Total PE"]]
pe.tot.hv <- y.hv[["Total PE"]]
combineval <- mergesum.z(pe.tot.v, pe.tot.hv)
combinecf <- y.cf[["Total PE"]]
irr.bench=vector()
irr.priv=vector()
#calculate quarterly irr
date.range <- index(combineval)
t = which(date.range == valdate)
b.mv <- combineval[(t-1)]
e.mv <- combineval[t]
cf.qtr <- y.cf[["Total PE"]] %>% .[index(.) <= date.range[t]] %>% 
   .[index(.) > date.range[t-1]]
cf.combine <- mergesum.z(-b.mv, cf.qtr, e.mv)
pe.qtr <- pestats(cf.combine, bench.lst$`^RUT`[time(cf.combine)])
irr.bench[1] = pe.qtr$ind.irr
irr.priv[1] = pe.qtr$irr
for(i in 2:length(dates)) {
  cf.i=combinecf[time(combinecf)<=valdate&time(combinecf)>dates[i]]
  dat <- mergesum.z(-combineval[dates[i]], cf.i, combineval[valdate])
  irr.i <- irr.z(dat, gips = TRUE)
  irr.priv[i] <- irr.i
  fvfactor <- as.numeric(lastinvec(bench.lst$`^RUT`))/bench.lst$`^RUT`
  cf.fv <- dat * fvfactor
  alpha <- log(1+irr.z(cf.fv, gips = TRUE))
  logpe.irr = log(1 + irr.i)
  logdm.irr = logpe.irr - alpha
  irr.bench[i] = -1+exp(logdm.irr)
}
i=5
itd.dat <- mergesum.z(combinecf[time(combinecf)<=valdate], combineval[valdate])
itd.tot <- pestats(itd.dat, bench.lst$`^RUT`[time(itd.dat)])
irr.priv[i] = itd.tot$irr
irr.bench[i] = itd.tot$ind.irr
totpereturn = rbind("Private Equity" = irr.priv,"Russell 2000"=irr.bench,
                    "Excess" = (irr.priv - irr.bench))
colnames(totpereturn) = c("Qtr","1 Year","3 Year","5 Year","ITD")

#calculate trailing irr's for strategies
strat <- unique(fundinfo[which(fundinfo$Portfolio == port), "catshort"])
pe.strat.ret <- data.frame()
for(i in strat) {
  curr.val <- y.v[[i]]
  hist.vals <- y.hv[[i]]
  combineval <- mergesum.z(curr.val, hist.vals)
  combinecf <- y.cf[[i]]
  irr.cat=vector()
  #calculate quarterly irr
  date.range <- index(combineval)
  t = which(date.range == valdate)
  b.mv <- combineval[(t-1)]
  e.mv <- combineval[t]
  cf.qtr <- y.cf[[i]] %>% .[index(.) <= date.range[t]] %>% 
    .[index(.) > date.range[t-1]]
  cf.combine <- mergesum.z(-b.mv, cf.qtr, e.mv)
  irr.cat[1] <- irr.z(cf.combine, gips = TRUE)
  #calculate 1, 3, 5 year IRR
  for(i in 2:length(dates)) {
    cf.i=combinecf[time(combinecf)<=valdate&time(combinecf)>dates[i]]
    dat <- mergesum.z(-combineval[dates[i]], cf.i, combineval[valdate])
    irr.cat[i] <- irr.z(dat, gips = TRUE)
  }
  i=5
  itd.dat <- mergesum.z(combinecf[time(combinecf)<=valdate], combineval[valdate])
  irr.cat[i] <- irr.z(itd.dat, gips = TRUE)
  pe.strat.ret <- rbind(pe.strat.ret, irr.cat)
  colnames(pe.strat.ret) = c("Qtr","1 Year","3 Year","5 Year","ITD")
}
rownames(pe.strat.ret) <- strat
pe.p2p.table <- rbind(totpereturn, pe.strat.ret)
pe.p2p.table = data.frame("Category" = rownames(pe.p2p.table),
                          pe.p2p.table, row.names = NULL)
colnames(pe.p2p.table)[-1] <- colnames(totpereturn)
pe.p2p.table$Qtr = paste0(round(pe.p2p.table$Qtr*100,2),"%")
pe.p2p.table$`1 Year` = paste0(round(pe.p2p.table$`1 Year`*100,1),"%")
pe.p2p.table$`3 Year` = paste0(round(pe.p2p.table$`3 Year`*100,1),"%")
pe.p2p.table$`5 Year` = paste0(round(pe.p2p.table$`5 Year`*100,1),"%")
pe.p2p.table$ITD = paste0(round(pe.p2p.table$ITD*100,1),"%")
hlines = c(-1, 0, 2, dim(pe.p2p.table)[1])
print(
  xtable(pe.p2p.table, digits = 2), 
  scalebox = .5,  align = rep("r",4),
  hline.after = hlines, include.rownames = FALSE)

## @knitr pe.nav.style
name.strat <- unique(fundinfo[which(fundinfo$Portfolio == port), "catlong"])
strat.val.list <- list()
vint.strat <- list()
for(s in strat) {
  ind <-  which(fundinfo$catshort == s)
  val.i = list()
  vint.i = vector()
  for(j in ind) {
    name <- as.character(fundinfo[j, "Short"])
    cv <- y.v[[name]]
    hv <- y.hv[[name]]
    navs <- zoosum(cv, hv)
    val.i[[name]] = navs
    vintage <- fundinfo[j, "Vintage"]
    vint.i[[name]] = vintage
  }
  val.strat <- do.call(merge, val.i)
  strat.val.list[[s]] <- zoo(rowSums(val.strat, na.rm = TRUE), index(val.strat))
  vint.strat[[s]] <- vint.i
}
val.strat <- do.call(merge, strat.val.list)
val.strat = subset(val.strat, index(val.strat) <= valdate)

#graph of NAV's
val.strat.df = data.frame("Date"=time(val.strat), coredata(val.strat))
colnames(val.strat.df)[-1] <- strat
strat.tidy <- gather(val.strat.df, Strategy, NAV, -Date)
strat.tidy$NAV = tomillions(strat.tidy$NAV)
strat.tidy$Strategy = factor(strat.tidy$Strategy, levels = 
                               strat)
#plot of just the market values
nav.strat <- ggplot()+
  geom_bar(data = strat.tidy,aes(x = Date,y = NAV, fill = Strategy), stat="identity")+
  ggtitle("NAV by Strategy") + 
  scale_fill_manual(values = IMD.palette()) + xlab("") + ylab("in Millions")+
  scale_y_continuous(labels = scales::dollar)+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
        legend.position="bottom", legend.box.spacing = unit(0, "cm"))

print(nav.strat)

## @knitr pe.pme.total

pe.cats <- c( "Total PE", "Total PE Legacy Portfolio", "Total PE Current Portfolio")
pme.cats <- pme.df[pe.cats, c("Fund TVPI", "Fund IRR", "Russell 2K PME")]
PME.cut = cut(x = pme.cats$`Russell 2K PME`, breaks = c(-Inf, .8, .95, 1.05, 1.2, +Inf),
              labels = c("<0.80", "0.80 to 0.95", "0.95 to 1.05", "1.05 to 1.20", ">1.20"))
pe.pme.data <- na.omit(data.frame("Category" = rownames(pme.cats), "IRR"=pme.cats$`Fund IRR`,
                                  "TVPI" = pme.cats$`Fund TVPI`, "PME.cut" = PME.cut))
vint.plot = ggplot(pe.pme.data,aes(x=TVPI,y=IRR, color = PME.cut)) + 
  geom_text_repel(aes(label = Category), show.legend = FALSE) + labs(color = "PME:")+
  geom_point(size = 2)+ scale_color_manual(drop = FALSE, values = PME.palette())+
  xlab("TVPI")+ scale_y_continuous(name="IRR",labels=percent) +
  ggtitle("Comparison of Total, Current, and Legacy Portfolios") + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(colour = guide_legend(reverse = T))
print(vint.plot)

## @knitr pe.pme.vint

vindx = which(pme.df$vintsum & pme.df$isvint & pme.df$Portfolio == port)
vint.short <- pme.df$name[vindx]

IRR <- round(pme.df$`Fund IRR`[vindx], 3)
PME <- pme.df$`Russell 2K PME`[vindx]
TVPI <- round(pme.df$`Fund TVPI`[vindx], 2)
PME.cut <- cut(
  x = PME, 
  breaks = c(-Inf, .8, .95, 1.05, 1.2, +Inf),
  labels = c("< 0.80", "0.80 to 0.95", "0.95 to 1.05", "1.05 to 1.20", "> 1.20")
)
pme.data <- na.omit(data.frame(Category = vint.short, TVPI, IRR, PME, PME.cut))
vint.plot = ggplot(pme.data,aes(x=TVPI,y=IRR, color = PME.cut))+ 
  geom_text_repel(aes(label = Category), show.legend = FALSE) + labs(color = "PME:")+
  geom_point(size = 2)+ scale_color_manual(drop = FALSE, values = PME.palette())+
  xlab("TVPI")+ scale_y_continuous(name="IRR",labels=percent)+
  ggtitle("Comparison of Vintage Year") + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(colour = guide_legend(reverse = T))
print(vint.plot)

## @knitr pe.pme.cat

vindx = which(pme.df$catsum & pme.df$iscat & pme.df$Portfolio == port)
strat.short <- pme.df$name[vindx]

IRR <- round(pme.df$`Fund IRR`[vindx], 3)
PME <- pme.df$`Russell 2K PME`[vindx]
TVPI <- round(pme.df$`Fund TVPI`[vindx], 2)
PME.cut <- cut(
  x = PME, 
  breaks = c(-Inf, .8, .95, 1.05, 1.2, +Inf),
  labels = c("< 0.80", "0.80 to 0.95", "0.95 to 1.05", "1.05 to 1.20", "> 1.20")
)
pme.data <- na.omit(data.frame(Category = strat.short, TVPI, IRR, PME, PME.cut))
strat.plot = ggplot(pme.data,aes(x=TVPI,y=IRR, color = PME.cut))+ 
  geom_text_repel(aes(label = Category), show.legend = FALSE) + labs(color = "PME:")+
  geom_point(size = 2)+ scale_color_manual(drop = FALSE, values = PME.palette())+
  xlab("TVPI")+ scale_y_continuous(name="IRR",labels=percent)+
  ggtitle("Comparison of Strategies") + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(colour = guide_legend(reverse = T))
print(strat.plot)

## @knitr popp.irr.table
port <- "POPP"
opp.tot.v <- y.v[["Total POPP"]]
opp.tot.hv <- y.hv[["Total POPP"]]
combineval <- mergesum.z(opp.tot.v, opp.tot.hv)
combinecf <- y.cf[["Total POPP"]]
irr.bench=vector()
irr.priv=vector()
#calculate quarterly irr
date.range <- index(combineval)
t = which(date.range == valdate)
b.mv <- combineval[(t-1)]
e.mv <- combineval[t]
cf.qtr <- y.cf[["Total POPP"]] %>% .[index(.) <= date.range[t]] %>% 
  .[index(.) > date.range[t-1]]
cf.combine <- mergesum.z(-b.mv, cf.qtr, e.mv)
opp.qtr <- pestats(cf.combine, bench.lst$Fixed8[time(cf.combine)])
irr.bench[1] = opp.qtr$ind.irr
irr.priv[1] = opp.qtr$irr
for(i in 2:length(dates)) {
  cf.i=combinecf[time(combinecf)<=valdate&time(combinecf)>dates[i]]
  dat <- mergesum.z(-combineval[dates[i]], cf.i, combineval[valdate])
  irr.i <- irr.z(dat, gips = TRUE)
  irr.priv[i] <- irr.i
  fvfactor <- as.numeric(lastinvec(bench.lst$Fixed8))/bench.lst$Fixed8
  cf.fv <- dat * fvfactor
  alpha <- log(1+irr.z(cf.fv, gips = TRUE))
  logpe.irr = log(1 + irr.i)
  logdm.irr = logpe.irr - alpha
  irr.bench[i] = -1+exp(logdm.irr)
}
i=5
itd.dat <- mergesum.z(combinecf[time(combinecf)<=valdate], combineval[valdate])
itd.tot <- pestats(itd.dat, bench.lst$Fixed8[time(itd.dat)])
irr.priv[i] = itd.tot$irr
irr.bench[i] = itd.tot$ind.irr
totoppreturn = rbind("Private Opportunistic Equity" = irr.priv,"Absolute 8%"=irr.bench,
                    "Excess" = (irr.priv - irr.bench))
colnames(totoppreturn) = c("Qtr","1 Year","3 Year","5 Year","ITD")

#calculate trailing irr's for strategies
funds <- fundinfo[which(fundinfo$Portfolio == port), "Short"]
opp.funds.ret <- data.frame()
for(i in funds) {
  curr.val <- y.v[[i]]
  hist.vals <- y.hv[[i]]
  combineval <- mergesum.z(curr.val, hist.vals)
  combinecf <- y.cf[[i]]
  irr.cat=vector()
  #calculate quarterly irr
  date.range <- index(combineval)
  t = which(date.range == valdate)
  b.mv <- combineval[(t-1)]
  e.mv <- combineval[t]
  cf.qtr <- y.cf[[i]] %>% .[index(.) <= date.range[t]] %>% 
    .[index(.) > date.range[t-1]]
  cf.combine <- mergesum.z(-b.mv, cf.qtr, e.mv)
  irr.cat[1] <- irr.z(cf.combine, gips = TRUE)
  #calculate 1, 3, 5 year IRR
  for(i in 2:length(dates)) {
    cf.i=combinecf[time(combinecf)<=valdate&time(combinecf)>dates[i]]
    dat <- mergesum.z(-combineval[dates[i]], cf.i, combineval[valdate])
    irr.cat[i] <- irr.z(dat, gips = TRUE)
  }
  i=5
  itd.dat <- mergesum.z(combinecf[time(combinecf)<=valdate], combineval[valdate])
  irr.cat[i] <- irr.z(itd.dat, gips = TRUE)
  opp.funds.ret <- rbind(opp.funds.ret, irr.cat)
  colnames(opp.funds.ret) = c("Qtr","1 Year","3 Year","5 Year","ITD")
}
rownames(opp.funds.ret) <- funds
opp.p2p.table <- rbind(totoppreturn, opp.funds.ret)
opp.p2p.table = data.frame("Category" = rownames(opp.p2p.table),
                          opp.p2p.table, row.names = NULL)
colnames(opp.p2p.table)[-1] <- colnames(totoppreturn)
opp.p2p.table$Qtr = paste0(round(opp.p2p.table$Qtr*100,1),"%")
opp.p2p.table$`1 Year` = paste0(round(opp.p2p.table$`1 Year`*100,1),"%")
opp.p2p.table$`3 Year` = paste0(round(opp.p2p.table$`3 Year`*100,1),"%")
opp.p2p.table$`5 Year` = paste0(round(opp.p2p.table$`5 Year`*100,1),"%")
opp.p2p.table$ITD = paste0(round(opp.p2p.table$ITD*100,1),"%")
hlines = c(-1, 0, 2, dim(opp.p2p.table)[1])
print(
  xtable(opp.p2p.table, digits = 2), 
  scalebox = .4,  align = rep("r",4),
  hline.after = hlines, include.rownames = FALSE)

## @knitr popp.nav
opp.val.list <- list()
for(s in funds) {
  cv <- y.v[[s]]
  hv <- y.hv[[s]]
  navs <- zoosum(cv, hv)
  opp.val.list[[s]] <- navs
}
val.opp <- do.call(merge, opp.val.list)
val.opp = subset(val.opp, index(val.opp) <= valdate)

#graph of NAV's
val.opp.df = data.frame("Date"=time(val.opp), coredata(val.opp))
opp.tidy <- gather(val.opp.df, Funds, NAV, -Date)
opp.tidy$NAV = tomillions(opp.tidy$NAV)

nav.opp <- ggplot()+
  geom_bar(data = opp.tidy,aes(x = Date,y = NAV, fill = Funds), stat="identity")+
  ggtitle("Private Opportunistic Equity Funds") + 
  xlab("") + ylab("in Millions")+
  scale_y_continuous(labels = scales::dollar)+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
        legend.position="bottom", legend.box.spacing = unit(0, "cm"),
        legend.text = element_text(size = 8), legend.key.size = unit(.25, "cm"))

print(nav.opp)


## @knitr popp.pme
vindx = which(pme.df$catsum & pme.df$iscat & pme.df$Portfolio == "POPP")
vindx = c(vindx, which(pme.df$Portfolio =="POPP" & pme.df$isfund ==T))
short.n <- pme.df$name[vindx]

IRR <- round(pme.df$`Fund IRR`[vindx], 3)
PME <- pme.df$`Russell 2K PME`[vindx]
TVPI <- round(pme.df$`Fund TVPI`[vindx], 2)
PME.cut <- cut(
  x = PME, 
  breaks = c(-Inf, .8, .95, 1.05, 1.2, +Inf),
  labels = c("< 0.80", "0.80 to 0.95", "0.95 to 1.05", "1.05 to 1.20", "> 1.20")
)
pme.data <- na.omit(data.frame(Category = short.n, TVPI, IRR, PME, PME.cut))
opp.pme = ggplot(pme.data,aes(x=TVPI,y=IRR, color = PME.cut))+ 
  geom_text_repel(aes(label = Category), show.legend = FALSE) + labs(color = "PME:")+
  geom_point(size = 2)+ scale_color_manual(drop = FALSE, values = PME.palette())+
  xlab("TVPI")+ scale_y_continuous(name="IRR",labels=percent)+
  ggtitle("Comparison of Oppotunistic Investments") + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(colour = guide_legend(reverse = T))
print(opp.pme)

## @knitr re.irr.table
port <- "RE"
re.tot.v <- y.v[["Total RE"]]
re.tot.hv <- y.hv[["Total RE"]]
combineval <- mergesum.z(re.tot.v, re.tot.hv)
combinecf <- y.cf[["Total RE"]]
irr.bench=vector()
irr.priv=vector()
#calculate quarterly irr
date.range <- index(combineval)
t = which(date.range == valdate)
b.mv <- combineval[(t-1)]
e.mv <- combineval[t]
cf.qtr <- y.cf[["Total RE"]] %>% .[index(.) <= date.range[t]] %>% 
  .[index(.) > date.range[t-1]]
cf.combine <- mergesum.z(-b.mv, cf.qtr, e.mv)
re.qtr <- pestats(cf.combine, bench.lst$ODCE[time(cf.combine)])
irr.bench[1] = re.qtr$ind.irr
irr.priv[1] = re.qtr$irr
for(i in 2:length(dates)) {
  cf.i=combinecf[time(combinecf)<=valdate&time(combinecf)>dates[i]]
  dat <- mergesum.z(-combineval[dates[i]], cf.i, combineval[valdate])
  irr.i <- irr.z(dat, gips = TRUE)
  irr.priv[i] <- irr.i
  fvfactor <- as.numeric(lastinvec(bench.lst$ODCE))/bench.lst$ODCE
  cf.fv <- dat * fvfactor
  alpha <- log(1+irr.z(cf.fv, gips = TRUE))
  logpe.irr = log(1 + irr.i)
  logdm.irr = logpe.irr - alpha
  irr.bench[i] = -1+exp(logdm.irr)
}
i=5
itd.dat <- mergesum.z(combinecf[time(combinecf)<=valdate], combineval[valdate])
itd.tot <- pestats(itd.dat, bench.lst$ODCE[time(itd.dat)])
irr.priv[i] = itd.tot$irr
irr.bench[i] = itd.tot$ind.irr
totrereturn = rbind("Real Estate" = irr.priv,"ODCE Net"=irr.bench,
                    "Excess" = (irr.priv - irr.bench))
colnames(totrereturn) = c("Qtr","1 Year","3 Year","5 Year","ITD")

#calculate trailing irr's for strategies
strat <- unique(fundinfo[which(fundinfo$Portfolio == port), "catshort"])
re.strat.ret <- data.frame()
for(i in strat) {
  curr.val <- y.v[[i]]
  hist.vals <- y.hv[[i]]
  combineval <- mergesum.z(curr.val, hist.vals)
  combinecf <- y.cf[[i]]
  irr.cat=vector()
  #calculate quarterly irr
  date.range <- index(combineval)
  t = which(date.range == valdate)
  b.mv <- combineval[(t-1)]
  e.mv <- combineval[t]
  cf.qtr <- y.cf[[i]] %>% .[index(.) <= date.range[t]] %>% 
    .[index(.) > date.range[t-1]]
  cf.combine <- mergesum.z(-b.mv, cf.qtr, e.mv)
  irr.cat[1] <- irr.z(cf.combine, gips = TRUE)
  #calculate 1, 3, 5 year IRR
  for(i in 2:length(dates)) {
    cf.i=combinecf[time(combinecf)<=valdate&time(combinecf)>dates[i]]
    dat <- mergesum.z(-combineval[dates[i]], cf.i, combineval[valdate])
    irr.cat[i] <- irr.z(dat, gips = TRUE)
  }
  i=5
  itd.dat <- mergesum.z(combinecf[time(combinecf)<=valdate], combineval[valdate])
  irr.cat[i] <- irr.z(itd.dat, gips = TRUE)
  re.strat.ret <- rbind(re.strat.ret, irr.cat)
  colnames(re.strat.ret) = c("Qtr","1 Year","3 Year","5 Year","ITD")
}
rownames(re.strat.ret) <- strat
re.p2p.table <- rbind(totrereturn, re.strat.ret)
re.p2p.table = data.frame("Category" = rownames(re.p2p.table),
                          re.p2p.table, row.names = NULL)
colnames(re.p2p.table)[-1] <- colnames(totrereturn)
re.p2p.table$Qtr = paste0(round(re.p2p.table$Qtr*100,1),"%")
re.p2p.table$`1 Year` = paste0(round(re.p2p.table$`1 Year`*100,1),"%")
re.p2p.table$`3 Year` = paste0(round(re.p2p.table$`3 Year`*100,1),"%")
re.p2p.table$`5 Year` = paste0(round(re.p2p.table$`5 Year`*100,1),"%")
re.p2p.table$ITD = paste0(round(re.p2p.table$ITD*100,1),"%")
hlines = c(-1, 0, 2, dim(re.p2p.table)[1])
print(
  xtable(re.p2p.table, digits = 2), 
  scalebox = .5,  align = rep("r",4),
  hline.after = hlines, include.rownames = FALSE)

## @knitr re.nav
#create historical strategy & vintage charts
name.strat <- unique(fundinfo[which(fundinfo$Portfolio == port), "catlong"])
strat.val.list <- list()
vint.strat <- list()
for(s in strat) {
  ind <-  which(fundinfo$catshort == s)
  val.i = list()
  vint.i = vector()
  for(j in ind) {
    name <- as.character(fundinfo[j, "Short"])
    cv <- y.v[[name]]
    hv <- y.hv[[name]]
    navs <- zoosum(cv, hv)
    val.i[[name]] = navs
    vintage <- fundinfo[j, "Vintage"]
    vint.i[[name]] = vintage
  }
  val.strat <- do.call(merge, val.i)
  strat.val.list[[s]] <- zoo(rowSums(val.strat, na.rm = TRUE), index(val.strat))
  vint.strat[[s]] <- vint.i
}
val.strat <- do.call(merge, strat.val.list)
val.strat = subset(val.strat, index(val.strat) <= valdate)

#graph of NAV's
val.strat.df = data.frame("Date"=time(val.strat), coredata(val.strat))
colnames(val.strat.df)[-1] <- strat
strat.tidy <- gather(val.strat.df, Strategy, NAV, -Date)
strat.tidy$NAV = tomillions(strat.tidy$NAV)
strat.tidy$Strategy[strat.tidy$Strategy %in% "Strat"]<-"SMA"
strat.tidy$Strategy = factor(strat.tidy$Strategy, levels = 
                               c("SMA", "EnRP", "HgRP","AZ", "CorePb",
                                 "CorePr"))
#plot of just the market values
nav.strat <- ggplot()+
  geom_bar(data = strat.tidy,aes(x = Date,y = NAV, fill = Strategy), stat="identity")+
  ggtitle("NAV by Strategy") + 
  scale_fill_manual(values = IMD.palette()) + xlab("") + ylab("in Millions")+
  scale_y_continuous(labels = scales::dollar)+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
        legend.position="bottom", legend.box.spacing = unit(0, "cm"),
        legend.text = element_text(size = 8), 
        legend.key.size = unit(.25, "cm"))
print(nav.strat)

## @knitr re.total
#scatterplots slice & dice
#by current/total/legacy/rclco
re.cats <- c( "Total RE", "Total RE Legacy Portfolio","RCLCO Underwriting", 
             "Total RE Current Portfolio")
pme.cats <- pme.df[re.cats, c("Fund TVPI", "Fund IRR", "ODCE PME")]
PME.cut = cut(x = pme.cats$`ODCE PME`, breaks = c(-Inf, .8, .95, 1.05, 1.2, +Inf),
              labels = c("<0.80", "0.80 to 0.95", "0.95 to 1.05", "1.05 to 1.20", ">1.20"))
re.pme.data <- na.omit(data.frame("Category" = rownames(pme.cats), "IRR"=pme.cats$`Fund IRR`,
                                  "TVPI" = pme.cats$`Fund TVPI`, "PME.cut" = PME.cut))
vint.plot = ggplot(re.pme.data,aes(x=TVPI,y=IRR, color = PME.cut)) + 
  geom_text_repel(aes(label = Category), show.legend = FALSE) + labs(color = "PME:")+
  geom_point(size = 2)+ scale_color_manual(drop = FALSE, values = PME.palette())+
  xlab("TVPI")+ scale_y_continuous(name="IRR",labels=percent) +
  ggtitle("Comparison of Current, Legacy, and Consultant") + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(colour = guide_legend(reverse = T))
print(vint.plot)

## @knitr re.strat
#by RE strategy
vindx = which(pme.df$catsum & pme.df$iscat & pme.df$Portfolio == port)
strat.short <- pme.df$name[vindx]

IRR <- round(pme.df$`Fund IRR`[vindx], 3)
PME <- pme.df$`ODCE PME`[vindx]
TVPI <- round(pme.df$`Fund TVPI`[vindx], 2)
PME.cut <- cut(
  x = PME, 
  breaks = c(-Inf, .8, .95, 1.05, 1.2, +Inf),
  labels = c("< 0.80", "0.80 to 0.95", "0.95 to 1.05", "1.05 to 1.20", "> 1.20")
)

pme.data <- na.omit(data.frame(Category = strat.short, TVPI, IRR, PME, PME.cut))
levels(pme.data$Category)[levels(pme.data$Category)=="Strat"]="SMA"

strat.plot = ggplot(pme.data,aes(x=TVPI,y=IRR, color = PME.cut))+ 
  geom_text_repel(aes(label = Category), show.legend = FALSE) + labs(color = "PME:")+
  geom_point(size = 2)+ scale_color_manual(drop = FALSE, values = PME.palette())+
  xlab("TVPI")+ scale_y_continuous(name="IRR",labels=percent)+
  ggtitle(paste(port, "Comparison of Strategies")) + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(colour = guide_legend(reverse = T))
print(strat.plot)

## @knitr re.sma
#by Strategic Manager 
vindx <- which(pme.df$cat == "Strat" & pme.df$isfund & pme.df$Portfolio == port)
strat.short <- pme.df$name[vindx]

IRR <- round(pme.df$`Fund IRR`[vindx], 3)
PME <- pme.df$`ODCE PME`[vindx]
TVPI <- round(pme.df$`Fund TVPI`[vindx], 2)
PME.cut <- cut(
  x = PME, 
  breaks = c(-Inf, .8, .95, 1.05, 1.2, +Inf),
  labels = c("< 0.80", "0.80 to 0.95", "0.95 to 1.05", "1.05 to 1.20", "> 1.20")
)

pme.data <- na.omit(data.frame(Category = strat.short, TVPI, IRR, PME, PME.cut))

sma.plot <- ggplot(pme.data,aes(x=TVPI,y=IRR, color = PME.cut))+ 
  geom_text_repel(aes(label = Category), show.legend = FALSE) + labs(color = "PME:")+
  geom_point(size = 2)+ scale_color_manual(drop = FALSE, values = PME.palette())+
  xlab("TVPI")+ scale_y_continuous(name="IRR",labels=percent)+
  ggtitle("Comparison of Separate Accounts") + theme(plot.title = element_text(hjust = 0.5)) +
  guides(colour = guide_legend(reverse = T))
print(sma.plot)

## @knitr re.rcl
#by RCLCO Underwriting
##take out RCLCO total
vindx <- which(pme.df$consultant == "RCLCO")
exclude = which(pme.df$`Fund IRR`[vindx] < -.20 | pme.df$`Fund TVPI`[vindx] == 0)
 #                 is.na(pme.df$`ODCE PME`[vindx]))
vindx = vindx[-exclude]
strat.short <- pme.df$name[vindx]

IRR <- round(pme.df$`Fund IRR`[vindx], 3)
PME <- pme.df$`ODCE PME`[vindx]
TVPI <- round(pme.df$`Fund TVPI`[vindx], 2)
PME.cut <- cut(
  x = PME, 
  breaks = c(-Inf, .8, .95, 1.05, 1.2, +Inf),
  labels = c("< 0.80", "0.80 to 0.95", "0.95 to 1.05", "1.05 to 1.20", "> 1.20")
)

pme.data <- na.omit(data.frame(Category = strat.short, TVPI, IRR, PME, PME.cut))

rcl.plot <- ggplot(pme.data,aes(x=TVPI,y=IRR, color = PME.cut))+ 
  geom_text_repel(aes(label = Category), show.legend = FALSE) + labs(color = "PME:")+
  geom_point(size = 2)+ scale_color_manual(drop = FALSE, values = PME.palette())+
  xlab("TVPI")+ scale_y_continuous(name="IRR",labels=percent)+
  ggtitle("RCLCO Underwriting") + theme(plot.title = element_text(hjust = 0.5)) +
  guides(colour = guide_legend(reverse = T))
print(rcl.plot)

## @knitr farm.irr.table
port <- "FARM"
ra.tot.v <- y.v[["Total FARM"]]
ra.tot.hv <- y.hv[["Total FARM"]]
combineval <- mergesum.z(ra.tot.v, ra.tot.hv)
combinecf <- y.cf[["Total FARM"]]
irr.bench=vector()
irr.priv=vector()
#calculate quarterly irr
date.range <- index(combineval)
t = which(date.range == valdate)
b.mv <- combineval[(t-1)]
e.mv <- combineval[t]
cf.qtr <- y.cf[["Total FARM"]] %>% .[index(.) <= date.range[t]] %>% 
  .[index(.) > date.range[t-1]]
cf.combine <- mergesum.z(-b.mv, cf.qtr, e.mv)
ra.qtr <- pestats(cf.combine, bench.lst$CPIxFE.350[time(cf.combine)])
irr.bench[1] = ra.qtr$ind.irr
irr.priv[1] = ra.qtr$irr
for(i in 2:length(dates)) {
  cf.i=combinecf[time(combinecf)<=valdate&time(combinecf)>dates[i]]
  dat <- mergesum.z(-combineval[dates[i]], cf.i, combineval[valdate])
  irr.i <- irr.z(dat, gips = TRUE)
  irr.priv[i] <- irr.i
  fvfactor <- as.numeric(lastinvec(bench.lst$CPIxFE.350))/bench.lst$CPIxFE.350
  cf.fv <- dat * fvfactor
  alpha <- log(1+irr.z(cf.fv, gips = TRUE))
  logpe.irr = log(1 + irr.i)
  logdm.irr = logpe.irr - alpha
  irr.bench[i] = -1+exp(logdm.irr)
}
i=5
itd.dat <- mergesum.z(combinecf[time(combinecf)<=valdate], combineval[valdate])
itd.tot <- pestats(itd.dat, bench.lst$CPIxFE.350[time(itd.dat)])
irr.priv[i] = itd.tot$irr
irr.bench[i] = itd.tot$ind.irr
totrareturn = rbind("Farmland" = irr.priv,"Core CPI + 350 Bps"=irr.bench,
                    "Excess" = (irr.priv - irr.bench))
colnames(totrareturn) = c("Qtr","1 Year","3 Year","5 Year","ITD")

#calculate trailing irr's for strategies
strat <- unique(fundinfo[which(fundinfo$Portfolio == port), "Short"])
ra.strat.ret <- data.frame()
for(i in strat) {
  curr.val <- y.v[[i]]
  hist.vals <- y.hv[[i]]
  combineval <- mergesum.z(curr.val, hist.vals)
  combinecf <- y.cf[[i]]
  irr.cat=vector()
  #calculate quarterly irr
  date.range <- index(combineval)
  t = which(date.range == valdate)
  b.mv <- combineval[(t-1)]
  e.mv <- combineval[t]
  cf.qtr <- y.cf[[i]] %>% .[index(.) <= date.range[t]] %>% 
    .[index(.) > date.range[t-1]]
  cf.combine <- mergesum.z(-b.mv, cf.qtr, e.mv)
  irr.cat[1] <- irr.z(cf.combine, gips = TRUE)
  #calculate 1, 3, 5 year IRR
  for(i in 2:length(dates)) {
    cf.i=combinecf[time(combinecf)<=valdate&time(combinecf)>dates[i]]
    dat <- mergesum.z(-combineval[dates[i]], cf.i, combineval[valdate])
    irr.cat[i] <- irr.z(dat, gips = TRUE)
  }
  i=5
  itd.dat <- mergesum.z(combinecf[time(combinecf)<=valdate], combineval[valdate])
  irr.cat[i] <- irr.z(itd.dat, gips = TRUE)
  ra.strat.ret <- rbind(ra.strat.ret, irr.cat)
  colnames(ra.strat.ret) = c("Qtr","1 Year","3 Year","5 Year","ITD")
}
rownames(ra.strat.ret) <- strat
ra.p2p.table <- rbind(totrareturn, ra.strat.ret)
ra.p2p.table = data.frame("Category" = rownames(ra.p2p.table),
                          ra.p2p.table, row.names = NULL)
colnames(ra.p2p.table)[-1] <- colnames(totrareturn)
ra.p2p.table$Qtr = paste0(round(ra.p2p.table$Qtr*100,1),"%")
ra.p2p.table$`1 Year` = paste0(round(ra.p2p.table$`1 Year`*100,1),"%")
ra.p2p.table$`3 Year` = paste0(round(ra.p2p.table$`3 Year`*100,1),"%")
ra.p2p.table$`5 Year` = paste0(round(ra.p2p.table$`5 Year`*100,1),"%")
ra.p2p.table$ITD = paste0(round(ra.p2p.table$ITD*100,1),"%")
hlines = c(-1, 0, 2, dim(ra.p2p.table)[1])
print(
  xtable(ra.p2p.table, digits = 2), 
  scalebox = .5,  align = rep("r",4),
  hline.after = hlines, include.rownames = FALSE)

## @knitr farm.nav
#create historical strategy
name.strat <- unique(fundinfo[which(fundinfo$Portfolio == port), "Csname"])
strat.val.list <- list()
for(s in strat) {
  cv <- y.v[[s]]
  hv <- y.hv[[s]]
  navs <- zoosum(cv, hv)
  strat.val.list[[s]] <- navs
}
val.strat <- do.call(merge, strat.val.list)
val.strat = subset(val.strat, index(val.strat) <= valdate)

#graph of NAV's
val.strat.df = data.frame("Date"=time(val.strat), coredata(val.strat))
colnames(val.strat.df)[-1] <- as.character(strat)
strat.tidy <- gather(val.strat.df, Strategy, NAV, -Date)
strat.tidy$NAV = tomillions(strat.tidy$NAV)
strat.tidy$Strategy = factor(strat.tidy$Strategy, levels = 
                               c("USFRT", "IFM"))
#plot of just the market values
nav.strat <- ggplot()+
  geom_bar(data = strat.tidy,aes(x = Date,y = NAV, fill = Strategy), stat="identity")+
  ggtitle("NAV by Strategy") + 
  scale_fill_manual(values = IMD.palette()) + xlab("") + ylab("in Millions")+
  scale_y_continuous(labels = scales::dollar)+
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
        legend.position="bottom", legend.box.spacing = unit(0, "cm"))
print(nav.strat)

## @knitr farm.perf
vindx = which(!pme.df$isvint & pme.df$Portfolio == port)
vindx = vindx[c(-3, -5)]
strat.short <- pme.df$name[vindx]

IRR <- round(pme.df$`Fund IRR`[vindx], 3)
PME <- pme.df$`CPIxFE+350 PME`[vindx]
TVPI <- round(pme.df$`Fund TVPI`[vindx], 2)
PME.cut <- cut(
  x = PME, 
  breaks = c(-Inf, .8, .95, 1.05, 1.2, +Inf),
  labels = c("< 0.80", "0.80 to 0.95", "0.95 to 1.05", "1.05 to 1.20", "> 1.20")
)

pme.data <- na.omit(data.frame(Category = strat.short, TVPI, IRR, PME, PME.cut))

strat.plot = ggplot(pme.data,aes(x=TVPI,y=IRR, color = PME.cut))+ 
  geom_text_repel(aes(label = Category), show.legend = FALSE) + labs(color = "PME:")+
  geom_point(size = 2)+ scale_color_manual(drop = FALSE, values = PME.palette())+
  xlab("TVPI")+ scale_y_continuous(name="IRR",labels=percent)+
  ggtitle(paste(port, "Comparison of Strategies")) + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(colour = guide_legend(reverse = T))
print(strat.plot)