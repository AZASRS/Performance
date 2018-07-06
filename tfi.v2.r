## @knitr setup
require(zoo)
require(xts)
require(tidyr)
require(ggplot2)
require(lubridate)
require(reshape2)
require(scales)
require(PerformanceAnalytics)
require(knitr)
require(gridExtra)
require(kableExtra)
require(xtable)
require(dplyr)
require(tidyr)
require(magrittr)
require(ggpubr)
require(grid)
require(ggrepel)

source('P:/IMD/Karl/R projects/basic financial.r',echo=FALSE)
path.rds = "P:/IMD/JohnD/Fixed Income/R Projects/Performance/rds/"

pme.df <- readRDS(paste0(path.rds,"pme.df.rds"))
pme.df$`Fund IRR`= pme.df$`Fund IRR`/100
fundinfo <- readRDS(paste0(path.rds,"fundinfo.rds"))
tomillions = function(x) x/1000000

as.of.note = "As of December 31, 2017"
JD.palette <- c("#af1313", "#d86868", "#d6c126", "#4b8bd1", "#0720b2")

#Opportunistic Debt
od.short <- c("Total OPP", as.character(filter(fundinfo, Portfolio == "OPP")[,2]))
od.pcaps <- readRDS(paste0(path.rds,"od.pcaps.rds"))
od.cf.adj <- readRDS(paste0(path.rds,"od.cf.adj.rds"))

#Private Debt
pd.short <- c("Total PD", as.character(filter(fundinfo, Portfolio == "PD")[,2]))
pd.pcaps <- readRDS(paste0(path.rds,"pd.pcaps.rds"))
pd.cf.adj <- readRDS(paste0(path.rds,"pd.cf.adj.rds"))

#Total Fixed Income Output
## @knitr tfi.return.table
tfi.return.table <- readRDS(file = paste0(path.rds,"tfi.return.table"))
hlines = c(-1, seq(0,nrow(tfi.return.table), by=(+3)))
print(
   xtable(tfi.return.table, digits = 2),
   scalebox = .5,
   hline.after = hlines,
   align = rep("r",4))

## @knitr plot.mv.tfi
readRDS(file = paste0(path.rds, "plot.mv.tfi"))

## @knitr plot.tfi.roll
readRDS(file = paste0(path.rds, "plot.tfi.roll"))

## @knitr tfi.dva
plot.tfi.dva <- readRDS(file = paste0(path.rds, "plot.tfi.dva"))
plot.tfi.dva.long <- readRDS(file = paste0(path.rds, "plot.tfi.dva.long"))
tfi.dva.plot<- ggarrange(plot.tfi.dva, plot.tfi.dva.long, nrow = 1, ncol = 2, common.legend = T, legend = "bottom")
annotate_figure(tfi.dva.plot, top  = text_grob("Total Fixed Income Dollar Value Added \n Relative to Sub-Asset Class Benchmarks", face = "bold", size = 16))

## @knitr plot.r.tfi.5yr
readRDS(file = paste0(path.rds, "plot.r.tfi.5yr"))

## @knitr plot.ex.tfi.5yr
readRDS(file = paste0(path.rds, "plot.ex.tfi.5yr"))

#Core Fixed Income Output
## @knitr irs.return.table
irs.return.table <- readRDS(file = paste0(path.rds,"irs.return.table"))
hlines = c(-1, seq(0,nrow(irs.return.table), by=(+3)))
print(
   xtable(irs.return.table, digits = 2), 
   scalebox = .5,  align = rep("r",4),
   hline.after = hlines, include.rownames = FALSE)

## @knitr plot.mv.irs
readRDS(file = paste0(path.rds,"plot.mv.irs"))

## @knitr plot.irs.roll
readRDS(file = paste0(path.rds,"plot.irs.roll"))

## @knitr irs.dva
plot.irs.dva <- readRDS(file = paste0(path.rds, "plot.irs.dva"))
plot.irs.dva.long <- readRDS(file = paste0(path.rds, "plot.irs.dva.long"))
irs.dva.plot <- ggarrange(plot.irs.dva, plot.irs.dva.long, nrow = 1, ncol = 2, common.legend = T, legend = "bottom")
annotate_figure(irs.dva.plot, top  = text_grob("Core Fixed Income Dollar Value Added \n Relative to Barclay's Aggregate Benchmark", face = "bold", size = 16))

## @knitr plot.r.irs.5yr
readRDS(file = paste0(path.rds,"plot.r.irs.5yr"))

## @knitr plot.ex.irs.5yr
readRDS(file = paste0(path.rds,"plot.ex.irs.5yr"))

## @knitr F2.cone


## @knitr hy.return.table
hy.return.table <- readRDS(file = paste0(path.rds,"hy.return.table"))
hy.return.table[7:9,4] <- NA #JP Morgan not 5 years
hlines = c(-1, seq(0,nrow(hy.return.table), by=(+3)))
print(
   xtable(hy.return.table, digits = 2), 
   scalebox = .5,  align = rep("r",4),
   hline.after = hlines, include.rownames = FALSE)

## @knitr plot.mv.hy
readRDS(file = paste0(path.rds,"plot.mv.hy"))

## @knitr plot.hy.roll
readRDS(file = paste0(path.rds,"plot.hy.roll"))

## @knitr hy.dva
plot.hy.dva <- readRDS(file = paste0(path.rds, "plot.hy.dva"))
plot.hy.dva.long <- readRDS(file = paste0(path.rds, "plot.hy.dva.long"))
hy.dva.plot <- ggarrange(plot.hy.dva, plot.hy.dva.long, nrow = 1, ncol = 2, common.legend = T, legend = "bottom")
annotate_figure(hy.dva.plot, top  = text_grob("High Yield Dollar Value Added \n Relative to Barclay's High Yield Benchmark", face = "bold", size = 16))

## @knitr plot.r.hy.5yr
readRDS(file = paste0(path.rds,"plot.r.hy.5yr"))

## @knitr plot.ex.hy.5yr
readRDS(file = paste0(path.rds,"plot.ex.hy.5yr"))

## @knitr pd.irr.table
pd.irr.table <- readRDS(file = paste0(path.rds,"pd.irr.table"))
pd.irr.table <- pd.irr.table[-3,] #Ares Europe

hlines = c(-1, 0, 2, dim(pd.irr.table)[1])
print(
   xtable(pd.irr.table, digits = 1), 
   scalebox = .6,  align = rep("r",4),
   hline.after = hlines, include.rownames = TRUE)

## @knitr plot.mv.pd
readRDS(file = paste0(path.rds,"plot.mv.pd"))

## @knitr plot.pd.pme
readRDS(file = paste0(path.rds,"plot.pd.pme"))

## @knitr od.irr.table
od.irr.table <- readRDS(file = paste0(path.rds,"od.irr.table"))
hlines = c(-1, 0, 2, dim(od.irr.table)[1])

print(
   xtable(as.data.frame(od.irr.table), digits = 1), 
   scalebox = .6,  align = rep("r",4),
   hline.after = hlines, include.rownames = TRUE)

## @knitr plot.mv.od
readRDS(file = paste0(path.rds,"plot.mv.od"))

## @knitr plot.od.pme
readRDS(file = paste0(path.rds,"plot.od.pme"))

## @knitr pd.name.table
pd.names <- readRDS(file = paste0(path.rds,"pd.names"))
print(
   xtable(pd.names), 
   scalebox = .5,  align = rep("l",2),
   hline.after = c(-1,0,dim(pd.names)[1]), include.rownames = FALSE)

## @knitr od.name.table
od.names <- readRDS(file = paste0(path.rds,"od.names"))
print(
   xtable(od.names), 
   scalebox = .5,  align = rep("l",2),
   hline.after = c(-1,0,dim(od.names)[1]), include.rownames = FALSE)
