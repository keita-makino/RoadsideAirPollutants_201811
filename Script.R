# set this directory as the root
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# remove all variables
rm(list = ls())

# libraries
library(lubridate)
library(tidyverse)
library(scales)
library(GGally)
loadNamespace("cowplot")
library(imputeTS)
library(MASS)
library(data.table)
library(CADFtest)
library(forecast)
library(astsa)
library(Metrics)
library(lmtest)
library(dyn)
library(recipes)
library(FNN)
library(caret)
library(keras)
library(hydroGOF)

use_session_with_seed(144, disable_gpu = FALSE)

# pasting
Texcat <- function(..., s = "") {
  return(paste(..., sep = s))
}

BasicThemeNoLegend = theme_bw() + theme(legend.position = "none", axis.text = element_text(size = rel(0.95),margin = margin(r = 6))) 
BasicThemeWithLegend = theme_bw() + theme(axis.text = element_text(size = rel(0.95),margin = margin(r = 6)))
unitText = c(expression(CO ~ (ppm)),
             expression(NO2 ~ (ppb)),
             expression(O3 ~ (ppb)),
             expression(PM1 ~ (ug/m^3)),
             expression(PM2.5 ~ (ug/m^3)),
             expression(PM10 ~ (ug/m^3)),
             expression(RH ~ ("%")),
             expression(TEMP ~ (C^o)),
             expression(WS ~ (m/s)),
             expression(Small ~ (Veh/Hr)),
             expression(Heavy ~ (Veh/Hr))
)

printPlot = 1
pollutantNames = c("CO","NO2","O3","PM2.5")

traffic <- read.csv("TrafficData.csv")

tTime <- as.POSIXct(traffic$Time)
tMonth <- month(tTime)
tDay <- day(tTime)
tHour <- hour(tTime)
tMin <- minute(tTime)
tMin10 <- tMin - tMin %% 10

tData <- data.frame(traffic[, 2:3] * 6, tMonth, tDay, tHour, tMin10)
tData.agg <- aggregate(. ~ tMin10 + tHour + tDay + tMonth, tData, sum)

tPlotData <- data.frame(
  as.POSIXct(sprintf("%02d:%02d", tData.agg$tHour, tData.agg$tMin), format = "%H:%M"),
  sprintf("%02d/%02d/17", tData.agg$tMonth, tData.agg$tDay),
  tData.agg$Small.car,
  tData.agg$big.car
)
colnames(tPlotData) <- c("Time", "Day", "Small", "Heavy")

if (printPlot == 1) {
  for (d in 1:2) {
    assign(
      Texcat("graph", d),
      graph2 <- ggplot(tPlotData, aes_string(x = "Time", y = colnames(tPlotData)[d + 2], colour = "Day", shape = "Day")) +
        scale_shape_manual(values = 1:7) +
        geom_line(size = 0.5) +
        geom_point(stroke = 0.7) +
        labs(y = unitText[d+9]) +
        scale_x_datetime(labels = date_format("%H:%M", tz = Sys.timezone())) +
        theme_bw() + theme(legend.position = "none", plot.caption = element_text(hjust = 0.5, size = rel(1.6)))
    )
  }
  graphGrid <- cowplot::plot_grid(graph1, graph2, align = "v", ncol = 2)
  legend <- cowplot::get_legend(graph1 + theme(legend.text = element_text(size = rel(0.95),margin = margin(r = 6)), legend.position = "bottom") +
                                  guides(shape = guide_legend(title = "Day", nrow = 1), col = guide_legend(title = "Day", nrow = 1)))
  
  graph <- cowplot::plot_grid(graphGrid, legend, ncol = 1, rel_heights = c(9, 1))
  ggsave("traffic.png", width = 11, height = 3.8, dpi = 200, units = "in")
  
  tPlotData$IfWeekend <- ifelse(as.POSIXlt(mdy(tPlotData[, 2]))$wday %% 6 == 0, "Weekday", "Weekend")
  for (d in 3:4) {
    assign(
      Texcat("graph", d),
      ggplot(tPlotData, aes_string(x = "Time", y = colnames(tPlotData)[d], color = "IfWeekend", shape = "IfWeekend")) +
        scale_shape_manual(values = 1:7) +
        stat_summary(fun.y = mean, geom = "line", aes(group = IfWeekend), size = 0.5) +
        stat_summary(fun.y = mean, geom = "point", aes(group = IfWeekend), stroke = 0.7) +
        labs(y = unitText[d+7]) +
        scale_x_datetime(labels = date_format("%H:%M", tz = Sys.timezone())) +
        theme_bw() + theme(legend.title = element_blank(), legend.position = "none", plot.caption = element_text(hjust = 0.5, size = rel(1.6)))
    )
  }
  graphGrid <- cowplot::plot_grid(graph3, graph4, align = "v", ncol = 2)
  legend <- cowplot::get_legend(graph4 + theme(legend.text = element_text(size = rel(0.95),margin = margin(r = 6)), legend.position = "bottom") + 
                                  guides(shape = guide_legend(nrow = 1), col = guide_legend(nrow = 1)))
  
  graph <- cowplot::plot_grid(graphGrid, legend, ncol = 1, rel_heights = c(16, 1))
  ggsave("trafficWeek.png", width = 11, height = 3.6, dpi = 200, units = "in")
}

air <- read.csv("AQData.csv")
colnames(air)[10] <- "WS"

aTime <- as.POSIXct(air$Time)
aMonth <- month(aTime)
aDay <- day(aTime)
aHour <- hour(aTime)
aMin <- minute(aTime)

aMin <- ceiling(aMin / 10) * 10
aHour = aHour + floor(aMin / 60)
aMin <- aMin %% 60

aData <- data.frame(air[, 2:dim(air)[2]], aMonth, aDay, aHour, aMin)

aPlotData <- data.frame(
  as.POSIXct(sprintf("%02d:%02d", aData$aHour, aData$aMin), format = "%H:%M"),
  sprintf("%02d/%02d/17", aData$aMonth, aData$aDay),
  aData[, 1:9]
)
colnames(aPlotData)[1:2] <- c("Time", "Day")
aPlotData$IfWeekend <- ifelse(as.POSIXlt(mdy(aPlotData[, 2]))$wday %% 6 == 0, "Weekday", "Weekend")

if (printPlot == 1) {
  for (d in 3:8) {
    assign(
      Texcat("graph", d),
      ggplot(aPlotData, aes_string(x = "Time", y = colnames(aPlotData)[d], color = "Day", shape = "Day")) +
        scale_shape_manual(values = 1:7) +
        geom_line(size = 0.5) +
        geom_point(stroke = 0.7) +
        labs(y = unitText[d-2]) +
        scale_x_datetime(labels = date_format("%H:%M", tz = Sys.timezone())) +
        BasicThemeNoLegend
    )
  }
  
  graphGrid <- cowplot::plot_grid(graph3, graph4, graph5, graph6, graph7, graph8, align = "v", ncol = 2)
  legend <- cowplot::get_legend(graph3 + theme(legend.text = element_text(size = rel(0.95),margin = margin(r = 6)),legend.position = "bottom") + 
                                  guides(shape = guide_legend(title = "Day", nrow = 1), col = guide_legend(title = "Day", nrow = 1)))
  
  graph <- cowplot::plot_grid(graphGrid, legend, ncol = 1, rel_heights = c(28, 1))
  ggsave("air.png", width = 11, height = 10.2, dpi = 200, units = "in")
  
  ### Environmental Factors
  for (d in 9:11) {
    assign(
      Texcat("graph", d),
      ggplot(aPlotData, aes_string(x = "Time", y = colnames(aPlotData)[d], color = "Day", shape = "Day")) +
        scale_shape_manual(values = 1:7) +
        geom_line(size = 0.5) +
        geom_point(stroke = 0.7) +
        labs(y = unitText[d-2]) +
        scale_x_datetime(labels = date_format("%H:%M", tz = Sys.timezone())) +
        BasicThemeNoLegend
    )
  }
  
  graphGrid <- cowplot::plot_grid(graph9,graph10,graph11, align = "v", ncol = 2)
  legend <- cowplot::get_legend(graph9 + theme(legend.text = element_text(size = rel(0.95),margin = margin(r = 6)),legend.position = "bottom") + 
                                  guides(shape = guide_legend(title = "Day", nrow = 1), col = guide_legend(title = "Day", nrow = 1)))
  
  graph <- cowplot::plot_grid(graphGrid, legend, ncol = 1, rel_heights = c(20, 1))
  ggsave("env.png", width = 11, height = 6.6, dpi = 200, units = "in")
  
  for (d in 3:8) {
    assign(
      Texcat("graph", d),
      ggplot(aPlotData, aes_string(x = "Time", y = colnames(aPlotData)[d], color = "IfWeekend", shape = "IfWeekend")) +
        scale_shape_manual(values = 1:2) +
        stat_summary(fun.y = mean, geom = "line", aes(group = IfWeekend), size = 0.5) +
        stat_summary(fun.y = mean, geom = "point", aes(group = IfWeekend), stroke = 0.7) +
        labs(y = unitText[d-2]) +
        scale_x_datetime(labels = date_format("%H:%M", tz = Sys.timezone())) +
        BasicThemeNoLegend
    )
  }
  
  graphGrid <- cowplot::plot_grid(graph3, graph4, graph5, graph6, graph7, graph8, align = "v", ncol = 2)
  legend <- cowplot::get_legend(graph4 + theme(legend.text = element_text(size = rel(0.95),margin = margin(r = 6)), legend.title = element_blank(),legend.position = "bottom") +
                                  guides(shape = guide_legend(nrow = 1), col = guide_legend(nrow = 1)))
  
  graph <- cowplot::plot_grid(graphGrid, legend, ncol = 1, rel_heights = c(28, 1))
  ggsave("airWeek.png", width = 11, height = 9.6, dpi = 200, units = "in")
  
  for (d in 9:11) {
    assign(
      Texcat("graph", d),
      ggplot(aPlotData, aes_string(x = "Time", y = colnames(aPlotData)[d], color = "IfWeekend", shape = "IfWeekend")) +
        scale_shape_manual(values = 1:2) +
        stat_summary(fun.y = mean, geom = "line", aes(group = IfWeekend), size = 0.5) +
        stat_summary(fun.y = mean, geom = "point", aes(group = IfWeekend), stroke = 0.7) +
        labs(y = unitText[d-2]) +
        scale_x_datetime(labels = date_format("%H:%M", tz = Sys.timezone())) +
        BasicThemeNoLegend
    )
  }
  
  graphGrid <- cowplot::plot_grid(graph9, graph10, graph11, align = "v", ncol = 2)
  legend <- cowplot::get_legend(graph9 + theme(legend.text = element_text(size = rel(0.95),margin = margin(r = 6)), legend.title = element_blank(), legend.position = "bottom") +
                                  guides(shape = guide_legend(nrow = 1), col = guide_legend(nrow = 1)))
  
  graph <- cowplot::plot_grid(graphGrid, legend, ncol = 1, rel_heights = c(24, 1))
  ggsave("envWeek.png", width = 11, height = 6.4, dpi = 200, units = "in")
}

if (printPlot == 1) {
  source("PointsWithLine.R")
  
  PlotDiag <- function(data, mapping, ...) {
    ggplot(data, mapping = mapping) + geom_density(..., alpha = 0.5)
  }
  
  graph <- ggpairs(aPlotData[, c(2, 6:8)],
                   columns = 2:4,
                   mapping = aes(colour = Day),
                   lower = list(continuous = PointsWithLine),
                   diag = list(continuous = PlotDiag),
                   upper = list(continuous = wrap("cor", alignPercent = 0.7))
  ) + BasicThemeWithLegend+ theme(strip.text = element_text(size = rel(0.95),margin = margin(r = 6)))
  ggsave("pmCorr.png", graph, width = 9.6, height = 6, dpi = 200, units = "in")
  
  graph <- ggpairs(aPlotData[, c(2:5, 7)],
                   columns = 2:5,
                   mapping = aes(colour = Day),
                   lower = list(continuous = PointsWithLine),
                   diag = list(continuous = PlotDiag),
                   upper = list(continuous = wrap("cor", alignPercent = 0.7))
  ) + BasicThemeWithLegend+ theme(strip.text = element_text(size = rel(0.95),margin = margin(r = 6)))
  ggsave("airCorr.png", graph, width = 12.8, height = 8, dpi = 200, units = "in")
  
  graph <- ggpairs(aPlotData[, c(2, 9:11)],
                   columns = 2:4,
                   mapping = aes(colour = Day),
                   lower = list(continuous = PointsWithLine),
                   diag = list(continuous = PlotDiag),
                   upper = list(continuous = wrap("cor", alignPercent = 0.7))
  ) + BasicThemeWithLegend+ theme(strip.text = element_text(size = rel(0.95),margin = margin(r = 6)))
  ggsave("envCorr.png", graph, width = 9.6, height = 6, dpi = 200, units = "in")
}

tCombinedData <- tPlotData %>%
  group_by(Day) %>%
  complete(Time = as.POSIXct(aPlotData$Time)) %>%
  as.data.frame()

cDataWithNA <- data.frame(aPlotData[,1:2], aPlotData[,c(3:5, 7, 10:11)], tCombinedData[,3:4])

PointsWithCor <- function(data, mapping, ..., method = "pearson") {
  df <- data.frame(x = eval(mapping$x, data), y = eval(mapping$y, data), c = eval(mapping$colour, data))
  
  xMin <- min(df$x, na.rm = TRUE)
  xMax <- max(df$x, na.rm = TRUE)
  yMin <- min(df$y, na.rm = TRUE)
  yMax <- max(df$y, na.rm = TRUE)
  
  sumdf <- df %>%
    group_by(c) %>%
    summarise(
      lab = sprintf("%1.3f", round(cor(x, y, use = "complete.obs"), 3)),
      x = xMin - (xMax - xMin) * 0.25,
      y = yMax - (yMax - yMin) * (min(as.numeric(c)) - 1) / max(as.numeric(df$c))
    ) %>% rbind(data.frame(
      c = "All",
      lab =  sprintf("%1.3f", round(cor(df$x, df$y, use = "complete.obs"), 3)),
      x = xMax,
      y = yMax
    ))
  
  graph = ggplot(data, mapping) + geom_point(aes(shape = df$c), stroke = 0.7) + 
    scale_shape_manual(values = 1:7) +
    geom_label(
      data = sumdf[-8,],
      mapping = aes(x = x, y = y, label = lab, color = c),
      hjust = 0, vjust = 1,
      inherit.aes = FALSE
    ) +
    geom_smooth(
      method = "lm", se=FALSE ,
      aes(x = data[as.character(mapping$x)],
          y = data[as.character(mapping$y)], color = NA)
    ) +
    geom_label(
      data = sumdf[8,],
      mapping = aes(x = x, y = y, label = lab, color = NA),
      hjust = 1, vjust = 1,
      inherit.aes = FALSE
    )
}

if (printPlot == 1) {
  graph <- ggduo(cDataWithNA[,-1],
                 columnsX = 2:5, columnsY = 6:9,
                 mapping = aes(colour = Day, alpha = 0.5),
                 types = list(continuous = PointsWithCor), showStrips = F
  ) + BasicThemeWithLegend+ theme(strip.text = element_text(size = rel(0.95),margin = margin(r = 6)))
  ggsave("airEnvCorr.png", graph, width = 12.8, height = 8, dpi = 200, units = "in")
}

cData <- data.frame(aPlotData[,1:2], apply(cbind(aPlotData[,c(3:5, 7, 10:11)], tCombinedData[,3:4]), 2, na.interpolation))

model.sarima.CO <- auto.arima(ts(cData$CO, frequency = 144))
model.sarima.NO2 <- auto.arima(ts(cData$NO2, frequency = 144))
model.sarima.O3 <- auto.arima(ts(cData$O3, frequency = 144))
model.sarima.PM2.5 <- auto.arima(ts(cData$PM2.5, frequency = 144))

PrintModelStats <- function(list) {
  for (i in 1:length(list)) {
    model <- list[[i]]
    print(model)
    print(coeftest(model))
  }
}

PrintModelStats(list(model.sarima.CO,model.sarima.NO2,
                     model.sarima.O3,model.sarima.PM2.5))

### prediction
t <- as.POSIXct(Texcat(mdy(cData$Day), " ", sapply(strsplit(as.character(cData$Time), "[ ]"), "[", 2)),
                format = "%Y-%m-%d %H:%M:%S"
)

mData.1 = data.frame(t = tail(t,-1),tail(cData[,3:6],-1),head(cData[,7:10],-1),head(cData[,3:6],-1))
mData.6 = data.frame(t = tail(t,-6),tail(cData[,3:6],-6),head(cData[,7:10],-6),head(cData[,3:6],-6))

colnames(mData.1)[10:13] = c("COP","NO2P","O3P","PM2.5P") 
colnames(mData.6)[10:13] = c("COP","NO2P","O3P","PM2.5P") 

PlotCCF = function(){
  for (i in 1:4) {
    df = data.frame(0:72)
    for (j in 1:8) {
      df[j+1] = ccf(cData[,pollutantNames[i]],cData[,2+j],plot = FALSE,lag.max = 72)$acf[73:145]
    }
    colnames(df) = c("Lag","CO","NO2","O3","PM2.5","TEMP","WS","Small","Heavy")
    df = df %>% melt(id = "Lag")
    graph = ggplot(df, aes(x = Lag, y = value)) +
      geom_hline(yintercept=0) + geom_line(aes(colour = variable)) +
      geom_point(aes(colour=variable,shape=variable), stroke = 0.7) +
      scale_shape_manual(values=1:8) + BasicThemeNoLegend +
      labs(x = "Lag (x10 minutes)", y = Texcat("CCF (vs. ",pollutantNames[i],")")) 
    assign(Texcat("graph",i),graph)
  }
  graphGrid = cowplot::plot_grid(graph1,graph2,graph3,graph4, align="v", ncol = 2)
  legend <- cowplot::get_legend(graph1 + theme(legend.title = element_blank(),
                                               legend.text = element_text(size = rel(0.95),margin = margin(r = 6)),
                                               legend.position = "bottom")
                                + guides(shape = guide_legend(nrow = 1), col = guide_legend(nrow = 1)))
  graph <- cowplot::plot_grid(graphGrid, legend, ncol = 1, rel_heights = c(30, 1))
  ggsave("ccf.png", width = 11, height = 6.9, dpi = 200, units = "in")
}

PlotCCF()

DoLinearModel <- function(lag) {
  data <- get(Texcat("mData.", lag)) %>% head(-144)
  CreateLinearModel <- function(y) {
    with(data, {
      model <- lm(as.formula(Texcat(y, "~ TEMP + WS + Small + Heavy")))
      print(summary(model))
      assign(
        Texcat("model.lm.", lag, ".", y), model,
        envir = globalenv()
      )
      assign(
        Texcat("prdic.lm.", lag, ".", y), predict(model, get(Texcat("mData.", lag))),
        envir = globalenv()
      )
    })
  }
  
  CreateLinearModel("CO")
  CreateLinearModel("NO2")
  CreateLinearModel("O3")
  CreateLinearModel("PM2.5")
}

DoLinearModel(1)
DoLinearModel(6)

PredictLinear <- function(target,lag) {
  model <- get(Texcat("model.lm.",lag,".", target))
  assign(
    Texcat("prdic.lm",lag,".", target),predict(model, get(Texcat("mData.",lag))) 
  )
}

for (i in 1:4) {
  for (j in c(1,6)) {
    PredictLinear(pollutantNames[i],j)
  }
}

DoAccumulatedLinearModel = function(target,lag,ifFull){
  data <- get(Texcat("mData.", lag))
  train = head(data,-144)

  param <- list(c(0, 0, 0, 0), c(0))
  newData = data[,1:5]

  for (i in 1:4) {
    c = ccf(train[,target],train[,5+i],plot = FALSE,lag.max = 72)$acf[73:145]
    param[[1]][i] = which(c == max(c))
    newData[,5+i] = data[,5+i] %>% lag(param[[1]][i])
  }
  if (ifFull) {
    newData[,10] = data[,Texcat(target,"P")]
    colnames(newData)[6:10] = c("TEMP","WS","Small","Heavy",Texcat(target,"P"))
    str = Texcat(target, "~ TEMP + WS + Small + Heavy + ",target,"P")
    assignStr = ".full."
  }else{
    colnames(newData)[6:9] = c("TEMP","WS","Small","Heavy")
    str = Texcat(target, "~ TEMP + WS + Small + Heavy")
    assignStr = "."
  }
  newData = newData %>% na.omit
  newTrain = newData %>% head(-144)

  model <- lm(as.formula(str),data = newTrain)
  print(summary(model))
  assign(
    Texcat("model.lm.lag",assignStr, lag, ".", target), model,
    envir = globalenv()
  )
  assign(
    Texcat("prdic.lm.lag",assignStr, lag, ".", target), predict(model, data),
    envir = globalenv()
  )
  assign(
    Texcat("param.lm.lag",assignStr, lag, ".", target), param,
    envir = globalenv()
  )

  set.seed(144)
  control = trainControl(method = "cv", number = 6)
  newTrainX =
  model = train(as.formula(str)
                ,trControl = control,method = "knn", data= newTrain,
                tuneGrid = expand.grid(k = 1:15))
  param[[2]] = model$finalModel$k
  assign(
    Texcat("model.knn",assignStr, lag, ".", target), model,
    envir = globalenv()
  )
  assign(
    Texcat("prdic.knn",assignStr, lag, ".", target), predict(model, data),
    envir = globalenv()
  )
  assign(
    Texcat("param.knn",assignStr, lag, ".", target), param,
    envir = globalenv()
  )
}

for (i in 1:4) {
  for (j in c(1,6)) {
    DoAccumulatedLinearModel(pollutantNames[i],j,FALSE)
    DoAccumulatedLinearModel(pollutantNames[i],j,TRUE)
  }
}

DoAccumulatedLinearModelOld <- function(target, lag) {
  data <- get(Texcat("mData.", lag))
  x <- data[, 6:9]
  y <- data[, target]
  
  param <- list(c(0, 0), c(0, 0))
  paramk <- list(c(0, 0), c(0, 0), 0)
  
  r <- Inf
  rk = Inf
  model <- NA
  
  for (numEnv in c(6, 12, 36, 72)) {
    for (numTrf in c(6, 12, 36, 72)) {
      for (coefEnv in numEnv * c(-4, -2, -1, -1 / 2, -1 / 4, -Inf, 1 / 4, 1 / 2, 1, 2, 4)) {
        for (coefTrf in numTrf * c(-4, -2, -1, -1 / 2, -1 / 4, -Inf, 1 / 4, 1 / 2, 1, 2, 4)) {
          x0 <- matrix(0, dim(x)[1], dim(x)[2])
          for (cl in 1:2) {
            for (i in 0:(numEnv - 1)) {
              x0[, cl] <- x0[, cl] + (lag(x[, cl], i) * exp(i * 1 / coefEnv))
            }
          }
          for (cl in 3:4) {
            for (i in 0:(numTrf - 1)) {
              x0[, cl] <- x0[, cl] + (lag(x[, cl], i) * exp(i * 1 / coefTrf))
            }
          }
          
          x1 <- x0 %>% tail(-max(numTrf, numEnv))
          x1[, 1:2] <- x1[, 1:2] / sum(exp(0:(numEnv - 1) * 1 / coefEnv))
          x1[, 3:4] <- x1[, 3:4] / sum(exp(0:(numTrf - 1) * 1 / coefTrf))
          
          df <- data.frame(
            TEMP = x1[, 1],
            WS = x1[, 2],
            Small = x1[, 3],
            Heavy = x1[, 4],
            y = y %>% tail(-max(numTrf, numEnv))
          )
          m <- lm(y ~ TEMP + WS + Small + Heavy, df %>% head(-288))
          
          fit <- predict(m, df %>% head(-144)%>% tail(144))
          rm <- rmse(fit, df$y %>% head(-144)%>% tail(144))
          
          if (rm < r) {
            r <- rm
            model <- m
            p <- c(rep(NA, max(numTrf, numEnv)), predict(model, df))
            param <- list(c(numEnv, coefEnv), c(numTrf, coefTrf))
          }
          
          df[,1:4] = df[,1:4] %>% apply(2,scale)
          for (k in 1:15) {
            pred = knn.reg(df[,1:4] %>% head(-288),test = df[,1:4], df$y %>% head(-288), k = k)$pred
            rmk = rmse(pred %>% head(-144)%>% tail(144), df$y %>% head(-144)%>% tail(144))
            if (rmk < rk) {
              rk <- rmk
              pk <- c(rep(NA, max(numTrf, numEnv)), pred)
              paramk <- list(c(numEnv, coefEnv), c(numTrf, coefTrf), k)
            }
          }
        }
      }
    }
  }
  
  print(target)
  print(lag)
  print(coeftest(model))
  print(param)
  print(paramk)
  
  assign(Texcat("model.lm.acc.", lag, ".", target), model, envir = globalenv())
  assign(Texcat("prdic.lm.acc.", lag, ".", target), p, envir = globalenv())
  assign(Texcat("param.lm.acc.", lag, ".", target), param, envir = globalenv())
  
  assign(Texcat("prdic.knn.", lag, ".", target), pk, envir = globalenv())
  assign(Texcat("param.knn.", lag, ".", target), paramk, envir = globalenv())
}

for (i in 1:4) {
  for (j in c(1,6)) {
    DoAccumulatedLinearModelOld(pollutantNames[i],j)
  }
}

### LSTM
DoLSTM <- function(target, lag , nTest = 144, batch = 144, step = 6, epoch = 1000, numUnit = 144, stop = 0) {
  
  nData = get(Texcat("mData.",lag))
  rec = recipe(nData[,2:9]) %>%
    step_center(everything()) %>%
    step_scale(everything()) %>%
    prep()
  nData[,2:9] = bake(rec, nData[,2:9])
  
  ln = dim(nData)[1] - batch + lag
  
  train.x <- array(NA, c(ln - nTest, step, 4))
  test.x <- array(NA, c(nTest, step, 4))
  
  y = tail(nData,ln)[,target]
  train.y = head(y,-nTest)
  test.y = tail(y,nTest)
  
  variables <- c("TEMP", "WS", "Small", "Heavy")
  
  for (k in 1:length(variables)) {
    for (j in 0:(step-1)) {
      train.x[, step-j, k] <- unlist(nData[,variables[k]] %>% tail(ln+j) %>% head(ln-nTest))
      test.x[, step-j, k] <- unlist(nData[,variables[k]] %>% tail(ln+j) %>% tail(nTest))
    }
  }
  
  model <- keras_model_sequential()
  
  model %>%
    layer_lstm(
      units = numUnit,
      input_shape = c(step, 4),
      batch_size = batch,
      return_sequences = TRUE,
      stateful = TRUE
    ) %>%
    layer_dropout(0.15) %>%
    layer_lstm(
      units = numUnit,
      return_sequences = FALSE,
      stateful = TRUE
    ) %>%
    layer_dropout(0.15) %>%
    layer_dense(units = 1)
  
  if (stop == 0) {
    model %>%
      compile(loss = "mse", optimizer = "adam") %>%
      fit(
        x = train.x,
        y = train.y,
        batch_size = batch,
        epochs = epoch,
        verbose = 2,
        shuffle = FALSE
      ) 
  }
  else{
    model %>%
      compile(loss = "mse", optimizer = "adam") %>%
      fit(
        x = train.x,
        y = train.y,
        batch_size = batch,
        epochs = epoch,
        verbose = 2,
        shuffle = FALSE,
        validation_split = batch/(ln-nTest),
        callbacks =  callback_early_stopping(patience = stop)
      ) 
  }
  
  
  pred.train <- model %>%
    predict(train.x, batch_size = batch) %>%
    .[, 1]  
  pred.test <- model %>%
    predict(test.x, batch_size = batch) %>%
    .[, 1]
  
  
  assign(Texcat("model.lstm.",numUnit,".",epoch,".",lag,".", target), model, envir = globalenv())
  assign(Texcat("prdic.lstm.",numUnit,".",epoch,".",lag,".", target),
         c(
           rep(NA,dim(nData)[1]-ln), pred.train,pred.test) * rec$steps[[2]]$sds[target] + rec$steps[[1]]$means[target],
         envir = globalenv()
  )
  
  save_model_hdf5(
    model
    , Texcat("model.lstm.",numUnit,".",epoch,".",lag,".",pollutantNames[i]), overwrite = TRUE,
    include_optimizer = TRUE)
}

for (i in 1:4) {
  for (j in c(1,6)) {
    print(c(i,j))
    DoLSTM(pollutantNames[i],j, numUnit = 96, stop = 100)
  }
}


### ANN
DoANN <- function(target, lag , nTest = 144, step = 6, batch = 144, epoch = 1000, numUnit = 144, stop = 0) {
  
  nData = get(Texcat("mData.",lag))
  rec = recipe(nData[,2:9]) %>%
    step_center(everything()) %>%
    step_scale(everything()) %>%
    prep()
  nData[,2:9] = bake(rec, nData[,2:9])
  
  ln = dim(nData)[1] - batch + lag
  
  x = tail(nData,ln + step -1)[,6:9]
  train.x <- array(NA, c(ln - nTest, step* 4))
  test.x <- array(NA, c(nTest, step * 4))
  
  y = tail(nData,ln)[,target]
  train.y = head(y,-nTest)
  test.y = tail(y,nTest)
  
  for (j in 1:step) {
    train.x[, 1:4+4*(j-1)] <- unlist(x %>% tail(-j+0.1) %>% head(ln-nTest))
    test.x[, 1:4+4*(j-1)] <- unlist(x %>% tail(-j+0.1) %>% tail(nTest))
  }
  
  model <- keras_model_sequential()
  
  model %>%
    layer_dense(
      units = numUnit,
      activation = 'relu',
      input_shape = c(4 * step)
    ) %>%
    layer_dropout(0.15) %>%
    layer_dense(
      units = numUnit/2,
      activation = 'relu'
    ) %>%
    layer_dropout(0.15) %>%
    layer_dense(units = 1)
  
  if (stop == 0) {
    model %>%
      compile(loss = "mse", optimizer = "adam") %>%
      fit(
        x = train.x,
        y = train.y,
        batch_size = batch,
        epochs = epoch,
        verbose = 2,
        shuffle = FALSE
      ) 
  }
  else{
    model %>%
      compile(loss = "mse", optimizer = "adam") %>%
      fit(
        x = train.x,
        y = train.y,
        batch_size = batch,
        epochs = epoch,
        verbose = 2,
        shuffle = FALSE,
        validation_split = batch/(ln-nTest),
        callbacks =  callback_early_stopping(patience = stop)
      ) 
  }
  
  
  pred.train <- model %>%
    predict(train.x, batch_size = batch) %>%
    .[, 1]  
  pred.test <- model %>%
    predict(test.x, batch_size = batch) %>%
    .[, 1]
  
  
  assign(Texcat("model.ann.",numUnit,".",epoch,".",lag,".", target), model, envir = globalenv())
  assign(Texcat("prdic.ann.",numUnit,".",epoch,".",lag,".", target),
         c(
           rep(NA,dim(nData)[1]-ln), pred.train,pred.test) * rec$steps[[2]]$sds[target] + rec$steps[[1]]$means[target],
         envir = globalenv()
  )
  
  save_model_hdf5(
    model, Texcat("model.ann.",numUnit,".",epoch,".",lag,".",pollutantNames[i]), overwrite = TRUE,
    include_optimizer = TRUE)
}


for (i in 1:4) {
  for (j in c(1,6)) {
    print(c(i,j))
    DoANN(pollutantNames[i],j, numUnit = 96, stop = 100)
  }
}


### Full Models

DoLinearModelFull <- function(target, lag) {
  data <- get(Texcat("mData.", lag)) %>% head(-144)
  CreateLinearModelFull <- function(y) {
    with(data, {
      model <- lm(as.formula(Texcat(y, "~ TEMP + WS + Small + Heavy + ",Texcat(target,"P"))))
      print(summary(model))
      assign(
        Texcat("model.lm.full.", lag, ".", y), model,
        envir = globalenv()
      )
      assign(
        Texcat("prdic.lm.full.", lag, ".", y), predict(model, get(Texcat("mData.", lag))),
        envir = globalenv()
      )
    })
  }
  
  CreateLinearModelFull(target)
}

for (i in 1:4) {
  for (j in c(1,6)) {
    DoLinearModelFull(pollutantNames[i],j)
  }
}


DoAccumulatedLinearModelFull <- function(target, lag) {
  data <- get(Texcat("mData.", lag))
  x <- data[, 6:13]
  y <- data[, target]
  
  param <- list(c(0, 0), c(0, 0))
  paramk <- list(c(0, 0), c(0, 0), 0)
  
  r <- Inf
  rk = Inf
  model <- NA
  
  for (numEnv in c(6, 12, 36, 72)) {
    for (numTrf in c(6, 12, 36, 72)) {
      for (coefEnv in numEnv * c(-4, -2, -1, -1 / 2, -1 / 4, -Inf, 1 / 4, 1 / 2, 1, 2, 4)) {
        for (coefTrf in numTrf * c(-4, -2, -1, -1 / 2, -1 / 4, -Inf, 1 / 4, 1 / 2, 1, 2, 4)) {
          x0 <- matrix(0, dim(x)[1], dim(x)[2])
          for (cl in 1:2) {
            for (i in 0:(numEnv - 1)) {
              x0[, cl] <- x0[, cl] + (lag(x[, cl], i) * exp(i * 1 / coefEnv))
            }
          }
          for (cl in 3:4) {
            for (i in 0:(numTrf - 1)) {
              x0[, cl] <- x0[, cl] + (lag(x[, cl], i) * exp(i * 1 / coefTrf))
            }
          }
          
          x1 <- x0 %>% tail(-max(numTrf, numEnv))
          x1[, 1:2] <- x1[, 1:2] / sum(exp(0:(numEnv - 1) * 1 / coefEnv))
          x1[, 3:4] <- x1[, 3:4] / sum(exp(0:(numTrf - 1) * 1 / coefTrf))
          
          df <- data.frame(
            TEMP = x1[, 1],
            WS = x1[, 2],
            Small = x1[, 3],
            Heavy = x1[, 4],
            yp = x[,Texcat(target,"P")] %>% tail(-max(numTrf, numEnv)),
            y = y %>% tail(-max(numTrf, numEnv))
          )
         
          m <- lm(y ~ TEMP + WS + Small + Heavy + yp, df %>% head(-288))
          
          fit <- predict(m, df %>% head(-144)%>% tail(144))
          rm <- rmse(fit, df$y %>% head(-144)%>% tail(144))
          
          if (rm < r) {
            r <- rm
            model <- m
            p <- c(rep(NA, max(numTrf, numEnv)), predict(model, df))
            param <- list(c(numEnv, coefEnv), c(numTrf, coefTrf))
          }
          
          df[,1:5] = df[,1:5] %>% apply(2,scale)
          for (k in 1:15) {
            pred = knn.reg(df[,1:5] %>% head(-288),test = df[,1:5], df$y %>% head(-288), k = k)$pred
            rmk = rmse(pred %>% head(-144)%>% tail(144), df$y %>% head(-144)%>% tail(144))
            if (rmk < rk) {
              rk <- rmk
              pk <- c(rep(NA, max(numTrf, numEnv)), pred)
              paramk <- list(c(numEnv, coefEnv), c(numTrf, coefTrf), k)
            }
          }
        }
      }
    }
  }
  
  print(target)
  print(lag)
  print(coeftest(model))
  print(param)
  print(paramk)
  
  assign(Texcat("model.lm.acc.full.", lag, ".", target), model, envir = globalenv())
  assign(Texcat("prdic.lm.acc.full.", lag, ".", target), p, envir = globalenv())
  assign(Texcat("param.lm.acc.full.", lag, ".", target), param, envir = globalenv())
  
  assign(Texcat("prdic.knn.full.", lag, ".", target), pk, envir = globalenv())
  assign(Texcat("param.knn.full.", lag, ".", target), paramk, envir = globalenv())
}

for (i in 1:4) {
  for (j in c(1,6)) {
    DoAccumulatedLinearModelFull(pollutantNames[i],j)
  }
}

for (i in 1:4) {
  for (j in c(1,6)) {
    print(summary(get(Texcat("model.lm.full.",j,".",pollutantNames[i]))))
  }
}

for (i in 1:4) {
  for (j in c(1,6)) {
    print(summary(get(Texcat("model.lm.acc.full.",j,".",pollutantNames[i]))))
    print(get(Texcat("param.lm.acc.full.",j,".",pollutantNames[i])))
  }
}

for (i in 1:4) {
  for (j in c(1,6)) {
    print(get(Texcat("param.knn.full.",j,".",pollutantNames[i])))
  }
}


### ANN -Full Model
DoANNFull <- function(target, lag , nTest = 144, step = 6, batch = 144, epoch = 1000, numUnit = 144, stop = 0) {
  
  nData = get(Texcat("mData.",lag))
  rec = recipe(nData[,2:9]) %>%
    step_center(everything()) %>%
    step_scale(everything()) %>%
    prep()
  nData[,2:9] = bake(rec, nData[,2:9])
  for (i in 10:13) {
    nData[,i] = (nData[,i] - rec$steps[[1]]$means[i-9]) / rec$steps[[2]]$sds[i-9]
  }
  
  ln = dim(nData)[1] - batch + lag
  
  x = tail(nData,ln + step -1)[,c("TEMP","WS","Small","Heavy",Texcat(target,"P"))]
  d = dim(x)[2]
  train.x <- array(NA, c(ln - nTest, step* d))
  test.x <- array(NA, c(nTest, step * d))
  
  y = tail(nData,ln)[,target]
  train.y = head(y,-nTest)
  test.y = tail(y,nTest)
  
  for (j in 1:step) {
    train.x[, 1:d+d*(j-1)] <- unlist(x %>% tail(-j+0.1) %>% head(ln-nTest))
    test.x[, 1:d+d*(j-1)] <- unlist(x %>% tail(-j+0.1) %>% tail(nTest))
  }
  
  model <- keras_model_sequential()
  
  model %>%
    layer_dense(
      units = numUnit,
      activation = 'relu',
      input_shape = c(d * step)
    ) %>%
    layer_dropout(0.025) %>%
    layer_dense(
      units = numUnit/2,
      activation = 'relu'
    ) %>%
    layer_dropout(0.025) %>%
    layer_dense(units = 1)
  
  if (stop == 0) {
    model %>%
      compile(loss = "mse", optimizer = "adam") %>%
      fit(
        x = train.x,
        y = train.y,
        batch_size = batch,
        epochs = epoch,
        verbose = 2,
        shuffle = FALSE
      ) 
  }
  else{
    model %>%
      compile(loss = "mse", optimizer = "adam") %>%
      fit(
        x = train.x,
        y = train.y,
        batch_size = batch,
        epochs = epoch,
        verbose = 2,
        shuffle = FALSE,
        validation_split = batch/(ln-nTest),
        callbacks =  callback_early_stopping(patience = stop)
      ) 
  }
  
  
  pred.train <- model %>%
    predict(train.x, batch_size = batch) %>%
    .[, 1]  
  pred.test <- model %>%
    predict(test.x, batch_size = batch) %>%
    .[, 1]
  
  
  assign(Texcat("model.ann.full.",numUnit,".",epoch,".",lag,".", target), model, envir = globalenv())
  assign(Texcat("prdic.ann.full.",numUnit,".",epoch,".",lag,".", target),
         c(
           rep(NA,dim(nData)[1]-ln), pred.train,pred.test) * rec$steps[[2]]$sds[target] + rec$steps[[1]]$means[target],
         envir = globalenv()
  )
  save_model_hdf5(
    model, Texcat("model.ann.full.",numUnit,".",epoch,".",lag,".",pollutantNames[i]), overwrite = TRUE,
    include_optimizer = TRUE)
}


for (i in 1:4) {
  for (j in c(1,6)) {
    print(c(i,j))
    DoANNFull(pollutantNames[i],j, numUnit = 30, stop = 100)
  }
}

### LSTM - Full Model
DoLSTMFull<- function(target, lag , nTest = 144, batch = 144, step = 6, epoch = 1000, numUnit = 144, stop = 0, split = 0) {
  
  variables <- c(Texcat(target,"P"),"TEMP","WS","Small","Heavy")
  d = length(variables)
  
  nData = get(Texcat("mData.",lag))
  rec = recipe(nData[,2:9]) %>%
    step_center(everything()) %>%
    step_scale(everything()) %>%
    prep()
  nData[,2:9] = bake(rec, nData[,2:9])
  for (i in 10:13) {
    nData[,i] = (nData[,i] - rec$steps[[1]]$means[i-9]) / rec$steps[[2]]$sds[i-9]
  }
  
  ln = dim(nData)[1] - max(batch,lag) + lag
  
  train.x <- array(NA, c(ln - nTest, step, d))
  test.x <- array(NA, c(nTest, step, d))
  
  y = tail(nData,ln)[,target]
  train.y = head(y,-nTest)
  test.y = tail(y,nTest)
  
  
  for (k in 1:length(variables)) {
    for (j in 0:(step-1)) {
      train.x[, step-j, k] <- unlist(nData[,variables[k]] %>% tail(ln+j) %>% head(ln-nTest))
      test.x[, step-j, k] <- unlist(nData[,variables[k]] %>% tail(ln+j) %>% tail(nTest))
    }
  }
  
  model <- keras_model_sequential()
  
  model %>%
    layer_lstm(
      units = numUnit,
      input_shape = c(step, d),
      batch_size = batch,
      return_sequences = TRUE,
      stateful = TRUE
    ) %>%
    layer_dropout(0.025) %>%
    layer_lstm(
      units = numUnit/2,
      batch_size = batch,
      return_sequences = FALSE,
      stateful = TRUE
    ) %>%
    layer_dropout(0.025) %>%
    layer_dense(units = 1)
  
  if (stop == 0) {
    model %>%
      compile(loss = "mse", optimizer = "adam") %>%
      fit(
        x = train.x,
        y = train.y,
        batch_size = batch,
        epochs = epoch,
        verbose = 2,
        shuffle = FALSE
      ) 
  }
  else{
    model %>%
      compile(loss = "mse", optimizer = "adam") %>%
      fit(
        x = train.x,
        y = train.y,
        batch_size = batch,
        epochs = epoch,
        verbose = 2,
        shuffle = FALSE,
        validation_split = ifelse(split==0,batch/(ln-nTest),split/(ln-nTest)),
        callbacks =  callback_early_stopping(patience = stop)
      ) 
  }
  
  
  pred.train <- model %>%
    predict(train.x, batch_size = batch) %>%
    .[, 1]  
  pred.test <- model %>%
    predict(test.x, batch_size = batch) %>%
    .[, 1]
  
  
  assign(Texcat("model.lstm.full.",numUnit,".",epoch,".",lag,".", target), model, envir = globalenv())
  assign(Texcat("prdic.lstm.full.",numUnit,".",epoch,".",lag,".", target),
         c(
           rep(NA,dim(nData)[1]-ln), pred.train,pred.test) * rec$steps[[2]]$sds[target] + rec$steps[[1]]$means[target],
         envir = globalenv()
  )
  
  save_model_hdf5(
    model
    , Texcat("model.lstm.full.",numUnit,".",epoch,".",lag,".",pollutantNames[i]), overwrite = TRUE,
    include_optimizer = TRUE)
}

for (i in 1:4) {
  for (j in c(1,6)) {
    print(c(i,j))
    DoLSTMFull(pollutantNames[i], j, batch = 144,
               epoch = 1000, numUnit = 30, stop = 25, split = 0)
  }
}



GetRMSE <- function(target, lag) {
  t <- get(Texcat("mData.",lag))[,target] %>% tail(144)
  
  vec = data.frame(
    get(Texcat("prdic.lm.", lag, ".", target))%>% tail(144),
    get(Texcat("prdic.lm.acc.", lag, ".", target))%>% tail(144),
    get(Texcat("prdic.knn.", lag, ".", target))%>% tail(144),
    get(Texcat("prdic.ann.96.1000.", lag, ".", target))%>% tail(144),
    get(Texcat("prdic.lstm.96.1000.", lag, ".", target))%>% tail(144),
    get(Texcat("prdic.lm.full.", lag, ".", target))%>% tail(144),
    get(Texcat("prdic.lm.acc.full.", lag, ".", target))%>% tail(144),
    get(Texcat("prdic.knn.full.", lag, ".", target))%>% tail(144),
    get(Texcat("prdic.ann.full.30.1000.", lag, ".", target))%>% tail(144),
    get(Texcat("prdic.lstm.full.30.1000.", lag, ".", target))%>% tail(144)
  )
  
  print(
    c
    (
      rmse(vec[,1],t,na.rm = TRUE),
      rmse(vec[,2],t,na.rm = TRUE),
      rmse(vec[,3],t,na.rm = TRUE),
      rmse(vec[,4],t,na.rm = TRUE),
      rmse(vec[,5],t,na.rm = TRUE),
      rmse(vec[,6],t,na.rm = TRUE),
      rmse(vec[,7],t,na.rm = TRUE),
      rmse(vec[,8],t,na.rm = TRUE),
      rmse(vec[,9],t,na.rm = TRUE),
      rmse(vec[,10],t,na.rm = TRUE)
    ),digits = 3
  )
}

for (j in c(1,6)) {
  for (i in 1:4) {
    GetRMSE(pollutantNames[i],j)
  }
}

GetMAPE <- function(target, lag) {
  actual.test <- get(Texcat("mData.",lag))[,target] %>% tail(144)
  print(
    c
    (
      100*(mean(abs((get(Texcat("prdic.lm.", lag, ".", target)) %>% tail(144) - actual.test)/actual.test), na.rm = TRUE)),
      100*(mean(abs((get(Texcat("prdic.lm.acc.", lag, ".", target)) %>% tail(144) - actual.test)/actual.test), na.rm = TRUE)),
      100*(mean(abs((get(Texcat("prdic.knn.", lag, ".", target)) %>% tail(144) - actual.test)/actual.test), na.rm = TRUE)),
      100*(mean(abs((get(Texcat("prdic.ann.96.1000.", lag, ".", target)) %>% tail(144) - actual.test)/actual.test), na.rm = TRUE)),
      100*(mean(abs((get(Texcat("prdic.lstm.96.1000.", lag, ".", target)) %>% tail(144) - actual.test)/actual.test), na.rm = TRUE)),
      100*(mean(abs((get(Texcat("prdic.lm.full.", lag, ".", target)) %>% tail(144) - actual.test)/actual.test), na.rm = TRUE)),
      100*(mean(abs((get(Texcat("prdic.lm.acc.full.", lag, ".", target)) %>% tail(144) - actual.test)/actual.test), na.rm = TRUE)),
      100*(mean(abs((get(Texcat("prdic.knn.full.", lag, ".", target)) %>% tail(144) - actual.test)/actual.test), na.rm = TRUE)),
      100*(mean(abs((get(Texcat("prdic.ann.full.30.1000.", lag, ".", target)) %>% tail(144) - actual.test)/actual.test), na.rm = TRUE)),
      100*(mean(abs((get(Texcat("prdic.lstm.full.30.1000.", lag, ".", target)) %>% tail(144) - actual.test)/actual.test), na.rm = TRUE))
    )
    ,digits = 3
  )
}


for (j in c(1,6)) {
  for (i in 1:4) {
    GetMAPE(pollutantNames[i],j)
  }
}

GetIA <- function(target, lag) {
  t <- get(Texcat("mData.",lag))[,target] %>% tail(144)
  
  print(
    cbind
    (
    get(Texcat("prdic.lm.", lag, ".", target))%>% tail(144),
    get(Texcat("prdic.lm.acc.", lag, ".", target))%>% tail(144),
    get(Texcat("prdic.knn.", lag, ".", target))%>% tail(144),
    get(Texcat("prdic.ann.96.1000.", lag, ".", target))%>% tail(144),
    get(Texcat("prdic.lstm.96.1000.", lag, ".", target))%>% tail(144),
    get(Texcat("prdic.lm.full.", lag, ".", target)) %>% tail(144),
    get(Texcat("prdic.lm.acc.full.", lag, ".", target)) %>% tail(144),
    get(Texcat("prdic.knn.full.", lag, ".", target)) %>% tail(144),
    get(Texcat("prdic.ann.full.30.1000.", lag, ".", target)) %>% tail(144),
    get(Texcat("prdic.lstm.full.30.1000.", lag, ".", target))%>% tail(144)
  ) %>% apply(2, function(x) d(x, t, na.rm = TRUE))
  ,digits = 3
  )
}


for (j in c(1,6)) {
  for (i in 1:4) {
    GetIA(pollutantNames[i],j)
  }
}

PlotPredictions <- function(target, lag, thickness, dataset = "first") {
  
  actual <- cData[, target]
  predU <- data.frame(
    get(Texcat("prdic.lm.", lag, ".", target)),
    get(Texcat("prdic.lm.acc.", lag, ".", target)),
    get(Texcat("prdic.knn.", lag, ".", target)),
    get(Texcat("prdic.ann.96.1000.", lag, ".", target)),
    get(Texcat("prdic.lstm.96.1000.", lag, ".", target))
  )
  predK <- data.frame(
    get(Texcat("prdic.lm.full.", lag, ".", target)),
    get(Texcat("prdic.lm.acc.full.", lag, ".", target)),
    get(Texcat("prdic.knn.full.", lag, ".", target)),
    get(Texcat("prdic.ann.full.30.1000.", lag, ".", target)),
    get(Texcat("prdic.lstm.full.30.1000.", lag, ".", target))
  )
  

  ln <- dim(predU)[1]

  dfU <- data.frame(
    Time = t,
    Actual = actual,
    predU %>% apply(2, function(x) c(rep(NA, length(t) - ln), x))
  )%>% tail(144)
  dfK <- data.frame(
    Time = t,
    Actual = actual,
    predK %>% apply(2, function(x) c(rep(NA, length(t) - ln), x))
  )%>% tail(144)

  colnames(dfU) <- c("Time", "Actual", "Linear", "Linear with Lagged Inputs", "k-NN", "FANN", "LSTM-RNN")
  colnames(dfK) <- c("Time", "Actual", "Linear", "Linear with Lagged Inputs", "k-NN", "FANN", "LSTM-RNN")
  ylab <- unitText[which(colnames(cData) == target) - 2]

  thick = list(rep(0.5,6),rep(0.5,6))
  thick[[1]][thickness[1]+1] = 1
  thick[[2]][thickness[2]+1] = 1

  dfUQ = dfU[,-1] %>% melt(id = "Actual")
  dfKQ = dfK[,-1] %>% melt(id = "Actual")
  dfUT = dfU  %>% melt(id = "Time")
  dfKT = dfK  %>% melt(id = "Time")

  graph1 =
    ggplot(dfUQ) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(aes(x=Actual,y=value,colour=variable,shape=variable), stroke = 0.7)+
    scale_shape_manual(values = 2:6)+
    scale_colour_manual(values = hcl(seq(15,315,60)[-1],100, 65)) +
    BasicThemeNoLegend + labs(y = "Predicted")
  
  graph2 <-
    ggplot(dfUT) +
    geom_line(aes(x = Time, y = value, color = variable, size = variable)) +
    geom_point(aes(x = Time, y = value, color = variable, shape = variable), stroke = 0.7) +
    scale_shape_manual(values = 1:6) +
    scale_colour_manual(values = hcl(seq(15,315,60),100, 65)) +
    scale_size_manual(values = thick[[1]]) +
    scale_x_datetime(labels = date_format("%H:%M", tz = Sys.timezone())) +
    BasicThemeNoLegend + labs(y = ifelse(target != "PM2.5", ylab, unitText[5]))

  graph3 =
    ggplot(dfKQ) +
    geom_point(aes(x=Actual,y=value,colour=variable,shape=variable), stroke = 0.7)+
    scale_shape_manual(values = 2:6)+
    scale_colour_manual(values = hcl(seq(15,315,60)[-1],100, 65)) +
    geom_abline(intercept = 0, slope = 1) +
    BasicThemeNoLegend + labs(y = "Predicted")
  
  graph4 <-
    ggplot(dfKT) +
    geom_line(aes(x = Time, y = value, color = variable, size = variable)) +
    geom_point(aes(x = Time, y = value, color = variable, shape = variable), stroke = 0.7) +
    scale_shape_manual(values = 1:6) +
    scale_colour_manual(values = hcl(seq(15,315,60),100, 65)) +
    scale_size_manual(values = thick[[2]]) +
    scale_x_datetime(labels = date_format("%H:%M", tz = Sys.timezone())) +
    BasicThemeNoLegend + labs(y = ifelse(target != "PM2.5", ylab, unitText[5]))
  return(list(graph1, graph2,graph3,graph4))
}

PlotFourGraphs = function(gList,name){
  graphGrid = cowplot::plot_grid(gList[[1]],gList[[2]],gList[[3]],gList[[4]], align="v", ncol = 2)
  legend <- cowplot::get_legend(gList[[2]] + theme(legend.title = element_blank(), legend.text = element_text(size = rel(0.95),margin = margin(r = 6)),legend.position = "bottom")
                                + guides(shape = guide_legend(nrow = 1), col = guide_legend(nrow = 1)))
  graph <- cowplot::plot_grid(graphGrid, legend, ncol = 1, rel_heights = c(36, 1))
  ggsave(Texcat(name, ".png"), width = 11, height = 6.9, dpi = 200, units = "in")
}

# PlotFourPollutants <- function(gList, name, dataset = "first") {
#   if (dataset == "first") {
#     graphGrid <- cowplot::plot_grid(gList[[1]], gList[[2]], gList[[3]], gList[[4]],
#                                     gList[[5]], gList[[6]], gList[[7]], gList[[8]],align = "v", ncol = 2)
#     legend <- cowplot::get_legend(gList[[2]] + theme(legend.title = element_blank(), legend.text = element_text(size = rel(0.95),margin = margin(r = 6)),legend.position = "bottom")
#                                   + guides(shape = guide_legend(nrow = 1), col = guide_legend(nrow = 1)))
#     
#     graph <- cowplot::plot_grid(graphGrid, legend, ncol = 1, rel_heights = c(36, 1))
#     ggsave(Texcat(name, ".png"), width = 11, height = 12.6, dpi = 200, units = "in")
#   }
#   else{
#     graphGrid <- cowplot::plot_grid(gList[[1]], gList[[2]], gList[[3]], gList[[4]],align = "v", ncol = 2)
#     legend <- cowplot::get_legend(gList[[2]] + theme(legend.title = element_blank(), legend.text = element_text(size = rel(0.95),margin = margin(r = 6)),legend.position = "bottom")
#                                   + guides(shape = guide_legend(nrow = 1), col = guide_legend(nrow = 1)))
#     
#     graph <- cowplot::plot_grid(graphGrid, legend, ncol = 1, rel_heights = c(18, 1))
#     ggsave(Texcat(name, ".png"), width = 11, height = 6.4, dpi = 200, units = "in")
#   }
# }

thickness = list(
  list(
    c(5,1),
    c(5,1),
    c(5,1),
    c(1,1)
  ),
  list(
    c(5,1),
    c(5,2),
    c(5,2),
    c(2,2)
  )
)

for (j in c(1,6)) {
  for (i in 1:4) {
    PlotFourGraphs(PlotPredictions(pollutantNames[i],j,thickness[[(j-1)%%2+1]][[i]]),Texcat("pred.",j,".",pollutantNames[i]))
  }
  # PlotFourPollutants(list(graph1[[1]],graph1[[2]],
  #                         graph2[[1]],graph2[[2]],
  #                         graph3[[1]],graph3[[2]],
  #                         graph4[[1]],graph4[[2]]), Texcat("pred.lag",j))
}


int = seq(as.POSIXct("2017/05/30 00:00:00"),as.POSIXct("2017/06/06 00:00:00"),by = "1 hour")
CLData = data.frame(unique(cut(t,int)),aggregate(cData[,3:8],list(cut(t,int)),mean)%>% .[,-1],aggregate(cData[,9:10],list(cut(t,int)),sum)%>% .[,-1])
write.csv(CLData,"CLData.csv")

PolAve = CLData[,2:5] %>% apply(2, min)
WD = c(330,300,270,270,270,270,250,230,210,170,130,90,60,30,0,13,26,40,20,0,340,23,67,110)
WDSD = c(20,20,10,5,5,6.67,13.33,13.33,20,26.67,26.67,23.33,20,20,5.67,8.67,9,5,13.33,13.33,7.67,29,29,23.33)
TrafficSum = CLData[,8] + CLData[,9]
CL4Df = data.frame(CLData[,6:7] %>% tail(24), round(TrafficSum/6) %>% tail(24), WD,WDSD)

PollutantNamesLong <- c("1CO", "2Nitrogen Dioxide", "Nan", "4Particulates")
PollutantWights = c(28,46,0,0)
for (i in 1:4) {
  for (j in 1:24) {
    if (i == 1 | i == 4) {
      name <- pollutantNames[i]
      str <- Texcat(
        "CL4\n", PollutantNamesLong[i], "\n400 ", PollutantWights[i],
        " 0 0 1 1 1 1 1 0\n1\n-250 2 1.8\nA\n1 -500 0 0 0 0 28 0 0 0\n11101Hour 1\n")
      if (i == 1) {
        txt = Texcat(str, CL4Df[j,3], "\n20.8\n", CL4Df[j,4], " ", max(0.5,CL4Df[j,2]), " 7 1000 ",
                     CL4Df[j,5], " ", PolAve[i], " ", CL4Df[j,1])
      }else if (i==4) {
        txt = Texcat(str, CL4Df[j,3], "\n0.12\n", CL4Df[j,4], " ", max(0.5,CL4Df[j,2]), " 7 1000 ",
                     CL4Df[j,5], " ", PolAve[i], " ", CL4Df[j,1])
      }
      cat(txt, file = Texcat(pollutantNames[i],"_",j,".dat"))
      txt = ""
    }
  }
}
COCL4 = c(3,3,7,5,5,6,9,14,10,12,16,22,8,6,6,6,11,10,8,5,5,4,4,3)*0.1
COLSTM = prdic.lm.full.1.CO %>% tail(144) %>% matrix(6) %>% colMeans
COActual = CLData[,2] %>% tail(24)

df = data.frame(COActual,COLSTM,COCL4)
colnames(df) = c("Actual","LSTM-RNN","CALINE4")
mape(df[,1],df[,2])
mape(df[,1],df[,3])

dfa =  df %>% melt(id = "Actual")

graph1 = ggplot(dfa) + geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(x = Actual, y = value, colour = variable, shape = variable)) +
  theme_bw() + theme(axis.text = element_text(size = rel(0.95),margin = margin(r = 6)),
                     legend.position = "bottom") + 
  labs(x = "Actual Value", y = "Predicted/Estimated") +
  guides(colour=guide_legend(title="Model"),shape =guide_legend(title="Model"))

df = data.frame(int%>%tail(25),rbind(df,rep(NA,3))) 
colnames(df)[1] = "Time"
dfb = df %>% melt(id = "Time")

graph2 = ggplot(dfb) + 
  geom_line(aes(x = Time, y = value, colour = variable)) +
  geom_point(aes(x = Time, y = value, colour = variable, shape = variable), stroke = 0.7) +
  theme_bw() + theme(axis.text = element_text(size = rel(0.95),margin = margin(r = 6)),
                     legend.position = "bottom") + 
  labs(x = "Time", y = "CO (ppm)") +
  scale_x_datetime(labels = date_format("%H:%M", tz = Sys.timezone())) +
  guides(colour=guide_legend(title="Model"),shape =guide_legend(title="Model"))

graph <- cowplot::plot_grid(graph1,graph2, align="v", ncol = 2)
ggsave("CALINE4.png", width = 11, height = 4, dpi = 200, units = "in")
