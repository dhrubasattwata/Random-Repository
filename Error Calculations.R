library(Metrics)
library(readxl)

Irish_IS <- read_excel("C:/Users/dhchoudhury/Desktop/Citi ICG/Test Plan/Test Plan for 133283/NEW TEST PLAN/RMSE_MAPE_Test #20.xlsx","Ireland In-Sample")
Irish_OS <- read_excel("C:/Users/dhchoudhury/Desktop/Citi ICG/Test Plan/Test Plan for 133283/NEW TEST PLAN/RMSE_MAPE_Test #20.xlsx","Ireland Out-Sample")

Italy_IS <- read_excel("C:/Users/dhchoudhury/Desktop/Citi ICG/Test Plan/Test Plan for 133283/NEW TEST PLAN/RMSE_MAPE_Test #20.xlsx","Italy In-Sample")
Italy_OS <- read_excel("C:/Users/dhchoudhury/Desktop/Citi ICG/Test Plan/Test Plan for 133283/NEW TEST PLAN/RMSE_MAPE_Test #20.xlsx","Italy Out-Sample")

Spain_LS <- read_excel("C:/Users/dhchoudhury/Desktop/Citi ICG/Test Plan/Test Plan for 133283/NEW TEST PLAN/RMSE_MAPE_Test #20.xlsx","Spain_LS")
Spain_LR <- read_excel("C:/Users/dhchoudhury/Desktop/Citi ICG/Test Plan/Test Plan for 133283/NEW TEST PLAN/RMSE_MAPE_Test #20.xlsx","Spain_LR")

Portugal_LS <- read_excel("C:/Users/dhchoudhury/Desktop/Citi ICG/Test Plan/Test Plan for 133283/NEW TEST PLAN/RMSE_MAPE_Test #20.xlsx","Portugal_LS")
Portugal_LR <- read_excel("C:/Users/dhchoudhury/Desktop/Citi ICG/Test Plan/Test Plan for 133283/NEW TEST PLAN/RMSE_MAPE_Test #20.xlsx","Portugal_LR")

### Spain Error Metrics ###

a1 <- mape(Spain_LS$Actual_LS, Spain_LS$Model_LS)
a2 <- rmse(Spain_LS$Actual_LS, Spain_LS$Model_LS)

b1 <- mape(Spain_LR$Actual_LR, Spain_LR$Model_LR)
b2 <- rmse(Spain_LR$Actual_LR, Spain_LR$Model_LR)

e_m_spain <- data.frame(rbind(a1,a2), rbind(b1,b2))
rownames(e_m_spain) <- c("MAPE", "RMSE")
colnames(e_m_spain) <- c("Loss Severity", "Liquidation Rate")

### Portugal Error Metrics ###

a1 <- mape(Portugal_LS$Actual_LS, Portugal_LS$Model_LS)
a2 <- rmse(Portugal_LS$Actual_LS, Portugal_LS$Model_LS)

b1 <- mape(Portugal_LR$Actual_LR, Portugal_LR$Model_LR)
b2 <- rmse(Portugal_LR$Actual_LR, Portugal_LR$Model_LR)

e_m_portugal <- data.frame(rbind(a1,a2), rbind(b1,b2))
rownames(e_m_portugal) <- c("MAPE", "RMSE")
colnames(e_m_portugal) <- c("Loss Severity", "Liquidation Rate")

##### Ireland In-Sample

a1 <- mape(Irish_IS$`Moodys D90`,Irish_IS$`model D90`)
a2 <- mape(Irish_IS$`Moodys CPR`,Irish_IS$`model CPR`)
# a3 <- mape(Irish_IS$`Moodys 60-90`,Irish_IS$`avg roll rate`)
a4 <- mape(Irish_IS$`Moodys loss`, Irish_IS$`model loss`)

b1 <- rmse(Irish_IS$`Moodys D90`,Irish_IS$`model D90`)
b2 <- rmse(Irish_IS$`Moodys CPR`,Irish_IS$`model CPR`)
# b3 <- rmse(Irish_IS$`Moodys 60-90`,Irish_IS$`avg roll rate`)
b4 <- rmse(Irish_IS$`Moodys loss`, Irish_IS$`model loss`)

Ir_IS_em <- data.frame(rbind(a1,a2,a4),rbind(b1,b2,b4))
colnames(Ir_IS_em) <- c("MAPE_IS", "RMSE_IS")
rownames(Ir_IS_em) <- c("D90","CPR","loss")


##### Ireland Out-Sample

c1 <- mape(Irish_OS$`Moodys D90`,Irish_OS$`model D90`)
c2 <- mape(Irish_OS$`Moodys CPR`,Irish_OS$`model CPR`)
# c3 <- mape(Irish_OS$`Moodys 60-90`,Irish_OS$`avg roll rate`)
c4 <- mape(Irish_OS$`Moodys loss`, Irish_OS$`model loss`)

d1 <- rmse(Irish_OS$`Moodys D90`,Irish_OS$`model D90`)
d2 <- rmse(Irish_OS$`Moodys CPR`,Irish_OS$`model CPR`)
# d3 <- rmse(Irish_OS$`Moodys 60-90`,Irish_OS$`avg roll rate`)
d4 <- rmse(Irish_OS$`Moodys loss`, Irish_OS$`model loss`)

Ir_OS_em <- data.frame(rbind(c1,c2,c4),rbind(d1,d2,d4))
colnames(Ir_OS_em) <- c("MAPE_OoS", "RMSE_OoS")
rownames(Ir_OS_em) <- c("D90","CPR","loss")


## Ireland Error Metric
Ir_EM <- data.frame(Ir_IS_em,Ir_OS_em)
View(Ir_EM)

########################################
########################################
##### Italy In-Sample

a1 <- mape(Italy_IS$moodyD90,Italy_IS$`model d90`)
a2 <- mape(Italy_IS$`Moodys CPR`,Italy_IS$`model CPR`)
# a3 <- mape(Italy_IS$`Moodys 60-90`,Italy_IS$`avg roll rate`)
a4 <- mape(Italy_IS$`moodys cumu recovery`, Italy_IS$`model cumu recovery`)

b1 <- rmse(Italy_IS$moodyD90,Italy_IS$`model d90`)
b2 <- rmse(Italy_IS$`Moodys CPR`,Italy_IS$`model CPR`)
# b3 <- rmse(Italy_IS$`Moodys 60-90`,Italy_IS$`avg roll rate`)
b4 <- rmse(Italy_IS$`moodys cumu recovery`, Italy_IS$`model cumu recovery`)

It_IS_em <- data.frame(rbind(a1,a2,a4),rbind(b1,b2,b4))
colnames(It_IS_em) <- c("MAPE_IS", "RMSE_IS")
rownames(It_IS_em) <- c("D90","CPR","Cumulative Recovery")


##### Italy Out-Sample

c1 <- mape(Italy_OS$moodyD90,Italy_OS$`model d90`)
c2 <- mape(Italy_OS$`Moodys CPR`,Italy_OS$`model CPR`)
# c3 <- mape(Italy_OS$`Moodys 60-90`,Italy_OS$`avg roll rate`)
c4 <- mape(Italy_OS$`moodys cumu recovery`, Italy_OS$`model cumu recovery`)

d1 <- rmse(Italy_OS$moodyD90,Italy_OS$`model d90`)
d2 <- rmse(Italy_OS$`Moodys CPR`,Italy_OS$`model CPR`)
# d3 <- rmse(Italy_OS$`Moodys 60-90`,Italy_OS$`avg roll rate`)
d4 <- rmse(Italy_OS$`moodys cumu recovery`, Italy_OS$`model cumu recovery`)

It_OS_em <- data.frame(rbind(c1,c2,c4),rbind(d1,d2,d4))
colnames(It_OS_em) <- c("MAPE_OoS", "RMSE_OoS")
rownames(It_OS_em) <- c("D90","CPR","Cumulative Recovery")


## Italy Error Metric
It_EM <- data.frame(It_IS_em,It_OS_em)
View(It_EM)

