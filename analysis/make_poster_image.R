library(demgsa)
library(ggplot2)
library(dplyr)

snapper <- mp.read.results("demographic_models/Demographics-Snapping Turtles/Snapping-Turtle-Metapop.mp")
snapper_highdisp <- mp.read.results("demographic_models/Demographics-Snapping Turtles/Snapping-Turtle-Metapop-highdisp.mp")
snapper_lowdisp <- mp.read.results("demographic_models/Demographics-Snapping Turtles/Snapping-Turtle-Metapop-lowdisp.mp")

snapper_res <- snapper$mp.file$results$PopAll
snapper_highdisp_res <- snapper_highdisp$mp.file$results$PopAll
snapper_lowdisp_res <- snapper_lowdisp$mp.file$results$PopAll

snapper_res$Scenario <- "Current"
snapper_res$Time <- 1:nrow(snapper_res)
snapper_highdisp_res$Scenario <- "High_Disp"
snapper_highdisp_res$Time <- 1:nrow(snapper_highdisp_res)
snapper_lowdisp_res$Scenario <- "Low_Disp"
snapper_lowdisp_res$Time <- 1:nrow(snapper_lowdisp_res)

snapper_res_all <- rbind(snapper_res, snapper_highdisp_res, snapper_lowdisp_res)

ggplot(data = snapper_res_all, 
       aes(x = Time, y = Mean, 
           ymin = Mean - StDev, ymax = Mean + StDev)) +
  geom_ribbon(aes(fill = Scenario), alpha= 0.2) +
  geom_line(aes(color = Scenario)) +
  ylab("Metapopulation Size") +
  theme_bw() +
  theme(text=element_text( size=36, face="bold" ))
ggsave(filename = "Scenarios.pdf", device = "pdf", width = 13, height = 8, units = "in")
