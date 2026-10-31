# Set working directory
setwd("C:/Users/chris/OneDrive/Documents/UH/Ch2Fluorometer/R")
library(tidyverse)
library(ggplot2)
library(paletteer)
library(patchwork)

instr <- read.csv("~/UH/Ch2Fluorometer/instrument_hourly_allyears_noblanks.csv")



ggplot(instr, aes(x=OB, y=Tryp, color=Sal)) + geom_point(alpha=0.5) + 
  stat_ellipse() + scale_y_log10() + scale_x_log10() +
  scale_color_gradientn(
    colors = c( "green3","aquamarine2", "dodgerblue","mediumpurple"),
    values = scales::rescale(c(2,25,33,35)),
    limits = c(2, 35)
  ) + theme_minimal()

ggplot(instr, aes(x=OB, y=Turb, color=Sal)) + geom_point(alpha=0.5) + 
  stat_ellipse() + scale_y_log10() + scale_x_log10() +
  scale_color_gradientn(
    colors = c( "green3","aquamarine2", "dodgerblue","mediumpurple"),
    values = scales::rescale(c(2,25,33,35)),
    limits = c(2, 35)
  ) + theme_minimal()

ggplot(instr, aes(x=OB, y=Chl_c3, color=Sal)) + geom_point(alpha=0.5) + 
  stat_ellipse() + scale_y_log10() + scale_x_log10() +
  scale_color_gradientn(
    colors = c( "green3","aquamarine2", "dodgerblue","mediumpurple"),
    values = scales::rescale(c(2,25,33,35)),
    limits = c(2, 35)
  ) + theme_minimal()

ggplot(instr, aes(x=Tryp, y=Chl_c3, color=Sal)) + geom_point(alpha=0.5) + 
  stat_ellipse() + scale_y_log10() + scale_x_log10() +
  scale_color_gradientn(
    colors = c( "green3","aquamarine2", "dodgerblue","mediumpurple"),
    values = scales::rescale(c(2,25,33,35)),
    limits = c(2, 35)
  ) + theme_minimal()

#(p1 | p2) / (p3 |p4)

#load brightener and tryptophan

brightener <- read.csv("~/UH/Ch2Fluorometer/br_lab_field_new.csv")
tryptophan <- read.csv("~/UH/Ch2Fluorometer/tr_lab_field_new.csv")

cor.test(brightener$br_field, brightener$br_lab)
cor.test(tryptophan$tr_field, tryptophan$tr_lab)

ggplot(brightener, aes(x=br_field, y=br_lab, color=sal, shape=factor(rain))) + geom_point(size=3) + 
  scale_color_gradientn(
    colors = c( "green3","aquamarine2", "dodgerblue","mediumpurple"),
    values = scales::rescale(c(2,25,33,35)),
    limits = c(2, 35)
  )+ 
  scale_y_log10() + scale_x_log10()


#for this plot I removed the ellipse since it is not significant
ggplot(tryptophan, aes(x=tr_field, y=tr_lab, color=sal, shape=factor(rain))) + geom_point(size=3) + 
  scale_color_gradientn(
    colors = c( "green3","aquamarine2", "dodgerblue","mediumpurple"),
    values = scales::rescale(c(2,25,33,35)),
    limits = c(2, 35)
  )+
  scale_y_log10() + scale_x_log10()


#load entero and sucralose data
entsuc <- read.csv("~/UH/Ch2Fluorometer/entero_sucralose_sal_rain.csv")
cor.test(entsuc$entero, entsuc$sucralose)

ggplot(entsuc, aes(x=entero, y=sucralose, color=sal, shape=factor(rain))) + geom_point(size=3) + 
  scale_color_gradientn(
    colors = c( "green3","aquamarine2", "dodgerblue","mediumpurple"),
    values = scales::rescale(c(2,25,33,35)),
    limits = c(2, 35)
  )+
  scale_y_log10() + scale_x_log10()

entsuc2 <- read.csv("~/UH/Ch2Fluorometer/ent_suc_bothdetect.csv")
cor.test(entsuc2$entero, entsuc2$sucralose)

entob <- read.csv("~/UH/Ch2Fluorometer/entero_optbr.csv")
cor.test(entob$entero, entob$ob)

ggplot(entob, aes(x=entero, y=ob, color=sal, shape=factor(rain))) + geom_point(size=3) + 
  scale_color_gradientn(
    colors = c( "green3","aquamarine2", "dodgerblue","mediumpurple"),
    values = scales::rescale(c(2,25,33,35)),
    limits = c(2, 35)
  )+
  scale_y_log10() + scale_x_log10() + stat_ellipse()

ggplot(entob, aes(x=entero, y=ob, color=sal)) + geom_point(size=3) + 
  scale_color_gradientn(
    colors = c( "green3","aquamarine2", "dodgerblue","mediumpurple"),
    values = scales::rescale(c(2,25,33,35)),
    limits = c(2, 35)
  )+
  scale_y_log10() + scale_x_log10() + stat_ellipse()
