###IGF meta-analysis###
install.packages('effectsize')
install.packages('metafor')
install.packages('forestplot')
install.packages('tidyverse')

library(metafor)
library(effectsize)
library(forestplot)
library(tidyverse)

setwd("C:/FISH/Manuscript_Working/OLD_IGF/data_files/IGF_data/")
getwd()

data = read.csv(file.choose())
datum = datum[, -2]
datum = escalc(measure="SMDH", n1i=treat_n, m1i=treat_mean, sd1i=treat_sd, n2i=ctrl_n, m2i=ctrl_mean, sd2i=ctrl_sd, data=data)

write.csv(datum, "data_SMDH.csv")

datum_OR = d_to_oddsratio(datum$yi)

datum_df = cbind(datum, datum_OR)

datum_final = rma.uni(yi, vi, method = "REML", data = datum_df, test = "adhoc", )

View(datum_final$data)

setDT(datum_final$data)[, lapply(.SD, sum, na.rm=TRUE), Names]

datum_final$data = datum_final$data[,-2]
datum_final$data %>% group_by(Paper, across(summarise_each(sum)))

sink("datum_final.txt")
on.exit(close(sink))  
datum_final

sink("datum_finaldata.csv")
on.exit(close(sink))  
datum_final$data

###Graphing and Stats###
  
png(filename = "IGF_forest.png")
  forest.rma(datum_final, addfit = TRUE, showweights = TRUE, header = TRUE, order = "obs")
dev.off()

png(filename = "IGF_funnel.png")
  funnel(datum_final)
dev.off()

sink("IGF_tests.txt")
datum_final
leave1out(datum_final)
influence(datum_final)
base_FSN = fsn(yi, vi, data=datum_df, weighted=TRUE, target=log(0.95))
  print("Fail-safe N")
  print(base_FSN$fsnum)
  print(base_FSN$pval)
  print(base_FSN$alpha)
datfin_tf = trimfill(datum_final)
  print("trim and fill analysis")
  print(datfin_tf$se)
  print(datfin_tf$pval)
  print(datfin_tf$tau2)
  print(datfin_tf$H2)
regtest(datum_final)
on.exit(close(sink()))


datum_df |> forestplot()
