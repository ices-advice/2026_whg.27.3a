
library(icesTAF)
library(stockassessment)
library(rmarkdown)

mkdir("report")
cp("boot/initial/report/*", "report/")


icesTAF::msg("Report: Making catch/assessment presentation")
render("report/whg.27.3a_advice_rfb.Rmd")


