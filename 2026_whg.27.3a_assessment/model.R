library(DATRAS)
library(icesTAF)
library(dplyr)

ly <- 2025

mkdir("model")

# ------------------------------------------------------------------------------
# load all inputs to get the f from the rfb-rule and implements   
load("input/input.RData")

# ------------------------------------------------------------------------------
# length-based values (fixed over time)
Lc   <- 9.0    # based on NSSK working document in 2026
Linf <- 69.7   # based on NSSK working document in 2026

# ------------------------------------------------------------------------------
# estimate f

#
# get lengths commercial fish - IC
ltemp <- lfs[[1]]
IC <- data.frame(length = ltemp$length, freq = ltemp$freq, year =2021+1)

id_end <- length(lfs)
for(j in c(2:id_end)){ 
  ltemp <- lfs[[j]]
  ltemp <- data.frame(length = ltemp$length, freq = ltemp$freq, year =2021+j)
  IC <- rbind(IC,ltemp)   
}

# get weighted mean of IC
IC_sub <- subset(IC,IC$length > Lc*10)
IC_sub$length <- IC_sub$length * IC_sub$freq
IC_sub <- aggregate(cbind(length,  freq) ~ year, data = IC_sub, sum)
IC_sub$weighted <- IC_sub$length / IC_sub$freq
IC_sub$length <- IC_sub$weighted/10

# now get the lengths from the surveys (to approximate industrial fisheries)


# get mean from survey IBTS Q3
x <- d
x <- subset(x,Survey %in% c("NS-IBTS") & Quarter %in% c(3))
HL <- x[["HL"]]
HL <- subset(HL, !(is.na(HL$Count)))

surv_sub <- data.frame(length = rep(HL$LngtCm,round(HL$Count,digits=0)),
                       year = rep(HL$Year,round(HL$Count,digits=0)),
                       Source= "Survey",Weight=1)

surv_sub <- subset(surv_sub,surv_sub$length > Lc)
dat1 <- aggregate(length~year,surv_sub,FUN= mean)
dat1$year <- as.numeric(as.character(dat1$year))
dat1 <- subset(dat1, dat1$year  > 2021)

# BITS Q1 and Q4
x <- d
x <- subset(x,Survey %in% c("BITS") & Quarter %in% c(1,4))
HL <- x[["HL"]]
HL <- subset(HL, !(is.na(HL$Count)))

surv_sub2 <- data.frame(length = rep(HL$LngtCm,round(HL$Count,digits=0)),
                        year = rep(HL$Year,round(HL$Count,digits=0)),
                        Source= "Survey",Weight=1)

surv_sub2 <- subset(surv_sub2,surv_sub2$length > Lc)
dat2 <- aggregate(length~year,surv_sub2,FUN= mean)
dat2$year <- as.numeric(as.character(dat2$year))
dat2 <- subset(dat2,dat2$year  > 2021)

dattot <- data.frame(year= dat1$year, length_survey=(dat1$length + dat2$length)/2)

# now get the relative proportion of commercial versus industrial for each year


dk <- alldat %>% bind_rows() %>%
  filter(Country == "Denmark", Catch.Cat. == "Landings") %>%
  mutate(use = ifelse(group == "MIS", "IBC", "HC") ) %>%
  group_by(Year, use) %>%
  summarise(catch = sum(Catch..kg) / 1000) %>%
  reshape2::dcast( Year~use, value.var = "catch", sum) %>% 
  mutate(Total = IBC + HC)
others <- alldat %>% bind_rows() %>%
  filter(Country != "Denmark", Catch.Cat. == "Landings") %>%
  group_by(Year, Country) %>%
  summarise(catch = sum(Catch..kg) / 1000) %>%
  reshape2::dcast( Year~Country, value.var = "catch", sum) %>% 
  transmute(Norway, Sweden, Others = Germany + Netherlands)
landings <- bind_cols(dk, others) %>%
  mutate(Landings = Total + Norway + Sweden + Others)  
discards <- alldat %>% bind_rows() %>%
  filter(Catch.Cat. == "Discards") %>%
  group_by(Year) %>%
  summarise(Discards = sum(Catch..kg) / 1000) %>%
  transmute(Discards)
OC <- bind_cols(landings, discards) %>% 
  mutate(Catch = Landings + Discards)

fr_ind <- data.frame(year= OC$Year, rel_p = OC$IBC/OC$Catch)
fr_ind <- subset(fr_ind,fr_ind$year %in% dattot$year)
dattot$fr_IBC <- fr_ind$rel_p
dattot <- cbind(dattot,IC_sub[match(dattot$year,IC_sub$year),c("length")])
colnames(dattot)[ncol(dattot)] <- "length_IC"

dattot$meanL <- dattot$length_IC * (1-dattot$fr_IBC) + dattot$length_survey * dattot$fr_IBC 

dattot$f <- dattot$meanL/ (0.75*Lc + 0.25 * Linf)

save(dattot,Lc,Linf, file= paste0("model/length_outputs_rfb_2022_",ly,".Rdata"))
