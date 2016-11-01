library(tidyverse)
library(apaTables)
library(cocor)
library(predictionInterval)

#Load Data
bfi <- read_csv("bfi2.csv")

#1
cocor(~A1+C1|E1+O1, data=as.data.frame(bfi))

#2
cocor(~A1+C1|A1+E1, data=as.data.frame(bfi))

#3
bfi_men <- bfi %>% filter(gender==1) %>% select(-gender)
bfi_women <- bfi %>% filter(gender==2) %>% select(-gender)

apa.cor.table(bfi_men)
apa.cor.table(bfi_women)

bfi_men <- as.data.frame(bfi_men)
bfi_women <- as.data.frame(bfi_women)

cocor(~A1+E1|A1+E1, data=list(bfi_men, bfi_women))

#4 Rating-raises differs from Rating-critical 
r.jk <- .59
r.jh <- .16
r.kh <- .38
n <- 30

cocor.dep.groups.overlap(r.jk, r.jh, r.kh, n)

##Comparing Rating-Raises correlation with complaints-Critical Correlation
r.jk <- .59
r.hm <- .19
r.jh <- .83
r.jm <- .16
r.kh <- .67
r.km <- .38
n <- 30

cocor.dep.groups.nonoverlap(r.jk, r.hm, r.jh, r.jm, r.kh, r.km, n)

## run a replication of the study
pi.r(r=.43,n=30,rep.n=100)

## figure out replication sample size with PI less than 0.5 difference
pi.r(r=.43,n=30,rep.n=100)
pi.r(r=.43,n=30,rep.n=1000)
pi.r(r=.43,n=30,rep.n=10000)

## Comparing correlations from 2 papers
r1.jk <- .43
r2.hm <- .10
n1=30
n2=1000

cocor.indep.groups(r1.jk, r2.hm, n1, n2)
