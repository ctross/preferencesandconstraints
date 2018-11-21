set.seed(1)
GGt_m1<-traceplot(fit_Trunc,pars=sample(names(fit_Trunc@sim$samples[[1]]),30))
ggsave("TraceTrunc.pdf",GGt_m1,width=11,height=8.5)


