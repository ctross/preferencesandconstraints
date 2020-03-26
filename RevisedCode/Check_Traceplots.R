set.seed(1)
GGt_m1<-traceplot(fit_Basic,pars= sample(names(fit_Basic@sim$samples[[1]])[c(1:150,54832:54865)],30)
)
ggsave("Trace_SRM.pdf",GGt_m1,width=11,height=8.5)


