
library(ggplot2)
t = cited_reference_count(wos)  
tc = t %>% group_by(ref) %>% mutate(cu = cumsum(n))
tct = tc %>% top_n(n=20, wt = cu)
ggplot(tct, aes(x=as.Date(PY, format = '%Y'), y=n)) + geom_col() + facet_wrap(~ref) + scale_x_date()
