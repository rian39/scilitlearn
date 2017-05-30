
library(devtools)
check()
document()
install()

library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)

library(scilitlearn)
setwd('~/R/scilitlearn/')
wos = load_data('inst/extdata/sample.tsv')
year_count(wos)
authors_top(wos)
title_words(wos)
tb1 = title_bigrams(wos)  
title_bigrams_separated(wos)  
tb = title_bigrams_filtered(wos)  
title_bigrams_count(wos) 
kw = keywords(wos, TRUE)
keyword_count(wos)  
keyword_count_top(wos, 2)
cr = cited_references_gather(wos)  
head(cr)
cited_reference_count(wos)  
field_tfidf(wos)
tfidf  = field_tfidf(wos)
tfidf  %>% arrange(-tf_idf)
abstract_topics(wos, 2,c('information', 'web', 'sl')) 
search_title_abstract_keywords(wos, 'infrastructure', TRUE) 
res = latex_format(wos[20:30,])
res
convert_to_bib('inst/extdata', 'test5.bib')
system('wc -l test5.bib')
wos_words = words_all_ranked_frequencies(wos, 'AB', TRUE)
head(wos_words)
tfidf_plot(tfidf(wos_words), 30) 



