
## Mon 15 May 2017 10:09:06 BST
- DONE:building package, but library dependencies don't seem to work properly ... Fixed it -- need to reference imported packages using package:: syntax
- found bibliometrix package on cran -- has many useful functions, but not in tidytext way

## Mon 15 May 2017 14:31:48 BST
-  chaning R version depends on in DESCRIPTION file

## Tue 16 May 2017 11:19:12 BST
- working out how to use functions from other packages. It seems like a lot of work ... to have to list them all in @import statements 

## Wed 17 May 2017 23:03:01 BST
- did more on the import stuff. @import pkg seems better than @importFrom pkg fn
- still showing errors - need to sort this out DONE

## Fri 19 May 2017 23:17:07 BST
- breaking file into separate areas -- authors, titles, keywords, abstracts, etc and testing these

## Sat 20 May 2017 09:30:38 BST
- worked on topic models for abstracts. Need to work out a way to add stopwords specific to the area of analysis 
- TODO: oxygenise abstracts

## Sat 20 May 2017 14:27:32 BST
- fixing up keyword counts so that I can use them alongside the abstracts. Need to be able to join on the ids 
f also cleaning up the topic models again
- all that was fine. But now installation is going very slow. I did add licence, and a couple of other things to DESCRIPTION, etc. Maybe I've broken something there. Or it could be to do with writing in a tunnel. 
- it eventually installs and seems to be working ok

## Wed 24 May 2017 12:58:46 BST
- adding simple search function for title/abstract/keywords; its in R/search.r 

## Wed 24 May 2017 22:48:12 BST
- TODO: add functions to turn search results in [@author_year] citations;  

## Thu 25 May 2017 17:28:10 BST
- extending search function a bit to make results more viewable ... 

## Fri 26 May 2017 09:17:18 BST
- writing function that returns latex reference  -- this is meant to feed into my writing workflow better ... 
- put the build package stuff into test_package.r so that can run and install.
- find a script from piwowar to convert isi csv to bibtex file; added that to exec/wos_to_bib.py, and moved data to inst/extdata/sample.tsv 
- added that script
- could probably do a lot with refmanageR package

## Mon 29 May 2017 22:23:14 BST
- fixing the tdidf functions that I added on monday

## Wed 07 Jun 2017 13:01:54 BST
- not sure what needs to be done next -- could integrate database stuff; not sure if I have any use for the network stuff? 
- wasted quite a lot of time trying to turn field names into variables. Works fine for select, but couldn't get it happening for unnest_tokens in tidytext ... 

## Thu 08 Jun 2017 08:01:07 BST
- fixing the convert to bib function;  

## Fri 09 Jun 2017 08:22:48 BST
- need to be able to save selected refs as bib; 
- added function to do this to bib_format, and also changed search to return the whole WoS record

## Sat 10 Jun 2017 11:38:40 BST
- would be good to get the trends in words easily visible http://varianceexplained.org/r/hn-trends/ for titles, keywords, or abstracts.
- the simplest case would be keywords  -- set up column with PY and PM, and count DE by month

## Wed 14 Jun 2017 09:55:03 BST
- added in code for exploring word change over time for main text fields in Wos R/words_over_time.r 
- also added code to do peaked terms -- using splines, etc -- to decide what is happening. 

## Fri 16 Jun 2017 10:42:14 BST
- changing latex_format so that it returns a single string with all found references. This can be used directly in rmd files.  Also changed the search function so that it does this too;  
