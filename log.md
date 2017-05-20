
## Mon 15 May 2017 10:09:06 BST
- TODO:building package, but library dependencies don't seem to work properly ... Fixed it -- need to reference imported packages using package:: syntax
- found bibliometrix package on cran -- has many useful functions, but not in tidytext way

## Mon 15 May 2017 14:31:48 BST
-  chaning R version depends on in DESCRIPTION file

## Tue 16 May 2017 11:19:12 BST
- working out how to use functions from other packages. It seems like a lot of work ... to have to list them all in @import statements 

## Wed 17 May 2017 23:03:01 BST
- did more on the import stuff. @import pkg seems better than @importFrom pkg fn
- still showing errors - need to sort this out

## Fri 19 May 2017 23:17:07 BST
- breaking file into separate areas -- authors, titles, keywords, abstracts, etc and testing these

## Sat 20 May 2017 09:30:38 BST
- worked on topic models for abstracts. Need to work out a way to add stopwords specific to the area of analysis 
- TODO: oxygenise abstracts

## Sat 20 May 2017 14:27:32 BST
- fixing up keyword counts so that I can use them alongside the abstracts. Need to be able to join on the ids 
- also cleaning up the topic models again
- all that was fine. But now installation is going very slow. I did add licence, and a couple of other things to DESCRIPTION, etc. Maybe I've broken something there. Or it could be to do with writing in a tunnel. 
- it eventually installs and seems to be working ok
