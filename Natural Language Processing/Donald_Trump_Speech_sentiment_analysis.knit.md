---
title: "R Notebook"
output:
  pdf_document: default
  html_notebook: default
---

How exactly did Donald Trump surprise the world and became the President of US? Let us take a look at what is it that he said to the people of US that appealed the most to them. 

I will perform a sentiment analysis of all the 56 speeches Trump gave across US en route to the Oval office. 

Let us load the libraries we are going to use. 

```r
library(readr)
```

```
## Warning: package 'readr' was built under R version 3.4.3
```

```r
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 3.4.3
```

```
## -- Attaching packages ----------------------- tidyverse 1.2.1 --
```

```
## v ggplot2 2.2.1     v purrr   0.2.4
## v tibble  1.4.1     v dplyr   0.7.4
## v tidyr   0.7.2     v stringr 1.2.0
## v ggplot2 2.2.1     v forcats 0.2.0
```

```
## Warning: package 'ggplot2' was built under R version 3.4.4
```

```
## Warning: package 'tibble' was built under R version 3.4.3
```

```
## Warning: package 'tidyr' was built under R version 3.4.3
```

```
## Warning: package 'purrr' was built under R version 3.4.3
```

```
## Warning: package 'dplyr' was built under R version 3.4.3
```

```
## Warning: package 'stringr' was built under R version 3.4.3
```

```
## Warning: package 'forcats' was built under R version 3.4.3
```

```
## -- Conflicts -------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(stringr)
library(tidytext)
```

```
## Warning: package 'tidytext' was built under R version 3.4.4
```

```r
library(tm)
```

```
## Warning: package 'tm' was built under R version 3.4.3
```

```
## Loading required package: NLP
```

```
## 
## Attaching package: 'NLP'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     annotate
```

Now, let us start by loading the text file of Trump's speeches. 


```r
Trump_speeches<-read_lines("C:/Users/rohan/Desktop/DMP/Assignment 3/full_speech.txt")
```


Create a corpus of all the speeches in the text file

```r
Trump_corpus<-VCorpus(VectorSource(Trump_speeches))
print(Trump_corpus)
```

```
## <<VCorpus>>
## Metadata:  corpus specific: 0, document level (indexed): 0
## Content:  documents: 74
```

We need to first clean the speeches of the common stop words, white spaces and punctuation marks. 


Let us start of by converting all the upper case letters to lower case letters. 


```r
Trump_corpus_clean<-tm_map(Trump_corpus,content_transformer(tolower))
```

Let us now continue to remove all the numbers. 


```r
Trump_corpus_clean<-tm_map(Trump_corpus_clean,removeNumbers)
```
Next, we will remove all the stop words. 


```r
Trump_corpus_clean<-tm_map(Trump_corpus_clean,removeWords,stopwords())
```
We also want to remove the word "Applause". Let us do that now. 


```r
Trump_corpus_clean<-tm_map(Trump_corpus_clean,removeWords,"applause")
```

Let us now move forward and remove the punctuation marks. 

```r
Trump_corpus_clean<-tm_map(Trump_corpus_clean,removePunctuation)
```

Let us now standardize the text by stemming. 


```r
library(SnowballC)
#Trump_corpus_clean<-tm_map(Trump_corpus_clean,stemDocument)
```

Finally, let us remove the white spaces in our corpus. 

```r
Trump_corpus_clean<-tm_map(Trump_corpus_clean,stripWhitespace)
```

Let us now make a Document Term Matrix of our corpus. 


```r
Trump_dcm<-DocumentTermMatrix(Trump_corpus_clean)
Trump_dcm
```

```
## <<DocumentTermMatrix (documents: 74, terms: 8193)>>
## Non-/sparse entries: 53339/552943
## Sparsity           : 91%
## Maximal term length: 22
## Weighting          : term frequency (tf)
```

Now, let us remove the sparce terms from our DCM. 

```r
Trump_dcm_sparce<-removeSparseTerms(Trump_dcm,.5)
Trump_final<-as.data.frame(as.matrix(Trump_dcm_sparce))
dim(Trump_final)
```

```
## [1]  74 300
```

```r
Trump_final[,1:10]
```

```
##    accomplish across administration africanamerican ago also always
## 1           0      2              2               0   4    1      4
## 2           0      0              1               0   0    2      0
## 3           0      0              6               0   1   11      4
## 4           0      1              2               0   1    7      0
## 5           0      5              1               2   1    8      1
## 6           0      5              3               0   0    7      1
## 7           0      0              3               0   1    5      1
## 8           1      3              3               2   3    8      2
## 9           0      0              0               0  13    6      2
## 10          1      2              0               0   9    5      4
## 11          0      1              4               1   3   13      1
## 12          0      2              0               0  10    1      5
## 13          0      0              0               0  15    0      2
## 14          1      2              7               0   3   11      1
## 15          0      4              3              11   0    2      0
## 16          0      3              4               4   1    2      4
## 17          0      0              5               3   1    3      3
## 18          0      1              1               4   0    7      1
## 19          0      4              1               5   2    7      1
## 20          0      0              2               9   1    4      0
## 21          2      7             15               1   1   11      3
## 22          2      3              2               1   0    5      0
## 23          0      2              2               2   0    2      0
## 24          0      2              0               4   0    3      2
## 25          1      1              1               2   0    5      0
## 26          0      2              4               0   1   10      0
## 27          0      1              2               6   1    4      1
## 28          1      3              3               6   8    7      2
## 29          1      3              1               0   0   11      0
## 30          1      0              1               1   0    6      0
## 31          0      3              2               1   0    1      1
## 32          1      0              0               0   1   10      0
## 33          1      0              0               5   0    3      0
## 34          1      0              0               0   0    2      0
## 35          1      1              1               0   0    4      0
## 36          1      1              0               3   0    6      0
## 37          0      1              1               0   1    0      0
## 38          1      0              0               4   0    3      0
## 39          1      3              0               1   0    5      2
## 40          1      0              4               4   0    8      1
## 41          1      2              0               3   0    7      2
## 42          2      0              0               5   0    2      1
## 43          2      1              0               7   0    2      0
## 44          2      2              1               2   0    3      1
## 45          2      1              2               3   1    2      1
## 46          1      1              0               2   0    4      1
## 47          0      0              2               0   0    4      0
## 48          1      0              1               3   0    9      0
## 49          1      0              0               3   2   13      0
## 50          1      2              0               3   1    9      0
## 51          1      0              0               0   0    6      1
## 52          0      2              2               0   5   12      3
## 53          0      1              3               0   1    3      0
## 54          0      2              1               1   4    4      0
## 55          2      2              9               0   1    6      0
## 56          1      0              6               0   1    6      0
## 57          1      0              8               0   0    6      1
## 58          1      0              7               4   2   10      2
## 59          1      3              7               1   2    5      0
## 60          1      2              7               1   2    4      0
## 61          1      0              6               1   0    9      0
## 62          0      4              3               1   3    7      1
## 63          1      2              5               0   0    9      1
## 64          1      0              3              16   0   10      0
## 65          2      0              2               1   2    8      2
## 66          0      1              5               1   2   10      3
## 67          0      0              0               0   2    1      0
## 68          1      1              3               0   0    6      0
## 69          1      1              3               2   0   10      0
## 70          1      2              3               1   0    7      1
## 71          1      0              6               1   2    7      1
## 72          0      1              6               2   1    9      0
## 73          2      0              3               2   4    6      0
## 74          0      1              0               0   0    3      1
##    amazing amendment america
## 1        1         2       6
## 2        0         0       5
## 3        0         0      18
## 4        0         1      17
## 5        2         0      18
## 6        0         0      13
## 7        5         4       3
## 8        3         2      26
## 9        1         1       1
## 10       0         3       6
## 11       1         0      13
## 12       7         7      10
## 13       4         7       7
## 14       0         0       8
## 15       0         0       8
## 16       1         0       6
## 17       1         1      10
## 18       0         1      10
## 19       0         0      13
## 20       1         0       8
## 21       5         2       8
## 22       1         0       9
## 23       0         0       6
## 24       4         0       5
## 25       1         0       7
## 26       0         0       6
## 27       1         0       9
## 28       5         7      10
## 29       4         0      13
## 30       2         0       5
## 31       1         0       4
## 32       1         0       3
## 33       0         2       7
## 34       0         2      10
## 35       2         1      12
## 36       3         1       7
## 37       0         0       1
## 38       0         0       5
## 39       0         1      10
## 40       0         0       7
## 41       0         1      11
## 42       0         2       8
## 43       0         2       9
## 44       0         2       8
## 45       0         2      10
## 46       2         3      14
## 47       0         0       1
## 48       2         2      14
## 49       2         2      16
## 50       1         1      13
## 51       0         0       4
## 52       3         3      14
## 53       0         0       3
## 54       0         0       3
## 55       1         1       6
## 56       1         1       8
## 57       1         1       8
## 58       2         3      11
## 59       2         2       8
## 60       1         2       8
## 61       1         2       8
## 62       1         2       2
## 63       1         1      18
## 64       1         0       6
## 65       1         2       5
## 66       1         1       4
## 67       0         0       1
## 68       1         1      10
## 69       0         1      14
## 70       2         0       2
## 71       0         1      10
## 72       0         2      12
## 73       5         1      11
## 74       2         0       2
```
Now let us plot the 15 most common words that featured in Trump's speeches. 


```r
Most_common<-colSums(Trump_final)
sort(Most_common,decreasing = TRUE)[1:15]
```

```
##     will    going   people  country  hillary  clinton     jobs american 
##     2522     1985     1421     1154      999      899      805      796 
##      one     know    great  america      new     said     just 
##      770      688      666      632      583      579      574
```


We will make a wordcloud of these words now. 


```r
wordcloud::wordcloud(names(Most_common),Most_common,random.order = FALSE,max.words = 15,colors = blues9)
```

![](Donald_Trump_Speech_sentiment_analysis_files/figure-latex/unnamed-chunk-14-1.pdf)<!-- --> 
Now, let us get the word frequencies in a dataframe so that we can plot them. 

```r
Word_freq<-as.data.frame(Most_common)

head(Word_freq)
```

```
##                 Most_common
## accomplish               51
## across                  102
## administration          191
## africanamerican         148
## ago                     120
## also                    435
```

```r
Word_freq$Word<-rownames(Word_freq)
rownames(Word_freq)<-c()
Word_freq<-Word_freq[c(2,1)]

colnames(Word_freq)<-c("Word","Frequency")
head(Word_freq)
```

```
##              Word Frequency
## 1      accomplish        51
## 2          across       102
## 3  administration       191
## 4 africanamerican       148
## 5             ago       120
## 6            also       435
```

```r
Word_freq$Word<-as.factor(Word_freq$Word)
```

Let us now create a bar plot of the top 15 most commonly used words by Trump. 


```r
str(Word_freq)
```

```
## 'data.frame':	300 obs. of  2 variables:
##  $ Word     : Factor w/ 300 levels "accomplish","across",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ Frequency: num  51 102 191 148 120 435 72 91 89 632 ...
```

```r
Top_15<-Word_freq[1:15,]

dim(Top_15)
```

```
## [1] 15  2
```

```r
ggplot(Top_15)+geom_bar(mapping = aes(reorder(Word,Frequency),Frequency,fill=Frequency),stat = "identity")+coord_flip()
```

![](Donald_Trump_Speech_sentiment_analysis_files/figure-latex/unnamed-chunk-16-1.pdf)<!-- --> 


PART B

In this part, I will:
Re-tokenize the text of all 56 Donald Trump Speeches into a new tidy text data frame, using bigrams as
tokens. Remove each bigram where either word is a stop word or the word "applause". Then plot the top 15
most common bigrams in Trump's speeches.

For the part A, I have used the corpus format. Now, I will use the tidytext format. 



```r
Trump_s<-tibble(line=1:length(Trump_speeches),text=Trump_speeches)
Trump_s
```

```
## # A tibble: 74 x 2
##     line text                                                             
##    <int> <chr>                                                            
##  1     1 "Trump: Wow. Whoa. That is some group of people. Thousands. So n~
##  2     2 " Good evening. Thank you very much. I speak to you today as a l~
##  3     3 "Thank you for the opportunity to speak to you, and thank you to~
##  4     4 "Thank you for joining me today. This was going to be a speech o~
##  5     5 "Today I'd like to share my thoughts about the stakes in this el~
##  6     6 "Thank you. I'd like to thank Chairman Jeff Miller for his leade~
##  7     7 "Thank you, everybody. Great honor. Great honor, thank you. This~
##  8     8 "Friends, delegates and fellow Americans: I humbly and gratefull~
##  9     9 "Donald Trump. So, it's been 235 days since crooked Hillary Clin~
## 10    10 "So how good is he? How good is he? [applause] Special. Special.~
## # ... with 64 more rows
```

```r
dim(Trump_s)
```

```
## [1] 74  2
```
Now, let us tidy the data.


```r
tidy_speeches<-Trump_s%>% unnest_tokens(word,text)
tidy_speeches
```

```
## # A tibble: 235,237 x 2
##     line word     
##    <int> <chr>    
##  1     1 trump    
##  2     1 wow      
##  3     1 whoa     
##  4     1 that     
##  5     1 is       
##  6     1 some     
##  7     1 group    
##  8     1 of       
##  9     1 people   
## 10     1 thousands
## # ... with 235,227 more rows
```

Now, let us first see the most common words. This is essentially an easier way to solve the problem 6. 

Before we proceed, let us first remove the common stop words. 

```r
new_list<-c("Applause")
new_list<-as.data.frame(new_list)
new_list$lexicon<-c("SMART")
colnames(new_list)<-c("word","lexicon")
new_list$word<-as.character(new_list$word)
stop_words<-rbind(stop_words,new_list)
tidy_speeches_imp<-tidy_speeches%>% anti_join(stop_words,by="word")%>% count(word,sort = TRUE)
```

```
## Warning: package 'bindrcpp' was built under R version 3.4.3
```

```r
tidy_speeches_final<-tidy_speeches_imp[-3,] #removing the word applause
colnames(tidy_speeches_final)<-c("Word_used","n")
```

Now, let us plot the top 15 most used words. 


```r
Top15_words<-tidy_speeches_final[1:15,]
ggplot(Top15_words)+geom_bar(mapping = aes(reorder(Word_used,n),n,fill=n),stat = "identity")+coord_flip()
```

![](Donald_Trump_Speech_sentiment_analysis_files/figure-latex/unnamed-chunk-20-1.pdf)<!-- --> 


Now, let us create a bigram of the Trump speeches. We will do so to get a deeper context in which the words were actually used. 


```r
Trump_bigrams<-Trump_s%>% unnest_tokens(bigram,text,token = "ngrams",n=2)
Trump_bigrams
```

```
## # A tibble: 235,163 x 2
##     line bigram          
##    <int> <chr>           
##  1     1 trump wow       
##  2     1 wow whoa        
##  3     1 whoa that       
##  4     1 that is         
##  5     1 is some         
##  6     1 some group      
##  7     1 group of        
##  8     1 of people       
##  9     1 people thousands
## 10     1 thousands so    
## # ... with 235,153 more rows
```


```r
tidy_speeches_big<-tidy_speeches%>% anti_join(stop_words,by="word")
str(tidy_speeches_big)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	85814 obs. of  2 variables:
##  $ line: int  1 1 1 1 1 1 1 1 1 1 ...
##  $ word: chr  "trump" "wow" "whoa" "people" ...
```

```r
str(Trump_s)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	74 obs. of  2 variables:
##  $ line: int  1 2 3 4 5 6 7 8 9 10 ...
##  $ text: chr  "Trump: Wow. Whoa. That is some group of people. Thousands. So nice, thank you very much. That's really nice. Th"| __truncated__ " Good evening. Thank you very much. I speak to you today as a lifelong supporter and true friend of Israel. I'm"| __truncated__ "Thank you for the opportunity to speak to you, and thank you to the Center for the National Interest for honori"| __truncated__ "Thank you for joining me today. This was going to be a speech on Hillary Clinton and how bad a President, espec"| __truncated__ ...
```



Now let us check the most common bigrams. 


```r
Trump_bigrams<-Trump_bigrams%>%count(bigram,sort = TRUE)
Trump_bigrams
```

```
## # A tibble: 76,867 x 2
##    bigram              n
##    <chr>           <int>
##  1 going to         1821
##  2 of the            987
##  3 we will           819
##  4 we are            710
##  5 in the            669
##  6 hillary clinton   663
##  7 our country       575
##  8 are going         558
##  9 to be             538
## 10 we have           473
## # ... with 76,857 more rows
```
We can see that there are still stop words in the bigrams which we should eliminate. 

For that, we will first split the bigrams into two words, eliminate the stop words and then reunite the words to form a bigram again. 



```r
library(tidyr)
Trump_bigrams_sep<-Trump_bigrams%>%separate(bigram,c("word1","word2"),sep = " ")
Trump_bigrams_sep
```

```
## # A tibble: 76,867 x 3
##    word1   word2       n
##  * <chr>   <chr>   <int>
##  1 going   to       1821
##  2 of      the       987
##  3 we      will      819
##  4 we      are       710
##  5 in      the       669
##  6 hillary clinton   663
##  7 our     country   575
##  8 are     going     558
##  9 to      be        538
## 10 we      have      473
## # ... with 76,857 more rows
```

```r
Trump_bigrams_sep<-Trump_bigrams_sep%>% filter(!word1 %in% stop_words$word)%>%filter(!word2 %in% stop_words$word)

Trump_bigrams_sep
```

```
## # A tibble: 14,257 x 3
##    word1     word2              n
##    <chr>     <chr>          <int>
##  1 hillary   clinton          663
##  2 donald    trump            172
##  3 african   american         164
##  4 american  people           119
##  5 trump     administration   112
##  6 hillary   clinton's        104
##  7 trade     deals            102
##  8 november  8th               91
##  9 middle    east              88
## 10 president obama             87
## # ... with 14,247 more rows
```

Now let us unite these bigrams again so that we can plot it. 


```r
Trump_bigrams_final<-Trump_bigrams_sep%>% unite(bigram,word1,word2,sep = " ")
Trump_bigrams_final
```

```
## # A tibble: 14,257 x 2
##    bigram                   n
##  * <chr>                <int>
##  1 hillary clinton        663
##  2 donald trump           172
##  3 african american       164
##  4 american people        119
##  5 trump administration   112
##  6 hillary clinton's      104
##  7 trade deals            102
##  8 november 8th            91
##  9 middle east             88
## 10 president obama         87
## # ... with 14,247 more rows
```

Now, let us plot the top 15 most commonly used bigrams in Trump's speeches. 


```r
Top15_bigram<-Trump_bigrams_final[1:15,]
ggplot(Top15_bigram)+geom_bar(mapping = aes(reorder(bigram,n),n,fill=n),stat = "identity")+coord_flip()
```

![](Donald_Trump_Speech_sentiment_analysis_files/figure-latex/unnamed-chunk-26-1.pdf)<!-- --> 



PART C

For the part C, I will do the following:

A sentiment analysis of Donald Trump's speeches. In order to make sure sentiments are
assigned to appropriate contexts, first tokenize the speeches into bigrams, and then filter out all bigrams
where the first word is any of "not", "no", or "never".

Now, we have to remove the bigrams where the first words are "no","not","never".

For that, let us use the separated bigrams from the previous question. 


```r
Negative<-c("no","not","never")
Tr<-c("trump","applause")
Trump_bigram_senti<-Trump_bigrams_sep%>% filter(!word1 %in% Negative)%>% filter(!word2%in% Tr)

Trump_bigram_senti%>% filter(word2=="trump")# checking if the word elimination worked.
```

```
## # A tibble: 0 x 3
## # ... with 3 variables: word1 <chr>, word2 <chr>, n <int>
```

```r
Trump_bigram_senti%>% filter(word1=="no")
```

```
## # A tibble: 0 x 3
## # ... with 3 variables: word1 <chr>, word2 <chr>, n <int>
```

Now let us get each of the 10 sentiments in the nrc into 10 separate dataframes.

We will need these to do the further analysis. 



```r
nrc<-get_sentiments("nrc")
unique(nrc$sentiment)
```

```
##  [1] "trust"        "fear"         "negative"     "sadness"     
##  [5] "anger"        "surprise"     "positive"     "disgust"     
##  [9] "joy"          "anticipation"
```

```r
nrc_trust<-nrc%>%filter(sentiment=="trust")
nrc_fear<-nrc%>%filter(sentiment=="fear")
nrc_negative<-nrc%>%filter(sentiment=="negative")
nrc_sadness<-nrc%>%filter(sentiment=="sadness")
nrc_anger<-nrc%>%filter(sentiment=="anger")
nrc_surprise<-nrc%>%filter(sentiment=="suprise")
nrc_positive<-nrc%>%filter(sentiment=="positive")
nrc_disgust<-nrc%>%filter(sentiment=="disgust")
nrc_joy<-nrc%>%filter(sentiment=="joy")
nrc_anticipation<-nrc%>%filter(sentiment=="anticipation")
```
Let us first create a new column in our dataframe so that we can later join the sentiment dataframes. 


```r
Trump_bigram_senti$word<-Trump_bigram_senti$word2
Trump_sentiment<-Trump_bigram_senti[,4]
class(Trump_sentiment)
```

```
## [1] "tbl_df"     "tbl"        "data.frame"
```

Now, let us see the top 10 words in Trump's speeches associated with trust. 


```r
Trust_trump<-Trump_sentiment%>% inner_join(nrc_trust,by="word")%>% count(word)%>%top_n(10)
```

```
## Selecting by n
```

```r
ggplot(Trust_trump)+geom_bar(mapping = aes(reorder(word,n),n,fill=n),stat = "identity")+coord_flip()+labs(x="Trust_words",y="Number")
```

![](Donald_Trump_Speech_sentiment_analysis_files/figure-latex/unnamed-chunk-30-1.pdf)<!-- --> 
Now, let us see the top 10 words in Trump's speeches associated with Fear. 


```r
Fear_trump<-Trump_sentiment%>% inner_join(nrc_fear,by="word")%>% count(word)%>%top_n(10)
```

```
## Selecting by n
```

```r
ggplot(Fear_trump)+geom_bar(mapping = aes(reorder(word,n),n,fill=n),stat = "identity")+coord_flip()+labs(x="Fear_words",y="Number")
```

![](Donald_Trump_Speech_sentiment_analysis_files/figure-latex/unnamed-chunk-31-1.pdf)<!-- --> 


Now, let us see the top 10 words in Trump's speeches associated with negative. 


```r
Negative_trump<-Trump_sentiment%>% inner_join(nrc_negative,by="word")%>% count(word)%>%top_n(10)
```

```
## Selecting by n
```

```r
ggplot(Negative_trump)+geom_bar(mapping = aes(reorder(word,n),n,fill=n),stat = "identity")+coord_flip()+labs(x="Negative_words",y="Number")
```

![](Donald_Trump_Speech_sentiment_analysis_files/figure-latex/unnamed-chunk-32-1.pdf)<!-- --> 

Now, let us see the top 10 words in Trump's speeches associated with sadness. 


```r
Sad_trump<-Trump_sentiment%>% inner_join(nrc_sadness,by="word")%>% count(word)%>%top_n(10)
```

```
## Selecting by n
```

```r
ggplot(Sad_trump)+geom_bar(mapping = aes(reorder(word,n),n,fill=n),stat = "identity")+coord_flip()+labs(x="Sad_words",y="Number")
```

![](Donald_Trump_Speech_sentiment_analysis_files/figure-latex/unnamed-chunk-33-1.pdf)<!-- --> 

Now, let us see the top 10 words in Trump's speeches associated with anger. 


```r
Anger_trump<-Trump_sentiment%>% inner_join(nrc_anger,by="word")%>% count(word)%>%top_n(10)
```

```
## Selecting by n
```

```r
ggplot(Anger_trump)+geom_bar(mapping = aes(reorder(word,n),n,fill=n),stat = "identity")+coord_flip()+labs(x="Anger_words",y="Number")
```

![](Donald_Trump_Speech_sentiment_analysis_files/figure-latex/unnamed-chunk-34-1.pdf)<!-- --> 

Now, let us see the top 10 words in Trump's speeches associated with surprise. 


```r
Surprise_trump<-Trump_sentiment%>% inner_join(nrc_surprise,by="word")%>% count(word)%>%top_n(10)
```

```
## Selecting by n
```

```r
ggplot(Surprise_trump)+geom_bar(mapping = aes(reorder(word,n),n,fill=n),stat = "identity")+coord_flip()+labs(x="Trust_words",y="Number")
```

![](Donald_Trump_Speech_sentiment_analysis_files/figure-latex/unnamed-chunk-35-1.pdf)<!-- --> 
There are no surprise words in trump's speeches. 

Now, let us see the top 10 words in Trump's speeches associated with positive. 


```r
Positive_trump<-Trump_sentiment%>% inner_join(nrc_positive,by="word")%>% count(word)%>%top_n(10)
```

```
## Selecting by n
```

```r
ggplot(Positive_trump)+geom_bar(mapping = aes(reorder(word,n),n,fill=n),stat = "identity")+coord_flip()+labs(x="Positive_words",y="Number")
```

![](Donald_Trump_Speech_sentiment_analysis_files/figure-latex/unnamed-chunk-36-1.pdf)<!-- --> 

Now, let us see the top 10 words in Trump's speeches associated with disgust. 


```r
disgust_trump<-Trump_sentiment%>% inner_join(nrc_disgust,by="word")%>% count(word)%>%top_n(10)
```

```
## Selecting by n
```

```r
ggplot(disgust_trump)+geom_bar(mapping = aes(reorder(word,n),n,fill=n),stat = "identity")+coord_flip()+labs(x="Disgust_words",y="Number")
```

![](Donald_Trump_Speech_sentiment_analysis_files/figure-latex/unnamed-chunk-37-1.pdf)<!-- --> 


Now, let us see the top 10 words in Trump's speeches associated with joy. 


```r
joy_trump<-Trump_sentiment%>% inner_join(nrc_joy,by="word")%>% count(word)%>%top_n(10)
```

```
## Selecting by n
```

```r
ggplot(joy_trump)+geom_bar(mapping = aes(reorder(word,n),n,fill=n),stat = "identity")+coord_flip()+labs(x="Joy_words",y="Number")
```

![](Donald_Trump_Speech_sentiment_analysis_files/figure-latex/unnamed-chunk-38-1.pdf)<!-- --> 

Now, let us see the top 10 words in Trump's speeches associated with anticipation. 


```r
anticipation_trump<-Trump_sentiment%>% inner_join(nrc_anticipation,by="word")%>% count(word)%>%top_n(10)
```

```
## Selecting by n
```

```r
ggplot(anticipation_trump)+geom_bar(mapping = aes(reorder(word,n),n,fill=n),stat = "identity")+coord_flip()+labs(x="anticipation_words",y="Number")
```

![](Donald_Trump_Speech_sentiment_analysis_files/figure-latex/unnamed-chunk-39-1.pdf)<!-- --> 






PART D


In this part, I will write a function to tokenize an input corpus and spit out the most frequent words. The idea is to automate the process of data cleansing and get a glimpse into the data at the first go. 




```r
Text_analysis_corpus<-function(mysrc_clean,nwords){
  mysrc_clean<-tm_map(mysrc_clean,removeNumbers)
  mysrc_clean<-tm_map(mysrc_clean,removeWords,stopwords())
  mysrc_clean<-tm_map(mysrc_clean,removePunctuation)
  library(SnowballC)
  mysrc_clean<-tm_map(mysrc_clean,stripWhitespace)
 mysrc_dcm<-DocumentTermMatrix(mysrc_clean)

 
 mysrc_sparce<-removeSparseTerms(mysrc_dcm,.5)
mysrc_final<-as.data.frame(as.matrix(mysrc_sparce))
Most_common<-colSums(Trump_final)
Top_words<-sort(Most_common,decreasing = TRUE)[1:nwords]
Word_freq<-as.data.frame(Top_words)


Word_freq$Word<-rownames(Word_freq)
rownames(Word_freq)<-c()
Word_freq<-Word_freq[c(2,1)]

colnames(Word_freq)<-c("Word","Frequency")

Word_freq$Word<-as.factor(Word_freq$Word)

ggplot(Word_freq)+geom_bar(mapping = aes(reorder(Word,Frequency),Frequency,fill=Frequency),stat = "identity")+coord_flip()
 }
```
Let us test if the method works. 

We will use the corpus from the question 6. 


```r
Text_analysis_corpus(Trump_corpus,5)
```

![](Donald_Trump_Speech_sentiment_analysis_files/figure-latex/unnamed-chunk-41-1.pdf)<!-- --> 


