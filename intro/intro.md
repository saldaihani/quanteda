quanteda: Quantitative Analysis of Textual Data
===============================================

An R package for managing and analyzing text, by Ken Benoit and Paul Nulty.

quanteda makes it easy to manage texts in the form of a
corpus, defined as a collection of texts that includes document-level
variables specific to each text, as well as meta-data for documents
and for the collection as a whole. quanteda includes tools to make it
easy and fast to manuipulate the texts the texts in a corpus, for
instance by tokenizing them, with or without stopwords or stemming, or
to segment them by sentence or paragraph units. 

quanteda implements
bootstrapping methods for texts that makes it easy to resample texts
from pre-defined units, to facilitate computation of confidence
intervals on textual statistics using techniques of non-parametric
bootstrapping, but applied to the original texts as data. quanteda
includes a suite of sophisticated tools to extract features of the
texts into a quantitative matrix, where these features can be defined
according to a dictionary or thesaurus, including the declaration of
collocations to be treated as single features. 

Once converted into a
quantitative matrix (known as a "dfm" for document-feature matrix),
the textual feature can be analyzed using quantitative methods for
describing, comparing, or scaling texts, or used to train machine
learning methods for class prediction.


How to Install
--------------

You can download the files and build the package from source, or you can use the devtools library to install the package directly from github.

This is done as follows:


```r
# devtools required to install quanteda from Github
if (!require(devtools)) install.packages("devtools")
```

```
## Loading required package: devtools
```

```r
library(devtools)
# install the latest version quanteda from Github
install_github("quanteda", username="kbenoit", dependencies=TRUE, quick=TRUE)
```

```
## Installing github repo quanteda/master from kbenoit
## Downloading master.zip from https://github.com/kbenoit/quanteda/archive/master.zip
## Installing package from /tmp/RtmphtWe5T/master.zip
## arguments 'minimized' and 'invisible' are for Windows only
## Installing quanteda
## '/usr/lib/R/bin/R' --vanilla CMD INSTALL  \
##   '/tmp/RtmphtWe5T/devtoolseed12086919/quanteda-master'  \
##   --library='/home/paul/R/x86_64-pc-linux-gnu-library/3.1'  \
##   --install-tests --no-docs --no-multiarch --no-demo 
## 
## Reloading installed quanteda
```

```r
# to install the latest dev branch version quanteda from Github use:
install_github("quanteda", username="kbenoit", dependencies=TRUE, quick=TRUE, ref="dev")
```

```
## Installing github repo quanteda/dev from kbenoit
## Downloading dev.zip from https://github.com/kbenoit/quanteda/archive/dev.zip
## Installing package from /tmp/RtmphtWe5T/dev.zip
## arguments 'minimized' and 'invisible' are for Windows only
## Installing quanteda
## '/usr/lib/R/bin/R' --vanilla CMD INSTALL  \
##   '/tmp/RtmphtWe5T/devtoolseed7d94742c/quanteda-dev'  \
##   --library='/home/paul/R/x86_64-pc-linux-gnu-library/3.1'  \
##   --install-tests --no-docs --no-multiarch --no-demo 
## 
## Reloading installed quanteda
```

Documentation
-------------

We are working on a package vignette, but for the moment the best documentation is 
found in the quanteda.pdf document, which is a formatted version of the man files
available for all of the quanteda functions.

Examples for any function can also be seen using (for instance, for `corpus()`):

```r
example(corpus)
```

```
## 
## corpus> ## Not run: 
## corpus> ##D # import texts from a directory of files
## corpus> ##D summary(corpus(directory("~/Dropbox/QUANTESS/corpora/ukManRenamed"),
## corpus> ##D                enc="UTF-8", docvarsfrom="filenames",
## corpus> ##D                source="Ken's UK manifesto archive",
## corpus> ##D                docvarnames=c("Country", "Level", "Year", "language")), 5))
## corpus> ##D summary(corpus(directory("~/Dropbox/QUANTESS/corpora/ukManRenamed"),
## corpus> ##D                enc="UTF-8", docvarsfrom="filenames",
## corpus> ##D                source="Ken's UK manifesto archive",
## corpus> ##D                docvarnames=c("Country", "Level", "Year", "language", "Party")), 5))
## corpus> ##D 
## corpus> ##D # choose a directory using a GUI
## corpus> ##D corpus(directory())
## corpus> ##D 
## corpus> ##D # from a zip file on the web
## corpus> ##D myzipcorp <- corpus(zipfiles("http://kenbenoit.net/files/EUcoalsubsidies.zip"),
## corpus> ##D                     notes="From some EP debate about coal mine subsidies")
## corpus> ##D docvars(myzipcorp, speakername=docnames(myzipcorp))
## corpus> ##D summary(myzipcorp)
## corpus> ## End(Not run)
## corpus> #
## corpus> ## import a tm VCorpus
## corpus> if (require(tm)) {
## corpus+     data(crude)    # load in a tm example VCorpus
## corpus+     mytmCorpus <- corpus(crude)
## corpus+     summary(mytmCorpus, showmeta=TRUE)
## corpus+ }
```

```
## Loading required package: tm
## Loading required package: NLP
```

```
## Corpus consisting of 20 documents.
## 
##            Text Types Tokens Sentences                    _author
##  reut-00001.xml    56     90         8                       <NA>
##  reut-00002.xml   224    439        21 BY TED D'AFFLISIO, Reuters
##  reut-00004.xml    39     51         4                       <NA>
##  reut-00005.xml    49     66         6                       <NA>
##  reut-00006.xml    59     88         3                       <NA>
##  reut-00007.xml   229    443        25                       <NA>
##  reut-00008.xml   232    420        23   By Jeremy Clift, Reuters
##  reut-00009.xml    96    134         9                       <NA>
##  reut-00010.xml   165    297        22                       <NA>
##  reut-00011.xml   179    336        20                       <NA>
##  reut-00012.xml   179    360        23                       <NA>
##  reut-00013.xml    67     92         3                       <NA>
##  reut-00014.xml    68    103         7                       <NA>
##  reut-00015.xml    71     97         4                       <NA>
##  reut-00016.xml    72    109         4                       <NA>
##  reut-00018.xml    90    144         9                       <NA>
##  reut-00019.xml   117    194        13                       <NA>
##  reut-00021.xml    47     77        12                       <NA>
##  reut-00022.xml   142    281        12 By BERNICE NAPACH, Reuters
##  reut-00023.xml    30     43         8                       <NA>
##       _datetimestamp _description
##  1987-02-26 17:00:56             
##  1987-02-26 17:34:11             
##  1987-02-26 18:18:00             
##  1987-02-26 18:21:01             
##  1987-02-26 19:00:57             
##  1987-03-01 03:25:46             
##  1987-03-01 03:39:14             
##  1987-03-01 05:27:27             
##  1987-03-01 08:22:30             
##  1987-03-01 18:31:44             
##  1987-03-02 01:05:49             
##  1987-03-02 07:39:23             
##  1987-03-02 07:43:22             
##  1987-03-02 07:43:41             
##  1987-03-02 08:25:42             
##  1987-03-02 11:20:05             
##  1987-03-02 11:28:26             
##  1987-03-02 12:13:46             
##  1987-03-02 14:38:34             
##  1987-03-02 14:49:06             
##                                           _heading _id _language
##           DIAMOND SHAMROCK (DIA) CUTS CRUDE PRICES 127        en
##    OPEC MAY HAVE TO MEET TO FIRM PRICES - ANALYSTS 144        en
##          TEXACO CANADA <TXC> LOWERS CRUDE POSTINGS 191        en
##          MARATHON PETROLEUM REDUCES CRUDE POSTINGS 194        en
##          HOUSTON OIL <HO> RESERVES STUDY COMPLETED 211        en
##      KUWAIT SAYS NO PLANS FOR EMERGENCY OPEC TALKS 236        en
##  INDONESIA SEEN AT CROSSROADS OVER ECONOMIC CHANGE 237        en
##              SAUDI RIYAL DEPOSIT RATES REMAIN FIRM 242        en
##            QATAR UNVEILS BUDGET FOR FISCAL 1987/88 246        en
##    SAUDI ARABIA REITERATES COMMITMENT TO OPEC PACT 248        en
##     SAUDI FEBRUARY CRUDE OUTPUT PUT AT 3.5 MLN BPD 273        en
##  GULF ARAB DEPUTY OIL MINISTERS TO MEET IN BAHRAIN 349        en
##  SAUDI ARABIA REITERATES COMMITMENT TO OPEC ACCORD 352        en
##   KUWAIT MINISTER SAYS NO EMERGENCY OPEC TALKS SET 353        en
##           PHILADELPHIA PORT CLOSED BY TANKER CRASH 368        en
##      STUDY GROUP URGES INCREASED U.S. OIL RESERVES 489        en
##      STUDY GROUP URGES INCREASED U.S. OIL RESERVES 502        en
##     UNOCAL <UCL> UNIT CUTS CRUDE OIL POSTED PRICES 543        en
##       NYMEX WILL EXPAND OFF-HOUR TRADING APRIL ONE 704        en
##      ARGENTINE OIL PRODUCTION DOWN IN JANUARY 1987 708        en
##            _origin _topics _lewissplit    _cgisplit _oldid
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET   5670
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET   5687
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET   5734
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET   5737
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET   5754
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET   8321
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET   8322
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET   8327
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET   8331
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET   8333
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET  12456
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET  12532
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET  12535
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET  12536
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET  12550
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET  12672
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET  12685
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET  12726
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET  12887
##  Reuters-21578 XML     YES       TRAIN TRAINING-SET  12891
##                                                 _places      _people
##                                                     usa         <NA>
##                                                     usa         <NA>
##                                                  canada         <NA>
##                                                     usa         <NA>
##                                                     usa         <NA>
##                                  c("kuwait", "ecuador")         <NA>
##                                   c("indonesia", "usa")         <NA>
##                            c("bahrain", "saudi-arabia")         <NA>
##                                                   qatar         <NA>
##                            c("bahrain", "saudi-arabia") hisham-nazer
##                                c("saudi-arabia", "uae")         <NA>
##  c("uae", "bahrain", "saudi-arabia", "kuwait", "qatar")         <NA>
##                            c("saudi-arabia", "bahrain") hisham-nazer
##                                                  kuwait         <NA>
##                                                     usa         <NA>
##                                                     usa         <NA>
##                                                     usa         <NA>
##                                                     usa         <NA>
##                                                     usa         <NA>
##                                               argentina         <NA>
##      _orgs _exchanges
##       <NA>       <NA>
##       opec       <NA>
##       <NA>       <NA>
##       <NA>       <NA>
##       <NA>       <NA>
##       opec       <NA>
##  worldbank       <NA>
##       opec       <NA>
##       <NA>       <NA>
##       opec       <NA>
##       opec       <NA>
##       opec       <NA>
##       opec       <NA>
##       opec       <NA>
##       <NA>       <NA>
##       <NA>       <NA>
##       <NA>       <NA>
##       <NA>       <NA>
##       <NA>      nymex
##       <NA>       <NA>
## 
## Source:  Converted from tm VCorpus 'crude'.
## Created: Mon Oct  6 16:46:55 2014.
## Notes:   .
## 
## 
## corpus> #
## corpus> # create a corpus from texts
## corpus> corpus(inaugTexts)
## Corpus consisting of 57 documents.
## 
## corpus> # create a corpus from texts and assign meta-data and document variables
## corpus> uk2010immigCorpus <- corpus(uk2010immig,
## corpus+                             docvars=data.frame(party=names(uk2010immig)),
## corpus+                             enc="UTF-8")
##   note: converted texts from UTF-8 to UTF-8.
```

There are also some demo functions that show off some of the package capabilities, such 
as `demo(quanteda)`.


Example
-------


```r
library(quanteda)
# create a corpus from the immigration texts from UK party platforms
uk2010immigCorpus <- corpus(uk2010immig,
                            docvars=data.frame(party=names(uk2010immig)),
                            notes="Immigration-related sections of 2010 UK party manifestos",
                            enc="UTF-8")
```

```
##   note: converted texts from UTF-8 to UTF-8.
```

```r
uk2010immigCorpus
```

```
## Corpus consisting of 9 documents.
```

```r
summary(uk2010immigCorpus, showmeta=TRUE)
```

```
## Corpus consisting of 9 documents.
## 
##          Text Types Tokens Sentences        party _encoding
##           BNP   969   2765       130          BNP     UTF-8
##     Coalition   133    231        12    Coalition     UTF-8
##  Conservative   234    452        15 Conservative     UTF-8
##        Greens   301    608        29       Greens     UTF-8
##        Labour   279    615        29       Labour     UTF-8
##        LibDem   239    434        22       LibDem     UTF-8
##            PC    72    101         5           PC     UTF-8
##           SNP    81    124         4          SNP     UTF-8
##          UKIP   303    625        41         UKIP     UTF-8
## 
## Source:  /home/paul/Dropbox/QUANTESS/testbook/intro/* on x86_64 by paul.
## Created: Mon Oct  6 16:46:55 2014.
## Notes:   Immigration-related sections of 2010 UK party manifestos.
```

```r
# key words in context for "deport", 3 words of context
kwic(uk2010immigCorpus, "deport", 3)
```

```
##                                               preword         word
##     [BNP, 71]                further immigration, the deportation 
##    [BNP, 139]                            The BNP will    deport   
##   [BNP, 1628] long-term resettlement programme.\n\n2.    Deport   
##   [BNP, 1633]          illegal immigrants\n\nWe shall    deport   
##   [BNP, 1653]                current unacceptably lax deportation 
##   [BNP, 1659]                           of people are   deported  
##   [BNP, 2169]                     enforced by instant deportation,
##   [BNP, 2180]         British immigration laws.\n\n8. Deportation 
##   [BNP, 2186]           Foreign Criminals\n\nWe shall    deport   
##   [BNP, 2198]                       This includes the deportation 
## [Greens, 566]                      subject to summary deportation.
## [LibDem, 194]         illegal labour.\n\n- Prioritise deportation 
## [LibDem, 394]                  flight risks.\n\n- End deportations
##   [UKIP, 317]                            laws or face deportation.
##                                   postword
##     [BNP, 71] of all illegal              
##    [BNP, 139] all foreigners convicted    
##   [BNP, 1628] all illegal immigrants\n\nWe
##   [BNP, 1633] all illegal immigrants      
##   [BNP, 1653] policies, thousands of      
##   [BNP, 1659] from the UK                 
##   [BNP, 2169] for anyone found            
##   [BNP, 2180] of all Foreign              
##   [BNP, 2186] all criminal entrants,      
##   [BNP, 2198] of all Muslim               
## [Greens, 566] They should receive         
## [LibDem, 194] efforts on criminals,       
## [LibDem, 394] of refugees to              
##   [UKIP, 317] Such citizens will
```

```r
# create a dfm, removing stopwords
mydfm <- dfm(uk2010immigCorpus, stopwords=TRUE)
```

```
## Creating dfm from a corpus: ...  removing stopwords ...  done.
```

```r
dim(mydfm)              # basic dimensions of the dfm
```

```
## [1]    9 1491
```

```r
topfeatures(mydfm, 20)  # 20 top words
```

```
##        will immigration     british      people      asylum     britain 
##          86          66          37          35          29          28 
##      system          uk  population     country         new      ensure 
##          27          27          21          20          19          17 
##  immigrants       shall citizenship    national      social         bnp 
##          17          17          16          14          14          13 
##     illegal        work 
##          13          13
```

```r
quartz()
```

```
## Warning: Quartz device is not available on this platform
```

```r
plot(mydfm)             # word cloud     
```

![plot of chunk quanteda_example](figure/quanteda_example.png) 
