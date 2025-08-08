---
title: 'MetaNLP: Natural Language Processing for Meta Analysis '
tags:
- R
- Natural Language Processing
- Meta Analysis
- Machine Learning
date: "31 July 2025"
output:
  html_document:
    df_print: paged
  pdf_document: default
authors:
- name: Nico Bruder
  orcid: "0009-0004-9522-2075"
  equal-contrib: true
  affiliation: 1
- name: Samuel Zimmermann
  orcid: "0009-0000-4828-9294"
  equal-contrib: true
  affiliation: 1
- name: Johannes Vey
  orcid: "000-0002-2610-9667"
  affiliation: 1
- name: Maximilian Pilz
  orcid: "000-0002-9685-1613"
  corresponding: true
  affiliation: 2
bibliography: paper.bib
affiliations:
- name: Institute of Medical Biometry, University of Heidelberg, Germany
  index: 1
- name: Department of Optimization, Fraunhofer Institute for Industrial Mathematics,
    Germany
  index: 2
editor_options:
  markdown:
    wrap: 80
---

# Summary

To facilitate the the time-consuming task of title-abstract screening
in the preparation of a systematic review, it is possible to partially automate
this process using machine learning models. These models are often trained on the
document-term matrix derived from the collection of titles and abstracts. In this paper, we present
the R package **MetaNLP**, which allows the transfer of a CSV file containing titles
and abstracts into a document-term matrix with just a few lines of code.
Furthermore, it provides a variety of functions to modify this matrix according to user
preferences and includes helpful summary and visualization features.

# Statement of need

When creating a systematic review, title-abstract (TIAB) 
screening is particularly time-consuming as it is standard practice for at least 
two human reviewers to independently read all identified titles and abstracts 
and decide whether each citation should be included or excluded for the next step, 
the full-text screening. However, this task involves plain text reading and could 
potentially be performed using natural language processing (NLP) and machine 
learning (ML) methods to tackle the problem of classifying the records in 
"include/exclude". A certain proportion of the publications to screen can be 
classified by humans and used as training data to train a ML model.
This ML model can then classify the remaining publications, either alone or
together with a second human reviewer.

Next to pre-implemented, but non-flexible machine learning tools like Rayyan [@Rayyan] 
or fine-tuned large language models [@Dennstadt2024], one important approach takes 
the titles and abstracts of the publications, computes the 
document-term matrix and trains ML models based on this matrix [@Lange2021; @Kebede2023; @Pilz2024].
This approach allows the user the flexibility to exclude negligible
words and to train different ML algorithms, making it easier to apply the best
algorithm for the problem at hand.

The R-package **MetaNLP** supports programmers that aim to employ the
this approach. It provides all necessary functionality to create and flexibly modify the document-term
matrix from a CSV file that contains titles and abstracts of the publications.
This allows the user to perform essential NLP tasks with minimal code and
quickly start the training of ML algorithms.


# Methodology

The titles and abstracts to process must be structured in a CSV file where one row 
corresponds to exactly one publication. The CSV must have one column `ID` to clearly 
identify each paper, one column `title` containing the titles, and another column `abstract` 
where all the abstracts are listed. A column `decision` is necessary when 
the CSV contains training data to allow for supervised ML algorithms.

**MetaNLP** then processes the CSV in several distinct steps: 
At first, the CSV file containing the titles and abstracts is read and scanned for 
missing values or empty strings. Words containing special characters or numbers are 
removed, upper case letters are set to lower case, and all unnecessary empty spaces are stripped.

In the second step, each word is lemmatized. Here, we make use of the package **textstem** [@textstem].

Following lemmatization, the words are subjected to stemming. The algorithm 
conducting the stemming is called Porter's stemming algorithm [@Porter1980],
implemented via the the R-package **SnowballC** [@SnowballC].

In the final step, the document-term matrix is created.
For each lemmatized and stemmed word, the number of appearances in the title
and abstract is determined, potentially weighted, and written into the matrix.

The whole processing pipeline described above makes heavy use of the R-package **tm** [@tm].

Additionally, **MetaNLP** offers the function `select_features` 
to apply elastic net regularization within the data processing workflow. Here, the 
R-package **glmnet** [@glmnet1; @glmnet2] is applied.

# Usage

**MetaNLP** creates the document-term matrix from a CSV file containing all
the abstracts and the "include/exclude" decision. It should have the following form:

![](figures/CSV_screenshot.png)

The creation of the document-term matrix is simple now.

```
library(MetaNLP)
dtm <- MetaNLP("data/example.csv", 
               bounds = c(2, Inf), 
               word_length = c(3, Inf),
               weighting = "frequency",
               language = "english")
```

The argument `bounds` specifies how often a word must occur to become part of 
the document-term matrix. The argument `word_length` determines the minimum and 
maximum number of characters a word should have. The argument `weighting` is used to 
specify the weighting function used for the creation of the document-term matrix.
The object `dtm` now has a slot `data_frame` which contains the desired document-term
matrix.

## Visualize `MetaNLP` objects

To obtain a comprehensive overview of the data, we can use the function `summary`.
This method shows the most common words in total and stratified by "include" and "exclude". 

```
summary(dtm, n = 5, stop_words = TRUE)
```

As stop words may not provide any useful information, they can be excluded from
the summary by specifying `stop_words = FALSE`. 

One way to visualize a `MetaNLP` object is the `plot` function which creates a bar 
chart of the $n$ most common words. The argument `decision` specifies whether the 
most frequent words from "exclude" or "include" should be included.
The default is "total", meaning that "exclude" and "include" are pooled.

```
plot(dtm, n = 10, decision = "total", stop_words = FALSE)
```

Another way is the function `wordcloud`. It has almost identical arguments as 
`plot`, but instead of a bar chart, it creates a wordcloud of the most common words.

```
wordcloud(dtm, max.words = 70, decision = "include")
```

## Delete words

A natural step to reduce the number of columns is the deletion of stop words.

```
dtm_delete <- delete_stop_words(dtm)
```

In general, words can be easily removed by the method `delete_words`,
which takes a MetaNLP-object and a list of words to delete as arguments.

```
dtm_delete <- delete_words(dtm_delete, c("aim", "administer"))
```

## Dimension reduction

In order to further reduce the dimensionality of the feature space, **MetaNLP** 
offers the method `select_features`.

```
dtm_1se <- select_features(dtm, alpha = 0.001, lambda = "1se", seed = 42)
dtm_min <- select_features(dtm, alpha = 0.001, lambda = "min", seed = 42)
```

The argument `alpha` denotes the elastic net mixing parameter and `lambda` is the 
parameter of the penalty. 
More detailed information is provided in the function's documentation.

## Test data

Suppose, a machine learning model has been trained using the document-term matrix and 
shall be used to classify more publications of the test data now. For this, 
the document-term matrix of the test data must have exactly the same features as the matrix
of the training data. The function `read_test_data` takes a `MetaNLP` object and 
the path to the test data as arguments and assimilates its columns by removing columns 
which only appear in the test data and by adding columns as zero-columns which only exist in the 
document-term matrix used for training.
 
```
test_dtm <- MetaNLP("data/example_test.csv")
test_read_dtm <- read_test_data(dtm, "data/example_test.csv")
```

## Non-english languages

The language of the abstracts can be passed to `MetaNLP`
via the `language` argument. This argument is quite important: the look-up table
used in lemmatization is language specific, as different languages have a 
completely different vocabulary. Additionally, there exist various versions
of Porter's stemming algorithm which are fine-tuned to different languages,
respectively. **MetaNLP** supports the languages German, Spanish, French and Russian.

```
dtm <- MetaNLP("data/example.csv", language = "english")
dtm_ef <- MetaNLP("data/example.csv", language = "french")
```

Language-specific special characters like "ê" or "ä" can be replaced via the method
`replace_special_characters`.



# References
