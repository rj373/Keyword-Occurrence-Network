getwd()

setwd("C:/Users/q1441217/Documents")

install.packages('readxl')

install.packages('igraph')

library('igraph')
library('quanteda')
library("readxl")

textdata <- read_excel("Keyword_data.xlsx", sheet = 'Keyword_data')

ind<-apply(textdata, 1, function(x) all(is.na(x)))

textdata <- textdata[ !ind, ]

# original corpus length and its first document

sotu_corpus <- corpus(textdata$Title)
                      
ndoc(sotu_corpus)

corpus_sentences <- corpus_reshape(sotu_corpus, to = "sentences")

ndoc(corpus_sentences)

# Build a dictionary of lemmas
lemma_data <- read.csv("baseform_en.tsv", encoding = "UTF-8")

# read an extended stop word list
stopwords_extended <- readLines("stopwords_en.txt", encoding = "UTF-8")

# Preprocessing of the corpus of sentences
corpus_tokens <- corpus_sentences %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma, valuetype = "glob", case_insensitive = TRUE,verbose = quanteda_options("verbose")) %>% 
  tokens_remove(pattern = stopwords_extended, padding = T)

# calculate multi-word unit candidates
sotu_collocations <- textstat_collocations(corpus_tokens, min_count = 5)
sotu_collocations <- sotu_collocations[1:49, ]

corpus_tokens <- tokens_compound(corpus_tokens, sotu_collocations)

minimumFrequency <- 5

# Create DTM, prune vocabulary and set binary values for presence/absence of types
binDTM <- corpus_tokens %>% 
  tokens_remove("") %>%
  dfm() %>% 
  dfm_trim(min_docfreq = minimumFrequency, max_docfreq = Inf) %>% 
  dfm_weight("boolean")

coocCounts <- t(binDTM) %*% binDTM

test_matrix <- as.matrix(coocCounts[,])

test_matrix <- mode(test_matrix)='numeric'

#colnames(test_matrix)

g1 <- graph_from_adjacency_matrix(test_matrix , mode='undirected', diag=F, weighted = TRUE )

class(g1)

plot(g1, edge.width= round(E(g1)$weight, 3),edge.label=round(E(g1)$weight, 3) )
