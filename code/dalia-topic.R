rm(list = ls(
  
))

#load packages

library(kableExtra)
library(quanteda)
library(tm)
library(tidyverse)
library(ggthemes)
library(ggwordcloud)
library(udpipe)
library(topicmodels)
library(stopwords)


## Query scopus

#EXACTSRCTITLE ( "journal of international business studies" OR "journal of world business" , OR "columbia journal of world business" OR "global strategy journal" OR "journal of international management" OR "international business review" OR "management international review" ) 
#AND ( LIMIT-TO ( DOCTYPE , "ar" ) ) AND ( LIMIT-TO ( LANGUAGE , "English" ) ) AND ( LIMIT-TO ( EXACTSRCTITLE , "Journal Of International Business Studies" ) OR LIMIT-TO ( EXACTSRCTITLE , "International Business Review" ) OR LIMIT-TO ( EXACTSRCTITLE , "Journal Of World Business" ) OR LIMIT-TO ( EXACTSRCTITLE , "Journal Of International Management" ) OR LIMIT-TO ( EXACTSRCTITLE , "Management International Review" ) 
OR LIMIT-TO ( EXACTSRCTITLE , "Global Strategy Journal" ) OR LIMIT-TO ( EXACTSRCTITLE , "Columbia Journal Of World Business" ) ) #####

# Read data


all_ib_scopus <-read.csv("../project/scopus (10).csv")  # Articles, and English
glimpse(all_ib_scopus)

## Count by type of IB journal

all_ib_scopus %>% group_by(Source.title) %>% 
  count(n())

# Filter for Source. title

journal_list <- c("Global Strategy Journal", "International Business Review", "Journal of International Business Studies",  
                  "Journal of International Management", "Journal of World Business", "Management International Review")

all_ib_scopus1 <-all_ib_scopus %>% filter(Source.title %in% journal_list)

str(all_ib_scopus1)  # the abstract is a character variable

## Number of articles per year

all_ib_scopus1 %>%     
  count(Year, Source.title) %>%    
  ggplot(aes(x = Year, y = n, color = Source.title, group = Source.title)) +    
  geom_line() +    
  geom_point() +   
  # geom_label(aes(label = n)) +   
  theme_minimal() + 
  ggtitle("Number of published articles per journal. Annual production.") +
  labs(color = "Source Title") 

# Generate a summary table
table_per_source <- all_ib_scopus1 %>%
  count(Source.title, Year) %>%
  arrange(Source.title, Year) %>%
  group_by(Source.title) %>%
  summarise(
    Annual_Production = paste0(Year, ": ", n, collapse = ", ")
  )

# Display the table in a nicely formatted style
kable(
  table_per_source,
  col.names = c("Source Title", "Annual Production"),
  caption = "Annual Article Production Per Source Title",
  align = "l"
)

## Abstracts analysis

all_ib_scopus2 <- all_ib_scopus1 %>% select(DOI, Abstract, Year) %>% 
  rename(absText = Abstract)

scopus_abstracts <- all_ib_scopus2$absText

scopus_abstracts[[1]]

## Remove stopwords and numbers

doc_corpus <-corpus(all_ib_scopus2$absText)
summary(doc_corpus)
doc_tokens <-tokens(doc_corpus)
doc_tokens



doc_tokens = tokens(doc_tokens, 
                    remove_punct = TRUE, 
                    remove_numbers = TRUE,
                    remove_symbols = TRUE
)
doc_tokens

# Remove stop-words in English

doc_tokens = tokens_select(doc_tokens,
                           stopwords('english'), # make sure of spelling for stopwords
                           selection = 'remove'
)
doc_tokens


## Corpus consisting of  documents.
#myDfm <- dfm(doc_tokens, verbose = FALSE)
#head(docfreq(myDfm), 50)  # count of documents in which a term occurs


# Remove specific unwanted tokens

doc_tokens <- tokens_remove(doc_tokens, pattern = c("i.e", "study", "Authors", "Author", "s",
                                                    "study", "research", "paper", "data", "results", "method", "methods", 
                                                    "analysis", "findings", "significant"))

doc_tokens[[1]]  # 1 abstract of the dataframe


doc_tokens1 = tokens_wordstem(doc_tokens)

# Create a document-feature matrix (DFM)
dfm1 <- dfm(doc_tokens1)
summary(dfm1)
class(dfm1)
dim(dfm1)  # Number of documents 6802 and number of terms 14688
featnames(dfm1)[1:10]  # First 10 terms



# Get the top 1000 most frequent terms and their frequencies
top_terms <- topfeatures(dfm1, n = 1000)
top_terms_names <- names(top_terms)



# Convert DFM to a Term-Document Matrix (TDM)

# Fit LDA model (k is the number of topics)


# Inspect the results


## anOTHER WAY

library(udpipe) # la cargamos
modelo_en <- udpipe::udpipe_download_model('english') # descarga el modelo y guarda la referencia  
modelo_en$file_model # referencia al modelo descargado
modelo_en <- udpipe_load_model(file = modelo_en$file_model) # cargamos el modelo en memoria



abstracts_anotados <- udpipe_annotate( 
  object = modelo_en, # el modelo de idioma
  x = all_ib_scopus2$absText, # el texto a anotar, 
  doc_id = all_ib_scopus2$DOI, # el id de cada oracion (el resultado tendrá 1 palabra x fila)
  trace = 20
) %>% as.data.frame(.) # convertimos el resultado en data frame

###

glimpse(abstracts_anotados)


##

abstracts_anotados2 <- abstracts_anotados %>% 
  filter(upos=="ADJ"| upos=="VERB"| upos=="NOUN" | upos=="ADV") %>%  # filtramos por tipo de palabra
  select(doc_id, lemma) %>% # seleccionamos solo las columnas que nos interesan, esto no es necesario
  filter(!lemma %in% stopwords::stopwords(language = "en")) %>% # filtrar las que no están en la tabla de stopwords
  filter(!lemma %in% c("findings","paper","results")) # filtramos palabras típicas del género de los documentos
glimpse(abstracts_anotados2)

abstracts_anotados2$lemma

##
library(tidytext)
abstracts_dtm <- abstracts_anotados2 %>%
  count(doc_id, lemma, sort = TRUE) %>% # contamos palabras x documento
  cast_dtm(doc_id, lemma, n) # convertimos a vector
abstracts_dtm



###

library(topicmodels)
k_topics <- 5 # numero de topicos
abstracts_tm <- topicmodels::LDA(
  abstracts_dtm, # vector de terminos por documentos
  k = k_topics, # cantidad de topicos
  method = "Gibbs", # metodo de sampleo de los documentos
  control = list(seed = 1:5, nstart=5, verbose=1000))

as.matrix(terms(abstracts_tm,5))  # get access to the 5 more important terms per topic


abstracts_tm_beta <- tidy(abstracts_tm, matrix = "beta")
abstracts_tm_gamma <- tidy(abstracts_tm, matrix = "gamma")
glimpse(abstracts_tm_beta)


abstracts_tm_beta %>% # principales términos en cada tópico
  group_by(topic) %>%
  top_n(15) %>%
  ungroup() %>%
  arrange(topic, -beta) %>% # vamos a mostrarlo como grafico
  ggplot(aes(x=reorder(term, (beta)),y=beta)) + 
  geom_col() +
  facet_wrap(~topic, scales = "free_y") +
  coord_flip() +
  theme_minimal()
