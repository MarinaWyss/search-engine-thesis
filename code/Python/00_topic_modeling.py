import pandas as pd
import numpy as np
from gsdmm import MovieGroupProcess
import nltk

path = "/Users/marinabennett/Desktop/Hertie/0. Thesis/search-engine-thesis/data/"
data = pd.read_csv(path + "fullDataSet.csv")

data["search_clean"] = data['search_term'].str.replace('[^\w\s]','')
data["tokenized_sents"] = data["search_clean"].apply(nltk.word_tokenize)

data_small = data.head(1000)
docs = data_small['tokenized_sents'].tolist()

# init of the Gibbs Sampling Dirichlet Mixture Model algorithm
mgp = MovieGroupProcess(K = 10, alpha = 0.1, beta = 0.1, n_iters = 10)
vocab = set(x for doc in docs for x in doc)
n_terms = len(vocab)

# fit model
y = mgp.fit(docs, n_terms)

# stats
doc_count = np.array(mgp.cluster_doc_count)
print('Number of documents per topics :', doc_count)

top_index = doc_count.argsort()[-10:][::-1]
print('Most important clusters (by number of docs inside):', top_index)

# topics
topics = mgp.cluster_word_distribution