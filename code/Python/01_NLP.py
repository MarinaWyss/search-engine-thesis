import pandas as pd
import numpy as np
import random
import torch
import transformers as ppb
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression

# load data
path = "/Users/marinabennett/Desktop/Hertie/0. Thesis/search-engine-thesis/data/forModels/"
full_data = pd.read_csv(path + "textBlockBefore.csv").drop(columns = "Unnamed: 0")

# clean data
full_data["text"] = full_data["text"].apply(lambda x: x.replace("?.", "?"))
# full_data_vote = full_data.dropna()

# train test split
train_turnout, test_turnout = train_test_split(full_data, test_size = 0.3, random_state = random.seed(2342))

# import BERT model and tokenizer
model_class = ppb.BertModel
tokenizer_class = ppb.BertTokenizer
pretrained_weights = 'bert-base-uncased'

# load model and tokenizer
tokenizer = tokenizer_class.from_pretrained(pretrained_weights)
model = model_class.from_pretrained(pretrained_weights)

# tokenization
tokenized = full_data["search_term"].apply((lambda x: tokenizer.encode(x, add_special_tokens = True, max_length = 511)))

max_len = 0
for i in tokenized.values:
    if len(i) > max_len:
        max_len = len(i)

padded = np.array([i + [0]*(max_len - len(i)) for i in tokenized.values])

# BERT model
attention_mask = np.where(padded != 0, 1, 0)

input_ids = torch.tensor(padded)
attention_mask = torch.tensor(attention_mask)

with torch.no_grad():
    last_hidden_states = model(input_ids, attention_mask = attention_mask)

# extract features and labels
features = last_hidden_states[0][:, 0, :].numpy()
labels = full_data["voteChoice"]

# train test split
train_features, test_features, train_labels, test_labels = train_test_split(features, labels)

# basic logistic regression model
lr_clf = LogisticRegression()
lr_clf.fit(train_features, train_labels)

# accuracy
lr_clf.score(test_features, test_labels)