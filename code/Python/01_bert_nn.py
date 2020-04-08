import pandas as pd
import numpy as np
import random
import re

import torch

import transformers as ppb

# NLP ----------------------------------------------------------------------

# load data
file = "/content/drive/My Drive/Colab Notebooks/textBlockPoliticalBefore.csv"
full_data = pd.read_csv(file)
full_data_vote = full_data.loc[full_data['voteChoice'].isin([1.0, 2.0])]

# text pre-processing
import nltk
nltk.download('stopwords')
from nltk.corpus import stopwords

sentences = full_data_vote['text']

# lowercase everything
sentences = [sentences.lower() for sentences in sentences]

# remove weird punctuation
full_data_vote['text'] = full_data_vote["text"].apply(lambda x: x.replace("?.", "?"))

# remove numbers
sentences = [re.sub('[0-9]','', s) for s in sentences]

# remove stopwords
clean = []
for item in sentences:
    for word in stopwords.words('english'):
        item = item.replace(" " + word + " ", ' ')
    clean.append(item)

full_data_vote['text'] = clean

# BERT embeddings
# importing model and tokenizer
model_class, tokenizer_class, pretrained_weights = (ppb.BertModel, ppb.BertTokenizer, 'bert-base-uncased')

# load model and tokenizer
tokenizer = tokenizer_class.from_pretrained(pretrained_weights)
model = model_class.from_pretrained(pretrained_weights)

# tokenization
tokenized = full_data_vote['text'].apply((lambda x: tokenizer.encode(x, add_special_tokens = True, max_length = 511)))

max_len = 0
for i in tokenized.values:
    if len(i) > max_len:
        max_len = len(i)

# padding
padded = np.array([i + [0]*(max_len - len(i)) for i in tokenized.values])

# attention
attention_mask = np.where(padded != 0, 1, 0)

input_ids = torch.tensor(padded)
attention_mask = torch.tensor(attention_mask)

# model
with torch.no_grad():
    last_hidden_states = model(input_ids, attention_mask = attention_mask)

# extract features and labels
features = last_hidden_states[0][:,0,:].numpy()

labels = full_data_vote['voteChoice']
labels = np.where(labels == 2.0, 0, 1)

# train test split
from sklearn.model_selection import train_test_split
train_features, test_features, train_labels, test_labels = train_test_split(features, labels, random_state = 42)

# balance classes
from imblearn.over_sampling import SMOTE
sm = SMOTE(random_state = 42)
train_features_sm, train_labels_sm = sm.fit_resample(train_features, train_labels)

# neural network
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras.layers import Dense, Dropout, Conv1D, GlobalMaxPooling1D

# feed-forward NN
model = tf.keras.Sequential([
    Dense(12, activation = 'relu', input_shape = (768,)),
    Dense(8, activation = 'relu', input_shape = (768,)),
    Dense(1, activation = 'sigmoid')])

model.compile(optimizer = 'adam',
              loss = 'binary_crossentropy',
              metrics = ['accuracy'])

model.fit(x = train_features_sm, y = train_labels_sm,
          epochs = 100,
          validation_split = 0.3,
          shuffle = True)

# prediction
test_pred = model.predict(test_features)
test_pred = (test_pred > 0.5)

from sklearn.metrics import classification_report
print(classification_report(test_labels, test_pred))

# Behavior ---------------------------------------------------------------------

# load data
file = "/content/drive/My Drive/Colab Notebooks/searchBehaviorBefore.csv"
full_data = pd.read_csv(file)

# prep data
full_data.drop(columns=['Unnamed: 0', 'pmxid'], inplace = True)

labels = full_data['turnout']
features = full_data.loc[:, full_data.columns.isin(['voteChoice', 'turnout']) == False]

# train test split
from sklearn.model_selection import train_test_split
train_features, test_features, train_labels, test_labels = train_test_split(features, labels, random_state = 42)

# feature scaling
from sklearn import preprocessing

names = train_features.columns
scaler = preprocessing.StandardScaler()
train_features_scaled = scaler.fit_transform(features)
train_features_scaled = pd.DataFrame(train_features_scaled, columns = names)

# balance classes
from imblearn.over_sampling import SMOTE
sm = SMOTE(random_state = 42)
train_features_sm, train_labels_sm = sm.fit_resample(train_features_scaled, train_labels)

# neural network
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras.layers import Dense, Dropout

model = tf.keras.Sequential([
    Dense(12, activation = 'relu', input_shape = (9,)),
    Dense(8, activation = 'relu'),
    Dense(1, activation = 'sigmoid')])

model.compile(optimizer = 'adam',
              loss = 'binary_crossentropy',
              metrics = ['accuracy'])

model.fit(x = train_features, y = train_labels,
          epochs = 100,
          validation_split = 0.3,
          shuffle = True)

# prediction and evaluation
test_pred = model.predict(test_features)
test_pred = (test_pred > 0.5)

from sklearn.metrics import classification_report
print(classification_report(test_labels, test_pred))

LIME --------------------------------------------------
# create index
orig_data = full_data.copy().reset_index()

Y = orig_data['voteChoice'].values
X = orig_data.drop(['voteChoice', 'turnout'], axis = 1)

# scale
names = X.columns
scaler = preprocessing.StandardScaler()
X = scaler.fit_transform(X)
X = pd.DataFrame(X, columns = names)

# determine which to compare
pred_probs = model.predict(X)
pred_probs = pred_probs[:,0]

compare_preds = pd.DataFrame({
    "index": orig_data['index'],
    "preds": pred_probs,
    'real': orig_data['voteChoice']
})

compare_preds['difference'] = abs(compare_preds['real'] - compare_preds['preds'])
compare_preds = compare_preds.sort_values(by = ['preds'])

import lime
import lime.lime_tabular
explainer = lime.lime_tabular.LimeTabularExplainer(X[list(X.columns)].astype(int).values,
                                                   mode='classification',
                                                   training_labels = full_data['voteChoice'],
                                                   feature_names=list(X.columns))

def prob(data):
    y_pred = model.predict(data).reshape(-1, 1)
    return np.hstack((1-y_pred, y_pred))

i = 379
exp = explainer.explain_instance(X.loc[i, X.columns].astype(int).values, prob)