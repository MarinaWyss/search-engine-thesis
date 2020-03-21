import pandas as pd

path = "/Users/marinabennett/Desktop/Hertie/0. Thesis/search-engine-thesis/data/forModels/"
full_before = pd.read_csv(path + "textBlockBefore.csv").drop(columns = "Unnamed: 0")


full_before["text"] = full_before["text"].replace(['?.', '?'])
