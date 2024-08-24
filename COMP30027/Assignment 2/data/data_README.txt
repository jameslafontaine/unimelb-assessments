================= Data Files =================

1. book_rating_train.csv
This file contains the book features and label for training instances.
Number of instances: 23063
Number of columns: 10
The columns are (the column names are in the first row):
	Name, Authors, PublishYear, PublishMonth, PublishDay, Publisher, Language, pagesNumber, Description, rating_label


The columns Name, Authors and Description contain the raw text data of these features.

The class label is in the last column: rating_label. There are 3 possible levels, 3, 4 or 5.

2. book_rating_test.csv
This file contains the book features for test instances.
Number of instances: 5766
Number of columns: 9
The columns are (the column names are in the first row):
	Name, Authors, PublishYear, PublishMonth, PublishDay, Publisher, Language, pagesNumber, Description


3. book_text_features_*.zip: preprocessed text features for training and test sets, 1 zipped file for each text encoding method

3.1 book_text_features_countvec.zip
CountVectorizer converts a collection of text documents to a matrix of token counts. There are 9 files in this folder.

(1) train_name_countvectorizer.pkl
This file contains the CountVectorizer extracted using the text of the book "name" in the training set.
To load the file in Python:
	vocab = pickle.load(open("train_name_countvectorizer.pkl", "rb"))
	
To access the list of vocabulary (this will give you a dict):
	vocab_dict = vocab.vocabulary_
	
More about how to use the CountVectorizer can be found: https://scikit-learn.org/stable/modules/generated/sklearn.feature_extraction.text.CountVectorizer.html

(2) train_authors_countvectorizer.pkl
This file contains the CountVectorizer extracted using the text of the book "authors" in the training set.

(3) train_desc_countvectorizer.pkl
This file contains the CountVectorizer extracted using the text of the book "description" in the training set.

(4) train_name_vec.npz
This file contains a sparse matrix of the CountVectorizer (Bag-of-Word) representation of the book names for training data.
The dense version of this matrix should be [23063 * size of vocabulary], and the element (i,j) in the matrix is the count of each vocabulary term j in instance i. The vocabulary corresponds to the vocabulary_ attribute of vocab (which can be checked as detailed in (1))

As a lot of elements in this matrix are zeros, it has been compressed to a sparse matrix. After loading, the sparse matrix can be used as a normal matrix for training or testing.

To load the sparse matrix:
	import scipy
	scipy.sparse.load_npz('train_name_vec.npz')

train_authors_vec.npz and train_desc_vec.npz are the sparse matrices for book authors and description, respectively.

(5) test_name_vec.npz
This file contains a sparse matrix of the CountVectorizer (Bag-of-Word) representation of the book names for test data. 
The dense version of this matrix should be [5766 * size of vocabulary]. The vocabulary is the one that has been extracted from training, but the elements in this matrix are the counts for each book in the test set.

To load the sparse matrix:
	import scipy
	scipy.sparse.load_npz('test_name_vec.npz')
	
test_authors_vec.npz and test_desc_vec.npz are the sparse matrices for book authors and description, respectively.

3.2 book_text_features_doc2vec.zip
Doc2vec is an NLP tool for representing documents as a vector of m features, and the feature values are continuous numbers (positive or negative). There are 6 files in the folder.
(1) train_name_doc2vec100.csv
This file contains a matrix of Doc2Vec representation of the book names for training data, with 100 features.
The dimension of this matrix is [23063 * 100], and the element (i,j) in the matrix is a numeric value for feature j of an instance i. 

To load the matrix:
	import pandas as pd
	pd.read_csv(r"train_name_doc2vec100.csv", index_col = False, delimiter = ',', header=None)

train_authors_doc2vec20.csv and train_desc_doc2vec100.csv are the matrices for book authors (20 features) and description (100 features), respectively.

(2) test_name_doc2vec100.csv
This file contains a matrix of Doc2Vec representation of the book names for test data, with 100 features extracted from the training data.
The dimension of this matrix is [5766 * 100], and the element (i,j) in the matrix is a numeric value for feature j of an instance i. 

To load the matrix:
	import pandas as pd
	pd.read_csv(r"test_name_doc2vec100.csv", index_col = False, delimiter = ',', header=None)

test_authors_doc2vec20.csv and test_desc_doc2vec100.csv are the matrices for book authors (20 features) and description (100 features), respectively.