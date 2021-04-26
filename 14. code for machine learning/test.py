# -*- coding: utf-8 -*-



# Python version
import sys

print('Python: {}'.format(sys.version))
# scipy
import scipy

print('scipy: {}'.format(scipy.__version__))
# numpy
import numpy

print('numpy: {}'.format(numpy.__version__))
# matplotlib
import matplotlib

print('matplotlib: {}'.format(matplotlib.__version__))
# pandas
import pandas

print('pandas: {}'.format(pandas.__version__))
# scikit-learn
import sklearn

print('sklearn: {}'.format(sklearn.__version__))

# Load libraries
from pandas.plotting import scatter_matrix
import matplotlib.pyplot as plt
from sklearn import model_selection
from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score
from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.naive_bayes import GaussianNB
from sklearn.svm import SVC
from sklearn.ensemble import VotingClassifier
import warnings
import re
import math

# Load dataset
# names = []
# for i in range(141):
#     names.append('gene_' + str(i))

dataset = pandas.read_csv(r'E:/pythontest/tcga.csv',
                          index_col=0
                          )
solutionset = pandas.read_csv(
    r'E:/pythontest/groupinput.csv')

# shape
print(dataset.shape)
print(solutionset.shape)
print(dataset.head(5))
print(solutionset.head(5))
'''
# head
#print(dataset.head(20))

# describe
print(dataset.describe())

# box and whisker plots
dataset.plot(kind='box', subplots=True, layout=(2,2), sharex=False, sharey=False)
plt.show()

# histograms
dataset.hist()
plt.show()

# scatter plot matrix
scatter_matrix(dataset)
plt.show()
'''

#X = sklearn.preprocessing.scale(dataset.values[:, :], axis=0)
X = dataset.values[:, :]
Y = solutionset.values[:, 1]
# print(X)
# print(Y)


'''
# Split-out validation dataset
array = dataset.values
X = array[:,0:4]
Y = array[:,4]
'''

validation_size = 0.2
seed = 2020
X_train, X_validation, Y_train, Y_validation = model_selection.train_test_split(X, Y, test_size=validation_size,
                                                                                random_state=seed)

# Test options and evaluation metric
scoring = 'accuracy'

warnings.filterwarnings(action='ignore', category=UserWarning)

# Spot Check Algorithms
models = []
models.append(('LR', LogisticRegression()))
models.append(('LDA', LinearDiscriminantAnalysis()))
models.append(('KNN', KNeighborsClassifier()))


# evaluate each model in turn
# results = []
# names = []
# for name, model in models:
#     kfold = model_selection.KFold(n_splits=10, random_state=seed)
#     cv_results = model_selection.cross_val_score(model, X_train, Y_train, cv=kfold, scoring=scoring)
#     results.append(cv_results)
#     names.append(name)
#     msg = "%s: %f (%f)" % (name, cv_results.mean(), cv_results.std())
#     print(msg)

"""
# Implement Voting

ensemble = []
resultsOFVoting = []
warnings.filterwarnings(action='ignore', category=DeprecationWarning)
ensemble = VotingClassifier(models)
resultsOFVoting = model_selection.cross_val_score(ensemble, X_train, Y_train, cv=kfold, scoring=scoring)
msg1 = "Voting Classifier: %f (%f)" % (resultsOFVoting.mean(), resultsOFVoting.std())
print(msg1)

"""


# Make predictions on unknown dataset
# print('Choose an Algorithm to make your prediction:')
# export validation data
data1 = pandas.DataFrame(Y_validation)
print(data1.head(5))
data1.rename(columns={0: 'Cluster_type'}, inplace=True)
data1.to_csv('Y_validation.csv')
data2 = pandas.DataFrame(index=range(0, len(X_validation)))
for name, model in models:
    print(name)
    choice = name
    # choice = input('Enter your choice: ')
    # choice = "SVM"
    best = ''
    if choice == 'LR':
        best = LogisticRegression()
    elif choice == 'LDA':
        best = LinearDiscriminantAnalysis()
    elif choice == 'KNN':
        best = KNeighborsClassifier()

    if best != '':
        best.fit(X_train, Y_train)
        predictions = best.predict(X_validation)
        pri_res = pandas.DataFrame(predictions)
        pri_res.rename(columns={0: f"{choice}_cluster"}, inplace=True)
        data2 = pandas.merge(pri_res, data2, left_index=True, right_index=True)
        res = open(f"{choice}_test_result.txt", "a")
        print(accuracy_score(Y_validation, predictions), file=res)
        print(confusion_matrix(Y_validation, predictions), file=res)
        print(classification_report(Y_validation, predictions), file=res)
        res.close
data2.to_csv("validation_predictions_on_score_results.csv")
print("complete")