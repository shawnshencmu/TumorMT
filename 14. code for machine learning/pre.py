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
from sklearn.ensemble import VotingClassifier
import warnings
import re
import math
from sklearn import preprocessing

# 注意改地址
dataset = pandas.read_csv(r'E:/pythontest/tcga.csv',
                          index_col=0
                          )
solutionset = pandas.read_csv(
    r'E:/pythontest/groupinput.csv')

# loading predicted data
X_pred = pandas.read_csv(r'E:/pythontest/mouse.csv',
                         index_col=0)
rowNam = pandas.DataFrame(list(X_pred.index))
rowNam.rename(columns={0: "sample_ID"}, inplace=True)
# X_pred = sklearn.preprocessing.scale(X_pred.values)
X_pred = X_pred.values

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

# X = sklearn.preprocessing.scale(dataset.values[:, :])
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

# validation_size = 0.2
seed = 2020

scoring = 'accuracy'

warnings.filterwarnings(action='ignore', category=UserWarning)

# Spot Check Algorithms
models = []
models.append(('LR', LogisticRegression()))
models.append(('LDA', LinearDiscriminantAnalysis()))
models.append(('KNN', KNeighborsClassifier()))

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

data2 = pandas.DataFrame(index=range(0, len(X_pred)))
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
        best.fit(X, Y)
        predictions = best.predict(X_pred)
        pre_res = pandas.DataFrame(predictions)
        pre_res.rename(columns={0: f"{choice}_cluster"}, inplace=True)
        data2 = pandas.merge(pre_res, data2, left_index=True, right_index=True)
        # res = open(f"{choice}_test_result.txt", "a")
        # print(accuracy_score(Y_validation, predictions), file=res)
        # print(confusion_matrix(Y_validation, predictions), file=res)
        # print(classification_report(Y_validation, predictions), file=res)
        # res.close
data3 = pandas.merge(rowNam, data2, left_index=True, right_index=True)
data3.to_csv("E:/pythontest/pre.csv", index=0)
print("complete")