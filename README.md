# Case Study - “Predicting Earning Manipulations by Indian Firms Using Machine Learning Algorithms” 

## **Problem Statement**

The number of manipulators is usually much less than non-manipulators (in the accompanying spreadsheet, the percentage of manipulators is less than 4% in the complete data), which results in the modeling problem of data imbalance. We noted that the data is unbalanced, and the class of interest is a minority class.

## **Objectives**

* Use sample data (220 cases including 39 manipulators) to develop a logistic regression model that can be used by MCA Technologies Private Limited for predicting the probability of earnings manipulation.

* Evaluate the performance of your logistic regression model. How does your model perform on the training and test datasets?

* Develop a decision tree model.

* Develop a logistic regression model using the complete data set (1200 non-manipulators and 39 manipulators), and compare the results with the previous logistic regression model.

## **Suggestions**

There could be a high risk of biased results because of the unequal distribution of manipulators and non-manipulators. standard classifiers only focus on reducing errors and not the structure of data so they have more bias towards the majority class data rather than the minority therefore there is a higher chance of miss classification of the minority class.

In order to overcome this, we use the following approaches:

• Under Sampling: where we balance the majority class by randomly sampling them in order for the number to match the minority class. It is useful when the data set is large. However, it runs a risk of ignoring important data points that might not be picked in majority class sampling.

• Over Sampling: where we increase the number of minority samples to match the majority sample by replicating them. This avoids information loss.

**Synthetic Minority Over Sampling Technique: ** A subset is taken from the minority class as an example and then new synthetic similar instances are created instead of replicating and adding samples from the minority classes. It overcomes the unbalances by generating artificial data.

The following algorithms can be deployed:

* Bagging: different training samples with replacements, trains each sample using the bootstrapped algorithm and aggregates the results in the end.
• Boosting: Ada Boosting- This requires the users to specify a set of weak learners or randomly generated weak learners before the actual learning process. The weight of each learner is adjusted at every step depending on whether it predicts a sample correctly.
