The run_analysis.R script preforms the following functions to get the desired output 

The original data was transformed by - 

1.  Giving appropriate column names to all the relevant dataframes in the data set.
2.  Merging the test and training data sets.
3.  Extracting only the mean and standard deviation for each of the different types of measurements.
4.  Replacing activity code numbers with actual activity name.
5.  Adding descriptive variable names as labels in the dataset.
6.  Creating the final data frame with average value of each functions by subject and activity.

Variables -
Data Frames- 
1. features-  contains all the different functions.
2. activities - contains all the possible activities.
3. test_subject - contains subject numbers for the test data set.
4. test_features - contains values of features for the test data set.
5. test_activity - contains activities in the test data set.
6. train_subject - contains subject numbers for the train data set.
7. train_features - contains values of features for the train data set.
8. train_activity - contains activities in the train data set.
9. Merged_Set - contains merged data of both test and train data sets.
10. Final_Data_Frame - the final output data frame required by the problem statement.
