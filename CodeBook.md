# JHU Getting and Cleaning Data Course Project
## Brief Description
This contains a codebook that modifies and updates the available codebooks with the data to indicate all the variables and 
summaries calculated, along with units, and any other relevant information
## Abstract 
Human Activity Recognition database built from the recordings of 30 subjects performing activities of daily living (ADL) 
while carrying a waist-mounted smartphone with embedded inertial sensors.
## Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person 
performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a 
smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear 
acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the 
data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected 
for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-
width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has 
gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and 
gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff 
frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.
## Attribute Information
For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables.
- Its activity label.
- An identifier of the subject who carried out the experiment.
## Data Sets Files
- 'README.txt'
- 'features_info.txt': Shows information about the variables used on the feature vector.
- 'features.txt': List of all features.
- 'activity_labels.txt': Links the class labels with their activity name.
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.
## Source Links
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.names
