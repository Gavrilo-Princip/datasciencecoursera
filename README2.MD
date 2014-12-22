Ecoursera_getting_cleaning_data
This repository has the information for the project for Coursera's Getting and Cleaning Data course and the course project. Getting and Cleaning Data is the third class in a series of 9 classes in the specialization. All the analysis is done in R programming.

Data Info: Data is from Human Activity Recognition Using Smartphones Dataset Version 1.0. Original data can be found at: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
Data provided by :
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
http://www.smartlab.ws
Data description: The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details.
Deliverables for Project 1
The Samsung tidy data I created can be reconstructed by following the steps I will spell out in this ReadMe document.
In this repository you will find a run_analysis.R file where by running the code you will get a tidy data file of Samsung info
The last deliverable for this project is a codebook. You can find a CodeBook.md markdown file where I list all the variables in the tidy list.
Bonus: I added my tidy data set found as a file phoneTidyData2.txt
Steps to run the Activities_analysis.R file.
Step 1. Original data can be found at http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones. Read in train and test data to have it handy for data manipulation.
Step 2. Create subject data frame with subjects used in the testing set data. We will build the data with subjectID, ActivityID, and Activity.
Step 3. Step 3: Create subject data from with subjects used in the training set data. We will build the data with subjectID, ActivityID, and Activity.
Step 4. Merge the Subject Train and Test data
Step 5. Clean up the features variables to Camel style to be compliant with R naming convention.
Step 6. Merge test and train data.
Step 7. Add feature labels to our phoneData.
Step 8. Extract columns that contain data for standard deviation (Std) and Mean.
Step 9. Merge the subject data with the phone data
Step 10. Save First Tidy Data set to a .txt file
Step 11. Create a second tidy data set with only subjectID, Activity, and average of each activity and each subject.
Step 12. Save a file copy of your final tidy data set in csv and txt
The results you should get by following the above steps is tidy data set of 180 rows by 68 columns that include the Subject and the activity of each record.nter file contents here
