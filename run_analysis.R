run_analysis <- function(path) {
      
      # libraries needed to utilize the functions in run_analysis
      #library(stringr)
      #library(tidyr)
      #library(reshape2)
      #library(plyr)
      #library(data.table)
      
      ## Sets the working directory to where the data set was downloaded
      setwd(path)
      
      ## reads the features.txt and activity_labels.txt files, which includes the domain, signal, function, axis, and activity labels
      features <- fread(".\\UCI HAR Dataset\\features.txt")
      activity_labels <-fread(".\\UCI HAR Dataset\\activity_labels.txt")
      
      ## reads the subject_test.txt, X_test.txt, and y_test.txt files, which include the test subject and activity labels, as well as the data values
      subject_test <- fread(".\\UCI HAR Dataset\\test\\subject_test.txt")
      test_set <- fread(".\\UCI HAR Dataset\\test\\X_test.txt")
      test_set_labels <- fread(".\\UCI HAR Dataset\\test\\y_test.txt")
      
      ## reads the subject_train.txt, X_train.txt, and y_train.txt files, which include the train subject and activity labels, as well as the data values
      subject_train <- fread(".\\UCI HAR Dataset\\train\\subject_train.txt")
      train_set <- fread(".\\UCI HAR Dataset\\train\\X_train.txt")
      train_set_labels <- fread(".\\UCI HAR Dataset\\train\\y_train.txt")
      
      # binds the labels and data values together as columns
      test_data <- cbind(subject_test,test_set_labels,test_set)
      train_data <- cbind(subject_train,train_set_labels,train_set)
      
      # binds the train and test data sets
      all_data <-rbind(test_data,train_data)
      
      # adds column names to the data set
      names(all_data) <- c("Subject","Activity",features$V2)
      
      # filters data for mean and standard deviation functions
      filtered_columns <- as.numeric(grep('Activity|Subject|*mean\\(\\)*|*std*',names(all_data)))
      filtered_data <- all_data[,..filtered_columns]
      
      # melts all data values (all columns except 1 and 2) into one "variable" column
      melt_data <- melt(filtered_data,id=colnames(filtered_data[,1:2]),measure.vars=names(filtered_data[,3:length(filtered_data)]))
      
      # separates the domain, signal, function, and axis columns from the variable column
      tidy_data <- mutate(melt_data,"Domain"= substr(melt_data$variable,1,1))
      tidy_data <- mutate(tidy_data,"variable"=substr(melt_data$variable,2,length(melt_data$variable)))
      tidy_data <- separate(tidy_data,col = variable, into = c("Signal", "Function","Axis"))
      
      # creates tidy labels for the domain, function, and activity columns
      domain_frame <- data.frame("Domain" = c("t","f"),"Domain_Description" = c("Time","Frequency"))
      function_frame <- data.frame("Function" = c("mean","std"),"Function_Description"=c("Mean","Standard_Deviation"))
      activity_frame <- data.frame("Activity" = activity_labels$V1, "Activity_Description" = str_to_title(activity_labels$V2))
      
      # adds domain, function, and activity columns as tidy labels
      tidy_data <- merge(domain_frame,tidy_data,by.y="Domain",all=TRUE)
      tidy_data <- merge(function_frame,tidy_data,by.y="Function",all=TRUE)
      tidy_data <- merge(activity_frame,tidy_data,by.y="Activity",all=TRUE)
      
      # replaces the original domain, function and activity columns with the tidy labels
      tidy_data <- mutate(tidy_data,Activity = Activity_Description,Function = Function_Description,Domain=Domain_Description)
      
      # reorders the columns in the order subject, activity, domain, signal, function, axis, and value; then sorts the data alphabetically from the first to the last column
      order <- c(7,1,5,8,3,9,10)
      tidy_data <- tidy_data[,order]
      tidy_data2 <- tidy_data[order(tidy_data$Subject,tidy_data$Activity,tidy_data$Domain,tidy_data$Signal,tidy_data$Function,tidy_data$Axis),]
      
      # calculates the average of each unique set of subject, activity, domain, signal, function, and axis values
      tidy_data3 <- ddply(tidy_data2,.(Subject,Activity,Domain,Signal,Function,Axis),summarize,average=mean(value))
      
      # outputs the tidy data set and saves it to a .csv file
      print(tidy_data3)
      write.csv(tidy_data3,".\\WearableComputingProject\\WearableComputingProject_Tidy_Data.csv",row.names=FALSE)
      
}