run_analysis <- function(path) {
      #library(stringr)
      #library(tidyr)
      #library(reshape2)
      #library(plyr)
      #library(data.table)
      
      setwd(path)
      
      features <- fread(".\\UCI HAR Dataset\\features.txt")
      activity_labels <-fread(".\\UCI HAR Dataset\\activity_labels.txt")
      
      subject_test <- fread(".\\UCI HAR Dataset\\test\\subject_test.txt")
      test_set <- fread(".\\UCI HAR Dataset\\test\\X_test.txt")
      test_set_labels <- fread(".\\UCI HAR Dataset\\test\\y_test.txt")
      
      subject_train <- fread(".\\UCI HAR Dataset\\train\\subject_train.txt")
      train_set <- fread(".\\UCI HAR Dataset\\train\\X_train.txt")
      train_set_labels <- fread(".\\UCI HAR Dataset\\train\\y_train.txt")
      
      test_data <- cbind(subject_test,test_set_labels,test_set)
      train_data <- cbind(subject_train,train_set_labels,train_set)
      
      all_data <-rbind(test_data,train_data)
      names(all_data) <- c("Subject","Activity",features$V2)
      filtered_columns <- as.numeric(grep('Activity|Subject|*mean\\(\\)*|*std*',names(all_data)))
      filtered_data <- all_data[,..filtered_columns]
      
      melt_data <- melt(filtered_data,id=colnames(filtered_data[,1:2]),measure.vars=names(filtered_data[,3:length(filtered_data)]))
      tidy_data <- mutate(melt_data,"Domain"= substr(melt_data$variable,1,1))
      tidy_data <- mutate(tidy_data,"variable"=substr(melt_data$variable,2,length(melt_data$variable)))
      tidy_data <- separate(tidy_data,col = variable, into = c("Signal", "Function","Axis"))
      
      
      domain_frame <- data.frame("Domain" = c("t","f"),"Domain_Description" = c("Time","Frequency"))
      function_frame <- data.frame("Function" = c("mean","std"),"Function_Description"=c("Mean","Standard_Deviation"))
      activity_frame <- data.frame("Activity" = activity_labels$V1, "Activity_Description" = str_to_title(activity_labels$V2))
      
      tidy_data <- merge(domain_frame,tidy_data,by.y="Domain",all=TRUE)
      tidy_data <- merge(function_frame,tidy_data,by.y="Function",all=TRUE)
      tidy_data <- merge(activity_frame,tidy_data,by.y="Activity",all=TRUE)
      
      tidy_data <- mutate(tidy_data,Activity = Activity_Description,Function = Function_Description,Domain=Domain_Description)
      order <- c(7,1,5,8,3,9,10)
      tidy_data <- tidy_data[,order]
      tidy_data2 <- tidy_data[order(tidy_data$Subject,tidy_data$Activity,tidy_data$Domain,tidy_data$Signal,tidy_data$Function,tidy_data$Axis),]
      tidy_data3 <- ddply(tidy_data2,.(Subject,Activity,Domain,Signal,Function,Axis),summarize,average=mean(value))
      print(tidy_data3)
      write.csv(tidy_data3,".\\WearableComputingProject_Tidy_Data.csv",row.names=FALSE)
      
}