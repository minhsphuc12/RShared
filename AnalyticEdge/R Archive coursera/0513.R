#this code will include the merging of Intertia data which is not...
#actually manipulated in any sort.
#the only data merged, given header and filtered is X,Y and Subject...
#files from 'test' and 'train' folders

#You should have folder named 'UCI HAR Dataset' in your working ...
#directory before running this script

#train data file path
a1=file.path(getwd(),'/UCI HAR Dataset/train/X_train.txt')
a2=file.path(getwd(),'/UCI HAR Dataset/train/y_train.txt')
a3=file.path(getwd(),'/UCI HAR Dataset/train/subject_train.txt')
a4=file.path(getwd(),'/UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt')
a5=file.path(getwd(),'/UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt')
a6=file.path(getwd(),'/UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt')
a7=file.path(getwd(),'/UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt')
a8=file.path(getwd(),'/UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt')
a9=file.path(getwd(),'/UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt')
a10=file.path(getwd(),'/UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt')
a11=file.path(getwd(),'/UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt')
a12=file.path(getwd(),'/UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt')

#test data file path
b1=file.path(getwd(),'/UCI HAR Dataset/test/X_test.txt')
b2=file.path(getwd(),'/UCI HAR Dataset/test/y_test.txt')
b3=file.path(getwd(),'/UCI HAR Dataset/test/subject_test.txt')
b4=file.path(getwd(),'/UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt')
b5=file.path(getwd(),'/UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt')
b6=file.path(getwd(),'/UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt')
b7=file.path(getwd(),'/UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt')
b8=file.path(getwd(),'/UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt')
b9=file.path(getwd(),'/UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt')
b10=file.path(getwd(),'/UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt')
b11=file.path(getwd(),'/UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt')
b12=file.path(getwd(),'/UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt')

#read train data
da1=read.table(a1)
da2=read.table(a2)
da3=read.table(a3)
da4=read.table(a4)
da5=read.table(a5)
da6=read.table(a6)
da7=read.table(a7)
da8=read.table(a8)
da9=read.table(a9)
da10=read.table(a10)
da11=read.table(a11)
da12=read.table(a12)

#read test data
db1=read.table(b1)
db2=read.table(b2)
db3=read.table(b3)
db4=read.table(b4)
db5=read.table(b5)
db6=read.table(b6)
db7=read.table(b7)
db8=read.table(b8)
db9=read.table(b9)
db10=read.table(b10)
db11=read.table(b11)
db12=read.table(b12)

#merging corresponding train and test data
dc1=rbind(da1,db1)
dc2=rbind(da2,db2)
dc3=rbind(da3,db3)
dc4=rbind(da4,db4)
dc5=rbind(da5,db5)
dc6=rbind(da6,db6)
dc7=rbind(da7,db7)
dc8=rbind(da8,db8)
dc9=rbind(da9,db9)
dc10=rbind(da10,db10)
dc11=rbind(da11,db11)
dc12=rbind(da12,db12)

#add header to data sets
u=file.path(getwd(),'/UCI HAR Dataset/features.txt')
u1=read.table(u)
u2=u1[,2]
u2=as.character(u2)
colnames(dc1)=u1[,2]




#filter data set to mean and deviation measurements only
u3=u2[(grepl("mean",u2)|(grepl("std",u2)))]
dcall=dc1[, which(names(dc1) %in% u3)]

#merge X, Y and Subject file
colnames(dc2)='Label'
colnames(dc3)='Subject'
dcall2=cbind(dc2,dc3,dcall)

#write tidy data to file
write.table(dcall2, file = "tidy_data.txt", sep = " ", col.names = colnames(dcall2))
