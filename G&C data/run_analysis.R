library(dplyr)
library(data.table)


dir  <- "./rawData" #Genera sub-segmento de ruta
zip  <- "./rawData/rawData.zip"#Genera sub-segmento de nombre de archivo
url  <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" #url del archivo
dir1 <- "./mydata" #subsegmento de ruta
dirdata<-"/UCI HAR Dataset/" #subsegmento de ruta de los archivos

try( 
  {
    if (!file.exists(dir)) { #Crea el directorio si no existe
      dir.create(dir)
      download.file(url = url, destfile = zip) #Descarga el archivo
    }
    if (!file.exists(dir1)) {#Crea directorio para almacenar los archivos al descomprimir 
      dir.create(dir1) 
      unzip(zipfile = zip, exdir = dir1) #Descomprime el archivo
    }
  }
  , silent = T)

#Train data
x_train <- read.table(paste(sep = "", dir1, dirdata, "train/X_train.txt")) #Lee la tabla en el archivo de entrenamiento
y_train <- read.table(paste(sep = "", dir1, dirdata, "train/Y_train.txt"))
s_train <- read.table(paste(sep = "", dir1, dirdata, "train/subject_train.txt"))


#test data
x_test <- read.table(paste(sep = "", dir1, dirdata, "test/X_test.txt")) #Lee la tabla en el archivo de testeo
y_test <- read.table(paste(sep = "", dir1, dirdata, "test/Y_test.txt"))
s_test <- read.table(paste(sep = "", dir1, dirdata, "test/subject_test.txt"))

data_x <- rbind(x_train, x_test) #Merges the training and the test sets to create one data set.
data_y <- rbind(y_train, y_test)
data_s <- rbind(s_train, s_test)

feature   <- read.table(paste(sep = "", dir1, dirdata, "features.txt")) ##Lee la tabla en el archivo de features
label     <- read.table(paste(sep = "", dir1, dirdata, "activity_labels.txt")) #Lee la tabla en el archivo de labels
label[,2] <- as.character(label[,2])

Cols <- grep("-(mean|std).*", as.character(feature[,2])) #Extracts only the measurements on the mean and standard deviation for each measurement. 
ColNames <- gsub("[-()]", "",
                 gsub("-std", "Std",
                      gsub("-mean", "Mean",
                           feature[Cols, 2])))
data_x <- data_x[Cols]  #Appropriately labels the data set with descriptive variable names. 
data <- cbind(data_s, data_y, data_x)
colnames(data) <- c("Subject", "Activity", ColNames)

data$Activity <- factor(data$Activity, levels = label[,1], labels = label[,2])
data$Subject <- as.factor(data$Subject)

data<-data.table(data)
data <- melt(data, id = c("Subject", "Activity"))
data <- dcast(data, Subject + Activity ~ variable, mean)
