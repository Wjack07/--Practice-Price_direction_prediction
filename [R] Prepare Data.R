#setwd("D:/Google Drive/BigData/Kaggle/[Case 3] Otto Group Product Classification Challenge")
setwd("C:/Users/jsc69/Google Drive/BigData/Kaggle/[Self Case] Indices prediction")

train = read.csv("trains.csv",header=TRUE,stringsAsFactors = F)
smp_size <- floor(0.8 * nrow(train))
set.seed(123)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

train_sample <- train[train_ind,]  # sample has the random ability
test_sample <- train[-train_ind,]

write.csv(train_sample , file = "Train_small.csv", row.names = FALSE)
write.csv(test_sample , file = "Test_small.csv", row.names = FALSE)
