#User Based Collaborative filtering
ubcf <- function(transaction.data , N = 5, Top_N_Similarity = 5){
        
        names(transaction.data) <- c("Customer.Id","Item")
        
        transaction.data <- na.omit(transaction.data)
        
        transaction.data <- lapply(transaction.data,as.character)
        
        transaction.data <- as.data.frame(transaction.data)
        
        transaction.data <- transaction.data[!duplicated(transaction.data),]
        
        if(!require(dplyr)){
                install.packages("dplyr")
                library(dplyr)
        }
                
                
        Destination.Frequency <- transaction.data %>% group_by(Item) %>% summarise(Frequency = length(Item))
        
        Destination.Frequency <- Destination.Frequency %>% arrange(desc(Frequency))
        
        Destination.Frequency <- Destination.Frequency$Item[1:N]
        
        Destination.Frequency <- as.character(Destination.Frequency)
        
        if(!require(reshape2)){
                install.packages("reshape2")
                library(reshape2)
        }
        
        
        Customer_Item.Matrix <- dcast(transaction.data,Customer.Id ~ Item,value.var = "Item",fun.aggregate = length,fill = 0)
        
        Customer_Similarity_matrix <- matrix(0,nrow = nrow(Customer_Item.Matrix),ncol = nrow(Customer_Item.Matrix))
        
        Customers <- Customer_Item.Matrix$Customer.Id
        
        Customer_Item.Matrix <- as.matrix(Customer_Item.Matrix[,-1])
        
        #Calculating Customer Similarity
        for(i in seq_along(Customers)){
                CustomerItem <- Customer_Item.Matrix[i,]
                
                for(j in i:length(Customers)){
                        PurchaseA <- Customer_Item.Matrix[i,]
                        PurchaseB <- Customer_Item.Matrix[j,]
                        Customer_Similarity_matrix[i,j] <- (sum(PurchaseA & PurchaseB)/sum(PurchaseA | PurchaseB))
                }
        }
        rm(i,j,PurchaseA, CustomerItem)
        
        Customer_Similarity_matrix <- Customer_Similarity_matrix + t(Customer_Similarity_matrix)
        diag(Customer_Similarity_matrix) <- 0
        
        Customer.Recommendations <- matrix("0",nrow = length(Customers),ncol = N)
        
        #Generating Recommendations
        for(i in seq_along(Customers)){
                TopN.SimilarCustomers <- order(Customer_Similarity_matrix[i,],decreasing = TRUE)[1:Top_N_Similarity]
                
                #Fixing Cold Start with TopN Decreasing Frequency items
                if(Customer_Similarity_matrix[i,][TopN.SimilarCustomers[1]] == 0){
                        Customer.Recommendations[i,] <- Destination.Frequency
                        next
                }
                Ordered_Recommendations <- order(colSums(!(Customer_Item.Matrix[i,]) & Customer_Item.Matrix[TopN.SimilarCustomers,]),decreasing = TRUE)
                Ordered_Recommendations <- Ordered_Recommendations[1:N]
                Customer.Recommendations[i,]  <- colnames(Customer_Item.Matrix)[Ordered_Recommendations]
        }
        Customer.Recommendations <- cbind(Customer.Id = as.character(Customers),Customer.Recommendations)
        colnames(Customer.Recommendations) <- c("Customer.Id",paste("Recommended.City",1:N,sep = ""))
        Customer.Recommendations <- as.data.frame(Customer.Recommendations)
        return(Customer.Recommendations)
        
}


mean_average_precision <- function(Recommended,Actual,N){
        
        names(Actual) <- c("Customer.Id","Item")
        
        Customers <- unique(Recommended[,"Customer.Id"])
        AvgPrecision <- 0
        
        for(i in 1:length(Customers)){
                if(Customers[i] %in% Actual$Customer.Id){
                        
                        Purchased <- unique(Actual[Actual$Customer.Id == Customers[i],"Item"])
                        normalizedN <- min(N,length(Purchased))
                        Recommendation <- Recommended[Recommended[,"Customer.Id"] == Customers[i],2:(normalizedN+1)]
                        
                        rel <- as.integer(Recommendation %in% Purchased)
                        AvgPrecision <- c(AvgPrecision, (1/normalizedN)*(sum(cumsum(rel)/1:normalizedN)) )
                }
                
        }
        AvgPrecision <- AvgPrecision[-1]
        results <- list(N = N,nMAP = mean(AvgPrecision),SDnMAP = sd(AvgPrecision))
        
        AvgPrecision <- 0
        for(i in 1:length(Customers)){
                if(Customers[i] %in% Actual$Customer.Id){
                        
                        Purchased <- unique(Actual[Actual$Customer.Id == Customers[i],"Item"])
                        Recommendation <- Recommended[Recommended[,"Customer.Id"] == Customers[i],2:(normalizedN+1)]
                        
                        rel <- as.integer(Recommendation %in% Purchased)
                        AvgPrecision <- c(AvgPrecision, (1/N)*(sum(cumsum(rel)/1:N)) )
                }
                
        }
        results$MAP <- mean(AvgPrecision)
        results$SDMAP <- sd(AvgPrecision)
        return(results)
}


cross_validation <- function(transaction_data,N = 5, folds = 5){
        
        if(!require(caret)){
                install.packages("caret")
                library(caret)
        }
        
        set.seed(5)
        data.folds <- createFolds(transaction_data$Item,k = folds,list = TRUE,returnTrain = FALSE)
        
        nMAP <- 0
        SDnMAP <- 0
        MAP <- 0
        SDMAP <- 0
        
        for(i in seq_along(data.folds)){
                
                index <- unname(unlist(data.folds[-i]))
                Customer.Recommendations <- ubcf(transaction_data[index,],N = N)
                recommendation_metrics <- mean_average_precision(Recommended = Customer.Recommendations,Actual = transaction_data[-index,],N = N)
                nMAP <- c(nMAP,recommendation_metrics$nMAP)
                SDnMAP <- c(SDnMAP,recommendation_metrics$SDnMAP)
                SDMAP <- c(SDMAP,recommendation_metrics$SDMAP)
                MAP <- c(MAP,recommendation_metrics$MAP)
        }
        
        nMAP <- nMAP[-1]
        SDnMAP <- SDnMAP[-1]
        SDMAP <- SDMAP[-1]
        MAP <- MAP[-1]
        return(list(nMAP = nMAP,SDnMAP = SDnMAP,MAP = MAP,SDMAP = SDMAP))
        
}



#Usage of above functions are as follows

setwd("D://Workspace/R/Recommendation System/RecommendationSystemJaccard_Modular/")


transaction.data <- read.csv("transactions.csv",stringsAsFactors = FALSE)

transaction.data <- transaction.data[,c("Customer.Id","Item")]


#Customer Recommendations

#Splitting data to Training and Testing sets
set.seed(37)
index <- sample(1:nrow(transaction.data),size = round(0.7*nrow(transaction.data)))

#Training Dataset
train <- transaction.data[index,]

#Testing Dataset
test <- transaction.data[-index,]

library(tictoc) #Time Code

tic()
Customer.Recommendations <- ubcf(transaction.data = train,N = 5,Top_N_Similarity = 5)
toc()

#Evaluating Recommmendation Outputs
Recommendation_metrics_test <- mean_average_precision(Recommended = Customer.Recommendations,Actual = test,N = 5)


#Performing Cross Validation
tic()
set.seed(43)
CustomerRecommendations5folds  <- cross_validation(transaction_data = train,N = 5,folds = 5 )
toc()

tic()
set.seed(68)
CustomerRecommendations10folds <- cross_validation(transaction_data = train,N = 5,folds = 10)
toc()

tic()
set.seed(39)
CustomerRecommendations15folds <- cross_validation(transaction_data = train,N = 5,folds = 15)
toc()

tic()
set.seed(54)
CustomerRecommendations20folds <- cross_validation(transaction_data = train,N = 5,folds = 20)
toc()


