markovRecommender <- function(transaction_data,N = 5){
        
        transaction_data <- transaction_data[!duplicated(transaction_data),]
        
        library(dplyr)
        
        transaction_data <- transaction_data %>% arrange(Customer.Id,StartDate)
        
        customertransactions <- transaction_data %>% group_by(Customer.Id) %>% summarise(Nooftrans = length(ProductID))
        
        customertransactions <- cumsum(customertransactions$Nooftrans+1)
        
        Customer_Recent_Purchase <- transaction_data %>% group_by(Customer.Id) %>% summarise(StartDate = max(StartDate))
        
        Customer_Recent_Purchase <- left_join(Customer_Recent_Purchase,transaction_data,by = c("Customer.Id","StartDate"))
        
        transactions <- transaction_data$ProductID
        
        rm(transaction_data)
        
        
        insert <- function(v,e,pos){
                return(c(v[1:(pos-1)],e,v[(pos):length(v)]))
        }
        
        for(i in 1:length(customertransactions)){
                transactions <- insert(transactions,"VOID",customertransactions[i])
        }
        
        rm(insert,customertransactions)
        
        #Inference matrix
        inference_matrix <- table(c("VOID",transactions),c(transactions,"VOID"))
        
        gc()
        
        inference_matrix <- unclass(inference_matrix)
        
        rm(transactions)
        
        inference_matrix <- inference_matrix[-which(rownames(inference_matrix)=="VOID"),-which(colnames(inference_matrix)=="VOID")]
        
        #Transition matrix
        Transition_matrix <- inference_matrix/rowSums(inference_matrix)
        
        Transition_matrix[is.nan(Transition_matrix)] <- 0
        
        rm(inference_matrix)
        
        
        #Identifying Recent Visited City
        Customer_Recent_Purchase <- Customer_Recent_Purchase[,c("Customer.Id","ProductID")]
        
        Customer_Recent_Purchase <- Customer_Recent_Purchase[!duplicated(Customer_Recent_Purchase["Customer.Id"]),]
        
        Customers <- unique(Customer_Recent_Purchase$Customer.Id)
        
        Customer.Recommendations <- vector("list",length(Customers))
        
        #Recommendations
        No.of.Recommendations <- N
        
        for(i in seq_along(Customers)){
                Recent.City <- Customer_Recent_Purchase[Customer_Recent_Purchase$Customer.Id == Customers[i],]$ProductID
                Customer.Recommendations[[i]] <- names(sort(Transition_matrix[Recent.City,],decreasing = TRUE)[1:No.of.Recommendations])
        }
        
        rm(Transition_matrix,Customer_Recent_Purchase,i,Recent.City)
        
        Customer.Recommendations <- do.call("rbind",Customer.Recommendations)
        
        Customer.Recommendations <- cbind(Customer.Id = Customers,Customer.Recommendations)
        rm(Customers)
        
        colnames(Customer.Recommendations)[-1] <- paste("Recommended.City",1:No.of.Recommendations,sep = "_")
        rm(No.of.Recommendations)
        
        return(Customer.Recommendations)
}

mean_average_precision <- function(Recommended,Actual,N){
        Customers <- unique(Recommended[,"Customer.Id"])
        AvgPrecision <- 0
        
        for(i in 1:length(Customers)){
                if(Customers[i] %in% Actual$Customer.Id){
                        
                        Purchased <- unique(Actual[Actual$Customer.Id == Customers[i],"ProductID"])
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
                        
                        Purchased <- unique(Actual[Actual$Customer.Id == Customers[i],"ProductID"])
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
        
        library(caret)
        set.seed(5)
        data.folds <- createFolds(transaction_data$ProductID,k = folds,list = TRUE,returnTrain = FALSE)
        
        nMAP <- 0
        SDnMAP <- 0
        MAP <- 0
        SDMAP <- 0
        
        for(i in seq_along(data.folds)){
                
                index <- unname(unlist(data.folds[-i]))
                Customer.Recommendations <- markovRecommender(transaction_data[index,],N = N)
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


transaction_data <- read.csv("data.csv",stringsAsFactors = FALSE,header = TRUE)

transaction_data <- transaction_data[,c("Customer.Id","StartDate","ProductID")]

transaction_data$StartDate <- as.Date(transaction_data$StartDate,format = "%m/%d/%y")

set.seed(3)
index <- sample(1:nrow(transaction_data),size = round(0.7*nrow(transaction_data)))

train <- transaction_data[index,]

test <- transaction_data[-index,]

rm(transaction_data)

set.seed(423)
CustomerRecommendations5folds <- cross_validation(transaction_data = train,N = 5,folds = 5)
set.seed(68)
CustomerRecommendations10folds <- cross_validation(transaction_data = train,N = 5,folds = 10)


Recommendation_metrics_test <- mean_average_precision(Recommended = CustomerRecommendations,Actual = test,N = 5)

