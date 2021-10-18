
base <- readr::read_csv("creditcard.csv")


##Time variable should not interfere in the model, so it will be deleted
base <- base[,-1]


## graph and modifying of Amount variable to be similar to others
g1 <- base |>
  ggplot2::ggplot(ggplot2::aes(x=factor(Class, labels = c("No","Yes")),
                      y=Amount))+
    ggplot2::geom_violin(fill = "Light Blue") +
    ggplot2::geom_boxplot(fill = "orange",
                 width = .3) +
    ggplot2::labs(x = "Fraud occurence",
         y = "Amount spent") +
    ggplot2::theme_classic()

plotly::ggplotly(g1)

preproc <- caret::preProcess(base[,29], method=c("center", "scale","YeoJohnson"))
base <- predict(preproc, base)



## splitting sample
train <- caret::createDataPartition(base$Class, p=0.75, list = F)
training <- base[train,]
testing <- base[-train,]


##Scale
training <- scale(training)
testing <- scale(testing)

##best cluster quantity
factoextra::fviz_nbclust(training, kmeans, method="gap_stat")


##Generating kmeans classifier
base_kmeans <- kmeans(training, 4)


##Graph to visualization
factoextra::fviz_cluster(base_kmeans, data= training)


##List with clusters
list <- as.data.frame(base_kmeans$cluster)



##Verifying accuracy
factoextra::fviz_cluster(base_kmeans, data= testing)
