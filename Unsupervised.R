
setwd("C:/Users/Igor/Documents/GitHub/Credit_Card_Fraud")
base <- readr::read_csv("creditcard.csv")




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

preproc <- caret::preProcess(base[,c(1,30)], method=c("pca"))
base <- predict(preproc, base)


## Excluindo variável resposta
fraud <- base[,29]
base <- base[,-29]


##Scale
base <- scale(base)


##best cluster quantity
factoextra::fviz_nbclust(base, kmeans, method="gap_stat")


##Generating kmeans classifier
base_kmeans <- kmeans(base, centers=4, nstart = 1000)


##Graph to visualization
factoextra::fviz_cluster(base_kmeans, data= base)


##List with clusters
list <- as.data.frame(base_kmeans$cluster)
list$fraud <- fraud
table(list)

## confusion matrix
table(fraud ,base_kmeans$cluster)
# clusters couldn't separate frauds and no frauds
