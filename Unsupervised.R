
base <- readr::read_csv("creditcard.csv")


##Time variable should not interfire in the model, so it will be deleted
base <- base[,-1]


## graph and modifying transaction variable to be similar to others
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
