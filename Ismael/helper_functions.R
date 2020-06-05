## function to test all simple and interaction effects
## in a logistic regression
## function to get all posible combinations
combinations <- function(x) {
        expand.grid(rep(list(TRUE:FALSE), x))
}

get_all_formulas <- function(independents, dependent) {
        independent_n <- length(independents)        
        combination_matrix <- combinations(independent_n)
        ## write out variable with combination matrix
        to_paste <- 1:(dim(combination_matrix)[1] - 1)%>% 
                map(function(x) independents[as.logical(unlist(combination_matrix[x, ]))])
        func <- 1:length(to_paste) %>% 
                map(function(x) paste(dependent, "~", paste(unlist(to_paste[x]), collapse = " + ")))
        return(unlist(unlist(func)))
}

# Training function
train_model <- function(data, formula) {
  train_control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 10,
                                savePredictions = TRUE)
  model <- train(formula, data = data, trControl = train_control, method = "glm", family = "binomial")
  return(model)
}

