library(readr)
library(plyr)
library(dplyr)
library(ranger)
library(ICEbox)
library(ALEPlot)

image.simple <- read_csv("data/image_simple.csv")
image.advanced <- read_csv("data/image_advanced.csv")
text.simple <- read_csv("data/text_simple.csv")


predict_fcn_ice = function(object, newdata) predict(object, newdata)$predictions
predict_fcn_ale <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)


split_data <- function(data){
  
  name <- deparse(substitute(data))
  
  vars = c("bs", "epochs", "lr", "C")
  #cats = c("bs", "epochs", "lr", "C", "dataset", "model_id", "eps_targeted")
  cats <<- c("bs", "epochs", "lr", "C", "sampling_rate", "dataset_size", "dataset", "model_id", "eps_targeted")
  
  set.seed(596291871)
  sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8,0.2))
  
  assign(paste(name, "all.x", sep="."), data %>% ungroup() %>% select(all_of(vars)), envir = .GlobalEnv)
  assign(paste(name, "all.x.cat", sep="."), data %>% ungroup() %>% select(all_of(cats)), envir = .GlobalEnv)
  
  #data.all.y <<- data$test_accuracy
  assign(paste(name, "all.y", sep="."), 
         (data %>% 
            group_by(dataset, model_id, eps_targeted) %>% 
            mutate(mmta = (test_accuracy - min(test_accuracy))/(max(test_accuracy) - min(test_accuracy))) %>% ungroup)$mmta,
         envir = .GlobalEnv)
  
  assign(paste(name, "train", sep="."),
         data[sample, ],
         envir = .GlobalEnv)
  
  assign(paste(name, "test", sep="."),
         data[!sample, ],
         envir = .GlobalEnv)
  
  
  assign(paste(name, "train.x", sep="."),
         data[sample, ] %>% ungroup() %>% select(all_of(vars)),
         envir = .GlobalEnv)
  
  assign(paste(name, "test.x", sep="."),
         data[!sample, ] %>% ungroup() %>% select(all_of(vars)),
         envir = .GlobalEnv)
  
  
  assign(paste(name, "train.x.cat", sep="."),
         data[sample, ] %>% ungroup() %>% select(all_of(cats)),
         envir = .GlobalEnv)
  
  assign(paste(name, "test.x.cat", sep="."),
         data[!sample, ] %>% ungroup() %>% select(all_of(cats)),
         envir = .GlobalEnv)   
  
  
  assign(paste(name, "train.y", sep="."),
         data[sample, ]$test_accuracy,
         envir = .GlobalEnv)
  
  assign(paste(name, "test.y", sep="."),
         data[!sample, ]$test_accuracy,
         envir = .GlobalEnv)
  
}


split_data(image.simple)


## Extremely randomized trees (ranger) v2
rf <- ranger(x = image.simple.train.x.cat,
             y = image.simple.train.y,
             splitrule="extratrees", 
             min.node.size = 1,
             num.random.splits=4,
             mtry = 4,
             num.trees = 512,
             replace = FALSE, sample.fraction = 1)

y_pred <- predict(rf, image.simple.test.x.cat)$predictions

image.simple.rm.test.mae <- round(mean(abs(y_pred - image.simple.test.y)), digits=3)
image.simple.rm.test.mse <- round(mean((y_pred - image.simple.test.y)**2), digits=3)

image.simple.rm <- ranger(x = image.simple.all.x.cat,
                          y = image.simple.all.y,
                          splitrule="extratrees", 
                          min.node.size = 1,
                          num.random.splits=4,
                          mtry = 4,
                          num.trees = 512,
                          replace = FALSE, sample.fraction = 1)



split_data(image.advanced)

## Extremely randomized trees (ranger) v2
rf <- ranger(x = image.advanced.train.x.cat,
             y = image.advanced.train.y,
             splitrule="extratrees", 
             min.node.size = 1,
             num.random.splits=4,
             mtry = 4,
             num.trees = 512,
             replace = FALSE, sample.fraction = 1)

y_pred <- predict(rf, image.advanced.test.x.cat)$predictions

image.advanced.rm.test.mae <- round(mean(abs(y_pred - image.advanced.test.y)), digits=3)
image.advanced.rm.test.mse <- round(mean((y_pred - image.advanced.test.y)**2), digits=3)

image.advanced.rm <- ranger(x = image.advanced.all.x.cat,
                            y = image.advanced.all.y,
                            splitrule="extratrees", 
                            min.node.size = 1,
                            num.random.splits=4,
                            mtry = 4,
                            num.trees = 512,
                            replace = FALSE, sample.fraction = 1)


split_data(text.simple)

## Extremely randomized trees (ranger) v2
rf <- ranger(x = text.simple.train.x.cat,
             y = text.simple.train.y,
             splitrule="extratrees", 
             min.node.size = 1,
             num.random.splits=4,
             mtry = 4,
             num.trees = 512,
             replace = FALSE, sample.fraction = 1)

y_pred <- predict(rf, text.simple.test.x.cat)$predictions

text.simple.rm.test.mae <- round(mean(abs(y_pred - text.simple.test.y)), digits=3)
text.simple.rm.test.mse <- round(mean((y_pred - text.simple.test.y)**2), digits=3)

text.simple.rm <- ranger(x = text.simple.all.x.cat,
                         y = text.simple.all.y,
                         splitrule="extratrees", 
                         min.node.size = 1,
                         num.random.splits=4,
                         mtry = 4,
                         num.trees = 512,
                         replace = FALSE, sample.fraction = 1)

rm(rf)


write_subset_to_disk_for_anova <- function(data_name, N=500)
{
  
  path = "data"
  
  x = as.data.frame(get(paste(data_name, "all.x.cat", sep=".")))
  y = get(paste(data_name, "all.y", sep="."))
  
  out <- cbind(x, data.frame(y=y))
  
  if(nrow(out)>N){
    out <- out %>% sample_n(N)
  }
  
  #out_x <- out %>% ungroup %>% select(c(bs, epochs, lr, C))
  out_x <- out %>% ungroup %>% select(-c(y))
  
  if(length(unique(out_x$eps_targeted))==1){
    out_x <- out_x %>% select(-c(eps_targeted))
  }
  
  if(length(unique(out_x$model_id))==1){
    out_x <- out_x %>% select(-c(model_id))
  }
  
  out_y <- out %>% ungroup %>% select(y)
  
  write.table(out_x, paste(path, paste("fanova", data_name, "x.csv", sep="_"), sep="/"), row.names=FALSE, sep=",")
  write.table(out_y, paste(path, paste("fanova", data_name, "y.csv", sep="_"), sep="/"), row.names=FALSE, sep=",")
}


write_subset_to_disk_for_anova("image.simple", N=250)
write_subset_to_disk_for_anova("image.advanced", N=250)
write_subset_to_disk_for_anova("text.simple", N=250)













