library(ggplot2)

pdf("main_effects_across_all_factors.pdf")

# Simple Image Classification {.tabset}
dx <- as.data.frame(image.simple.all.x.cat)
rm <- image.simple.rm

## By dataset
plot_dataset <- function(i){
  
  ff <- tempfile()
  png(filename=ff)
  ale.svhn <- ALEPlot(dx %>% filter(dataset == "SVHN"), rm, pred.fun=predict_fcn_ale, J=i, K=50, NA.plot = TRUE)
  ale.cifar <- ALEPlot(dx %>% filter(dataset == "CIFAR-10"), rm, pred.fun=predict_fcn_ale, J=i, K=50, NA.plot = TRUE)
  dev.off()
  unlink(ff)
  
  df <- rbind(data.frame(x=ale.cifar$x.values, f_x=ale.cifar$f.values, Dataset="CIFAR-10"), 
              data.frame(x=ale.svhn$x.values, f_x=ale.svhn$f.values, Dataset="SVHN"))
  
  return(ggplot(df, aes(x=x, y=f_x, color=Dataset)) + geom_line() + ylab("f(x)")) 
}

print(plot_dataset(1) + xlab("BS"))
print(plot_dataset(5) + xlab("q"))
print(plot_dataset(2) + xlab("Epochs"))
print(plot_dataset(3) + xlab("Learning rate"))
print(plot_dataset(4) + xlab("Clipping Threshold"))


## By model
plot_model <- function(i){
  
  ff <- tempfile()
  png(filename=ff)
  ale.cnn <- ALEPlot(dx %>% filter(model_id == "DP-CNN"), rm, pred.fun=predict_fcn_ale, J=i, K=50, NA.plot = TRUE)
  ale.r18 <- ALEPlot(dx %>% filter(model_id == "R18"), rm, pred.fun=predict_fcn_ale, J=i, K=50, NA.plot = TRUE)
  ale.r34 <- ALEPlot(dx %>% filter(model_id == "R34"), rm, pred.fun=predict_fcn_ale, J=i, K=50, NA.plot = TRUE)
  dev.off()
  unlink(ff)
  
  df <- rbind(data.frame(x=ale.cnn$x.values, f_x=ale.cnn$f.values, Model="DP-CNN"),
              data.frame(x=ale.r18$x.values, f_x=ale.r18$f.values, Model="R18"),
              data.frame(x=ale.r34$x.values, f_x=ale.r34$f.values, Model="R34"))
  return(ggplot(df, aes(x=x, y=f_x, color=Model)) + geom_line() + ylab("f(x)")) 
}

print(plot_model(1) + xlab("BS"))
print(plot_model(5) + xlab("q"))
print(plot_model(2) + xlab("Epochs"))
print(plot_model(3) + xlab("Learning rate"))
print(plot_model(4) + xlab("Clipping Threshold"))

## By privacy level
plot_eps <- function(i){
  
  ff <- tempfile()
  png(filename=ff)
  ale.e3 <- ALEPlot(dx %>% filter(eps_targeted == 3), rm, pred.fun=predict_fcn_ale, J=i, K=50, NA.plot = TRUE)
  ale.e5 <- ALEPlot(dx %>% filter(eps_targeted == 5), rm, pred.fun=predict_fcn_ale, J=i, K=50, NA.plot = TRUE)
  ale.e7 <- ALEPlot(dx %>% filter(eps_targeted == 7.5), rm, pred.fun=predict_fcn_ale, J=i, K=50, NA.plot = TRUE)
  dev.off()
  unlink(ff)
  
  df <- rbind(data.frame(x=ale.e3$x.values, f_x=ale.e3$f.values, Epsilon="3"),
              data.frame(x=ale.e5$x.values, f_x=ale.e5$f.values, Epsilon="5"),
              data.frame(x=ale.e7$x.values, f_x=ale.e7$f.values, Epsilon="7.5"))
  
  return(ggplot(df, aes(x=x, y=f_x, color=Epsilon)) + geom_line() + ylab("f(x)")) 
}

print(plot_eps(1) + xlab("BS"))
print(plot_eps(5) + xlab("q"))
print(plot_eps(2) + xlab("Epochs"))
print(plot_eps(3) + xlab("Learning rate"))
print(plot_eps(4) + xlab("Clipping Threshold"))


# Intermediate Image Classification

dx <- as.data.frame(image.advanced.all.x.cat)
rm <- image.advanced.rm

## By dataset
plot_dataset <- function(i){
  
  ff <- tempfile()
  png(filename=ff)
  ale.in <- ALEPlot(dx %>% filter(dataset == "ImageNette"), rm, pred.fun=predict_fcn_ale, J=i, K=50, NA.plot = TRUE)
  ale.cifar <- ALEPlot(dx %>% filter(dataset == "CIFAR-100"), rm, pred.fun=predict_fcn_ale, J=i, K=50, NA.plot = TRUE)
  dev.off()
  unlink(ff)
  
  df <- rbind(data.frame(x=ale.cifar$x.values, f_x=ale.cifar$f.values, Dataset="CIFAR-100"), 
              data.frame(x=ale.in$x.values, f_x=ale.in$f.values, Dataset="ImageNette"))
  
  return(ggplot(df, aes(x=x, y=f_x, color=Dataset)) + geom_line() + ylab("f(x)")) 
}

print(plot_dataset(1) + xlab("BS"))
print(plot_dataset(5) + xlab("q"))
print(plot_dataset(2) + xlab("Epochs"))
print(plot_dataset(3) + xlab("Learning rate"))
print(plot_dataset(4) + xlab("Clipping Threshold"))


## By model
plot_model <- function(i){
  
  ff <- tempfile()
  png(filename=ff)
  ale.cnn <- ALEPlot(dx %>% filter(model_id == "DP-CNN"), rm, pred.fun=predict_fcn_ale, J=i, K=50, NA.plot = TRUE)
  ale.r18 <- ALEPlot(dx %>% filter(model_id == "R18"), rm, pred.fun=predict_fcn_ale, J=i, K=50, NA.plot = TRUE)
  ale.d121 <- ALEPlot(dx %>% filter(model_id == "D121"), rm, pred.fun=predict_fcn_ale, J=i, K=50, NA.plot = TRUE)
  dev.off()
  unlink(ff)
  
  df <- rbind(data.frame(x=ale.cnn$x.values, f_x=ale.cnn$f.values, Model="DP-CNN"),
              data.frame(x=ale.r18$x.values, f_x=ale.r18$f.values, Model="R18"),
              data.frame(x=ale.d121$x.values, f_x=ale.d121$f.values, Model="D121"))
  
  return(ggplot(df, aes(x=x, y=f_x, color=Model)) + geom_line() + ylab("f(x)")) 
}

print(plot_model(1) + xlab("BS"))
print(plot_model(5) + xlab("q"))
print(plot_model(2) + xlab("Epochs"))
print(plot_model(3) + xlab("Learning rate"))
print(plot_model(4) + xlab("Clipping Threshold"))

# Simple Text

dx <- as.data.frame(text.simple.all.x.cat)
rm <- text.simple.rm

## By dataset
plot_dataset <- function(i){
  
  ff <- tempfile()
  png(filename=ff)
  ale.news <- ALEPlot(dx %>% filter(dataset == "NEWS"), rm, pred.fun=predict_fcn_ale, J=i, K=50, NA.plot = TRUE)
  ale.imdb <- ALEPlot(dx %>% filter(dataset == "IMDB"), rm, pred.fun=predict_fcn_ale, J=i, K=50, NA.plot = TRUE)
  dev.off()
  unlink(ff)
  
  df <- rbind(data.frame(x=ale.imdb$x.values, f_x=ale.imdb$f.values, Dataset="NEWS"), 
              data.frame(x=ale.news$x.values, f_x=ale.news$f.values, Dataset="IMDB"))
  
  return(ggplot(df, aes(x=x, y=f_x, color=Dataset)) + geom_line() + ylab("f(x)")) 
}

print(plot_dataset(1) + xlab("BS"))
print(plot_dataset(5) + xlab("q"))
print(plot_dataset(2) + xlab("Epochs"))
print(plot_dataset(3) + xlab("Learning rate"))
print(plot_dataset(4) + xlab("Clipping Threshold"))



## By model
plot_model <- function(i){
  
  ff <- tempfile()
  png(filename=ff)
  ale.rnn <- ALEPlot(dx %>% filter(model_id == "SimpleRNN"), rm, pred.fun=predict_fcn_ale, J=i, K=50, NA.plot = TRUE)
  ale.lstm <- ALEPlot(dx %>% filter(model_id == "SimpleLSTM"), rm, pred.fun=predict_fcn_ale, J=i, K=50, NA.plot = TRUE)
  dev.off()
  unlink(ff)
  
  df <- rbind(data.frame(x=ale.rnn$x.values, f_x=ale.rnn$f.values, Model="RNN"),
              data.frame(x=ale.lstm$x.values, f_x=ale.lstm$f.values, Model="LSTM"))
  
  return(ggplot(df, aes(x=x, y=f_x, color=Model)) + geom_line() + ylab("f(x)")) 
}

print(plot_model(1) + xlab("BS"))
print(plot_model(5) + xlab("q"))
print(plot_model(2) + xlab("Epochs"))
print(plot_model(3) + xlab("Learning rate"))
print(plot_model(4) + xlab("Clipping Threshold"))


dev.off()

