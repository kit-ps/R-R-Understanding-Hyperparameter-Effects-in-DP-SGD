plot_main_effects <- function(set, hp){
  
  name <- deparse(substitute(data))
  
  x = as.data.frame(get(paste(set, "all.x.cat", sep=".")))
  y = get(paste(set, "all.y", sep="."))
  rm = get(paste(set, "rm", sep="."))
  
  j = which(cats == hp)
  
  frac = 250 / nrow(x)
  if(frac>1) {
    frac=1
  }
  
  ice.plot<- ice(object = rm, X = x, y = y, predictor = hp, predictfcn = predict_fcn_ice, verbose = FALSE)
  plot(ice.plot, frac_to_plot = frac, x_quantile=FALSE, main=set)
  assign(paste(set, hp, "ice", sep="."), ice.plot, envir = .GlobalEnv)
  
  assign(paste(set, hp, "ale", sep="."), ALEPlot(x, rm, pred.fun=predict_fcn_ale, J=j, K=50, NA.plot = TRUE), envir = .GlobalEnv)
}

pdf("main_effects.pdf")

plot_main_effects("image.simple", "bs")
plot_main_effects("image.advanced", "bs")
plot_main_effects("text.simple", "bs")


plot_main_effects("image.simple", "sampling_rate")
plot_main_effects("image.advanced", "sampling_rate")
plot_main_effects("text.simple", "sampling_rate")


plot_main_effects("image.simple", "epochs")
plot_main_effects("image.advanced", "epochs")
plot_main_effects("text.simple", "epochs")


plot_main_effects("image.simple", "lr")
plot_main_effects("image.advanced", "lr")
plot_main_effects("text.simple", "lr")


plot_main_effects("image.simple", "C")
plot_main_effects("image.advanced", "C")
plot_main_effects("text.simple", "C")

dev.off()



