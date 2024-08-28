pdf("interaction_effects.pdf")

calc_ice <- function(set, hp){
  x = as.data.frame(get(paste(set, "all.x.cat", sep=".")))
  y = get(paste(set, "all.y", sep="."))
  rm = get(paste(set, "rm", sep="."))  
  
  ice.plot<- ice(object = rm, X = x, y = y, predictor = hp, predictfcn = predict_fcn_ice, verbose = FALSE)
  assign(paste(set, hp, "ice", sep="."), ice.plot, envir = .GlobalEnv)
  
}

# Learning rate and clipping threshold

calc_ice("image.simple", "lr")
calc_ice("image.simple", "C")
#plot(image.simple.lr.ice, centered = TRUE, frac_to_plot = 0.05)
#plot(image.simple.C.ice, centered = TRUE, frac_to_plot = 0.05)


plot(image.simple.lr.ice, centered = TRUE, frac_to_plot = 0.05, color_by = "C")
plot(image.simple.C.ice, centered = TRUE, frac_to_plot = 0.05, color_by = "lr")

image.simple.lr.dice <- dice(image.simple.lr.ice)
image.simple.C.dice <- dice(image.simple.C.ice)

image.simple.lr.dice$d_ice_curves[image.simple.lr.dice$d_ice_curves > 1] = 0
image.simple.lr.dice$d_ice_curves[image.simple.lr.dice$d_ice_curves < (-1.4)] = 0

image.simple.C.dice$d_ice_curves[image.simple.C.dice$d_ice_curves > 1] = 0
image.simple.C.dice$d_ice_curves[image.simple.C.dice$d_ice_curves < (-2)] = 0

image.simple.C.dice$sd_deriv[image.simple.C.dice$sd_deriv > 1] = 0

#plot(image.simple.lr.dice, frac_to_plot = 0.05)
#plot(image.simple.C.dice, frac_to_plot = 0.05)

plot(image.simple.lr.dice, frac_to_plot = 0.05, color_by = "C")
plot(image.simple.C.dice, frac_to_plot = 0.05, color_by = "lr")

ALEPlot(as.data.frame(image.simple.all.x.cat), image.simple.rm, pred.fun=predict_fcn_ale, J=c(3,4), K=50, NA.plot = TRUE)
ALEPlot(as.data.frame(image.advanced.all.x.cat), image.advanced.rm, pred.fun=predict_fcn_ale, J=c(3,4), K=50, NA.plot = TRUE)
ALEPlot(as.data.frame(text.simple.all.x.cat), text.simple.rm, pred.fun=predict_fcn_ale, J=c(3,4), K=50, NA.plot = TRUE)


# Epochs
calc_ice("image.simple", "epochs")
plot(image.simple.epochs.ice, centered = TRUE, frac_to_plot = 0.05, color_by = "lr")
plot(image.simple.epochs.ice, centered = TRUE, frac_to_plot = 0.05, color_by = "C")

ALEPlot(as.data.frame(image.simple.all.x.cat), image.simple.rm, pred.fun=predict_fcn_ale, J=c(2,3), K=50, NA.plot = TRUE)
ALEPlot(as.data.frame(image.simple.all.x.cat), image.simple.rm, pred.fun=predict_fcn_ale, J=c(2,4), K=50, NA.plot = TRUE)

dev.off()


