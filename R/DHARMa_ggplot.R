#' Function to make ggplot versions of DHARMa plots
#'
#' @param model A model which is compatible with the DHARMa package
#'
#' @returns A ggplot object containing DHARMa equivalent plots
#' @export
#'
#' @examples
#' # plots <- DHARMa_ggplot(g3) # specify model

DHARMa_ggplot <- function(model){

  # Get randomized quantile residuals
  simulationOutput <- NULL #initialise object
  simulationOutput <- DHARMa::simulateResiduals(model)
  residuals <- simulationOutput$scaledResiduals

  # Generate theoretical quantiles of uniform
  theoretical <- stats::qunif(p = (1:length(residuals) - 0.5) / length(residuals))
  # p is a vector of probabilities to calculate the quantiles

  # Now plot the residual qq-plot
  qq <- ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = theoretical, y = sort(residuals))) +
    ggplot2::geom_abline(slope = 1, linetype = "dashed", colour = "red") +
    ggplot2::labs(x = "Theoretical quantiles",
         y = "Randomized quantile residuals")+
    ggplot2::theme_bw() + ggplot2::theme(axis.text = ggplot2::element_text(colour="black", size=10))

  #qq

  # Extract the linear predictors from the models (c.)
  # Heteroscedacticity
  # Extract the necessary onjects to make ggplots
  quantiles <- DHARMa::testQuantiles(simulationOutput, plot=T)
  pred <- quantiles$predictions

  # Get the rank transformed model predictors
  predictor <- simulationOutput$fittedPredictedResponse # extract from DHARMa
  predictor <- rank(predictor, ties.method = "average") # rank-transform
  predictor <- predictor / max(predictor) # scale on 0-1

  # Vector of quantiles for plotting
  quants <- c(0.25, 0.5, 0.75)

  # now plot them
  full_var <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = quants, linetype = "dashed", colour = "black") +
    ggplot2::geom_point(ggplot2::aes(x = predictor, y = residuals), alpha = 0.05) +
    ggplot2::geom_line(ggplot2::aes(x = pred$pred, y = pred$`1`), colour = "black") +
    ggplot2::geom_line(ggplot2::aes(x = pred$pred, y = pred$`3`), colour = "black") +
    ggplot2::geom_line(ggplot2::aes(x = pred$pred, y = pred$`5`), colour = "black") +
    ggplot2::labs(x = "Model predictions (rank transformed)",
         y = "Randomized quantile residuals") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text = ggplot2::element_text(colour="black", size=10))

  result_list <- list(qq,full_var)
  plot_a <- result_list[[1]]
  plot_a

  plot_b <- result_list[[2]]
  plot_b

  combo_plot <- patchwork::wrap_plots(plot_a, plot_b, ncol = 2)

  return(combo_plot)
}
