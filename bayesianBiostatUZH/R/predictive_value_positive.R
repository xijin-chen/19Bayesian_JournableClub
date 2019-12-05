#' Predictive value positive
#' @param alpha Sets the false positive rate (alpha); 5\% is conventional false positive rate
#' @param beta Sets the true positive rate (power); 90\% is conventional power
#' @param trueEffectiveness Sets the guestimated true effectiveness rate; 50\% by default.
#' @param N Sets the trial number.
#' @author Mark James Thompson
#' @example #Example from Spiegelhalter
#' predictive_value_positive(N=200,beta=0.8,trueEffectiveness=0.1)
#'
#' @export
predictive_value_positive <-
  function(alpha = 0.05,
           beta = 0.9,
           N = 1000,
           trueEffectiveness = 0.5) {
    # Baseline parameters are typical values for a clinical trial.
    #Sets the truly insignificant trials
    trueInsignigicant <- N - N * trueEffectiveness

    true_negative <- (1 - alpha) * trueInsignigicant
    false_negative <- (N * trueEffectiveness) * (beta)
    false_positive <- alpha * trueInsignigicant
    true_positive <- (N * trueEffectiveness) * (1 - beta)
    tab <- rbind(cbind(true_negative, false_negative),
                 cbind(false_positive, true_positive))
    #Make human readable
    colnames(tab) <- c("effective=FALSE", "effecitve=TRUE")
    rownames(tab)    <-
      c("trialSignificant=TRUE", "trialSignificant=FALSE")

  # Hence the prior odds on the treatment being ineffective (H0) are:
  priorOdds = (1-trueEffectiveness)/trueEffectiveness
  # multiplied by the likelihood ratio
  likelihoodRatio <-  (alpha/(beta))
  # Posterior odds
  predictive_value_positive_odds  <-  priorOdds*likelihoodRatio
  # Posterior probability
  predictive_value_positive_probability <-  odds_to_probability(predictive_value_positive_odds)

  list(table = tab,
       PPV = predictive_value_positive_probability)
  }
