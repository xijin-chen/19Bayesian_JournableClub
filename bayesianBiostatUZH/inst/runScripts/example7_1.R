#---------------------------------------------------------------------------------------------------------------
# Code for example 7.1 in Spiegelhalter 2006
# Date: 2019/05/15
#---------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------
######### LIBRARIES

library(ggplot2)
library(dplyr)
library(cowplot)
library(bayesianBiostatUZH)

#---------------------------------------------------------------------------------------------------------------
######### FUNCTIONS

# ggplot function for subplots in example 7.1 (just a plotting function)
figure7.1 <- function(dd,
                      title,
                      tag,
                      legend = F,
                      xlab = T) {
  plot <- ggplot(dd, aes(
    x = exp(x),
    y = value,
    linetype = label
  )) +
    geom_line(size = 0.9) +
    geom_segment(aes(
      x = 1,
      y = 0,
      xend = 1,
      yend = 2.65
    ), show.legend = F) +
    geom_ribbon(
      data = filter(dd, label == "Posterior" & x <= 0),
      aes(ymax = value),
      ymin = 0,
      show.legend = F,
      fill = "#7fcdbb",
      colour = NA,
      alpha = 0.5
    ) +
    scale_x_log10(
      breaks = c(0.3, 0.8, 1, 1.3, 1.8, 2.8, 3.8),
      limits = c(0.3, 3.8),
      expand = c(0.01, 0.01)
    ) +
    labs(
      title = title,
      tag = tag,
      x = expression("favours 3rd gen."  %<-% "OR" %->% "favours 2nd gen.")
    ) +
    scale_linetype_manual(values = c(1, 2, 3), name = '') +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 10),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 11, margin = margin(t = 20)),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line.x = element_line(
        size = 0.5,
        linetype = "solid",
        colour = "black"
      ),
      legend.position = "none"
    )
  if (legend == T) {
    plot <- plot + theme(
      legend.key = element_rect(fill = "white"),
      legend.position = c(-0.1, 0.8),
      legend.text = element_text(size = 10),
      legend.key.size = unit(1.4, "line")
    )
  }
  if (xlab == F) {
    plot <- plot + theme(axis.title.x = element_blank())
  }
  return(plot)
}

#---------------------------------------------------------------------------------------------------------------
######### Parameters and distributions

x <- seq(-1.4, 1.4, 0.001) # x values
prior.moments.exp1 <-
  c(log(0.8), log(1.6 / 0.4) / (2 * qnorm(0.975))) # moments of expert 1 prior
prior.moments.exp2 <-
  c(0, log(2 / 0.5) / (2 * qnorm(0.975)))  # moments of expert 2 prior
prior1 <-
  dnorm(x, mean = prior.moments.exp1[1], sd = prior.moments.exp1[2]) # expert 1 prior distr
prior2 <-
  dnorm(x, mean = prior.moments.exp2[1], sd = prior.moments.exp2[2]) # expert 2 prior distr
likeli.moments <-
  c(log(2), log(2.7 / 1.4) / (2 * qnorm(0.975))) # likelihood moments, no bias modelled
bias.moments <-
  c(log(1.3), log(1.67 / 0.6) / (2 * qnorm(0.975))) # moments of bias

#---------------------------------------------------------------------------------------------------------------
######### NO BIAS

likelihood <-
  dnorm(x, mean = likeli.moments[1], sd = likeli.moments[2]) # likelihood

# first subplot of row 1: no bias, expert 1
post.moments1 <-
  posterior_normal(
    prior.mean = prior.moments.exp1[1],
    # posterior moments
    prior.sd = prior.moments.exp1[2],
    estimate.mean = likeli.moments[1],
    estimate.se = likeli.moments[2]
  )
posterior1 <-
  dnorm(x, mean = post.moments1[1], sd = post.moments1[2]) # posterior distr

dd.a1 <-
  data.frame(
    x = rep(x, 3),
    # dataframe for plot: row 1 column 1 (expert 1 - no bias)
    label = rep(c("Likelihood", "Prior", "Posterior"), each = length(x)),
    value = c(likelihood, prior1, posterior1)
  )

a1 <-
  figure7.1(
    dd.a1,
    title = "Expert 1\nBias: none",
    tag = "A",
    legend = T,
    xlab = F
  ) # plot it

# ----------- second subplot of row 1: no bias, expert 2
post.moments2 <-
  posterior_normal(
    prior.mean = prior.moments.exp2[1],
    # posterior moments
    prior.sd = prior.moments.exp2[2],
    estimate.mean = likeli.moments[1],
    estimate.se = likeli.moments[2]
  )
posterior2 <-
  dnorm(x, mean = post.moments2[1], sd = post.moments2[2]) # posterior distr

dd.a2 <-
  data.frame(
    x = rep(x, 3),
    # dataframe for plot: row 1 column 2 (expert 2 - no bias)
    label = rep(c("Likelihood", "Prior", "Posterior"), each = length(x)),
    value = c(likelihood, prior2, posterior2)
  )
# plot: row 1 column 2 (expert 2 - no bias)
a2 <-
  figure7.1(dd.a2,
            title = "Expert 2\nBias: none",
            tag = "B",
            xlab = F)


#---------------------------------------------------------------------------------------------------------------
######### NON-SYSTEMATIC BIAS

likelihood <-
  dnorm(x,
        mean = likeli.moments[1],
        sd = sqrt(likeli.moments[2] ^ 2 + bias.moments[2] ^ 2))

# first subplot of row 2: non-systematic bias, expert 1
post.moments1 <-
  posterior_normal(
    prior.mean = prior.moments.exp1[1],
    prior.sd = prior.moments.exp1[2],
    estimate.mean = likeli.moments[1],
    estimate.se = likeli.moments[2],
    bias.sd = bias.moments[2]
  )
posterior1 <-
  dnorm(x, mean = post.moments1[1], sd = post.moments1[2])

dd.b1 <- data.frame(
  x = rep(x, 3),
  label = rep(c("Likelihood", "Prior", "Posterior"), each = length(x)),
  value = c(likelihood, prior1, posterior1)
)

b1 <-
  figure7.1(dd.b1,
            title = "Expert 1\nBias: 0% \u00B1 67%",
            tag = "C",
            xlab = F)

# second subplot of row 2: non-systematic bias, expert 2
post.moments2 <-
  posterior_normal(
    prior.mean = prior.moments.exp2[1],
    prior.sd = prior.moments.exp2[2],
    estimate.mean = likeli.moments[1],
    estimate.se = likeli.moments[2],
    bias.sd = bias.moments[2]
  )
posterior2 <-
  dnorm(x, mean = post.moments2[1], sd = post.moments2[2])

dd.b2 <- data.frame(
  x = rep(x, 3),
  label = rep(c("Likelihood", "Prior", "Posterior"), each = length(x)),
  value = c(likelihood, prior2, posterior2)
)

b2 <-
  figure7.1(dd.b2,
            title = "Expert 2\nBias: 0% \u00B1 67%",
            tag = "D",
            xlab = F)


#---------------------------------------------------------------------------------------------------------------
######### SYSTEMATIC BIAS

likelihood <-
  dnorm(
    x,
    mean = likeli.moments[1] - bias.moments[1],
    sd = sqrt(likeli.moments[2] ^ 2 + bias.moments[2] ^ 2)
  )

# first subplot of row 3: systematic bias, expert 1
post.moments1 <-
  posterior_normal(
    prior.mean = prior.moments.exp1[1],
    prior.sd = prior.moments.exp1[2],
    estimate.mean = likeli.moments[1],
    estimate.se = likeli.moments[2],
    bias.mean = bias.moments[1],
    bias.sd = bias.moments[2]
  )
posterior1 <-
  dnorm(x, mean = post.moments1[1], sd = post.moments1[2])

dd.c1 <- data.frame(
  x = rep(x, 3),
  label = rep(c("Likelihood", "Prior", "Posterior"), each = length(x)),
  value = c(likelihood, prior1, posterior1)
)

c1 <-
  figure7.1(dd.c1, title = "Expert 1\nBias: 30% \u00B1 67%", tag = "E")

# second subplot of row 3: systematic bias, expert 2
post.moments2 <-
  posterior_normal(
    prior.mean = prior.moments.exp2[1],
    prior.sd = prior.moments.exp2[2],
    estimate.mean = likeli.moments[1],
    estimate.se = likeli.moments[2],
    bias.mean = bias.moments[1],
    bias.sd = bias.moments[2]
  )
posterior2 <-
  dnorm(x, mean = post.moments2[1], sd = post.moments2[2])

dd.c2 <- data.frame(
  x = rep(x, 3),
  label = rep(c("Likelihood", "Prior", "Posterior"), each = length(x)),
  value = c(likelihood, prior2, posterior2)
)

c2 <-
  figure7.1(dd.c2, title = "Expert 2\nBias: 30% \u00B1 67%", tag = "F")

#---------------------------------------------------------------------------------------------------------------
######### PLOT

plot_grid(a1, a2, b1, b2, c1, c2, align = "h", ncol = 2)
