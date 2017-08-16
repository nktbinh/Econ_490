get_significance = function(model) {
  delta_deviance = model$null.deviance - model$deviance
  df = model$df.null - model$df.residual
  sig = pchisq(delta_deviance, df, lower.tail=FALSE)
}

get_chiscores = function(dframe, varnames) {
  nvar = length(varnames)
  scores = numeric(nvar)
  for(i in seq_len(nvar)) {
    model = glm(paste("alcbin~",varnames[i]), dframe,
                family=binomial(link="logit"))
    scores[i] = get_significance(model)
  }
  
  sframe = data.frame(var=varnames,
                      scores=scores, stringsAsFactors=FALSE)
  sframe
}

varnames = setdiff(colnames(df_train), "alcbin")
sframe = get_chiscores(df_train, varnames)

scoreplot = function(frm, threshold, sort=1) {
  n = dim(frm)[1]
  frm$var = reorder(frm$var, frm$scores*sort, FUN=sum)
  frm$goodvar = frm$scores < threshold
  
  ggplot(frm, aes(x=var, y=scores, ymin=0, ymax=scores, color=goodvar)) +
    geom_pointrange() +
    geom_hline(yintercept=threshold, color="red", linetype=2) +
    scale_color_manual(values=c("TRUE"="darkgreen", "FALSE"="darkgray")) +
    theme(legend.position="none")
}

threshold = 0.05
scoreplot(sframe, threshold)