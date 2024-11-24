library(arules)

rule_map <- function(rules, 
                     metric = "confidence"){
  
  verify_rules(rules)
  verify_metric(metric)
  
  antecedents <- labels(lhs(rules))
  consequents <- labels(rhs(rules))

  metric <- tolower(metric)
  
  metrics <- quality(rules)[[metric]]
  rule_df <- data.frame(
    antecedents = antecedents,
    consequents = consequents,
    metric = metrics)
  
  heatmap_matrix <- xtabs(metric ~ antecedents + consequents, data = rule_df)
  
  par(mar = c(10, 10, 10, 10))
  heatmap(
    heatmap_matrix,
    Rowv = NA,       
    Colv = NA,        
    scale = "none",   
    xlab = "Consequents",
    ylab = "Antecedents",
    cexCol = 0.5,
    cexRow = 0.5,
    las = 2)
  
}

verify_rules <- function(rules){
  
  if(!inherits(rules, "rules"))
    stop("Input must be of class 'rules'. Please provide a valid rule set.")
  
}

verify_metric <- function(metric){
  
  valid_metrics <- c("confidence", "support", "lift")
  
  if (!is.character(metric) || !tolower(metric) %in% valid_metrics)
    stop("'metric' must be one of 'confidence,' 'support,' or 'lift'. Please provide a valid metric.")
  
}
