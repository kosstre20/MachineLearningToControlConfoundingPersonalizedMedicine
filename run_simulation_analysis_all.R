

# ---- FUNCTION: Compute Monte Carlo Error ----
compute_monte_carlo_error = function(results_list, nrep = 1000) {
  SDs = do.call(rbind, lapply(results_list, function(res) apply(res, 2, sd, na.rm = TRUE)))
  rownames(SDs) = names(results_list)
  mc_errors = cbind(
    apply(SDs / sqrt(nrep), 1, max),
    apply(SDs / sqrt(2 * (nrep - 1)), 1, max)
  )
  colnames(mc_errors) = c("MC_biases", "MC_SD")
  return(round(mc_errors, 2))
}


# ---- FUNCTION: Compute Summary Metrics ----
compute_summary_metrics = function(results_list, true.psi) {
  method_names = names(results_list)
  biases = do.call(rbind, lapply(results_list, function(res) colMeans(res, na.rm = TRUE) - true.psi))
  sds    = do.call(rbind, lapply(results_list, function(res) apply(res, 2, sd, na.rm = TRUE)))
  rmses  = sqrt(biases^2 + sds^2)
  ratio  = rmses[-1, ] / matrix(rmses[1, ], nrow = length(results_list) - 1, ncol = length(true.psi), byrow = TRUE)
  rownames(biases) = rownames(sds) = rownames(rmses) = method_names
  rownames(ratio) = method_names[-1]
  list(Bias = biases, SD = sds, RMSE = rmses, Ratio = ratio)
}


# ---- FUNCTION: Main Analysis ----
run_simulation_analysis_all = function(true.psi,
                                        scenarios = c("simple", "medium", "complex"),
                                        data_prefix,
                                        rql_prefix,
                                        suffix,
                                        nrep = 1000) {
  all_results = list()
  monte_carlo_errors = list()
  
  for (scenario in scenarios) {
    load(paste0(data_prefix, scenario, suffix))
    load(paste0(rql_prefix, scenario, suffix))
    true = if (is.list(true.psi)) true.psi[[scenario]] else true.psi
    

    possible_methods = list(
      Logit  = if (exists("results.logit"))  results.logit  else NULL,
      RF     = if (exists("results.random")) results.random else NULL,
      Bayes  = if (exists("results.bayes"))  results.bayes  else NULL,
      Neural = if (exists("results.neural")) results.neural else NULL,
      SVM    = if (exists("results.SVM"))    results.SVM    else NULL,
      SL     = if (exists("results.SL"))     results.SL     else NULL,
      RQL    = if (exists("results.RQL"))    results.RQL    else NULL
    )
    
    # Deletes NULLs automatically
    results_list = Filter(Negate(is.null), possible_methods)
    
    summary = compute_summary_metrics(results_list, true)
    all_results[[scenario]] = summary
    monte_carlo_errors[[scenario]] = compute_monte_carlo_error(results_list, nrep)
  }
  
  return(list(results = all_results, MCerror = monte_carlo_errors))
}


# ---- FUNCTION: Display Summary ----

display_summary_for_n=function(results_by_n, n, methods = NULL, colnames.psi) {
  selected=results_by_n[[paste0("n", n)]]$results
  
  return(list(
    Bias   = lapply(selected, function(x) `colnames<-`(round(x$Bias, 2), colnames.psi)),
    SD     = lapply(selected, function(x) `colnames<-`(round(x$SD, 2), colnames.psi)),
    RMSE   = lapply(selected, function(x) `colnames<-`(round(x$RMSE, 2), colnames.psi)),
    Ratio  = lapply(selected, function(x) `colnames<-`(round(x$Ratio, 2), colnames.psi)),
    Methods = methods,
    Scenarios = names(selected)
  ))
}

# ---- FUNCTION: Extract and format all metrics for a given n (Studies 1-4) ----

extract_all_metrics_for_n=function(all_results_by_n, n, methods = NULL, colnames.psi, add_labels = TRUE) {
  res_n=display_summary_for_n(all_results_by_n, n, methods = methods, colnames.psi = colnames.psi)
  
  combine=function(res, metric) {
    do.call(cbind, lapply(c("simple", "medium", "complex"), function(scn) res[[metric]][[scn]]))
  }
  
  Bias  =combine(res_n, "Bias")
  SD    =combine(res_n, "SD")
  RMSE  =combine(res_n, "RMSE")
  Ratio =combine(res_n, "Ratio")
  
  all_metrics=rbind(Bias, SD, RMSE, Ratio)
  
  if (add_labels) {
    if (is.null(methods)) methods=simulation_methods
    rownames(all_metrics)=c(
      paste0("Bias_", methods),
      paste0("SD_", methods),
      paste0("RMSE_", methods),
      paste0("Ratio_", methods[-1])
    )
    colnames(all_metrics)=rep(colnames.psi, times = 3)
  }
  
  return(round(all_metrics, 2))
}

# ---- FUNCTION: Extract and format all metrics for a given n (Study 5) ----

extract_all_metrics_Study5_for_n = function(all_results_by_n, n, methods = NULL, colnames.psi, add_labels = TRUE) {
  res_n = display_summary_for_n(all_results_by_n, n, methods = methods, colnames.psi = colnames.psi)
  
  combine_per_scenario = function(metric) {
    lapply(c("simple", "medium", "complex"), function(scn) res_n[[metric]][[scn]])
  }
  
  Bias_list  = combine_per_scenario("Bias")
  SD_list    = combine_per_scenario("SD")
  RMSE_list  = combine_per_scenario("RMSE")
  Ratio_list = combine_per_scenario("Ratio")
  
  scenarios = c("simple", "medium", "complex")
  result_list = list()
  
  for (i in seq_along(scenarios)) {
    Bias  = Bias_list[[i]]
    SD    = SD_list[[i]]
    RMSE  = RMSE_list[[i]]
    Ratio = Ratio_list[[i]]
    
    all_metrics = rbind(Bias, SD, RMSE, Ratio)
    
    if (add_labels) {
      if (is.null(methods)) methods = simulation_methods
      rownames(all_metrics) = c(
        paste0("Bias_", methods),
        paste0("SD_", methods),
        paste0("RMSE_", methods),
        paste0("Ratio_", methods[-1])  
      )
      colnames(all_metrics) = colnames.psi
    }
    
    result_list[[scenarios[i]]] = round(all_metrics, 2)
  }
  
  return(result_list)
}


