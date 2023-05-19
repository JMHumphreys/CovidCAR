#' Run a series of INLA models
#'
#' This function runs a list of INLA models specified in \code{formulas.list} on data stacked in \code{dataStack}. The function returns a list of model outputs with names matching the input formula names.
#'
#' @param formulas.list A named list of model formulas to be run. Names should match the desired output names.
#' @param dataStack Stacked data object as created by \code{inla.stack.data()} function from the INLA package.
#' @param likelihood A character vector specifying which likelihood family to use for each model. If a length 1 character string is provided, it will be used for all models. If a character vector of length n_models is provided, it will be applied in order.
#' @param config A logical value indicating whether or not to retain GMRF representation for sampling.
#' @param verbose A logical value indicating whether or not to print details to screen while running.
#' @param archive A logical value indicating whether or not to save the output in an archive file. Default is TRUE.
#'
#' @return models_out A list of model outputs with names corresponding to the input formula names.
#'
#' @export run_model_list
run_model_list <- function(formulas.list, dataStack, likelihood = "gaussian", config=FALSE, verbose=FALSE, archive=TRUE){

  models_out = list()

  n_models <- length(formulas.list)

  if (length(likelihood) == 1) {
    likelihood_vect <- rep(likelihood, n_models)
    cli_h2(paste0("Specifying a ", likelihood, " likelihood for all models."))
  } else if (length(likelihood) == n_models) {
    likelihood_vect <- likelihood
    cli_h2("Model likelihoods will be applied in order provided.")
  } else {
    cli_abort(paste0("A vector of ", length(likelihood), " likelihoods provided, but ", n_models, " models are requested?"))
  }

  return_quants = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99) #CovHub req'd

  for(i in 1:length(formulas.list)){

    cli_alert("Running models ", names(formulas.list)[[i]])
    cli_alert_success("Model {paste0(names(formulas.list)[[i]])} completed!")

    models_out[[i]] = inla(formulas.list[[i]], #model formula
                           data = inla.stack.data(dataStack), #data organized as a list
                           family = likelihood_vect[i], #likelihood family
                           verbose = verbose,      #print details to screen while running
                           quantiles = return_quants, #quantiles to return
                           control.fixed = list(prec = 1, prec.intercept=1), #proper intercept/linear
                           control.predictor = list(
                             A = inla.stack.A(dataStack), #data stack (again)
                             compute = TRUE, #return fitted values
                             link = 1),   #default link functions
                           control.inla = list(strategy="adaptive", #experimental algorithm to speed up
                                               int.strategy = "eb"), #Empirical Bayes (uses mode to speed up)
                           control.compute = list(dic = F, cpo = F, waic = F, config=FALSE)) #comparison metrics
    #config=TRUE: retain GMRF representation for sampling
    cli_progress_done()
  }


  names(models_out) = names(formulas.list)

  if(archive == TRUE){

    archive_name <- paste0(out_dir_name,"/run_archive/")
    if (!dir.exists(archive_name)) {
      dir.create(archive_name)
    }

    num_files <- length(list.files(archive_name))
    archive_name <- paste0(archive_name, Sys.Date(), "-models-run-v", num_files,".RData")

    save(list=c("models_out", "formulas.list", "dataStack", "train_data"),
         file=paste0(archive_name), version = 2)
  }

  return(models_out)
}
