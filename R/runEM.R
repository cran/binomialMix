#' Run an EM algorithm to obtain a mixture of binomial with K clusters
#'
#' This function is the main function of this package. 
#' The objective is to provide a clustering of the 80 campaigns that we have on our dataset.
#' The specification of this algorithm is that we can have longitudinal data, i.e n observations for a single campaign. 
#'
#' @importFrom dplyr summarise
#' @importFrom dplyr n
#' @importFrom dplyr pull
#' @importFrom dplyr distinct
#' @importFrom rlang sym
#' @importFrom stats glm
#' @importFrom stats na.omit
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#'
#' @param formula  A formula or Character which links target variable and predictor variables
#' @param var_weights A character value corresponding to the weights variable
#' @param K A numeric value representing the number of clusters chosen for the mixture
#' @param df A dataframe to cluster
#' @param col_id A character value (colname) corresponding to the id column name
#' @return a summary list of EM algorithm results : loglikelihood, beta/lambda/tau estimation at each iteration,
#' bic/icl value,number of fisher iteration at each EM iteration 
#' @examples
#' ## Load data :
#' data(adcampaign)
#' ## Run mixture :
#'\dontrun{
#' result_mixture<-runEM(formula="ctr~timeSlot",
#'                       var_weights="impressions",
#'                       K=2,
#'                       df=adcampaign,
#'                       col_id="id")
#' ## Analysis of results :
#' plot(result_mixture[[1]],type="l") #gives you the loglikelihood evolution
#' # list of the estimated parameter for each cluster for each iteration :
#' result_mixture[[2]]  
#' # list of the estimated parameter for each cluster for each iteration
#' result_mixture[[3]] #list of ids proportion in each cluster for each iteration
#'#list of matrices containing probability to be in cluster k for each id :
#' result_mixture[[4]] 
#' # BIC value :
#' result_mixture[[5]] 
#' # ICL value :
#' result_mixture[[6]] 
#' # list of number fisher scoring iterations for each iteration
#' result_mixture[[7]]
#'} 
#' @export
runEM <-
function(formula, var_weights, K, df, col_id = "id") {
    
    ############################ Definition of some useful variables ############################

    	    
    df <- as.data.frame(df)
    count_row<-NULL    
    N <- df %>% dplyr::summarise(count_row=n()) %>% pull(count_row)
    count_id<-NULL
    distinct_id <- df %>% dplyr::select((!!sym(col_id))) %>% distinct() %>% dplyr::summarise(count_id=n()) %>% pull(count_id)
    loglik <- rep(NA, 30)
    target <- extract_target(formula)
    formula <- as.formula(formula)
    if ( any(str_detect(df[,col_id], "[:alpha:]")==TRUE) ){
    new_id<-as.numeric(factor(levels(df[,col_id])))
    levels(df[,col_id])<-new_id
    }
    
    ########################### Initialisation ##################
    
    # Initialisation des matrices :
    design_mat <- init_design_matrices(formula, df,col_id)
    df_id <- design_mat[[1]]
    n_id <- design_mat[[2]]
    matrix_id <- design_mat[[3]]
    
    # Initialisation des beta : 
    beta_hk <- list()
    df_beta<-init_subset(df,K,col_id)
    beta_init<-list()
    for (k in 1:K){
        df_glm<-df_beta[[k]]
        beta_init[[k]]<-as.numeric(glm(formula,
                                       family="binomial",
                                       data=df_glm,
                                       weights = df_glm[,var_weights])$coefficients)
    }
    beta_hk[[1]]<-do.call(cbind,beta_init)
    rm(df_glm,beta_init)
    
    tau <- list()
    tau[[1]] <- init_tau(df, K, col_id)  #matrice de proba que la camp c soit dans le cluster K
    
    lambda <- list()
    lambda[[1]] <- init_lambda(K)  #proportion de chaque distribution lambda_k à estimer
    
    # Logloss with initialized parameters
    loglik[1] <- Incomplete_Loglikelihood_binomiale(df, col_id, target, var_weights, df_id, matrix_id, lambda[[1]], beta_hk[[1]], K)
    
    m <- 2
    it <- 1
    diff_param <- 1
    fisher_it <- list()
    
    ########## Itération et mise à jour des paramètres jusqu'à convergence ###########
    
    while((diff_param > 1e-04) & (it < 30)) {
        
        lambda_old <- lambda[[m - 1]]
        beta_hk_old <- beta_hk[[m - 1]]

        lambda[[m]] <- lambda[[m - 1]]
        beta_hk[[m]] <- beta_hk[[m - 1]]
        tau[[m]] <- tau[[m - 1]]
        
        ######### E step #########
        
        ##### Mise à jour des probabilités d'appartenance à chaque cluster
        
        tau[[m]] <- update_tau(df, K, col_id, beta_hk, lambda, m, df_id, n_id, matrix_id, var_weights, target)
        
        ######### M Step #########
        
        #### Mise à jour lambda :
        lambda[[m]] <- apply(tau[[m]], 1, sum)/distinct_id
        
        #### Mise à jour beta :
        fisher_crit <- ifelse(it < 5, 0.01, ifelse((it >= 5) & (it < 10), 0.001, ifelse((it >= 10) & (it < 20), 1e-04, 1e-05)))
        
        fisher_it[[m]] <- vector(length = 2)
        
        for (k in 1:K) {
            
            it_f <- 0
            beta <- beta_hk[[m]][, k]
            beta_update <- beta_hk[[m]][, k] + 1
            
            while ((max(abs(beta_update - beta)) > fisher_crit) & (it_f < 10)) {

                beta_update <- as.numeric(beta)
                w_inv <- list()
                z <- list()
                
                it_f <- it_f + 1
                
                w_inv <- update_w(df, col_id, var_weights, beta_update, df_id, matrix_id)
                z <- update_z(df, col_id, target, beta_update, df_id, matrix_id)
                
                beta <- update_beta(formula, df, k, col_id, tau, m, w_inv, z, matrix_id)
                
            }
            
            fisher_it[[m]][k] <- it_f
            beta_hk[[m]][, k] <- beta
            
        }
        
        ### Itération suivante :
        
        loglik[m] <- Incomplete_Loglikelihood_binomiale(df, col_id, target, var_weights, df_id, matrix_id, lambda[[m]], beta_hk[[m]], K)
        
        param_list_old <- rbind(lambda_old, beta_hk_old)
        param_list <- rbind(lambda[[m]], beta_hk[[m]])
        diff_param <- max(abs(param_list_old - param_list))
        
        m <- m + 1
        it <- it + 1
        
    }
    
    bic <- my_BIC(length(beta_hk[[m - 1]]) + (length(lambda[[m - 1]]) - 1), loglik, N)
    
    icl <- my_ICL(df, col_id, K, length(beta_hk[[m - 1]]) + (length(lambda[[m - 1]]) - 1), loglik, tau[[m - 1]], N)
    
    list_result<-list() 
    list_result<-list(as.vector(na.omit(loglik)), beta_hk, lambda, tau, bic, icl, fisher_it)
    return(list_result) 

}
