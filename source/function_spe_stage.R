##flow char##
#########################################################################################


flow_chart <- function(base,critere,patient_perdu,table_grp) {
  nb_crit <- length(critere)
  
  debut_du_plot <- paste0("digraph {
node [fontname = Helvetica, shape = rectangle]; \n", 
                          paste0("base; ",paste0("critere_",letters[1:nb_crit], collapse = "; "),"; ",
                                 paste0("perte_",letters[1:nb_crit], collapse = "; "),"\n"),
                          paste0("secret_node",c(1:nb_crit) ,
                                 "[height=0, width=0, margin=0,shape=point, style=invis]; \n",collapse = "")
                          #paste0("secret_node",c(1:nb_crit), collapse = "; ")
  )
  
  
  
  elements <- paste0("base [label = '",  "Patient dans la base OSFP (n = ",
                     base ," )","'] \n", 
                     paste0("critere_",letters[1:nb_crit], "  [label = '",
                            "Patients restant  (n = ",
                            base - cumsum(patient_perdu), " )" ,"' ]", collapse = " \n"),"\n",
                     paste0("perte_",letters[1:nb_crit], "  [label = '", critere ,  " (n = ",
                            patient_perdu, " )","' ]", collapse = " \n"),"\n",
                     paste0("group_obs_",letters[1:length(table_grp)], "  [label = '","Nombre de patients" ,  " (n = ",
                            table_grp, " ) \n" ,round((table_grp * 100 )/(base - sum(patient_perdu)),2) ," % ","' ]", collapse = " \n")
                     
  )
  
  rang <- paste0(paste0("{rank=same;","secret_node",c(1:nb_crit)," ",
                        "perte_",letters[1:nb_crit],"}",collapse = " \n"),"\n",
                 "{rank=same;",paste0("group_obs_",letters[1:length(table_grp)],collapse = "  "),"}",
                 "\n")
  
  
  lien <- paste0(
    paste0("base -> secret_node1 [arrowhead = none] \n", collapse = "") ,
    paste0("secret_node",c(1:nb_crit)," -> ","critere_",
           letters[1:nb_crit]," \n", collapse = "") ,
    paste0("critere_",letters[1:(nb_crit-1)]," -> ","secret_node",
           c(2:nb_crit)," [arrowhead = none] \n", collapse = "") , 
    paste0("secret_node",c(1:nb_crit) ," -> ", "perte_",
           letters[1:nb_crit], collapse = "\n"),"  \n", 
    paste0("critere_",letters[nb_crit] ," -> ", "group_obs_", letters[1:length(table_grp)],"[minlen='2.9']" , collapse = "\n") ,"  \n", 
    collapse = "") 
  
  
  fin_plot <- " \n }" 
  
  res <- paste0(debut_du_plot,elements,rang,lien,fin_plot)
  grViz(res)
  return(res)}

##function principale du stage##
#########################################################################################
calcule_pds_stage <- function(donne,expo,covar,out_come,percentile_tronc = c(0,1,5,10,25,50)/100 ){
  tempcall <- match.call()
  
  
  fun_trunc <- function(x,.probs) {
    pmin(pmax(x, quantile(x, probs = .probs)), 
         quantile(x, probs = 1-.probs))}
  exposure <- donne[,as.character(tempcall$expo)]
  mod1 <-   multinom(formula =   eval(parse(text =
                                              paste("as.numeric(",deparse(tempcall$expo),")", paste0("~ ",paste0(covar, collapse = " + ")), # récupération des paramètre de la fonction
                                                    sep = "")
  )),data = donne, na.action = na.fail ,trace = FALSE)
  
  res <- data.frame(exposition = exposure , denominator = NA) %>% 
    setNames(c("exposition", "denominator")) 
  
  
  res <- res %>% 
    group_by(exposition) %>%
    mutate(n = n(),numerator = n / nrow(.)) %>% select(-n)
  
  
  proba_tps_inv <- predict(mod1, type = "probs") %>% 
    as.data.frame() %>% 
    rename("1" = names(.)[1]) %>% 
    mutate("0" = 1 - apply(.,1,sum))
  res$denominator <- sapply(1:nrow(res), 
                            function(x) proba_tps_inv[x,as.character(as.numeric(res$exposition[x]))]) # reste a incorporé dans un mutate
  
  
  
  #PS propencenty score
  res <- res %>% mutate(PS = numerator/denominator, poids = 1/PS) 
  
  
  test_moy <- function_box_plot_expo_func(donne,
                                          deparse(substitute(expo)),
                                          deparse(substitute(out_come))
                                          )
  
  plot_poids <- res %>% 
    ggplot(aes(x = denominator)) +
    geom_histogram(binwidth = hist_bins(res$denominator)) +
    facet_wrap( ~ exposition,ncol = 1) + theme_bw()
  
  
  # moyenne et dispersion des poids par groupe
  moyenne_pds <- res %>% group_by(exposition)  %>% 
    summarise("moyenne stabilisé" = mean(PS) ,
              "sd stabilisé" = sd(PS), 
              "moyenne non stabilisé" = mean(PS / numerator ) ,
              "sd non stabilisé" = sd(PS / numerator))
  
  
  # troncature des poids 
  poid_trunc_df <- map(percentile_tronc,function(x) fun_trunc(res$poids,x)) %>% 
    as.data.frame() %>% 
    structure(names = paste0("( ",
                             as.character(percentile_tronc),
                             ";",
                             as.character(1 - percentile_tronc),
                             " )" ) )
  
  
  # potentiellement a changé avec t.df mais la flemme
  
  tableau_pds_trunc <- poid_trunc_df %>% summarise_all(list(~mean(.),
                                                            ~sd(.),
                                                            ~min(.),
                                                            ~max(.))) %>%
    t %>% 
    as.data.frame %>% 
    rownames_to_column %>% 
    separate(rowname, 
             into = c("Troncature",
                      "fun"),
             sep = "_") %>% 
    pivot_wider(names_from = fun ,
                values_from = V1)
  
  
  return(list(df = res,
              res_intermediaire = list(
                poids = list(
                  moyenne_pds = moyenne_pds,
                  poids_tronc = poid_trunc_df,
                  summary_pds_trunc = tableau_pds_trunc,
                  plot_posit = plot_poids),
                regression_modele_pds = list(
                  regression_temps_ind = mod1,
                  data_frame_coef = proba_tps_inv),
                test_covar = test_moy)
  )
  )
}


##Imputation si changment de données##
#########################################################################################
impute_si_changement <- function(data_frame_a_verif,path,reimputation = FALSE){
  df_impute_ex <- tryCatch(read_rds(path),  
                           error = function(e) data.frame())  
  colanmes_manq_func <- data_frame_a_verif %>%
    select_if(data_frame_a_verif %>% 
                summarise_all(list(~sum(is.na(.)))) != 0) %>% 
    colnames()
  
  if (all(sort(colnames(data_frame_a_verif)) == sort(colnames(df_impute_ex)) ) & 
      (nrow(data_frame_a_verif) ==  nrow(df_impute_ex))) {
    res <- df_impute_ex
  } else if (any(sort(colnames(data_frame_a_verif)) != sort(colnames(df_impute_ex))) | reimputation | nrow(df_impute_ex) == 0) {
    cat("nécessité de réimputer les données 
      ça va être long + ou - une heure")
    imputed_Data_func <- mice::mice(data_frame_a_verif,
                                    m = 10,
                                    maxit = 50, 
                                    method = 'pmm',
                                    printFlag = FALSE)
    if (!is.null(imputed_Data_func$loggedEvents)) { 
      print("imputation avec warning")
      print(imputed_Data_func$loggedEvents)}
    pdf(file = "graph/Imputation_plot.pdf",width = 32, height = 18 ,onefile = TRUE) 
    mice::densityplot(imputed_Data_func, data = as.formula(paste0("~",paste0(colanmes_manq_func,collapse = "+")))
    )
    dev.off()
    pdf(file = "graph/manquant_plot.pdf",width = 32, height = 18 ,onefile = TRUE) 
    visdat::vis_miss(data_frame_a_verif %>% select(all_of(colanmes_manq_func)), cluster = TRUE)
    dev.off()
    res <- mice::complete(imputed_Data_func)
    saveRDS(res, file = paste0("data/genere/data_impute.rds"))
  } else {stop("Condition non remplie problème fonction")}
return(res)
}

##########################################################################################
# function récupérant les variables pour table descriptive des variables

# df_one_hot_encode_fonc généré avec one_hot_fb

recup_var_table_verif_impute <- function(df_one_hot_encode_fonc,name_expo_fonc){
  res <-   res <- df_one_hot_encode_fonc %>% 
    select(-all_of(name_expo_fonc)) %>% 
    mutate_if(is.factor, ~as.numeric(as.character(.))) %>% # points litigieux j'utilise cette méthode pour convertir mes facteur booleen en numeric a surveilllé a l'avenir
    summarise_all(list(fonc_med = ~round(median(.,na.rm = TRUE),2),
                       fonc_quart1 = ~quantile(.,0.25,na.rm = TRUE),
                       fonc_quart2 = ~quantile(.,0.75,na.rm = TRUE),
                       fonc_mean = ~mean(.,na.rm = TRUE),
                       fonc_sd = ~sd(.,na.rm = TRUE),
                       fonc_min = ~min(.,na.rm = TRUE),
                       fonc_max = ~max(.,na.rm = TRUE))) %>% 
    pivot_longer(cols = everything(),
                 names_to = c(".value", "level"),
                 names_pattern = "(.*)_fonc_(.*)") %>% 
    t.df(.,"level") %>% column_to_rownames(var = "key")
  return(res)
  }

##########################################################################################
# Vérifications sommaire de l'imputations plus générations des tables

# df_one_hot_encode_fonc généré avec one_hot_fb
test_imputation <- function(df__pre_imput_func,df__post_imput_func,name_expo_fonc,check = TRUE) {
  colanmes_manq_func <- df__pre_imput_func %>%
    select_if(df__pre_imput_func %>% summarise_all(list(~sum(is.na(.)))) != 0) %>% 
    colnames()
  
  table_pre_imput <-  df__pre_imput_func %>% 
    select(all_of(colanmes_manq_func),all_of(name_expo_fonc)) %>% 
    recup_var_table_verif_impute(name_expo_fonc) 
  table_post_imput <-  df__post_imput_func %>% 
    select(all_of(colanmes_manq_func),all_of(name_expo_fonc)) %>% 
    recup_var_table_verif_impute(name_expo_fonc) 
  
  table_diff_pre_post <- table_pre_imput - table_post_imput
  table_diff_prop_pre_post <- table_diff_pre_post / table_pre_imput
  table_diff_prop_pre_post[table_pre_imput == 0] <- 0
  
  if (check == TRUE) {
    if (length(colanmes_manq_func) == 0)     stop('Pas de valeur manquante dans la DF 
                                                pré imputation. Tu es sur ?  
                                                Si oui passe `check = FALSE`')
    if (any(df__post_imput_func %>% summarise_all(list(~sum(is.na(.)))) != 0)) {
      cat("les colonnes suivante de la df post imputations contiennent des manquants" ,  
          df__post_imput_func %>%
            select_if(df__post_imput_func %>% summarise_all(list(~sum(is.na(.)))) != 0) %>% 
            colnames() ,
          "\n pour ne pas faire de check `check = FALSE`")
      stop('Il reste des manquant dans la DF post imput')}
    
    table_pre_imput_col_ss_manq <- df__pre_imput_func %>% 
      select(-all_of(colanmes_manq_func))  %>% 
      recup_var_table_verif_impute(name_expo_fonc) 
    table_post_imput_col_ss_manq <-  df__post_imput_func %>% 
      select(-all_of(colanmes_manq_func)) %>% 
      recup_var_table_verif_impute(name_expo_fonc) 
    diff_diff_de_zero <- table_pre_imput_col_ss_manq - table_post_imput_col_ss_manq != 0
    
    if (any(diff_diff_de_zero)) {
      cat("les valeurs des colones sans valeurs manquantes", 
          "ont était modifié par l'imputations cela concerne les colones suivante :\n",
          paste0(names(which(rowSums(diff_diff_de_zero) > 0)),collapse = "  , "),
          "\n pour ne pas faire de check `check = FALSE`")
      stop( " différence dans des colonnes non imputé")
    }
    if (any(table_diff_prop_pre_post$mean > 0.1)){
      cat("Il semble avoir eu un problème lors de l'imputations les variables : " ,
          rownames(table_diff_prop_pre_post[table_diff_prop_pre_post$mean > 0.1,]),
          "on subit une variation de lors moyennes supérieurs a 10%")
      
    }
  }
  res <- list(T_diff = table_diff_pre_post, T_prop_diff = table_diff_prop_pre_post)
  return(res)
  }


##########################################################################################
