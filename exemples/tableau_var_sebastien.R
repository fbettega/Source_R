`%>%` <- dplyr::`%>%`
require(tidyverse)
require(caret)
require(stringr)
##########################################################################################

# transposé dataframe
t.df <- function(df,pivot=NULL){
  if (is.null(pivot)){
    pivot <- "row_id"
    df <- df %>% dplyr::mutate(row_id = 1:nrow(df))
  }
  res <- df %>% tidyr::pivot_longer(cols = -!!pivot,"key","value") %>%
    tidyr::pivot_wider(names_from = !!pivot,values_from = value)
  return(res) 
}


##########################################################################################
# fonction maison pour rechercher le type des données 
## objectifs suivant différence entre int et float
typages_function <- function(df,nb_moda_max_fact = NULL ){
  if (is.null(nb_moda_max_fact)) {
    df %>% 
      dplyr::summarise_all(list(~dplyr::n_distinct(na.omit(.)))) %>%
      t.df() %>% dplyr::filter(`1` > 2) %>% 
      dplyr::arrange(`1`) %>% 
      print
    stop('Si la liste des facteurs n\'est pas fournie le nombre de modalitées à partir
           duquel un facteur doit etre considéré comme un numérique avec `nb_moda_max_fact = `,
           \n pour vous aider dans le choix du nombre de modalitées la liste des variables
           avec plus de deux modalitées différentes est présentée au  dessus.')}
  else {
    temp_moda_par_var <- df %>%
      dplyr::summarise_all(list(~dplyr::n_distinct(na.omit(.)))) %>% 
      t.df() %>% 
      dplyr::mutate(binaire = `1` == 2, numeric = `1` >= nb_moda_max_fact,
             multinomial = (`1` < nb_moda_max_fact & `1` > 2)) %>% 
      dplyr::arrange(`1`) 
    list_factor <- temp_moda_par_var %>% 
      dplyr::filter(multinomial) %>%
      dplyr::select(key) %>% 
      unlist(use.names = FALSE)
    liste_booleen <- temp_moda_par_var %>% 
      dplyr::filter(binaire) %>%
      dplyr::select(key) %>% 
      unlist(use.names = FALSE)
    liste_numeric <- temp_moda_par_var %>% 
      dplyr::filter(numeric) %>%
      dplyr::select(key) %>% 
      unlist(use.names = FALSE)
    res <- list(colonne_type = list(facteur = list_factor,
                                    booleen = liste_booleen,
                                    numerique = liste_numeric),
                data_frame_tot = temp_moda_par_var)
  }
  return(res)
}

##########################################################################################
# fonction maison pour le one hot encoding
one_hot_fb <- function(df, nb_moda_max_fact = NULL, list_factor = NULL){
  if (is.null(list_factor)) {
    list_factor <- typages_function(df, nb_moda_max_fact)$colonne_type$facteur
    cat("Le nombre maximum de modalité par facteur est de ",
        nb_moda_max_fact,
        "\n pour supprimer ce warning utilisez `list_factor = ", "c( ",
        paste0("\"",list_factor,"\"",collapse = " , "),
        " )` \n au lieu de `nb_moda_max_fact = ", nb_moda_max_fact,"`")
  }
  
  
  
  df <- df %>% dplyr::mutate_at( list_factor,as.factor)
  dmy <- caret::dummyVars(paste0(" ~ ", paste0(list_factor,collapse = " + ")), data = df)
  trsf <- data.frame(predict(dmy, newdata = df))
  res <- df %>% dplyr::select(-all_of(list_factor)) %>% cbind(trsf) # ajout all_of retirer si bug 
  return(res)
  # reste a ajouter une partie qui renomme les varibles mieux 
}


##########################################################################################
# function récupérant les variables pour table descriptive des variables

# df_one_hot_encode_fonc généré avec one_hot_fb

recup_var_table_res <- function(df_one_hot_encode_fonc,name_expo_fonc){
  res <- df_one_hot_encode_fonc %>% 
    dplyr::select(-all_of(name_expo_fonc)) %>% 
    dplyr::mutate_if(is.factor, ~as.numeric(as.character(.))) %>% # points litigieux j'utilise cette méthode pour convertir mes facteur booleen en numeric a surveilllé a l'avenir
    dplyr::summarise_all(list(fonc_med = ~median(.,na.rm = TRUE),
                       fonc_quart1 = ~quantile(.,0.25,na.rm = TRUE),
                       fonc_quart2 = ~quantile(.,0.75,na.rm = TRUE),
                       fonc_n = ~sum(.,na.rm = TRUE), 
                       fonc_pourcent = ~mean(.,na.rm = TRUE)*100,
                       fonc_nb_NA = ~sum(is.na(.))
    )
    ) %>% 
    tidyr::pivot_longer(cols =  everything(),
                 names_to = c(".value", "level"),
                 names_pattern = "(.*)_fonc_(.*)") %>% 
    t.df(.,"level") 
  return(res)}


##########################################################################################
# table descriptive des variables

# df_one_hot_encode_fonc généré avec one_hot_fb
# Version prévue pour échapper les caracter latex
table_resume_latex <- function(df_one_hot_encode_fonc,name_expo_fonc,nom_grp = "clusters", p_val = FALSE, arrondie = TRUE, alpha = 0.05) {
  
  # Typage des variables a réusmer en booleen ou numeric
  # Car normalement df pré_one hot encode
  table_bool_var <-   df_one_hot_encode_fonc %>% 
    dplyr::select(-all_of(name_expo_fonc)) %>% 
    dplyr::summarise_all(list(~dplyr::n_distinct(., na.rm = TRUE))) %>% 
    t.df %>% 
    rename(booleen = `1`) %>% 
    {temp_verif_bool <<- .} %>% 
    dplyr::mutate(booleen = booleen <= 2)
  
  
  if (any(temp_verif_bool$booleen < 2)) {
    print(temp_verif_bool$key[temp_verif_bool$booleen < 2])
    stop("moins de deux valeurs distinct pour une variables")}
  
  
  
  name_all_grp <- paste0("all_",nom_grp)
  
  
  all_cluster_descript_var <- df_one_hot_encode_fonc %>% 
    recup_var_table_res(name_expo_fonc) %>% 
    #{ifelse(arrondie ,arrondie_df(.), . )} %>% print %>% 
    dplyr::mutate(nb_NA = ifelse(nb_NA == 0,"",paste0(" NA : ", nb_NA ))) 
  
  
  nb_grp <- df_one_hot_encode_fonc %>% dplyr::select(all_of(name_expo_fonc)) %>% 
    unique() %>% 
    unlist(use.names = FALSE) %>% 
    sort
  
  
  group_cluster_descript_var <- lapply(nb_grp, function(x) {
    col_name <- paste0(nom_grp,"_",x)
    res <- df_one_hot_encode_fonc  %>%
      dplyr::filter(!!sym(name_expo_fonc) == x) %>% 
      recup_var_table_res(name_expo_fonc) %>% 
      inner_join(table_bool_var,by = "key") %>% 
      dplyr::mutate(nb_NA = ifelse(nb_NA == 0,"",paste0(" NA : ", nb_NA ))) %>% 
      dplyr::mutate( {{col_name}} := ifelse( booleen
                                      , paste0(n ,"(",round(pourcent,1), "%)", nb_NA),
                                      paste0(round(med,2) ,"(",
                                             round(quart1,1),";",
                                             round(quart2,1), ")", nb_NA))) %>% 
      dplyr::select(all_of(col_name))
    return(res)
  }
  )
  
  table_res <- all_cluster_descript_var %>%  
    inner_join(table_bool_var,by = "key") %>% 
    dplyr::mutate( {{name_all_grp}} := ifelse(booleen
                                       , paste0(n ,"(",round(pourcent,1), "%)",  nb_NA),
                                       paste0(round(med,2) ,"(",
                                              round(quart1,1),";", 
                                              round(quart2,1), ")", nb_NA) ) )  %>% 
    dplyr::select(key,all_of(name_all_grp)) %>% 
    cbind(bind_cols(group_cluster_descript_var)) %>% data.frame(., row.names = 1)
  
  # si choix de calculer les p-val 
  if (p_val) {
    # prevoir un groupe de plus pour le toutes les catégorie
    nb_group_pval <-  as.character(c(as.numeric(nb_grp),max(as.numeric(nb_grp)) + 1 ))
    # création de toutes les combinaisons de groupe a tester
    combin_grp <- nb_group_pval %>%  combn(2)
    # Création du groupe supplémentaire tout les groupes en dupliquant la dataframe avec un  groupes de plus
    df_pval <- df_one_hot_encode_fonc %>% 
      dplyr::mutate(!!sym(name_expo_fonc) := max(as.numeric(nb_group_pval))) %>% 
      rbind(df_one_hot_encode_fonc)
    
    
    
    # création  de la table avec p-value pour chaque combin et rename chaque colonnes a_b 
    combin_total_pval <- apply(combin_grp, 2, function(x)
      df_pval %>%
        summarise_at(vars(-(sym(name_expo_fonc) )),
                     list(~t.test(.[!!sym(name_expo_fonc) == x[1]],
                                  .[!!sym(name_expo_fonc)  ==  x[2]])$p.value)) %>%  
        t.df %>% 
        rename_at("1",list( ~paste0(x[1],"_",x[2])))
    )
    
    # transformation de la p-value en booléen en avec comme seuil le alpha définis en appliquant une correction de bonneferonni en considérant le nombre total de test nb ligne X nb collones
    result_pval <- bind_cols(combin_total_pval) %>%
      rename(key = key...1) %>% 
      dplyr::select(key,contains("_")) %>%
      dplyr::mutate_at(vars(-key),list(~(. < (alpha / ncol(combin_grp))
      ))
      ) %>%  # hypothèse et correction de bonneferonnie 
      dplyr::mutate_at(vars(-key), function(x) {
        x_var <- rlang::enquo(x)
        ifelse(x , rlang::quo_name(x_var), "non") # remplacement des p-val non signif par une chaine spécifique
      })
    
    # REcherche avec une simili boucle des p-val signif pour chaque colonnnes 
    # on en lève la chaine spécifique de non corrélation 
    df_pval_final <- lapply(nb_group_pval, function(x) {
      result_pval %>% dplyr::select(key,contains(x)) %>% 
        dplyr::mutate_at(vars(contains(x)),
                  list(~str_remove_all(.,paste(c("_",x), collapse = "|")))) %>% 
        unite(!!sym(paste0(nom_grp,"_",x)) ,contains(x),sep = ", ")
    }
    ) %>% bind_cols() %>% 
      rename(key = key...1) %>% 
      dplyr::select(key,contains(nom_grp)) %>% 
      rename(!!sym(name_all_grp) := paste0(nom_grp,"_",max(as.numeric(nb_group_pval)))) %>%   
      dplyr::mutate_all(list(~str_remove_all(.,"non, |, non"))) %>%  
      dplyr::mutate_all(list(~str_remove_all(.,"non")))
    
    if (df_pval_final %>%  
       transmute_at(vars(-key),list(~str_detect(.,"non"))) %>% as.matrix() %>% any) {
      stop("il reste des p-val non traité")}
    
    # Gestion des tables latex pour que les différences statisquement significative soit en subscript
    # en échappant les underscore
    table_res_pval <-  table_res %>% 
      rownames_to_column() %>%
      tidyr::pivot_longer(-rowname,values_to = "valeur") %>%
      inner_join((df_pval_final %>% 
                    tidyr::pivot_longer(-key,values_to = "pvalue") ),  
                 by = c("rowname" = "key", "name" = "name")) %>%  
      dplyr::mutate(combin = paste0(valeur,"\\textsubscript{",pvalue, "}")) %>%  
      dplyr::select(rowname,name,combin) %>%  
      tidyr::pivot_wider(names_from = name, values_from = combin) %>%  
      column_to_rownames()   
    
    table_res_pval <- table_res_pval %>% 
      rownames_to_column() %>% 
      dplyr::mutate_at(vars(-rowname),list(~str_replace_all(.,"%","\\\\%"))) %>% 
      column_to_rownames() %>% 
      dplyr::select(all_of(name_all_grp),sort(tidyselect::peek_vars()))
    
    
    rownames(table_res_pval) <- str_replace_all(rownames(table_res_pval),"_","\\\\_")
    colnames(table_res_pval) <- str_replace_all(colnames(table_res_pval),"_","\\\\_")
    
    
  } else {table_res_pval <- table_res %>% 
    dplyr::select(all_of(name_all_grp),sort(tidyselect::peek_vars()))}
  
  nb_pat_par_grp <- c(nrow(df_one_hot_encode_fonc),
                      table(df_one_hot_encode_fonc[,name_expo_fonc]))
  res <- rbind(n = nb_pat_par_grp,table_res_pval)
  return(res)
}

load("exemple_sebastion_df_exemple.RData") # en exemple tu as les 1000 premiere ligne de mo jeux de données


one_hot_fb(exemple_box_plot) 

one_hot_fb(exemple_box_plot,nb_moda_max_fact = 10) 

one_hot_fb(exemple_box_plot,list_factor =  c(  "Vis_init_FDR_fumeur" , 
                                                         "Mesure_unique_ATC_id_typeDiabete" , 
                                                         "Mesure_unique_CHA_nombreDents" , 
                                                         "OTH_date_modif" ,
                                                         "FDR_fumeur" , 
                                                         "SYM_troubleMemoire_echelle" ,
                                                         "PPC_id_typeMasque" ,
                                                         "Vis_init_BIO_dateBilan" ,
                                                         "Mesure_unique_ATC_SJSRCriteresDiagnostiques" ,
                                                         "Mesure_unique_ATC_SJSRscoreSeverite" , 
                                                         "CHA_autoPADiastolique" ,
                                                         "BIO_dateBilan" ,
                                                         "INS_obs_categ" ,
                                                         "Vis_init_SYM_nombreMictionsNocturnes" ,
                                                         "SYM_nombreMictionsNocturnes" ,
                                                         "PPC_dateArretTraitement" ,
                                                         "Vis_init_CHA_autoPADiastolique" , 
                                                         "Vis_init_CHA_autoPASystolique" , 
                                                         "CHA_autoPASystolique" , 
                                                         "INS_PPC_effet_ind"  )
                     )

# deuxieme exmple pour la fonction
load("tableau_resume_exemple.RData")

table_resume_latex_ss_escape <- table_resume_latex(df_one_hot_encode_fonc =  tableau_resume_ex,
                                                   name_expo_fonc = "INS_obs_categ",
                                                   nom_grp  = "grp_obs",
                                                   p_val = TRUE,
                                                   alpha = 0.05)


print_kabble <- table_resume_latex_ss_escape %>%
  kable(longtable =  TRUE,
        booktabs = TRUE,
        escape = FALSE,
        caption = "Table des variables pour chaque groupe d'observance"
  ) %>%
  kable_styling(latex_options = c("hold_position", "repeat_header"),
                font_size = 7) %>%
  add_footnote("Les chiffres dans chaque collonnes sont les colonnes statistiquement différentes au seuil de 5 % avec une correction de Bonferroni", notation ="alphabet") %>% 
  landscape() 

print_kabble