##########################################################
##                    function                          ##
##########################################################
deriv <- function(x, y) diff(y) / diff(x)

get.elbow.points.indices <- function(x, y, threshold) {
  d1 <- deriv(x, y) # first derivative
  indices <- which(abs(d1) > threshold)
  return(indices)
}
`%notin%` <- Negate(`%in%`)



##########################################################################################
# fonction maison pour rechercher le type des données 

## objectifs suivant différence entre int et float
typages_function <- function(df,nb_moda_max_fact = NULL ){
  if (is.null(nb_moda_max_fact)) {
    df %>% 
      summarise_all(list(~n_distinct(na.omit(.)))) %>%
      t.df() %>% filter(`1` > 2) %>% 
      arrange(`1`) %>% 
      print
    stop('Si la liste des facteurs n\'est pas fournis le nombre de modalité à partir
           duquel un facteur doit etre considéré comme un numéric avec `nb_moda_max_fact = `,
           \n pour vous aidez dans le choix du nombre de modalité la liste des variables
           avec plus de deux modalité différente est présenté au  dessus')}
  else {
    temp_moda_par_var <- df %>% summarise_all(list(~n_distinct(na.omit(.)))) %>% 
      t.df() %>% 
      mutate(binaire = `1` == 2, numeric = `1` >= nb_moda_max_fact,
             multinomial = (`1` < nb_moda_max_fact & `1` > 2)) %>% 
      arrange(`1`) 
    list_factor <- temp_moda_par_var %>% 
      filter(multinomial) %>%
      select(key) %>% 
      unlist(use.names = FALSE)
    liste_booleen <- temp_moda_par_var %>% 
      filter(binaire) %>%
      select(key) %>% 
      unlist(use.names = FALSE)
    liste_numeric <- temp_moda_par_var %>% 
      filter(numeric) %>%
      select(key) %>% 
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
  
  
  
  df <- df %>% mutate_at( list_factor,as.factor)
  dmy <- dummyVars(paste0(" ~ ", paste0(list_factor,collapse = " + ")), data = df)
  trsf <- data.frame(predict(dmy, newdata = df))
  res <- df %>% select(-all_of(list_factor)) %>% cbind(trsf) # ajout all_of retirer si bug 
  return(res)
  # reste a ajouter une partie qui renomme les varibles mieux 
}


##########################################################################################
# function récupérant les variables pour table descriptive des variables

# df_one_hot_encode_fonc généré avec one_hot_fb

recup_var_table_res <- function(df_one_hot_encode_fonc,name_expo_fonc){
  res <- df_one_hot_encode_fonc %>% 
    select(-all_of(name_expo_fonc)) %>% 
    mutate_if(is.factor, ~as.numeric(as.character(.))) %>% # points litigieux j'utilise cette méthode pour convertir mes facteur booleen en numeric a surveilllé a l'avenir
    summarise_all(list(fonc_med = ~median(.,na.rm = TRUE),
                       fonc_quart1 = ~quantile(.,0.25,na.rm = TRUE),
                       fonc_quart2 = ~quantile(.,0.75,na.rm = TRUE),
                       fonc_n = ~sum(.,na.rm = TRUE), 
                       fonc_pourcent = ~mean(.,na.rm = TRUE)*100,
                       fonc_nb_NA = ~sum(is.na(.))
    )
    ) %>% 
    pivot_longer(cols =  everything(),
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
    select(-all_of(name_expo_fonc)) %>% 
    summarise_all(list(~n_distinct(., na.rm = TRUE))) %>% 
    t.df %>% 
    rename(booleen = `1`) %>% 
    {temp_verif_bool <<- .} %>% 
    mutate(booleen = booleen <= 2)
  
  
  if (any(temp_verif_bool$booleen < 2)) {
    print(temp_verif_bool$key[temp_verif_bool$booleen < 2])
    stop("moins de deux valeurs distinct pour une variables")}
  
  
  
  name_all_grp <- paste0("all_",nom_grp)
  
  
  all_cluster_descript_var <- df_one_hot_encode_fonc %>% 
    recup_var_table_res(name_expo_fonc) %>% 
    #{ifelse(arrondie ,arrondie_df(.), . )} %>% print %>% 
    mutate(nb_NA = ifelse(nb_NA == 0,"",paste0(" NA : ", nb_NA ))) 
  
  
  nb_grp <- df_one_hot_encode_fonc %>% select(all_of(name_expo_fonc)) %>% 
    unique() %>% 
    unlist(use.names = FALSE) %>% 
    sort
  
  
  group_cluster_descript_var <- lapply(nb_grp, function(x) {
    col_name <- paste0(nom_grp,"_",x)
    res <- df_one_hot_encode_fonc  %>%
      filter(!!sym(name_expo_fonc) == x) %>% 
      recup_var_table_res(name_expo_fonc) %>% 
      inner_join(table_bool_var,by = "key") %>% 
      mutate(nb_NA = ifelse(nb_NA == 0,"",paste0(" NA : ", nb_NA ))) %>% 
      mutate( {{col_name}} := ifelse( booleen
                                      , paste0(n ,"(",round(pourcent,1), "%)", nb_NA),
                                      paste0(round(med,2) ,"(",
                                             round(quart1,1),";",
                                             round(quart2,1), ")", nb_NA))) %>% 
      select(all_of(col_name))
    return(res)
  }
  )
  
  table_res <- all_cluster_descript_var %>%  
    inner_join(table_bool_var,by = "key") %>% 
    mutate( {{name_all_grp}} := ifelse(booleen
                                       , paste0(n ,"(",round(pourcent,1), "%)",  nb_NA),
                                       paste0(round(med,2) ,"(",
                                              round(quart1,1),";", 
                                              round(quart2,1), ")", nb_NA) ) )  %>% 
    select(key,all_of(name_all_grp)) %>% 
    cbind(bind_cols(group_cluster_descript_var)) %>% data.frame(., row.names = 1)
  
  # si choix de calculer les p-val 
  if (p_val) {
    # prevoir un groupe de plus pour le toutes les catégorie
    nb_group_pval <-  as.character(c(as.numeric(nb_grp),max(as.numeric(nb_grp)) + 1 ))
    # création de toutes les combinaisons de groupe a tester
    combin_grp <- nb_group_pval %>%  combn(2)
    # Création du groupe supplémentaire tout les groupes en dupliquant la dataframe avec un  groupes de plus
    df_pval <- df_one_hot_encode_fonc %>% 
      mutate(!!sym(name_expo_fonc) := max(as.numeric(nb_group_pval))) %>% 
      rbind(df_one_hot_encode_fonc)
    
    
    
    # création  de la table avec p-value pour chaque combin et rename chaque colonnes a_b 
    combin_total_pval <- apply(combin_grp, 2, function(x)
      df_pval %>%
        summarise_at(vars(-(sym(name_expo_fonc) )),list(~t.test(.[!!sym(name_expo_fonc) == x[1]], .[!!sym(name_expo_fonc)  ==  x[2]])$p.value)) %>% 
        t.df %>% 
        rename_at("1",list( ~paste0(x[1],"_",x[2])))
    )
    
    # transformation de la p-value en booléen en avec comme seuil le alpha définis en appliquant une correction de bonneferonni en considérant le nombre total de test nb ligne X nb collones
    result_pval <- bind_cols(combin_total_pval) %>%
      rename(key = key...1) %>% 
      select(key,contains("_")) %>%
      mutate_at(vars(-key),list(~(. < (alpha / ncol(combin_grp))
      ))
      ) %>%  # hypothèse et correction de bonneferonnie 
      mutate_at(vars(-key), function(x) {
        x_var <- rlang::enquo(x)
        ifelse(x , rlang::quo_name(x_var), "non") # remplacement des p-val non signif par une chaine spécifique
      })
    
    # REcherche avec une simili boucle des p-val signif pour chaque colonnnes 
    # on en lève la chaine spécifique de non corrélation 
    df_pval_final <- lapply(nb_group_pval, function(x) {
      result_pval %>% select(key,contains(x)) %>% 
        mutate_at(vars(contains(x)),list(~str_remove_all(.,paste(c("_",x), collapse = "|")))) %>% 
        unite(!!sym(paste0(nom_grp,"_",x)) ,contains(x),sep = ", ")
    }
    ) %>% bind_cols() %>% 
      rename(key = key...1) %>% 
      select(key,contains(nom_grp)) %>% 
      rename(!!sym(name_all_grp) := paste0(nom_grp,"_",max(as.numeric(nb_group_pval)))) %>%   
      mutate_all(list(~str_remove_all(.,"non, |, non"))) %>%  
      mutate_all(list(~str_remove_all(.,"non")))
    
    if(df_pval_final %>%  transmute_at(vars(-key),list(~str_detect(.,"non"))) %>% as.matrix() %>% any) {
      stop("il reste des p-val non traité")}
    
    # Gestion des tables latex pour que les différences statisquement significative soit en subscript
    # en échappant les underscore
    table_res_pval <-  table_res %>% 
      rownames_to_column() %>%
      pivot_longer(-rowname,values_to = "valeur") %>%
      inner_join((df_pval_final %>% pivot_longer(-key,values_to = "pvalue") ), by = c("rowname" = "key", "name" = "name")) %>% 
      mutate(combin = paste0(valeur,"\\textsubscript{",pvalue, "}")) %>% 
      select(rowname,name,combin) %>% 
      pivot_wider(names_from = name, values_from = combin) %>% 
      column_to_rownames()   
    
    table_res_pval <- table_res_pval %>% 
      rownames_to_column() %>% 
      mutate_at(vars(-rowname),list(~str_replace_all(.,"%","\\\\%"))) %>% 
      column_to_rownames() %>% 
      #mutate_all(funs(str_replace_all(.,"%","\\\\%"))) %>% 
      select(all_of(name_all_grp),sort(tidyselect::peek_vars()))
    
    
    rownames(table_res_pval) <- str_replace_all(rownames(table_res_pval),"_","\\\\_")
    colnames(table_res_pval) <- str_replace_all(colnames(table_res_pval),"_","\\\\_")
    
    
  } else {table_res_pval <- table_res %>% 
    select(all_of(name_all_grp),sort(tidyselect::peek_vars()))  }
  
  nb_pat_par_grp <- c(nrow(df_one_hot_encode_fonc),
                      table(df_one_hot_encode_fonc[,name_expo_fonc]))
  res <- rbind(n = nb_pat_par_grp,table_res_pval)
  return(res)
}

#########################################################################################
##########################################################################################
# table descriptive des variables

# df_one_hot_encode_fonc généré avec one_hot_fb
# attention la version p-val true et prevue pour etre print latex ss escape
table_resume_html <- function(df_one_hot_encode_fonc,name_expo_fonc,nom_grp = "clusters", p_val = FALSE, arrondie = TRUE, alpha = 0.05) {
  
  # Typage des variables a réusmer en booleen ou numeric
  # Car normalement df pré_one hot encode
  table_bool_var <-   df_one_hot_encode_fonc %>% 
    select(-all_of(name_expo_fonc)) %>% 
    summarise_all(list(~n_distinct(., na.rm = TRUE))) %>% 
    t.df %>% 
    rename(booleen = `1`) %>% 
    {temp_verif_bool <<- .} %>% 
    mutate(booleen = booleen <= 2)
  
  
  if (any(temp_verif_bool$booleen < 2)) {
    print(temp_verif_bool$key[temp_verif_bool$booleen < 2])
    stop("moins de deux valeurs distinct pour une variables")}
  
  
  
  name_all_grp <- paste0("all_",nom_grp)
  
  
  all_cluster_descript_var <- df_one_hot_encode_fonc %>% 
    recup_var_table_res(name_expo_fonc) %>% 
    #{ifelse(arrondie ,arrondie_df(.), . )} %>% print %>% 
    mutate(nb_NA = ifelse(nb_NA == 0,"",paste0(" NA : ", nb_NA ))) 
  
  
  nb_grp <- df_one_hot_encode_fonc %>% select(all_of(name_expo_fonc)) %>% 
    unique() %>% 
    unlist(use.names = FALSE) %>% 
    sort
  
  
  group_cluster_descript_var <- lapply(nb_grp, function(x) {
    col_name <- paste0(nom_grp,"_",x)
    res <- df_one_hot_encode_fonc  %>%
      filter(!!sym(name_expo_fonc) == x) %>% 
      recup_var_table_res(name_expo_fonc) %>% 
      inner_join(table_bool_var,by = "key") %>% 
      mutate(nb_NA = ifelse(nb_NA == 0,"",paste0(" NA : ", nb_NA ))) %>% 
      mutate( {{col_name}} := ifelse( booleen
                                      , paste0(n ,"(",round(pourcent,1), "%)", nb_NA),
                                      paste0(round(med,2) ,"(",
                                             round(quart1,1),";",
                                             round(quart2,1), ")", nb_NA))) %>% 
      select(all_of(col_name))
    return(res)
  }
  )
  
  table_res <- all_cluster_descript_var %>%  
    inner_join(table_bool_var,by = "key") %>% 
    mutate( {{name_all_grp}} := ifelse(booleen
                                 , paste0(n ,"(",round(pourcent,1), "%)",  nb_NA),
                                 paste0(round(med,2) ,"(",
                                        round(quart1,1),";", 
                                        round(quart2,1), ")", nb_NA) ) )  %>% 
    select(key,all_of(name_all_grp)) %>% 
    cbind(bind_cols(group_cluster_descript_var)) %>% data.frame(., row.names = 1)
  
  # si choix de calculer les p-val 
  if (p_val) {
    # prevoir un groupe de plus pour le toutes les catégorie
    nb_group_pval <-  as.character(c(as.numeric(nb_grp),max(as.numeric(nb_grp)) + 1 ))
    # création de toutes les combinaisons de groupe a tester
    combin_grp <- nb_group_pval %>%  combn(2)
    # Création du groupe supplémentaire tout les groupes en dupliquant la dataframe avec un  groupes de plus
    df_pval <- df_one_hot_encode_fonc %>% 
      mutate(!!sym(name_expo_fonc) := max(as.numeric(nb_group_pval))) %>% 
      rbind(df_one_hot_encode_fonc)
    
    
    
    # création  de la table avec p-value pour chaque combin et rename chaque colonnes a_b 
    combin_total_pval <- apply(combin_grp, 2, function(x)
      df_pval %>%
        summarise_at(vars(-(sym(name_expo_fonc) )),list(~t.test(.[!!sym(name_expo_fonc) == x[1]], .[!!sym(name_expo_fonc)  ==  x[2]])$p.value)) %>% 
        t.df %>% 
        rename_at("1",list( ~paste0(x[1],"_",x[2])))
    )
    
    # transformation de la p-value en booléen en avec comme seuil le alpha définis en appliquant une correction de bonneferonni en considérant le nombre total de test nb ligne X nb collones
    result_pval <- bind_cols(combin_total_pval) %>%
      rename(key = key...1) %>% 
      select(key,contains("_")) %>%
      mutate_at(vars(-key),list(~(. < (alpha / ncol(combin_grp))
      ))
      ) %>%  # hypothèse et correction de bonneferonnie 
      mutate_at(vars(-key), function(x) {
        x_var <- rlang::enquo(x)
        ifelse(x , rlang::quo_name(x_var), "non") # remplacement des p-val non signif par une chaine spécifique
      })

    # REcherche avec une simili boucle des p-val signif pour chaque colonnnes 
    # on en lève la chaine spécifique de non corrélation 
    df_pval_final <- lapply(nb_group_pval, function(x) {
      result_pval %>% select(key,contains(x)) %>% 
        mutate_at(vars(contains(x)),list(~paste0("~",str_remove_all(.,paste(c("_",x), collapse = "|")),"~"))) %>% 
        unite(!!sym(paste0(nom_grp,"_",x)) ,contains(x),sep = "~,~ ")
    }
    ) %>% bind_cols() %>% 
      rename(key = key...1) %>% 
      select(key,contains(nom_grp)) %>% 
      rename(!!sym(name_all_grp) := paste0(nom_grp,"_",max(as.numeric(nb_group_pval)))) %>%   
      mutate_all(list(~str_remove_all(.,"~non~~,~ |~,~ ~non~"))) %>%  
      mutate_all(list(~str_remove_all(.,"~non~")))
    
    if(df_pval_final %>%  transmute_at(vars(-key),list(~str_detect(.,"non"))) %>% as.matrix() %>% any) {
      stop("il reste des p-val non traité")}
    
    # Gestion des tables latex pour que les différences statisquement significative soit en subscript
    # en échappant les underscore
    table_res_pval <-  table_res %>% 
      rownames_to_column() %>%
      pivot_longer(-rowname,values_to = "valeur") %>%
      inner_join((df_pval_final %>% pivot_longer(-key,values_to = "pvalue") ), by = c("rowname" = "key", "name" = "name")) %>% 
      mutate(combin = paste0(valeur,pvalue)) %>% 
      select(rowname,name,combin) %>% 
      pivot_wider(names_from = name, values_from = combin) %>% 
      column_to_rownames()   
    
    table_res_pval <- table_res_pval %>% 
      rownames_to_column() %>% 

      column_to_rownames() %>% 
      select(all_of(name_all_grp),sort(tidyselect::peek_vars()))
    
    
  } else {table_res_pval <- table_res %>% 
    select(all_of(name_all_grp),sort(tidyselect::peek_vars()))  }
  
  nb_pat_par_grp <- c(nrow(df_one_hot_encode_fonc),
                      table(df_one_hot_encode_fonc[,name_expo_fonc]))
  res <- rbind(n = nb_pat_par_grp,table_res_pval)
  return(res)
}

#########################################################################################
function_box_plot_expo_func <- function(donne,expo,out_come){
  `%>%` <- dplyr::`%>%`
  donne <- donne %>% as.data.frame()
  exposure <- donne[,expo] %>% as.factor()
  test_equal_moy <-  donne %>% dplyr::mutate(expo_fact = as.factor(exposure))  %>%
    dplyr::select(-all_of(expo))
  
  
  # Création du box plot simple
  plot_test_res <- test_equal_moy %>%  
    ggplot2::ggplot( ggplot2::aes(x = expo_fact, y = get(out_come), color = expo_fact)) +
    ggplot2::geom_boxplot()  + 
    ggplot2::labs(y = paste0("Outcome final ",
                             out_come),
                  x = paste0("Facteur d'exposition ",
                             expo) )
  
  
  
  if (length(levels(test_equal_moy$expo_fact)) == 2) { # Si deux modalité T test
    moy1 <- test_equal_moy %>% dplyr::filter(expo_fact == levels(expo_fact)[1]) # création var t test
    moy2 <- test_equal_moy %>% dplyr::filter(expo_fact == levels(expo_fact)[2])# création var t test
    res_testt <-  t.test(moy1[,out_come],moy2[,out_come]) # test
    sum_ttest <- summary(res_testt) # récupération résultat 
    
    plot_test_res <- plot_test_res +  
      # ajout de la significativité au box plot
      ggpubr::stat_compare_means(comparisons = list(c(1,2)), tip.length=0.01,
                                 label = "p.signif", 
                                 symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                                    symbols = c("****", "***", "**", "*", "ns")))
    
    # préparation de la liste de sortie de la ffonction
    test_moy <- list(res_test = res_testt,
                     plot_test = plot_test_res,
                     res_annexe = list(summary_ttest = sum_ttest))
    
  } else if (length(levels(test_equal_moy$expo_fact)) > 2) { # si plus de 2 facteurs anova nécessaire
    # Vérifications des hypothèses abandonné kruskalwallis tout le temps la mais a l'avenir reflexion sur ajouter une clause if
    # potentiellement vérif hypo anova clairement pas vérifiables
    
    # Anova test pour produire des résultat annexe notament test de thukey et leven  test
    anova_T_temp <- aov(eval(parse(text =
                                     paste(out_come,"~","expo_fact", # récupération des paramètre de la fonction
                                           sep = "")
    )), data = test_equal_moy)
    
    # Tukey test 
    tukey_anov <- TukeyHSD(anova_T_temp)
    # leven test
    leven_T <- car::leveneTest(eval(parse(text =
                                            paste(out_come,"~","expo_fact", # récupération des paramètre de la fonction
                                                  sep = "")
    )), data = test_equal_moy)
    # kruskal_waliis car pas vérif hypo trop compliqué
    kurskal_T <- kruskal.test(eval(parse(text =
                                           paste(out_come,"~","expo_fact", # récupération des paramètre de la fonction
                                                 sep = "")
    )), data = test_equal_moy)
    sum_anova <- summary(kurskal_T)
    
    
    # Tableau regroupant les moyenne par groupe de facteur d'exposition
    moyenne_par_grp <- test_equal_moy %>%
      dplyr::ungroup() %>%
      dplyr::group_by(expo_fact) %>%
      dplyr::summarise_at(.vars = dplyr::vars(dplyr::all_of(out_come)),  .funs = list(moy = ~mean(.)))
    
    # recherche de tout les comparaison effectuer par le test de tukey
    liste_facteur_compare_tukey <- rownames(tukey_anov$expo_fact) %>% 
      str_extract_all(".+(?=-)|(?<=-).+") %>%
      lapply(., function(x) factor(x, levels = levels(exposure)))
    #   stringr::str_extract_all("[:digit:]+") 
    
    # Boolén visant a ne garder que les les comparaisons des colones au colones adjacente
    # 1 avec 2 Vrai
    #2 avec 5 faux
    bool_facteur_adj <- lapply(liste_facteur_compare_tukey, 
                               function(x) diff(as.numeric(x))) %>% 
      unlist() %>% abs(.) == 1
    
    # Récupération des comparaison vrai
    temp_compare_boxplot  <- liste_facteur_compare_tukey[bool_facteur_adj] %>% lapply(., function(x) as.numeric(x))
    
    plot_test_res <- plot_test_res + 
      ggpubr::stat_compare_means(comparisons = temp_compare_boxplot, tip.length=0.01,
                                 label = "p.signif", 
                                 symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                                    symbols = c("****", "***", "**", "*", "ns")))
    
    test_moy <- list(res_test = kurskal_T, # les résultat de kruskalwallis
                     plot_test = plot_test_res, # les box plot
                     res_annexe = list(summary_anov = sum_anova , # regroupe les éléments liée a l'anova et au test associé 
                                       ano_va_ss_hypo = list(tukey = tukey_anov,
                                                             anova = anova_T_temp,
                                                             leven_test = leven_T),
                                       moy_grp = moyenne_par_grp))
    
  } else print(paste0("pas de test pour facteur avec ",
                      length(unique(test$INS_obs_categ))," modalitée(s)"))
  
  return(test_moy)
}


##########################################################################################
#print vector
print.vecteur <- function(x){
  for (name in x) {
    cat("-", name, '\n')
  }
}
##########################################################################################

# transposé dataframe
t.df <- function(df,pivot=NULL){
  if (is.null(pivot)){
    pivot <- "row_id"
    df <- df %>% mutate(row_id=1:nrow(df))
  }
  res <- df %>% pivot_longer(cols = -!!pivot,"key","value") %>%
    pivot_wider(names_from = !!pivot,values_from = value)
  return(res) 
}




hist_bins <- function(x){
  bw <- 2 * IQR(x) / length(x)^(1/3)
  return(bw)}


##########################################################################################

# compte le nombres de valeur manquante par variables avec les individus groupé par une variables
compte_na_par_var_par_grp <- function(df,group_col,colonnes){
  df_NA_var_fonc <- df %>% select(all_of(group_col),all_of(colonnes)) %>% setDT 
  nb_val_manq_par_var <- df_NA_var_fonc %>% 
    .[, lapply(.SD,  function(x) sum(is.na(x))), group_col] %>% 
    select(-all_of(group_col)) %>% 
    gather(name,presence_na) %>%  # reshape datset
    count(name, presence_na) %>%  # count combinations
    pivot_wider(names_from = name,
                values_from = n,
                values_fill = list(n = 0))
  return(nb_val_manq_par_var)
}

##########################################################################################
# test les hypothèse d'une anova 

model.line.hypo <- function(model_test_hypo){
  #shapi<-shapiro.test(model$residuals)
  shapi <- suppressWarnings(ks.test(x=model_test_hypo$residuals,y='pnorm'))
  bartletI <- bartlett.test(residuals(model_test_hypo)~
                              I(model_test_hypo$model[,2]:
                                  model_test_hypo$model[,ncol(model_test_hypo$model)])
                            )$p.value
  if (shapi$p.value > 0.05){
    
    res1<-paste("On ne peut pas rejeter l'hypothèse H0 de normalité des residus la p-value
              du test de shapiro étant de ", 
                formatC(shapi$p.value , format = "e", digits = 2) ,
                "\nce qui est superieur au seuil alpha 5%")
  } else {
    res1 <- paste("On rejete l'hypothèse H0 de normalité des residus la p-value
                du test de shapiro étant de ",
                  formatC(shapi$p.value , format = "e", digits = 2) ,
                  "\nce qui est inférieur au seuil alpha 5%")
  }
  if (bartletI>0.05){
    bartlet <- c()
    for (i in 1:ncol(m$model[,-1])){
      bartlet <- c(bartlet,bartlett.test(m$model[,1],m$model[,i+1])$p.value)
      
    }
    res2 <- paste(
    "On ne peut pas rejeter l'hypothèse H0 d'homocédasticité des interactions la p-value
                du test de bartlet étant de ",
                  formatC(bartletI , format = "e", digits = 2) ,
                  "\nce qui est superieur au seuil alpha 5%")
  }  else {
    bartlet <- 0
    res2 <- paste(
    "On rejete l'hypothèse H0 d'homocédasticité des interactions la p-value du test
                de bartlet étant de ",
                  formatC(bartletI , format = "e", digits = 2) ,
                  "\nce qui est inférieur au seuil alpha 5%")
  }
  
  if (min(bartlet) > 0.05){res3 <- paste(
  "On ne peut pas rejeter l'hypothèse H0 d'homocédasticité des variances la plus petite
                                     p-value du test de bartlet étant de ", 
                                         formatC(min(bartlet) , format = "e", digits = 2) ,"\nce qui est superieur au seuil alpha 5%")
  }  else {
    res3 <- paste(
    "On   rejete l'hypothèse H0 d'homocédasticité des variances la plus petite
                p-value du test de bartlet étant de ",
                  formatC(min(bartlet) , format = "e", digits = 2) ,
                  "\nce qui est inférieur au seuil alpha 5%")
  }
  if (((shapi$p.value>0.05) == TRUE) & 
      ((bartletI > 0.05) == TRUE) &
      ((min(bartlet) > 0.05) == TRUE)) {
    resF <- "On accepte toutes les hypothèses du test d'annova à plus de 2 facteurs \n"}
  else {
    resF <- "On rejette au moins une hypothèse"
  }
  res <- paste(res1,res2,res3,resF,sep = '\n \n')
  return(cat(res))
}


##########################################################################################
# reset param graphique
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}
#usage
#par(resetPar())
##########################################################################################
# fait des arrondie + notation scientifique pour les nombres a virugules dans les DF en 
# conservant les integer telquel
arrondie_df <- function(df_func){
  res <-   df_func %>%  
    rownames_to_column() %>% 
    mutate_if(is.numeric,
              ~ifelse(.%%1==0,as.character(round(.,0)),ifelse((. > 10^3| 1/abs(.) > 10^3 ),
                      formatC(., format = "e", digits = 1), # tentative de gestion des nombres 
                      as.character(round(.,3))
                      )
              )
    ) %>% 
    column_to_rownames() 
  return(res)
}
##########################################################################################
# convertie les heures  avec virgule en heure et minute
roud_hour <- function(decimal_hour){
  heure <- floor(decimal_hour)
  minutes <- round(60 * (decimal_hour - floor(decimal_hour)), 0)
  res <- sprintf("%02d h %02d min", heure, minutes)
  return(res)
}


#########################################################################################
# liste les  colonnes iodentiques
column_comparator <- function(col_a_compar,df_compar_func){ # recherche les collonnes identique et les regroupe sous la forme de nom de collones supprimé : nom de colonnes restant matchant 
  # Attention une colonnes restante étant égale a 2 coçlonnes supprimé génère donc deux liste
  liste_colum_identique <-   lapply(seq_along(df_compar_func),function(x) { 
    col_a_compar_temp <- df_compar_func[,x] %>% unlist( use.names = FALSE)
    column_compared <- (col_a_compar_temp == col_a_compar ) | 
      (is.na(col_a_compar_temp) & is.na(col_a_compar ))
    matching_col <- c(names(df_compar_func[,x]),names(which(apply(column_compared,2,all))))
  })
  liste_colum_identique  <- lapply(liste_colum_identique, function(x) x[length(x) > 1]) %>% compact()
  return(liste_colum_identique)
}
#########################################################################################
# retire les colonnes iodentiques
distinc_col <- function(df_func,return_list_col_supr = TRUE){
  column_unique_df <- df_func %>% t.df() %>% distinct_at(vars(-key),.keep_all = TRUE)
  df_col_unique <- df_func %>% select(all_of(column_unique_df$key))
  
  if (return_list_col_supr) {
    col_supr <- colnames(df_func)[colnames(df_func) %notin% column_unique_df$key]
    df_col_supr <- df_func %>% select(all_of(col_supr))
    
    
    liste_col_supr <- column_comparator(df_col_supr,df_col_unique)
    
    res <- list(df = df_col_unique, colonne_suprime =  liste_col_supr)
  } else {res <- df_col_unique}
  return(res)
}
#########################################################################################
# include svg en pdf

include_svg = function(path) {
  if (knitr::is_latex_output()) {
    output = xfun::with_ext(path, 'pdf')
    # you can compare the timestamp of pdf against svg to avoid conversion if necessary
    system2('rsvg-convert', c('-f', 'pdf', '-a', '-o', shQuote(c(output, path))))
  } else {
    output = path
  }
  knitr::include_graphics(output)
}