
# Retorna a Covariate_plot d'un objecte matchit()  -------------------------
# Llances un objecte m-out, variables que vols eliminar i si vols etiquetar segons conductor
covariate_plot<-function(dt=m.out,vars_remove=NULL, etiquetar=F,subtitle="oGLD vs SGLT-2i group",...) {
  
  # vars_remove<-c("age", "sexe","tempsdm_cat4", "iyearsem","qmedea")
  # m.out,vars_remove = c("qmedea","age"),etiquetar = T
  # dt=m.out
  # vars_remove=NULL
  # etiquetar = F
  # taulavariables=conductor_variables
  
  # Preparar dades a plotejar
  dt_pre<-summary(dt,standardize = T)$sum.all %>% tibble::as_tibble(rownames = "var") %>% mutate(Sample="Unmatched",id=dplyr::row_number())
  dt_post<-summary(dt,standardize = T)$sum.matched %>% tibble::as_tibble(rownames = "var") %>% mutate(Sample="Matched",id=dplyr::row_number())
  #
  # Preparar i ordenar per id
  dt_total<- 
    dt_pre %>% dplyr::bind_rows(dt_post) %>% 
    mutate (stat=`Std. Mean Diff.`) %>% 
    dplyr::filter(var!="distance") %>% 
    dplyr::filter(!is.na(stat)) %>% 
    mutate(var=factor(var,levels=rev(dt_pre$var)))   # Convertir a factor per que surti ordenat
  
  # He generar variables+nivells indexat
  llista_vars<-names(dt$X)
  
  # les variables exact s'han de factoritzar
  dt$X<-dt$X %>% mutate_at(all.vars(dt$exact),as.factor)
  
  vars_df<-
    llista_vars %>% set_names(llista_vars) %>%
    purrr::map(~levels(dt$X[[.x]])) %>%
    tibble::enframe() %>%
    tidyr::unnest(cols = c(value))
  
  vars_df<-
    tibble::as_tibble(llista_vars) %>% dplyr::select(name=value) %>% 
    dplyr::left_join(vars_df,by="name") %>% 
    dplyr::mutate(
      value=ifelse(is.na(value) | value=="NA","",value),
      var=paste0(name,value))
  
  # Juntar noms de variables + levels
  dt_total<-dt_total %>% dplyr::left_join(vars_df,by="var")
  
  # Eliminar vars a eliminar
  dt_total<-dt_total %>% dplyr::filter(!name%in%vars_remove)
  
  # Etiquetar variables
  if (etiquetar) dt_total<-dt_total %>% etiquetar_taula(camp = "name", ...)
  
  # Afegir nivells exepte Yes / Si i eliminar cat No
  dt_total<-
    dt_total %>% 
    mutate(name=if_else(value=="" | value=="Yes" | value=="Si",
                        name,paste0(name,":",value))) %>% 
    filter(value!="No") %>% 
    filter(value!="0")
  
  # Preque mantingui l'ordre  
  dt_total$name<- factor(dt_total$name, levels=rev(unique(dt_total$name)),ordered=T)
  
  ggplot2::ggplot(aes(y = name, x = stat, group = Sample), data = dt_total) + 
    ggplot2::theme(panel.background = element_rect(fill = "white"),
                   axis.text.x = element_text(color = "black"),
                   axis.text.y = element_text(color = "black"),
                   panel.border = element_rect(fill = NA, color = "black"),
                   plot.background = element_blank(),
                   legend.background = element_blank(),
                   legend.key = element_blank()) + 
    geom_point(aes(colour=Sample),size=3) +
    
    ggplot2::geom_vline(xintercept = c(-0.1,0,0.1) , linetype = 2, color = "gray8")+
    ggplot2::theme(legend.position = "top")+
    ggplot2::labs(y = NULL, x = "Standardized mean difference",
                  title=paste0("Covariate plot \n ", subtitle))+
    theme(plot.title = element_text(hjust = 0.5))
  
  
}
