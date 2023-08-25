## Final Script 

rm(list = ls())

library(httr)
library(rvest)
library(stringr)
library(igraph)

get_links <- function (target = "/wiki/Russian_invasion_of_Ukraine",
                       html_base = "https://en.wikipedia.org")
{
  html_page <- read_html(paste0(html_base,target)) # lis le texte html de la page
  a_elements <- html_elements(html_page, "a") # sélectionne les éléments liens (balise "a" en html)
  href <- as.vector(html_attr(a_elements, name = "href")) # sélectionne les liens contenus dans les balises "a"
  wikipage <- str_detect(href,"/wiki/") # sélectionne les liens internes à Wikipedia sous forme d'une liste de boléens
  
  list_of_wikipages1 <- href[wikipage=="TRUE"] # sélectionne la liste des liens Wikipedia
  
  df2 <- cbind.data.frame(list_of_wikipages1,
                          str_detect(list_of_wikipages1, ".jpg"),
                          str_detect(list_of_wikipages1, "http"),
                          str_detect(list_of_wikipages1, ":"),
                          str_detect(list_of_wikipages1, "//"))
  
  final_wikipages_link <- df2[(df2[[2]] == FALSE &
                                 df2[[3]] == FALSE & 
                                 df2[[4]] == FALSE &
                                 df2[[5]] == FALSE), 1]
  
  return (final_wikipages_link)
}

get_el_from_target <- function (target = "/wiki/Russian_invasion_of_Ukraine", 
                                language = "english",
                                depth = 2)
{
  # Language :
  if (language == "english" | language == "en") {html_base <- "https://en.wikipedia.org" }
  else if (language == "français" | language == "fr") {html_base <- "https://fr.wikipedia.org" }
  else if (language == "russe" | language == "ru") {html_base <- "https://ru.wikipedia.org" }
  else if (language == "ukrainien" | language == "uk") {html_base <- "https://uk.wikipedia.org" }
  else if (language == "deutsch" | language == "de") {html_base <- "https://de.wikipedia.org" }
  else {print("Error : Language not recognised.")}
  
  # Depth level :
  if (depth == 1)
  {
    # Obtention des liens
    depth1 <- get_links(target, html_base = html_base)
    
    # Création de l'edge-list
    StartingPointsDepth1 <- c(rep(target,length(depth1)))
    
    # Finalisation de l'edge-list
    el1 <- data.frame(From = StartingPointsDepth1, To = depth1)
    saveRDS(el1,paste0("/Users/pgcd/Documents/FU/S8/Bachelorarbeit/Labo/Data/el1",language,".RDS"))
    write.csv2(el1, file = paste0("/Users/pgcd/Documents/FU/S8/Bachelorarbeit/Labo/Data/el1",language,".csv"))
    saveRDS(el1,paste0("/Users/pgcd/Documents/FU/S8/Bachelorarbeit/Labo/Data/el1",language,".RDS"))
    return (el1)
  }
  if (depth == 2) # Approximatively 10 minutes for depth2 with the default target in english. 
  {
    # Obtention des liens
    depth1 <- get_links(target, html_base = html_base)
    depth2 <- list()
    for (i in (1:length(depth1)))
    {
      if(!is.na(depth1[i]))
      {
        depth2[i] <- list(get_links(depth1[i], html_base = html_base))
      }
      else
      {
        depth2[i] <- NA
      }
    }
    # Warning message: In read_xml.raw(raw, encoding = encoding, base_url = base_url, as_html = as_html, : Tag figcaption invalid [801]
    
    # Création de l'edge-list
    StartingPointsDepth1 <- c(rep(target,length(depth1)))
    StartingPointsDepth2 <- list()
    for (i in (1:length(depth1)))
    {
      StartingPointsDepth2[i] <- list(rep(depth1[i],length(depth2[[i]])))
    }
    StartingPointsDepth2 <- unlist(StartingPointsDepth2)
    EndingPointsDepth2 <- unlist(depth2)
    
    # Finalisation de l'edge-list
    el1 <- data.frame(From = StartingPointsDepth1, To = depth1)
    write.csv2(el1, file = paste0("/Users/pgcd/Documents/FU/S8/Bachelorarbeit/Labo/Data/el1",language,".csv"))
    saveRDS(el1,paste0("/Users/pgcd/Documents/FU/S8/Bachelorarbeit/Labo/Data/el1",language,".RDS"))
    el2 <- data.frame(From = StartingPointsDepth2, To = EndingPointsDepth2)
    el2 <- rbind.data.frame(el1,el2)
    write.csv2(el2, file = paste0("/Users/pgcd/Documents/FU/S8/Bachelorarbeit/Labo/Data/el2",language,".csv"))
    saveRDS(el2,paste0("/Users/pgcd/Documents/FU/S8/Bachelorarbeit/Labo/Data/el2",language,".RDS"))
    
    # Finalisation des résultats
    result <- list(el1 = el1,el2 = el2)
    saveRDS(result,paste0("/Users/pgcd/Documents/FU/S8/Bachelorarbeit/Labo/Data/result2",language,".RDS"))
    return(result)
  }
  else
  {
    print("Error : Depth level not defined.")
  }
  
}

quant_analysis <- function(el)
{
  # Clean the edge list : 
  el2 <- na.omit(el)
  
  # Measure the things
  g <- graph_from_data_frame(el2)
  order <- vcount(g)
  edges <- ecount(g)
  density <- edge_density(g, loops = TRUE)
  
  deg <- degree(g, mode = "all")
  largest_nodes <- sort(deg, decreasing = T)[1:10]
  largest_nodes <- data.frame(names(largest_nodes), as.vector(largest_nodes))
  colnames(largest_nodes) <- c("Pages", "Connections")
  
  # Store the data
  statistical_measures <- list(order = order,
                               edges = edges,
                               density = density,
                               largest_nodes = largest_nodes)
  saveRDS(statistical_measures, paste0("/Users/pgcd/Documents/FU/S8/Bachelorarbeit/Labo/Data/",deparse(substitute(el)),"stats.RDS"))
  return (statistical_measures)
}

# Get the data for the first time : 
{
  el_en <- get_el_from_target(target = "/wiki/Russian_invasion_of_Ukraine", language = "en")
  el_fr <- get_el_from_target(target = "/wiki/Invasion_de_l'Ukraine_par_la_Russie", language = "fr")
  el_ru <- get_el_from_target(target = "/wiki/Вторжение_России_на_Украину_(с_2022)", language = "ru")
  el_de <- get_el_from_target(target = "/wiki/Russischer_Überfall_auf_die_Ukraine_seit_2022", language = "de")
  el_uk <- get_el_from_target(target = "/wiki/Російське_вторгнення_в_Україну_(з_2022)", language = "uk")
}

# Load the data : 
{
  el_en <- readRDS(file = "/Users/pgcd/Documents/FU/S8/Bachelorarbeit/Labo/Data/result2en.RDS")
  el_fr <- readRDS(file = "/Users/pgcd/Documents/FU/S8/Bachelorarbeit/Labo/Data/result2fr.RDS")
  el_de <- readRDS(file = "/Users/pgcd/Documents/FU/S8/Bachelorarbeit/Labo/Data/result2de.RDS")
  el_ru <- readRDS(file = "/Users/pgcd/Documents/FU/S8/Bachelorarbeit/Labo/Data/result2ru.RDS")
  el_uk <- readRDS(file = "/Users/pgcd/Documents/FU/S8/Bachelorarbeit/Labo/Data/result2uk.RDS")
}

# Procede to the quantitative analysis :
{
  g1_en <- quant_analysis(el_en$el1)
  g2_en <- quant_analysis(el_en$el2)
  g1_de <- quant_analysis(el_de$el1)
  g2_de <- quant_analysis(el_de$el2)
  g1_fr <- quant_analysis(el_fr$el1)
  g2_fr <- quant_analysis(el_fr$el2)
  g1_ru <- quant_analysis(el_ru$el1)
  g2_ru <- quant_analysis(el_ru$el2)
  g1_uk <- quant_analysis(el_uk$el1)
  g2_uk <- quant_analysis(el_uk$el2)
  g_stats <- c(g1_en,g2_en,
               g1_de,g2_de,
               g1_fr,g2_fr,
               g1_ru,g2_ru,
               g1_uk,g2_uk)
}

