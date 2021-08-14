library(tidyverse)
library(tidytext)

library(ape)
library(ggtree)


####### Data are not fully available! 
# Most of the texts used in the experiment were taken from the Russian National Corpus and can be accessed by individuals only after contacting the Corpus directly. Female poets' texts (except for Anna Bunina's poems) digitized by me are available in the folder "fem_corpus". 
# The present code shows the experiment settings; the variable "fem_corpus" was a tibble with two columns: author names as 'id-s' and lemmatized texts merged together as 'texts' (one row = one author's corpus)

# After sampling, all the resulting matrices with scaled word frequencies were stored in the variable "matrices" (list) and available as "Smirnova_matrices.Rda".
# The resulting list of phylo lists is stored as the "Smirnova_list_of_trees.Rda"; it was used for creating the plot Smirnova_Plot1.png (see below)


####### Experiment settings #######

n_iterations <- 100
list_of_trees <- vector(length=n_iterations, mode="list")
matrices <- list()

for(i in 1:n_iterations) {
  
  message(paste0("Iteration -- ", i))
  
  # random sampling; 8000 words were chosen from each author's corpus
  sampled <- fem_corpus %>% # fem_corpus is the variable containing the data: id = author's name, texts = all texts by the author merged together as a character vector
    unnest_tokens(input = texts, output = word, token = "words") %>% 
    group_by(id) %>% 
    sample_n(8000) %>% 
    ungroup()
  
  # counting word frequencies
  counted <- sampled %>% 
    count(word, sort = T)
  
  # long table to matrix
  wide <- sampled  %>% 
    group_by(id)  %>% 
    count(word) %>%
    mutate(n=n/sum(n)) %>% 
    spread(key=word, value=n, fill = 0)
  
  names <- wide$id
  matrix <- wide[,-1]
  
  # normalization
  mat_fin <- matrix[counted$word] %>% as.matrix() %>% scale()
  rownames(mat_fin) <- names
  
  # counting distances for 250 MFW starting from the 50th
  mat_fin <- mat_fin[,50:299]
  
  # store matrix
  matrices[[i]] <- mat_fin
  
  # calculating distances
  delta <- dist(mat_fin, method = "manhattan")/250
  
  # clusterisation 
  list_of_trees[[i]] <- delta %>% 
    hclust(method = "ward.D2") %>% 
    as.phylo()
  
}

save(matrices, file = "final/Smirnova_matrices.Rda")
save(list_of_trees, file = "final/Smirnova_list_of_trees.Rda")

####### Plot 1: creating the plot from prepared phylo data #######

# load prepared data (the list of phylo lists)
load("final/Smirnova_list_of_trees.Rda")

# creating the consensus tree with 50% agreement
consensus = ape::consensus(list_of_trees, p=0.5)

# making the plot
plot=ggtree(consensus,layout="circular",size=0.5) 
plot + geom_tiplab(aes(color=label),hjust=-.1,size=7.5) + guides(color=F) + 
  theme(plot.margin = unit(c(4,4,4,4), "cm"))

ggsave("final/Smirnova_Plot1.png", plot = last_plot(), width = 15, height = 15, dpi = 300)  
