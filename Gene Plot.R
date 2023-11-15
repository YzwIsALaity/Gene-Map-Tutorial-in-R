library(ggplot2)
library(gggenes)
library(gridExtra)

Dt <- read.csv('Synthetic Gene 1.csv')

SingleGenome <- Dt[which(Dt$Molecule == 'Rosa LSL-Myc'), ]
# Version 1: default
p1 <- 
  ggplot(SingleGenome, 
         aes(xmin = Start, xmax = End,      # Specify start/end location for a gene
             y = Molecule,                  # Specify genome
             fill = Gene)) +                # Specify color for genes
  geom_gene_arrow() +                     # Draw gene map as arrows
  scale_fill_brewer(palette = "Set3") +   # Specify color palette
  theme_genes() +                         # Specify theme for gene map
  theme(axis.text.x = element_blank(),    # Remove x axis 
        axis.ticks.x = element_blank(),   
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank(),   # Remove title in y axis
        axis.text.y = element_text(face = 'bold', color = 'black'),
        legend.title = element_text(face = 'bold', color = 'black'))
p1 

# Version 2: modify the shape of arrowhead
p2 <- 
  ggplot(SingleGenome, 
         aes(xmin = Start, xmax = End,      # Specify start/end location for a gene
             y = Molecule,                  # Specify genome
             fill = Gene)) +                # Specify color for genes
  geom_gene_arrow(arrowhead_height = unit(3, "mm"),    # Modify height and width
                  arrowhead_width = unit(1, "mm")) +                     
  scale_fill_brewer(palette = "Set3") +   # Specify color palette
  theme_genes() +                         # Specify theme for gene map
  theme(axis.text.x = element_blank(),    # Remove x axis 
        axis.ticks.x = element_blank(),   
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank(),   # Remove title in y axis
        axis.text.y = element_text(face = 'bold', color = 'black'),
        legend.title = element_text(face = 'bold', color = 'black'))
p2 

# Version 3: add label to genes and remove legend
p3 <- 
  ggplot(SingleGenome, 
         aes(xmin = Start, xmax = End,      # Specify start/end location for a gene
             y = Molecule,                  # Specify genome
             fill = Gene,                   # Specify color for genes
             label = Gene)) +                
  geom_gene_arrow(arrowhead_height = unit(3, "mm"),    # Modify height and width
                  arrowhead_width = unit(1, "mm")) +        
  geom_gene_label() +                     # Specify labels
  scale_fill_brewer(palette = "Set3") +   # Specify color palette
  theme_genes() +                         # Specify theme for gene map
  theme(axis.text.x = element_blank(),    # Remove x axis 
        axis.ticks.x = element_blank(),   
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank(),   # Remove title in y axis
        axis.text.y = element_text(face = 'bold', color = 'black'),
        legend.position = 'none')                        
p3

# Show figure
grid.arrange(p1, p2, p3, ncol = 1)

p4 <- 
  ggplot(Dt, 
         aes(xmin = Start, xmax = End,      # Specify start/end location for a gene
             y = Molecule,                  # Specify genome
             fill = Gene,                   # Specify color for genes
             label = Gene)) +                
  geom_gene_arrow(arrowhead_height = unit(3, "mm"),    # Modify height and width
                  arrowhead_width = unit(1, "mm")) +        
  geom_gene_label() +                     # Specify labels
  facet_wrap(~ Molecule, scales = "free", ncol = 1) + 
  scale_fill_brewer(palette = "Set3") +   # Specify color palette
  theme_genes() +                         # Specify theme for gene map
  theme(axis.text.x = element_blank(),    # Remove x axis 
        axis.ticks.x = element_blank(),   
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank(),   # Remove title in y axis
        axis.text.y = element_text(face = 'bold', color = 'black'),
        legend.position = 'none')                        
p4

dummies <- make_alignment_dummies(example_genes, 
                                  aes(xmin = start, xmax = end,y = molecule,id = gene), 
                                  on = "genE")

p5 <- 
  ggplot(example_genes, 
         aes(xmin = start, xmax = end,      # Specify start/end location for a gene
             y = molecule,                  # Specify genome
             fill = gene))+                 # Specify color for genes
  geom_gene_arrow(aes(forward = orientation),
                  arrowhead_height = unit(3, "mm"),    # Modify height and width
                  arrowhead_width = unit(1, "mm")) +        
  geom_blank(data = dummies) + 
  facet_wrap(~ molecule, scales = "free", ncol = 1) + 
  scale_fill_brewer(palette = "Set3") +   # Specify color palette
  theme_genes() +                         # Specify theme for gene map
  theme(axis.text.x = element_blank(),    # Remove x axis 
        axis.ticks.x = element_blank(),   
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank(),   # Remove title in y axis
        axis.text.y = element_text(face = 'bold', color = 'black'))                        
p5


