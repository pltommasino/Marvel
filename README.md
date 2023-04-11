![marvel](https://user-images.githubusercontent.com/123829470/230710560-46774087-32bd-4e75-9abb-f3bfbe0d7e44.png)

# MARVEL SOCIAL NETWORK :superhero_man:

## PREFACE

The goal of this project is to analyze Social Network techniques. Its process is complex and it is performed by several steps, which I will analyze below in this report.

For this project I use `R` (programming language), but also an open-source software for network visualization and analysis: `Gephi`.

The first phase of Social Network Analysis includes collecting and selecting datasets that can be useful for further analysis. In my case, I analyzed and considered from [kaggle.com](https://www.kaggle.com/) dataframes regarding the Marvel universe, all comic books and their respective heroes within them.

Everything presented within this file, can be easily reproduced with the R file, and can be viewable in Gephi (all graphs can be taken in the 'GEPHI Graph' folder).

With this project, I wanted to analyze the network of the heroes, that is, the encounters they had within each individual comic book. I decided to analyze the social network of the Marvel universe, using 2 datasets:

* `edges.csv`: represents the heroes' paths and relationships within the social network, consisting of hero (hero) and comic (comic in which he appears).
* `hero.csv`: represents the encounters (relationships) between heroes specifically, composed of hero1 and hero2.

## INTRODUCTION

I uploaded dataset:

```r
edges_df <- read.csv(file = "~/path/edges.csv")
hero_df <- read.csv(file = "~/path/hero-network.csv")
```

Below I have loaded all the libraries I needed for the project:

```r
library(ggthemes)
library(dplyr)
library(tidyverse)
library(igraph)
library(threejs)
```

I filtered the `edges_df` table, selecting the `hero` variable, grouping it and summing all the times the hero variable had the same value (hero), so as to count the number of times the particular hero appears within all comics. We then turned it into a data frame because it was easier to study. By sorting them in a descending manner previously, we took the top 20 heroes with the most appearances and from that we created a graph.

```r
edges_df_su <- edges_df %>%
  select(hero) %>%
  group_by(hero) %>%
  summarize(count=n()) %>%
  arrange(desc(count))
edges_df_su <- as.data.frame(edges_df_su)

edges_df_su1 <- as.data.frame(edges_df_su[1:20,])
```

I show the graph with the most featured heroes.

```r
g <- ggplot(edges_df_su1, mapping = aes(x = reorder(hero, count), count, fill=hero))+
  geom_bar(stat="identity")+
  geom_text(aes(label=count), hjust=1.3, vjust=0.8, color="black", size=3.5)+
  theme_minimal()+
  coord_flip()
g
```

![ggplot-hero](https://user-images.githubusercontent.com/123829470/230553120-65021569-7e11-407e-b803-d19802403f41.png)

The heroes with the most apparances within the analyzed comic books are Spider-Man and Captain America.


## SOCIAL NETWORK

For the construction of the social network I used the hero_df dataframe. In R (following command) to create a graph from dataframe I used `graph_from_data_frame` function from `igraph` package.

```r
hero_n <- graph_from_data_frame(hero_df, directed = F)
```

In parallel I loaded the dataset into `Gephi`, so that I visualize the graph. I set `"Repulsion Strenght" = 10000.0` in Force Atlas section, because in this way the nodes are spaced out and it make the graph more visualizable.

<img width="760" alt="graph" src="https://user-images.githubusercontent.com/123829470/230563122-09aec61d-5b05-4fc8-a680-4ba12444b7ab.png">

Simultaneously with R I was able to translate the graph into statistics: the graph has 6426 nodes and from 574467 links.

```r
V(hero_n) 
```
```r
## + 6426/6426 vertices, named, from fcea7df:
##    [1] LITTLE, ABNER        BLACK PANTHER/T'CHAL STEELE, SIMON/WOLFGA
##    [4] RAVEN, SABBATH II/EL IRON MAN IV/JAMES R. IRON MAN/TONY STARK 
##    [7] ERWIN, CLYTEMNESTRA  PRINCESS ZANDA       CARNIVORE/COUNT ANDR
##   [10] GHOST                ZIMMER, ABE          FU MANCHU           
##   [13] SHANG-CHI            SMITH, SIR DENIS NAY STARSHINE II/BRANDY 
##   [16] MAN-THING/THEODORE T TARR, BLACK JACK     WU, LEIKO           
##   [19] JACKSON, STEVE       RESTON, CLIVE        ROM, SPACEKNIGHT    
##   [22] DOCTOR DREDD         MYSTIQUE/RAVEN DARKH HYBRID/JAMES JIMMY M
##   [25] DESTINY II/IRENE ADL ROGUE /              AVALANCHE/DOMINIC PE
##   [28] PYRO/ALLERDYCE JOHNN TORPEDO III/BROCK JO CLARK, SARAH        
## + ... omitted several vertices
```
```r
E(hero_n)
```
```r
## + 574467/574467 edges from fcea7df (vertex names):
##  [1] LITTLE, ABNER       --PRINCESS ZANDA      
##  [2] LITTLE, ABNER       --BLACK PANTHER/T'CHAL
##  [3] BLACK PANTHER/T'CHAL--PRINCESS ZANDA      
##  [4] LITTLE, ABNER       --PRINCESS ZANDA      
##  [5] LITTLE, ABNER       --BLACK PANTHER/T'CHAL
##  [6] BLACK PANTHER/T'CHAL--PRINCESS ZANDA      
##  [7] STEELE, SIMON/WOLFGA--FORTUNE, DOMINIC    
##  [8] STEELE, SIMON/WOLFGA--ERWIN, CLYTEMNESTRA 
##  [9] STEELE, SIMON/WOLFGA--IRON MAN/TONY STARK 
## [10] STEELE, SIMON/WOLFGA--IRON MAN IV/JAMES R.
## + ... omitted several edges
```

## EDGES

Analyzing a network where there are many characters in comics, the network can be defined as a `weighted network`, where the weight of each link is very important. Dataframe didn't have a weight of each link, in fact I created it. I gave each relationship between characters (each row of the dataframe) a `weight = 1`. Within the `igraph` package is the `simplify()` function, which helped us significantly in grouping the same type of relationship.

```r
E(hero_n)$weight <- 1
hero_n <- igraph::simplify(hero_n, remove.loops = TRUE, remove.multiple = TRUE, edge.attr.comb = list(weight="sum", "ignore"))
```

I visualized the graph in `Gephi`, using a color scale based on weight links, from blue (less weight) to red (more weight)

![graph-weight](https://user-images.githubusercontent.com/123829470/230579274-552ec91b-f591-4209-99f1-0e2125f46210.png)

In R, I transformed the graph into a dataframe with the `as_data_frame()` function (again within the `igraph` package), I discover that the link weight, and thus the strongest link between two heroes is between **"PATRIOT/JEFF MACE"** and **"MISS AMERICA/MADELIN"** with a weight of **1894**.

```r
a <- igraph::as_data_frame(hero_n)
a$from[order(a$weight, decreasing = T)][1]
```
```r
## [1] "PATRIOT/JEFF MACE"
```

```r
a$to[order(a$weight, decreasing = T)][1]
```
```r
## [1] "MISS AMERICA/MADELIN"
```
```r
a$weight[order(a$weight, decreasing = T)][1]
```
```r
## [1] 1894
```

With some functions in `igraph`, I analyze if graph had identified my weights, and if it had been simplified with the `simplify()` function:

```r
is.weighted(hero_n)
```
```r
## [1] TRUE
```
```r
is.simple(hero_n)
```
```r
## [1] TRUE
```

With other functions, I analyzed and discover density and average distance. Density is defined as the number of link a hero has, divided by the total number of possible link he could have. Instead, the average distance of a graph is the average of the elements of its graph distance matrix.

```r
dens <- edge_density(hero_n)
dens
```
```r
## [1] 0.008099731
```
```r
dist <- mean_distance(hero_n, directed = FALSE)
dist
```
```r
## [1] 2.638427
```

## COMPARISON WITH THE ERDOS-RÉNYI MODEL

In graph theory, the **Erdos-Rényi model** is one of the most reliable models for generating random graphs. This process is used to generate a random distribution as a standard model to be followed to compare results.

```r
n<-20
gl <- vector('list', n)
for(i in 1:n){
  gl[[i]] <- erdos.renyi.game(n = gorder(hero_n), p.or.m = dens, type = "gnp")
}
```

I compare the random graphs with the distribution of the mean distance

```r
gl.apl <- lapply(gl, mean_distance, directed = FALSE)
gl.aplu <- unlist(gl.apl)
hist(gl.aplu)
abline(v = dist, lty = 3, lwd=2)
```

![hist](https://user-images.githubusercontent.com/123829470/230641802-dcbcc63f-2c97-45ba-8a34-3691c232d2dc.png)

This part calculates whether the average distance of random graphs is shorter than observation or not. If most of the average distances generated by random graphs are greater than observed, it means that the observed result is shorter than normal; otherwise, it is greater than normal.

```r
sum(gl.aplu < dist)/n
```
```r
## [1] 0
```

From this result it can understand that the result I observed is shorter than normal.

## NODES

When I finished the study with links, let me move on to the study of nodes. Initially, I made use of a graph, to see how many relevant nodes there were. In fact, some of them had a strong impact, most did not.

```r
hist(graph.strength(hero_n),col="blue",xlab="VertexStrength",ylab="Frequency",main="", breaks = 1000, xlim=c(0, 1000))
```

![hist-2](https://user-images.githubusercontent.com/123829470/230643122-8822d6d3-a200-4dd0-a404-b345169d89f0.png)

After having made this "general" analysis, let me go into more detail. In fact, nodes can be considered relevant through many aspects: by `degree`, `betweenness`, `eigen centrality` and `closeness`. In fact, I analyzed with `R` the different values and made a ranking of the top 5 most important nodes (heroes) and I realized with `Gephi`, the different graphs with the various features.

## DEGREE

**Degree** (degree centrality) is the simplest measure of centrality to calculate. A node's degree is simply a count of how many social connections (links) it has. Degree centrality for a node is simply its degree. A node with 10 social connections would have a degree centrality of 10. A node with 1 arc would have a degree centrality of 1.

![graph-degree](https://user-images.githubusercontent.com/123829470/230664644-b59aa4b1-674a-4a33-b943-e1bbeeb16cef.png)

In this case is relevant **Captain America** with a degree of **1906**

```r
all_deg <- degree(hero_n, mode = c("all"))
all_deg[order(all_deg, decreasing = T)][1:5]
```
```r
##      CAPTAIN AMERICA SPIDER-MAN/PETER PAR IRON MAN/TONY STARK  
##                 1906                 1737                 1522 
## THING/BENJAMIN J. GR MR. FANTASTIC/REED R 
##                 1416                 1379
```

## BETWEENNESS

Betweenness is a way to detect the amount of influence a node has on the flow of information in a graph (it is a measure of a node's influence in a network).

![graph-bet](https://user-images.githubusercontent.com/123829470/230666198-12002317-0a5e-4c8c-8e2c-ccf958a1389f.png)

```r
betw <- betweenness(hero_n, directed = F)
betw[order(betw, decreasing = T)][1:5]
```
```r
## SPIDER-MAN/PETER PAR      CAPTAIN AMERICA IRON MAN/TONY STARK  
##             768886.0             639944.8             530577.5 
##  HAVOK/ALEX SUMMERS      WOLVERINE/LOGAN  
##             517604.5             481495.9
```

Once again, **Spider Man** was relevant in our case with a betweenness of **768886.0**

## EIGEN CENTRALITY

Eigen centrality is a measure of the influence of a node in a network. Relative scores are assigned to all nodes in the network based on the concept that connections to high-scoring nodes contribute more to that node's score than equal connections to low-scoring nodes. A high eigenvector score means that a node is connected to many n.

![graph-eig](https://user-images.githubusercontent.com/123829470/230666632-d66539e6-7319-451e-9621-3a5ae0501b26.png)

```r
g.ec <- eigen_centrality(hero_n)
g.ec$vector[order(g.ec$vector, decreasing = T)][1:5]
```
```r
##      CAPTAIN AMERICA THING/BENJAMIN J. GR HUMAN TORCH/JOHNNY S 
##            1.0000000            0.8527414            0.8378977 
## MR. FANTASTIC/REED R IRON MAN/TONY STARK  
##            0.8232495 
```

In this case **Captain America** ranks first in order of importance with a maximum score of **1.0000000**

## CLOSENESS

Closeness assigns a score to each node based on their "closeness" to all other nodes in the network. This measure calculates the shortest paths among all nodes, then assigns each node a score based on the sum of the shortest paths and is used to find the nodes that are in the best position to affect the entire network most quickly.

![graph-clo](https://user-images.githubusercontent.com/123829470/230707506-b8bb496d-8ddf-40c0-b7fc-9ae742e38214.png)

In this case, unexpectedly, after several first places by Captain America and Spider Man in other measures, in the closeness ranks first **Living Mummy** with a score of **6.334727e-05**

```r
igraph::is.connected(hero_n)
```
```r
## [1] FALSE
```
```r
hero_cc <- components(hero_n,mode = 'strong')
BigComp <- which.max(hero_cc$csize)
Main_heron <- induced_subgraph(hero_n, which(hero_cc$membership == BigComp))
cl.n <- closeness(Main_heron)
cl.n[order(cl.n, decreasing = T)][1:5]c
```
```r
##         LIVING MUMMY IRON MAN/TONY STARK  AJAK/TECUMOTZIN [ETE 
##         6.334727e-05         6.321512e-05         6.303978e-05 
## HUMAN TORCH/JOHNNY S THING/BENJAMIN J. GR 
##         6.290891e-05         6.279435e-05
```

## COMMUNITY

To conclude the social network analysis, I decided to search within it whether there were any "communities", whether any node was more closely connected with one rather than another.

In *R* I used the `walktrap.community()` function. It is an approach based on random walks. The general idea is that if random walks are performed on the graph, the walks are more likely to stay within the same community because there are only a few edges leading outside a given community. `walktrap.community()` performs short random walks of 3-4-5 steps (depending on the parameters set) and uses the results of these random walks to join separate communities in a bottom-up approach. It is somewhat slower than the `fastgreedy.community()` approach but more accurate than the latter (according to the original publication).

The condition for which several communities were created is 5. With ```r which.max(sizes(net_comm)``` I know the largest community that has been created within network.

```r
net_comm<-walktrap.community(hero_n, steps = 5)
sizes(net_comm)
```
```r
## Community sizes
##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16 
##  976 2141   51   90  111  118    9    4  141   14   23   28    9    4  535   11 
##   17   18   19   20   21   22   23   24   25   26   27   28   29   30   31   32 
##  219  119   11   50   44   53  113  191   17   42   18    9    2   16  130   28 
##   33   34   35   36   37   38   39   40   41   42   43   44   45   46   47   48 
##   13   20   13   51    4    9    5    6    4   20    9   10    5   28    6   24 
##   49   50   51   52   53   54   55   56   57   58   59   60   61   62   63   64 
##    2   20   19    3   15   10    5   15   37    2   25   13    5    4    5    7 
##   65   66   67   68   69   70   71   72   73   74   75   76   77   78   79   80 
##   17    4    5    5    8   14    5    3    2   10    3    3   23    5    8    5 
##   81   82   83   84   85   86   87   88   89   90   91   92   93   94   95   96 
##    3    4    5    8    5   13   10    7    5    6    5    3    3   22    5    4 
##   97   98   99  100  101  102  103  104  105  106  107  108  109  110  111  112 
##    8    9   10   13    7    5    4    5    5    2    7    5    4    3    8    4 
##  113  114  115  116  117  118  119  120  121  122  123  124  125  126  127  128 
##    5    9    7    8    3    9    3    4    2    4    2    2    2    2    3    3 
##  129  130  131  132  133  134  135  136  137  138  139  140  141  142  143  144 
##    6    3    4    2    8    4    4    3    6    3    4    6    4    5    3    3 
##  145  146  147  148  149  150  151  152  153  154  155  156  157  158  159  160 
##    5    4    2   10    6    2    7    3    4    2    5    3    3    2    5    3 
##  161  162  163  164  165  166  167  168  169  170  171  172  173  174  175  176 
##    4    3    5    4    7    4    8    4    7    8    9    6    4    6    3    2 
##  177  178  179  180  181  182  183  184  185  186  187  188  189  190  191  192 
##    2    2    2    2    2    2    2    1    1    1    1    1    1    1    1    1 
##  193  194  195  196  197  198  199  200  201  202  203  204  205  206  207  208 
##    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1 
##  209  210  211  212  213  214  215  216  217  218  219  220  221  222  223  224 
##    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1 
##  225  226  227  228  229  230  231  232  233  234  235  236  237  238  239  240 
##    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1 
##  241 
##    1
```
```r
net_c <- set_vertex_attr(hero_n, "community", value = membership(net_comm))
which.max(sizes(net_comm))
```
```r
## 2 
## 2
```

I then visualized it, also in `Gephi`:

![graph-community](https://user-images.githubusercontent.com/123829470/230708520-cf7529c8-cf2c-4dde-bcda-dab5984ac2a1.png)

With these commands, I then built a "community graph", meaning that the nodes that appeared in the plot corresponded to a particular community, represented with a number (as we can see below).

```r
c_f<-as.factor(membership(net_comm))
hero_n_c<- contract.vertices(hero_n, membership(net_comm))
hero_n_c<-simplify(hero_n_c)
lab<-sort(unique(membership(net_comm)))
plot(hero_n_c, vertex.label=lab)
```

![r-community](https://user-images.githubusercontent.com/123829470/230708747-28efe14c-5866-4a51-89c9-1ee53a3b7669.png)

## A PARTICULAR COMMUNITY: 118

From different communities, I decided to analyze community number 118, that is to see within it how many nodes (heroes) there were, and how many links (relation) there were between them.

```r
m<-118
sum(vertex_attr(net_c)$community==m)
```
```r
## [1] 9
```
```r
subam <- subgraph.edges(graph=net_c, eids=which(vertex_attr(net_c)$community==m), delete.vertices = TRUE)
sum(vertex_attr(net_c)$community==m)==gsize(subam)
```
```r
## [1] TRUE
```
```r
plot(subam, vertex.label.color = "black",edge.color = 'black', layout= layout.fruchterman.reingold(subam))
```

![community-118](https://user-images.githubusercontent.com/123829470/230709106-9542419f-39c2-4751-a80b-84028e2187cb.png)

```r
V(subam)
```
```r
## + 10/10 vertices, named, from 267ba20:
##  [1] IRON MAN IV/JAMES R. CAPRICORN/WILLARD WE LIBRA/GUSTAV BRANDT 
##  [4] SAGITTARIUS/HARLAN V CANCER/JACK KLEVENO  GEMINI/JOSHUA LINK  
##  [7] PISCES/NOAH PERRICON SCORPIO II           ARIES II/GROVER RAYM
## [10] VIRGO/ELAINE MCLAUGH
```

```r
E(subam)
```
```r
## + 9/9 edges from 267ba20 (vertex names):
## [1] IRON MAN IV/JAMES R.--CAPRICORN/WILLARD WE
## [2] IRON MAN IV/JAMES R.--LIBRA/GUSTAV BRANDT 
## [3] IRON MAN IV/JAMES R.--SAGITTARIUS/HARLAN V
## [4] IRON MAN IV/JAMES R.--CANCER/JACK KLEVENO 
## [5] IRON MAN IV/JAMES R.--GEMINI/JOSHUA LINK  
## [6] IRON MAN IV/JAMES R.--PISCES/NOAH PERRICON
## [7] IRON MAN IV/JAMES R.--SCORPIO II          
## [8] IRON MAN IV/JAMES R.--ARIES II/GROVER RAYM
## [9] IRON MAN IV/JAMES R.--VIRGO/ELAINE MCLAUGH
```

In order to have a better visualization of the graph, I transformed the graph into a dataframe, and the latter into a .csv file, and then I could also analyze this community on the `Gephi` viewer.

```r
eroi <- c("IRON MAN IV/JAMES R.", "CAPRICORN/WILLARD WE", "LIBRA/GUSTAV BRANDT",
          "SAGITTARIUS/HARLAN V", "CANCER/JACK KLEVENO", "GEMINI/JOSHUA LINK", 
          "PISCES/NOAH PERRICON", "SCORPIO II", "ARIES II/GROVER RAYM", "VIRGO/ELAINE MCLAUGH")
a <- hero_df[hero_df$hero1 %in% eroi & hero_df$hero2 %in% eroi,]
write.csv(a, file = "comm118.csv", row.names = FALSE)
```

<img width="717" alt="subcomm-118" src="https://user-images.githubusercontent.com/123829470/230709417-8d8f33b9-8556-40c9-90ec-37378d98026a.png">
