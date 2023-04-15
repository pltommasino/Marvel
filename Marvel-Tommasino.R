library(ggthemes)
library(dplyr)
library(tidyverse)
library(igraph)
library(threejs)

edges_df <- read.csv(file = "data/edges.csv")
hero_df <- read.csv(file = "data/hero-network.csv")

edges_df_su <- edges_df %>%
  select(hero) %>%
  group_by(hero) %>%
  summarize(count=n()) %>%
  arrange(desc(count))
edges_df_su <- as.data.frame(edges_df_su)
edges_df_su[1:20,]
summary(edges_df_su)
#6439 Hero
edges_df_su1 <- as.data.frame(edges_df_su[1:20,])

g <- ggplot(edges_df_su1, mapping = aes(x = reorder(hero, count), count, fill=hero))+
  geom_bar(stat="identity")+
  geom_text(aes(label=count), hjust=1.3, vjust=0.8, color="black", size=3.5)+
  theme_minimal()+
  coord_flip()
g


#Build Social Network graph
######################################### GRAPH ON GEPHI ################################################ 
hero_n <- graph_from_data_frame(hero_df, directed = F)
V(hero_n) #contents in vertices
gorder(hero_n) # Count number of vertices
E(hero_n) #contents in edges
gsize(hero_n)# Count number of edges
#vertex_attr(hero_n)

E(hero_n)$weight <- 1 #give each edge inital weight is one
hero_n <- igraph::simplify(hero_n, remove.loops = TRUE, remove.multiple = TRUE, edge.attr.comb = list(weight="sum", "ignore")) # simplify graph for removing loop
E(hero_n)$weight[1:100] # check weight of the first 100 edges
a <- igraph::as_data_frame(hero_n)
a$from[order(a$weight, decreasing = T)][1]
a$to[order(a$weight, decreasing = T)][1]

is.weighted(hero_n) # check status of graph is weighted or not
is.simple(hero_n) # check status of graph is simplifed or not

summary(hero_n)

# La densità si riferisce alle "connessioni" tra i partecipanti. La densità è definita 
# come il numero di connessioni che un partecipante ha, diviso per il totale delle possibili 
# connessioni che un partecipante potrebbe avere
dens <- edge_density(hero_n)
dens

dist <- mean_distance(hero_n, directed = FALSE)
dist

#Compare with Erdos-Renyi graph
n<-20
gl <- vector('list', n)
for(i in 1:n){
  gl[[i]] <- erdos.renyi.game(n = gorder(hero_n), p.or.m = dens, type = "gnp")
}

gl.apl <- lapply(gl, mean_distance, directed = FALSE)
gl.apl
gl.aplu <- unlist(gl.apl)
gl.aplu

hist(gl.aplu)
abline(v = dist, col = "red", lty = 3, lwd=2)

sum(gl.aplu < dist)/n


# I risultati hanno indicato che alcuni vertici hanno un forte impatto, la maggior parte non lo sono.
hist(graph.strength(hero_n),col="blue",xlab="VertexStrength",ylab="Frequency",main="", breaks = 1000, xlim=c(0, 1000))

## DEGREE
all_deg <- degree(hero_n, mode = c("all"))
#top 5 most popular
all_deg[order(all_deg, decreasing = T)][1:5]
######################################### GRAPH ON GEPHI ################################################ 


## BETWEENNESS
betw <- betweenness(hero_n, directed = F)
#top 5 most popular
betw[order(betw, decreasing = T)][1:5]
######################################### GRAPH ON GEPHI ################################################ 


## EIGEN CENTR.
g.ec <- eigen_centrality(hero_n)
#top 5 most popular
g.ec$vector[order(g.ec$vector, decreasing = T)][1:5]
######################################### GRAPH ON GEPHI ################################################ 


## CLOSENESS
igraph::is.connected(hero_n) #do we have a connected graph?
hero_cc <- components(hero_n,mode = 'strong')
BigComp <- which.max(hero_cc$csize)
Main_heron <- induced_subgraph(hero_n, which(hero_cc$membership == BigComp))
cl.n <- closeness(Main_heron)
#top 5 most popular
cl.n[order(cl.n, decreasing = T)][1:5]
######################################### GRAPH ON GEPHI ################################################ 
  


#### COMMUNITY
net_comm<-walktrap.community(hero_n, steps = 5)
sizes(net_comm)
net_c <- set_vertex_attr(hero_n, "community", value = membership(net_comm))
which.max(sizes(net_comm))

c_f<-as.factor(membership(net_comm))
hero_n_c<- contract.vertices(hero_n, membership(net_comm))
hero_n_c<-simplify(hero_n_c)
lab<-sort(unique(membership(net_comm)))
plot(hero_n_c, vertex.label=lab)

m<-118
sum(vertex_attr(net_c)$community==m)
subam <- subgraph.edges(graph=net_c, eids=which(vertex_attr(net_c)$community==m), delete.vertices = TRUE)
sum(vertex_attr(net_c)$community==m)==gsize(subam) #ok
plot(subam, vertex.label.color = "black",edge.color = 'black',layout=layout.fruchterman.reingold(subam))
  
V(subam)
E(subam)

eroi <- c("IRON MAN IV/JAMES R.", "CAPRICORN/WILLARD WE", "LIBRA/GUSTAV BRANDT",
          "SAGITTARIUS/HARLAN V", "CANCER/JACK KLEVENO", "GEMINI/JOSHUA LINK", 
          "PISCES/NOAH PERRICON", "SCORPIO II", "ARIES II/GROVER RAYM", "VIRGO/ELAINE MCLAUGH")
a <- hero_df[hero_df$hero1 %in% eroi & hero_df$hero2 %in% eroi,]
write.csv(a, file = "comm118.csv", row.names = FALSE)



  
