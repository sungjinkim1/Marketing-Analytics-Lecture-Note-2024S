library(readr)
seg <- read_csv("G:/My Drive/Teaching/Marketing Analytics/Bloomsbury material/Chapter Examples/Chapter 3/retail_segmentation.csv")

seg_base_var = 
seg %>% select(avg_order_size,
               avg_order_freq,crossbuy,multichannel,per_sale,tenure) #%>% scale()
a = dist(seg_base_var)

scale(seg_base_var)
help(scale)
help(dist)
?hclust

seg_hclust1 = hclust(dist(seg_base_var))
seg_hclust = hclust(dist(scale(seg_base_var)),method = "complete")
seg_base_var
seg_hclust$merge
a = seg_hclust$height
a
seg_hclust1
x_axis = c(1:10)
y_axis = sort(seg_hclust$height,decreasing = T)[1:10]
y_axis

dat = data.frame(x_axis, y_axis)
glimpse(dat)

ggplot(data = dat, aes(x_axis,y_axis)) +
  
  geom_line()

seg_kmeans = kmeans(x = seg_base_var,centers = 6)
segment = seg_kmeans$cluster
seg_results = bind_cols(seg,segment = segment)
glimpse(seg_results)
fviz_nbclust()
library(cluster)
library(factoextra)
gap_stat <- clusGap(seg_base_var, 
                    FUN = kmeans,
                    K.max = 10,
                    B = 50)
fviz_gap_stat(gap_stat)
fviz_nbclust(seg_base_var, kmeans, method = "silhouette")
