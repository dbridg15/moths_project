
plt.df <- data.frame(ffd.resp, lfd.resp)
plt.df$col <- ss.df$col


rm(ffd.resp, lfd.resp)

ggplot(plt.df, aes(ffd.resp, lfd.resp)) +
    geom_point(aes(color = plt.df$col)) +
    theme_classic()


