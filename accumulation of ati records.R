# ati accumulation of records
##------ Tue Sep 17 13:13:19 2024 ------##

# date of records ----
ati.date.sort = st_set_geometry(tree_in_country[!is.na(tree_in_country$date.verified),c("date.verified" , "Country" )], NULL)
ati.date.sort$date.verified = as.Date(ati.date.sort$date.verified, "%Y-%m-%d")
ati.date.sort = ati.date.sort[order(ati.date.sort$date.verified),]

ati.date.sort <- ati.date.sort %>% #group_by(Country) %>%
  mutate(number = row_number())
ati.date.sort$number = ati.date.sort$number /1000

p.ati.record.data = ggplot(ati.date.sort, aes(x = date.verified, y = number)) +
  geom_line() +
  theme_pubr()+
  ylab ("ATI records\n(thousands)") +
  xlab("") +
  theme(legend.position =c(0.8,0.8),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 8),
        legend.title = element_text(size = 10 , face = "bold"),
        legend.text =  element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title =  element_text(size = 10)
  ) 

ggsave(plot = p.ati.record.data ,paste("ati.cumulative.date.pdf"))
ggsave(plot = p.ati.record.data ,paste("ati.cumulative.date.svg"))


