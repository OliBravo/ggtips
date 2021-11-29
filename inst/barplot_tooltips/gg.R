library(ggplot2)
library(dplyr)
data(iris)

d = iris %>%
  group_by(Species) %>%
  summarize(mu = mean(Petal.Length), sd = sd(Petal.Length))

# wykres słupkowy z wąsami SD + paleta z kolorem białym
p = ggplot(d, mapping = aes(x = Species)) +
  geom_bar(mapping = aes(y = mu, fill = Species), stat = "identity") +
  geom_errorbar(mapping = aes(ymin = mu - sd, ymax = mu + sd)) +
  scale_fill_brewer(palette = "RdGy")

grob = ggplot_build(p)

saveRDS(grob, "gg1.rds")

# j.w. + punkty w różnych kształtach (również kwadraciki!)
p2 = p +
  geom_jitter(
    data = iris,
    mapping = aes(
      x = Species,
      y = Petal.Length,
      colour = Petal.Width,
      shape = Species
    )
  )

grob2 = ggplot_build(p2)

saveRDS(grob2, "gg2.rds")

# j.w. + kafelki
p3 = p2 +
  facet_wrap(~ Species)

grob3 = ggplot_build(p3)

saveRDS(grob3, "gg3.rds")
