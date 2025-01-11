library(tidyverse)

ggplot(data = mpg) +
  geom_point(mapping = aes(x=hwy, y=cyl))

ggplot(mpg)+
  geom_point(mapping = aes(x=class, y=drv, colour = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = displ, size = displ))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = year, 
                           shape = class, size = hwy))

ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(aes(color=displ, size=displ))

ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(aes(color=displ, size=displ))

ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(aes(color=displ<5, size=displ))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = class)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_jitter(mapping = aes(alpha = 0.1)) + 
  facet_wrap(~ drv) + 
  geom_smooth() +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  theme_bw() + 
  labs(x = "Displacement", y = "Highway miles per gallon") +
  theme(legend.position = "none")

housingData <- read_csv("housingData.csv")
housingData <- as_tibble(housingData)

housingData <- housingData %>% 
  mutate(across(where(is.character), as.factor)) 

ggplot(housingData, aes(x = MSZoning)) + 
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of General Zoning Classification", 
       x = "Zoning", y = "Count")

ggplot(housingData, aes(x = LotArea)) + 
  geom_histogram(bins = 30, fill = "cornflowerblue", color = "black") +
  labs(title = "Distribution of Lot Areas", 
       x = "Lot Area (sq ft)", y = "Frequency")

ggplot(housingData, aes(x = MSZoning, y = LotFrontage)) + 
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Lot Frontage Across Different Zonings", 
       x = "Zoning", y = "Lot Frontage (Linear Feet)")

gplot(housingData, aes(x = GrLivArea, y = SalePrice)) + 
  geom_point(aes(color = MSZoning), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Living Area vs Sale Price by Zoning", 
       x = "Above Grade Living Area (sq ft)", y = "Sale Price ($)")

ggplot(housingData, aes(x = GrLivArea, y = SalePrice)) + 
  geom_jitter(aes(color = MSZoning), alpha = 0.6, width = 0.3, height = 0) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Living Area vs Sale Price by Zoning", 
       x = "Above Grade Living Area (sq ft)", y = "Sale Price ($)")

ggplot(housingData, aes(x = GrLivArea, y = SalePrice)) + 
  geom_point(aes(color = MSZoning), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  facet_wrap(~MSZoning) +
  labs(title = "Living Area vs Sale Price by Zoning", 
       x = "Above Grade Living Area (sq ft)", y = "Sale Price ($)")

ggplot(housingData, aes(x = YearBuilt, fill = BldgType)) + 
  geom_density(alpha = 0.7) +
  labs(title = "Density of Building Years by Type", 
       x = "Year Built", y = "Density")

ggplot(housingData, aes(x = MSZoning)) + 
  geom_bar(aes(fill = MSZoning), color = "black") +
  facet_wrap(~ Neighborhood) +
  labs(title = "Zoning Classification by Neighborhood", 
       x = "Zoning", y = "Count") +
  theme_minimal()

ggplot(housingData, aes(x = MSZoning, y = OverallQual, fill = MSZoning)) + 
  geom_violin(trim = FALSE) +
  labs(title = "Overall Quality by Zoning Classification", 
       x = "Zoning", y = "Overall Quality") +
  theme_light()

library(corrplot)

numericalData <- housingData %>% select(SalePrice, LotArea, YearBuilt, GrLivArea)
corrMatrix <- cor(numericalData, use = "complete.obs")
corrplot(corrMatrix, method = "color")

