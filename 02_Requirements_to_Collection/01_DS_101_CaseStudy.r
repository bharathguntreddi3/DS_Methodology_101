###  Recipe finding Recipe classification case study based on the ingredients

R.Version()$version.string  # check version

download.file("https://cf-courses-deloperSkillsNetwork-DS0103EN-SkillsNetwork/labs/Module%202/recipes.csv",
              destfile = "recipes.csv", quiet = TRUE)

print("Done!") # takes about 30 seconds
# dwonloaded the recipes dataset

# read data into R frame
recipes <- read.csv("/resources/data/recipes.csv")

head(recipes)

nrow(recipes)

ncol(recipes)