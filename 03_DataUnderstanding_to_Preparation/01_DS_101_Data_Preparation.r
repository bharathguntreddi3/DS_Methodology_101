R.Version()$version.string  # check version

download.file("https://cf-courses-deloperSkillsNetwork-DS0103EN-SkillsNetwork/labs/Module%202/recipes.csv",
              destfile = "recipes.csv", quiet = TRUE)

print("Done!") # takes about 30 seconds
# dwonloaded the recipes dataset

# read data into R frame
recipes <- read.csv("recipes.csv")

head(recipes)

nrow(recipes)

ncol(recipes)

# check wheter these ingredents are present in the dataframe or not
grep("rice", names(recipes), value = TRUE) # yes as rice
grep("wasabi", names(recipes), value = TRUE) # yes
grep("soy", names(recipes), value = TRUE) # yes as soy_sauce
# recipe containing these 3 ingredents we can confidently say that it is japanese

# check the table needs any cleaning or not
base::table(recipes$country) # frequency table

colnames(recipes)[1] = "cuisine"   # name of column showing cuisine

recipes$cuisine <- tolower(as.character(recipes$cuisine))    # all cuisine names to lower

recipes

# making the cuisine names consistent
recipes$cuisine[recipes$cuisine == "austria"] <- "austrian"
recipes$cuisine[recipes$cuisine == "belgium"] <- "belgian"
recipes$cuisine[recipes$cuisine == "china"] <- "chinese"
recipes$cuisine[recipes$cuisine == "canada"] <- "canadian"
recipes$cuisine[recipes$cuisine == "netherlands"] <- "dutch"
recipes$cuisine[recipes$cuisine == "france"] <- "french"
recipes$cuisine[recipes$cuisine == "germany"] <- "german"
recipes$cuisine[recipes$cuisine == "india"] <- "indian"
recipes$cuisine[recipes$cuisine == "indonesia"] <- "indonesian"
recipes$cuisine[recipes$cuisine == "iran"] <- "iranian"
recipes$cuisine[recipes$cuisine == "israel"] <- "jewish"
recipes$cuisine[recipes$cuisine == "italy"] <- "italian"
recipes$cuisine[recipes$cuisine == "japan"] <- "japanese"
recipes$cuisine[recipes$cuisine == "korea"] <- "korean"
recipes$cuisine[recipes$cuisine == "lebanon"] <- "lebanese"
recipes$cuisine[recipes$cuisine == "malaysia"] <- "malaysian"
recipes$cuisine[recipes$cuisine == "mexico"] <- "mexican"
recipes$cuisine[recipes$cuisine == "pakistan"] <- "pakistani"
recipes$cuisine[recipes$cuisine == "philippines"] <- "philippine"
recipes$cuisine[recipes$cuisine == "scandinavia"] <- "scandinavian"
recipes$cuisine[recipes$cuisine == "spain"] <- "spanish_portuguese"
recipes$cuisine[recipes$cuisine == "portugal"] <- "spanish_portuguese"
recipes$cuisine[recipes$cuisine == "switzerland"] <- "swiss"
recipes$cuisine[recipes$cuisine == "thailand"] <- "thai"
recipes$cuisine[recipes$cuisine == "turkey"] <- "turkish"
recipes$cuisine[recipes$cuisine == "irish"] <- "uk-and-irish"
recipes$cuisine[recipes$cuisine == "uk-and-ireland"] <- "uk-and-irish"
recipes$cuisine[recipes$cuisine == "vietnam"] <- "vietnamese"

recipes

# Remove cuisines with < 50 recipes
# sort the table of cuisines by descending order
t <- sort(base::table(recipes$cuisine), decreasing = T)

t

# get cuisines with >= 50 recipes
filter_list <- names(t[t >= 50])

filter_list

before <- nrow(recipes) # number of rows of original dataframe
print(paste0("Number of rows of original dataframe is ", before))

recipes <- recipes[recipes$cuisine %in% filter_list,]

after <- nrow(recipes)
print(paste0("Number of rows of processed dataframe is ", after))

print(paste0(before - after, " rows removed!"))


# convert all columns into factors. this is to run classification model later
recipes[,names(recipes)] <- lapply(recipes[,names(recipes)], as.factor)

recipes

str(recipes)     # check the structure fo recipe

# recipes containing rice and soy and wasabi and seaweed.
check_recipes <- recipes[
  recipes$rice == "Yes" &
    recipes$soy_sauce == "Yes" &
    recipes$wasabi == "Yes" &
    recipes$seaweed == "Yes",
]

check_recipes


# Connting ingredients across recipes

# sum the row count when the value of the row in a column is equal to "Yes" (value of 2)
ingred <- unlist(
  lapply( recipes[, names(recipes)], function(x) sum(as.integer(x) == 2)))

# transpose the dataframe so that each row is an ingredient
ingred <- as.data.frame( t( as.data.frame(ingred) ))

ing_df <- data.frame("ingredient" = names(ingred), "count" = as.numeric(ingred[1,]))[-1,] # nolint

ing_df


# sort in descending order
ing_df_sort <- ing_df[order(ing_df$count, decreasing = TRUE),]
rownames(ing_df_sort) <- 1:nrow(ing_df_sort)

ing_df_sort


# create a dataframe of the counts of ingredients by cuisine, normalized by the number of 
# recipes pertaining to that cuisine
by_cuisine_norm <- aggregate(recipes, by = list(recipes$cuisine), FUN = function(x) round(sum(as.integer(x) == 2)/length(as.integer(x)),4))
# remove the unnecessary column "cuisine"
by_cuisine_norm <- by_cuisine_norm[,-2]

# rename the first column into "cuisine"
names(by_cuisine_norm)[1] <- "cuisine"

head(by_cuisine_norm)


for(nation in by_cuisine_norm$cuisine){
  x <- sort(by_cuisine_norm[by_cuisine_norm$cuisine == nation,][-1], decreasing = TRUE)
  cat(c(toupper(nation)))
  cat("\n")
  cat(paste0(names(x)[1:4], " (", round(x[1:4]*100,0), "%) "))
  cat("\n")
  cat("\n")
}


