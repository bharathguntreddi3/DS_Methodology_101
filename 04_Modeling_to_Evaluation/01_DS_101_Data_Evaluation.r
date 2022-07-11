# click here and press Shift + Enter
download.file("https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-DS0103EN-SkillsNetwork/labs/Module%202/recipes.csv",
              destfile = "recipes.csv", quiet = TRUE)

recipes <- read.csv("recipes.csv")

# fix name of the column displaying the cuisine
colnames(recipes)[1] = "cuisine"

# convert cuisine names to lower case
recipes$cuisine <- tolower(as.character(recipes$cuisine)) 

# make the cuisine names consistent
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

# remove data for cuisines with < 50 recipes:
t <- sort(base::table(recipes$cuisine), decreasing = T)
filter_list <- names( t[ t >= 50 ] )
recipes <- recipes[recipes$cuisine %in% filter_list, ]
recipes$cuisine <- as.factor(as.character(recipes$cuisine))
sort(base::table(recipes$cuisine), decreasing = T)

# convert all of the columns into factors (to run the classification model later)
recipes[,names(recipes)] <- lapply(recipes[,names(recipes)] , as.factor)


# install libraries if not installed
if("rpart" %in% rownames(installed.packages()) == FALSE) {install.packages("rpart", repo = "http://mirror.las.iastate.edu/CRAN/")}
if("rpart.plot" %in% rownames(installed.packages()) == FALSE) {install.packages("rpart.plot", repo = "http://mirror.las.iastate.edu/CRAN/")}

# load libraries
library(rpart)
library(rpart.plot)

print("Libraries loaded!")

head(recipes)

# select subset of cuisines
bamboo_tree <- rpart(formula=cuisine ~ ., data=recipes[recipes$cuisine %in% c("korean", "japanese", "chinese", "thai","indian"),], method ="class")

print("Decision tree model saved to bamboo_tree!")

# plot bamboo_tree
rpart.plot(bamboo_tree, type=3, extra=2, under=TRUE, cex=0.75, varlen=0, faclen=0)

bamboo <- recipes[recipes$cuisine %in% c("korean", "japanese", "chinese", "thai", "indian"),]

base::table(as.factor(as.character(bamboo$cuisine)))

# set sample size
sample_n <- 30

# take 30 recipes from each cuisine
set.seed(4) # set random seed
korean <- bamboo[base::sample(which(bamboo$cuisine == "korean") , sample_n), ]
japanese <- bamboo[base::sample(which(bamboo$cuisine == "japanese") , sample_n), ]
chinese <- bamboo[base::sample(which(bamboo$cuisine == "chinese") , sample_n), ]
thai <- bamboo[base::sample(which(bamboo$cuisine == "thai") , sample_n), ]
indian <- bamboo[base::sample(which(bamboo$cuisine == "indian") , sample_n), ]

# create the dataframe
bamboo_test <- rbind(korean, japanese, chinese, thai, indian)

# check that we have 30 recipes from each cuisine
base::table(as.factor(as.character(bamboo_test$cuisine)))

bamboo_train <- bamboo[!(rownames(bamboo) %in% rownames(bamboo_test)),]

base::table(as.factor(as.character(bamboo_train$cuisine)))

bamboo_train_tree <- rpart(formula=cuisine ~ ., data=bamboo_train, method="class")

rpart.plot(bamboo_train_tree, type=3, extra=2, under=TRUE, cex=0.75, varlen=0, faclen=0)

bamboo_pred_cuisines <- predict(bamboo_train_tree, subset(bamboo_test, select=-c(cuisine)), type="class")

bamboo_confusion_matrix <- base::table(
  paste(as.character(bamboo_test$cuisine),"_true", sep=""),
  paste(as.character(bamboo_pred_cuisines),"_pred", sep="")
)

round(prop.table(bamboo_confusion_matrix, 1)*100, 1)