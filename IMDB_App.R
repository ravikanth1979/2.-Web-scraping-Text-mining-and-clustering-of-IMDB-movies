
library(dplyr)
library(rvest)
library(jsonlite)
library(stringr)
library(sets)
library(ggplot2)
library(tm)
library(caret)
library(cluster)
library(rstudioapi)

url = "https://www.imdb.com/chart/top"

html = read_html(url)
rank_data = html_nodes(html, ".wlb_ribbon")
j = 1
movie_ids = list()
for (i in rank_data) {
  movie_ids[j] = html_attr(i, name = "data-tconst")
  j = j + 1
}

for (i in 1:250) {
  url = 'http://www.omdbapi.com/?i='
  print(movie_ids[i])
  url = str_replace_all(paste(url, movie_ids[i], "&apikey=e2795d"), " ", "")
  print(url)
  json_data = fromJSON(url)
  if (i == 1) {
    df_omdb = as.data.frame(json_data[-15])
  }
  else {
    df_omdb = rbind(df_omdb, as.data.frame(json_data[-15]))
  }
}

# Now I have collected all of the information from the omdb database.
# To get a more richer dataset of movies we will now try to get the database
# from imdb.
url = "http://www.imdb.com/title/"
tt_stry_pl = "/plotsummary?ref_=tt_stry_pl"
df_omdb$Plot = list(list())
for (i in 1:250) {
  print(i)
  page = read_html(str_replace_all(paste(url, df_omdb$imdbID[i], tt_stry_pl), " ", ""))
  #print(page)
  zebra_list = html_nodes(page, ".ipl-zebra-list__item")
  summary_list = list()
  i1 = 1
  for (j in zebra_list) {
    summary_list[i1] = html_text(j, trim = TRUE)
    i1 = i1 + 1
  }
  df_omdb$Plot[[i]] = summary_list
}

#Cleaning the dataset
#First step in cleaning the data is to convert Year to a categorical variable.
#Year 1990 was chosen as a suitable cutoff. Movies released before 1990 became 0
#and after 1990 became 1. After doing this we performed one hot encoding and added dummy
#variables of Year(now 0,1) in the dataframe.

df_omdb$Year = as.numeric(df_omdb$Year)
for (i in 1:250) {
  df_omdb$Year[i] = ifelse(df_omdb$Year[i] < 1990, 0, 1)
}

dummy_year = factor(df_omdb$Year)

for (i in 1:250) {
  df_omdb$dummy_runtime[i] = as.integer(strsplit(as.character(df_omdb$Runtime[i]), " ")[[1]][1])
}
df_omdb$Runtime = as.integer(df_omdb$dummy_runtime)

df_omdb$dummy_runtime <- NULL

for (i in 1:250) {
  df_omdb$Runtime[i] = ifelse(df_omdb$Runtime[i] <= 125, 0, 1)
}

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

source("IMDB_Data_Cleaning.R")

#Get the unique genres contained in the dataframe
genres = clean('Genre')
#Add one column for every genre in the dataframe
for (genre in genres) {
  print(genre)
  #df_omdb[,paste("genre:","Action")] = 0
  i = 1
  for (g in df_omdb[, "Genre"]) {
    #print(genre)
    print(g)
    print(str_count(strsplit(g, ", "), genre))
    df_omdb[i, paste("genre:", genre)] = str_count(strsplit(g, ", "), genre)
    i = i + 1
  }
}

# Thus, we can take the top 30 actors each having more than 3 movies in the top 250 movie list of imdb.
#Adding actors to our dataset
actors = names(top('Actors'))
for (actor in actors[1:30]) {
  print(actor)
  i = 1
  for (a in df_omdb[, "Actors"]) {
    #print(genre)
    print(a)
    print(str_count(strsplit(a, ", "), actor))
    df_omdb[i, paste("Actor:", actor)] = str_count(strsplit(a, ", "), actor)
    i = i + 1
  }
}
plot_column('Director', 20)
plot_column('Genre', 20)
directors = names(top('Director'))

for (director in directors[1:20]) {
  print(director)
  i = 1
  for (d in df_omdb[, "Director"]) {
    #print(genre)
    print(d)
    print(str_count(strsplit(d, ", "), director))
    df_omdb[i, paste("Director:", director)] = str_count(strsplit(d, ", "), director)
    i = i + 1
  }
}

writers1 = sets::set()
writers2 = sets::set()
for (writer_string in df_omdb[, "Writer"])
  writers1 = append(writers1, strsplit(writer_string, ", "))

writers1 = unique(sort(unlist(writers1), decreasing = FALSE))

for (j in writers1)
  writers2 = append(writers2, unlist(strsplit(j, ' \\('))[1])

writers2 = unlist(writers2)[-c(1, 2, 3)]
dummy_writers = data.frame(matrix("", ncol = 3, nrow = 10))

# Add one column for every writer in the dataframe

for (writer in writers2) {
  dummy_writers[, writer] = 0
  for (w in df_omdb[, "Writer"]) {
    count = str_count(strsplit(w, ", "), writer)
    dummy_writers[, writer] = dummy_writers[, writer] +  count
  }
}

dummy_writers = as.data.frame(sort(unique(dummy_writers), decreasing = TRUE))
df = data.frame(unlist(dummy_writers))
df[, 2] = names(dummy_writers)
df = df[order(df[, 1], decreasing = TRUE), ]
df = df[1:10, ]
p <- ggplot(df) +
  geom_bar(
    aes(x = df[, 2], y = df[, 1], fill = df[, 2]),
    width = 0.5,
    stat = "identity",
    position = position_dodge()
  ) +
  labs (x = " ", y = " ", title = "Frequency of word occurences")

plot_column('Language', 21)
plot_column('Country', 20)

#Adding all of the top 10 countries to our datset
countries = names(top('Country'))
for (country in countries[1:10]) {
  print(country)
  i = 1
  for (c in df_omdb[, "Country"]) {
    #print(genre)
    print(c)
    print(str_count(strsplit(c, ", "), country))
    df_omdb[i, paste("Country:", country)] = str_count(strsplit(c, ", "), country)
    i = i + 1
  }
}

head(df_omdb)
for (i in 1:250) {
  str = ""
  for (j in 1:length(df_omdb$Plot[[i]])) {
    str = paste(str, df_omdb$Plot[[i]][[j]], ",")
  }
  df_omdb$Plot[i] = str
}
plot = df_omdb$Plot
plot = gsub("-", " ", plot)

pronouns = c('i', 'he', 'she', 'it', 'him', 'they', 'we', 'us', 'them', 'he ')
others = c("'d","co","ed","put","say","get","can","become","los","sta","la","use","ask","iii","else",
  "doesn't","dr.","well","let","soon","finally","around","little","would","set","use","place","still","three",
  "arrive","next","anoth","keep","must","mr.","bring","much","many","eventually","explain","asks","along",
  "may","small","hold","realize","think","continue","last","behind","discover","something","several","end",
  "large","high","mr","the","dr","mr")
names = c("harry","travis","tommy","joe","jack","dorothy","mike","george","frank","frankie","frank ","sarah","andrew",
  "taylor","arthur","luke","kane","wallace","parker","danny","tony","michael","luke","kane","danny","john",
  "max","tom","paul","ca","neil","maria","barry","anna","jerry","alex","terry","henry","gordon","leonard",
  "wayne","vincent","jimmy","jordan","sam","nick","nicholson","jake","rocky","-harry","-travis","-tommy",
  "-joe","-jack","-dorothy","-mike","-george","-frank","-frankie","-frank ","-sarah","-andrew","-taylor",
  "-arthur","-luke","-kane","-wallace","-parker","-danny","-tony","-michael","-luke","-kane","-danny",
  "-john","-max","-tom","-paul","-ca","-neil","-maria","-barry","-anna","jerry","-alex","-terry","-henry",
  "-gordon","-leonard","-wayne","-vincent","-jimmy","-jordan","-sam","-nick","-nicholson","-jake","-rocky")

new_stop_words = c(stopwords("english"), pronouns, others, names)

plot_corpus = VCorpus(VectorSource(plot))
plot_corpus = tm_map(plot_corpus, content_transformer(tolower))
plot_corpus <- tm_map(plot_corpus, PlainTextDocument)
plot_corpus = tm_map(plot_corpus, removeNumbers)
plot_corpus = tm_map(plot_corpus, removePunctuation)
plot_corpus = tm_map(plot_corpus, removeWords, new_stop_words)
plot_corpus = tm_map(plot_corpus, stemDocument)
plot_corpus = tm_map(plot_corpus, stripWhitespace)

dtm = DocumentTermMatrix(plot_corpus)
dtm = removeSparseTerms(dtm, 0.9)
dataset_plot = as.data.frame(as.matrix(dtm))

tdm = TermDocumentMatrix(plot_corpus, control = list(weighting = weightTfIdf))
tf_idf = rowSums(as.matrix(tdm))
tf_idf = sort(tf_idf, decreasing = T)
tf_idf = head(tf_idf, 100)
top_words = names(tf_idf)

print(top_words)

dataset_plot = dataset_plot[, names(dataset_plot) %in% top_words]
df_final2 = cbind(df_omdb, dataset_plot)


plot_dtm = TermDocumentMatrix(plot_corpus, control = list(minWordLength = c(1, Inf)))
wordFrequency = rowSums(as.matrix(plot_dtm))
wordFrequency <- subset(wordFrequency, wordFrequency >= 100)

wordFrequency_new = list()
wordFrequency_new$names = names(wordFrequency)
wordFrequency_new$frequency = as.integer(wordFrequency)

wordFrequency_new = as.data.frame(wordFrequency_new)
wordFrequency_new = wordFrequency_new[order(wordFrequency_new$numbers, decreasing = TRUE), ]

drops = c('Title','Released','Genre','Director','Writer','Actors','Plot','Language','Country','Awards',
  'Poster','Ratings','Metascore','imdbVotes','imdbID','Type','DVD','BoxOffice','Production','Website','Response',
  'Rated','imdbRating','Year'
)

df_final2 = df_final2[,!(names(df_final2) %in% drops)]
df_final2 = scale(df_final2)
pca = preProcess(x = df_final2,
                 method = 'pca',
                 pcaComp = 2)
ex_pca = predict(pca, df_final2)
ex_pca <- na.omit(ex_pca)
rownames(ex_pca) = NULL
distance <- dist(ex_pca, method = "euclidean")
fviz_dist(distance)
tendency <- get_clust_tendency(ex_pca, 30, graph = TRUE)
print(paste(
  "Cluster tendency using Hopkins Statistics...",
  tendency$hopkins_stat
))
set.seed(123)
fviz_nbclust(ex_pca, kmeans, method = "wss")
fviz_nbclust(ex_pca, kmeans, method = "silhouette")

kmeans = kmeans(x = ex_pca, centers = 3)
y_kmeans = kmeans$cluster

kmeans$betweenss
fviz_cluster(kmeans, ex_pca)
count = 1
clustered_movies = list()
for (clus_num in kmeans$cluster) {
  print(df_omdb$Title[count])
  print(as.character(clus_num))
  df_omdb[count, "Cluster"] = clus_num
  count = count + 1
}

for (clust in unique(sort(df_omdb$Cluster))) {
  print(paste("Cluster No:", clust, " movies........."))
  print("######################################################################")
  print(as.character(df_omdb$Title[df_omdb$Cluster == clust]))
  print("######################################################################")
}


