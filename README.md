# 2.-Web-scraping-Text-mining-and-clustering-of-IMDB-movies

2.1	Introduction:

Internet Movie Database(IMDb)[1] is an online database related to movies, TV programs and internet streams, including cast, production crew and personnel biographies, plot summaries and fan reviews and ratings. Originally IMDb.com, Inc., a subsidiary of Amazon. IMDb website displays the movie list based on different criteria like top 250 movies, top Indian movies, top English movies, top Asians movies etc. I have taken top 250 all-time movies list as part of my analysis. 
The case study is related to gather all time top 250 movie list from IMDb web site by using web scraping  and then analyse the underlying movie related text data. I have used R for web scraping, text mining and then finally get the dataset in the format required for clustering. I have used this dataset for clustering using K-means[2] in R and SAS EM clustering.


2.2	Aim and Objective of the task:

This case study aims to find out the type of natural cluster that exists among the top 250 movies from IMDB. The most challenging task is to retrieve data from webpage and data cleaning which requires lot of effort. The key objectives of this task are
•	Identify intrinsic pattern or clusters among top rated movies.
•	Find out similarities among the movies of the same cluster. 
•	Use Principal Component Analysis and Dimensionality Reduction to improve clustering output
•	Repeat the above steps for different clustering algorithms(K-means, Hierarchical and SAS EM clustering)

2.3	Brief Literature Review:

Most of the top-rated movies in the Internet movie database (IMDB) are depreciatively applauded and are generally a safe bet in terms of commercial success. Apparently, it would be exciting to investigate if these top movies have some distinct features responsible for their high ratings. Unsupervised machine learning techniques have used, more specifically, clustering algorithms. These clusters give us information about category of movies and underlying recurrent pattern. I used dimensionality reduction techniques such as PCA which has followed by K-Means and SAS EM clustering to find inherent clusters in the data.

Keywords:
IMDb; top 250 movies; clustering of movies; data clustering; principle component analysis;

