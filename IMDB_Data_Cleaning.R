
clean <- function(column_name){
  # This function takes a column from the dataframe and splits two elements
  # if they are separated by a comma.
  # For ex. in Actors column there might be values such as Christian Bale, Morgan Freeman.
  # This will separate these two actors and store them individually in a list."""
  name=sets::set()
  for(name_string in df_omdb[,column_name]){
    if(!(name_string %in% c("X100", "X20")))
      name = append(name, strsplit(name_string,", "))
  }
  return(unique(sort(unlist(name), decreasing = FALSE)))
}

top <- function(column_name){
  # This function takes its input as name of the column and returns a sorted list of the 
  # elements which occur very frequently in that column in descending order.
  name=clean(column_name)
  dummy_name = data.frame(100,20)
  for(n in name){
    dummy_name[,n] = 0
    for(nm in df_omdb[,column_name]){
      count = str_count(strsplit(nm,", "), n)
      dummy_name[,n] = dummy_name[,n] +  count 
    }
  }
  dummy_name = dummy_name[ , !(names(dummy_name) %in% c("X100", "X20"))]
  return(sort(unique(dummy_name),decreasing = TRUE))
}

plot_column <- function(column_name,n_elem_display){
  # This function is used to plot a bar graph of a column of the dataframe.
  # It takes its argument as name of column and number of elements to display and
  # return a bar graph of the user defined number of top elements which occur
  # frequently in that column.
  name=clean(column_name)
  nlt = top(column_name)
  df = data.frame(unlist(nlt))
  df[,2] = names(nlt)
  df = df[order(df[,1], decreasing = TRUE),]
  df = df[1:n_elem_display,]
  black.bold.italic.12.text <- element_text(face = "bold.italic", color = "black", size = 12)
  black.bold.italic.14.title <- element_text(face = "bold.italic", color = "black", size = 14)
  Names = df[,2]
  p <- ggplot(df)+
    geom_bar(aes(x = reorder(df[,2], -df[,1]), y = df[,1], fill = Names), width=0.5, stat = "identity", position=position_dodge())+
    coord_flip() +
    labs (x = " ", y = " ", title = paste("Frequency of ",column_name," name occurences"))
  return(p+theme(axis.text = black.bold.italic.12.text, title = black.bold.italic.14.title ))
}
