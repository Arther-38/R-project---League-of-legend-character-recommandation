library(rvest)
install.packages("rvest")
doc <- read_html("https://www.leagueofgraphs.com/champions/builds") # 把網頁先存在一個變數裡，不用每次都連線造訪
name<-doc %>% html_nodes("span.name")
View(name)

name<-doc %>% html_nodes("span.name") %>% html_text()

