library(tm)
library(wordcloud)
library(memoise)

# The list of valid wines
wines <<- list("Cabernet Franc" = "cabfranc", "Cabernet Sauvignon" = "cabsauv", "Chardonnay" = "chardonnay",
               "Chenin Blanc" = "cheninblanc", "Gamay" = "gamay", "Gewurztraminer" = "gewurztraminer",
               "Grenache" = "grenache", "Malbec" = "malbec", "Merlot" = "merlot", "Petite Sirah" = "petitesirah",
               "Pinot Noir" = "pinotnoir", "Riesling" = "riesling", "Sauvignon Blanc" = "sauvblanc",
               "Syrah" = "syrah", "Tempranillo" = "tempranillo", "Zinfandel" = "zinfandel")

#Load data file that has corpuses
load("./wine_data.rda")

getTermMatrix <- memoise(function(wine) {

  if (!(wine %in% wines))
    stop("Unknown wine")
  

  if(wine == "cabsauv") {
    text <- cabsauv_var
  }
  else if(wine == "cabfranc") {
    text <- cabfranc_var
  }
  else if(wine == "chardonnay") {
    text <- chardonnay_var
  }
  else if(wine == "cheninblanc") {
    text <- cheninblanc_var
  }
  else if(wine == "gamay") {
    text <- gamay_var
  }
  else if(wine == "gewurztraminer") {
    text <- gewurztraminer_var
  }
  else if(wine == "grenache") {
    text <- grenache_var
  }
  else if(wine == "malbec") {
    text <- malbec_var
  }
  else if(wine == "merlot") {
    text <- merlot_var
  }
  else if(wine == "petitesirah") {
    text <- petitesirah_var
  }
  else if(wine == "pinotnoir") {
    text <- pinotnoir_var
  }
  else if(wine == "riesling") {
    text <- riesling_var
  }
  else if(wine == "syrah") {
    text <- syrah_var
  }
  else if(wine == "tempranillo") {
    text <- tempranillo_var
  }
  else if(wine == "zinfandel") {
    text <- zinfandel_var
  }
  else if(wine == "sauvblanc") {
    text <- sauvblanc_var
  }
  else {
    
  }
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but", "found",
                      "wine", "grown", "part", "end", "planted", "varietal", "variety", "state",
                      "planted", "growing", "widely", "small", "made", "long", "examples", "produce",
                      "range", "grapes", "examples", "grape", "light", "grown", "primarily", "common",
                      "high", "varieties", "characteristics", "concentration", "barrel", "vine",
                      "techniques", "typical", "typically", "slight", "producers", "winemaking", "quality",
                      "wines", "small", "south", "winemakers", "years", "notes", "bottle", "give",
                      "fairly", "tend", "grow", "thin", "produced", "version", "plantings", "important",
                      "makes", "dom", "reputation", "bottlings", "make", "regarded", "successful",
                      "century", "producer", "states", "basic", "acres", "similar", "identified",
                      "good", "descriptors", "making", "increasingly", "success", "minus", "due",
                      "susceptible", "united", "produces", "practices", "slightly", "success",
                      "called", "proved", "joseph", "labelled", "commonly", "growers", "appellation",
                      "chardonnays", "cfte", "note", "californias", "include", "bottles", "generally",
                      "forrester", "fact", "fermentation", "quarts", "viticulture", "results", "roter",
                      "component", "outbreak", "cft", "true", "greatest", "santa", "extremely",
                      "germanys", "appellations", "cfterftie", "rftie", "vineyard", "cuttings",
                      "mystery", "established", "region", "regions", "great", "areas", "dor",
                      "vineyards", "savennieres", "gamayus", "vines", "county", "acreage", "rhfne",
                      "banyuls", "time", "los", "labeled", "recent", "amounts", "producing", 
                      "vintners", "types", "prohibition", "field", "testing", "research", "provide",
                      "related", "bottled", "provide", "tree", "taste", "del", "origin", "style",
                      "uneven", "bunch", "valley", "notably", "world", "consumers", "district",
                      "attention", "central", "cask", "sites"))
  
  if(wine == "cabsauv") {
    myCorpus = tm_map(myCorpus, removeWords, c("cabernet", "sauvignon", "cabernets", "cab"))
  }
  else if(wine == "cabfranc") {
    myCorpus = tm_map(myCorpus, removeWords, c("cabernet", "franc", "sauvignon"))
  }
  else if(wine == "chardonnay") {
    myCorpus = tm_map(myCorpus, removeWords, c("chardonnay"))
  }
  else if(wine == "cheninblanc") {
    myCorpus = tm_map(myCorpus, removeWords, c("chenin", "blanc", "blancs", "chenins"))
  }
  else if(wine == "gamay") {
    myCorpus = tm_map(myCorpus, removeWords, c("gamay", "gamays"))
  }
  else if(wine == "gewurztraminer") {
    myCorpus = tm_map(myCorpus, removeWords, c("traminer", "gewfcrztraminer", "gewurz", "gewurztraminer"))
  }
  else if(wine == "grenache") {
    myCorpus = tm_map(myCorpus, removeWords, c("grenache"))
  }
  else if(wine == "malbec") {
    myCorpus = tm_map(myCorpus, removeWords, c("malbec", "malbecs"))
  }
  else if(wine == "merlot") {
    myCorpus = tm_map(myCorpus, removeWords, c("merlot"))
  }
  else if(wine == "petitesirah") {
    myCorpus = tm_map(myCorpus, removeWords, c("petite", "sirah"))
  }
  else if(wine == "pinotnoir") {
    myCorpus = tm_map(myCorpus, removeWords, c("pinot", "noir"))
  }
  else if(wine == "riesling") {
    myCorpus = tm_map(myCorpus, removeWords, c("riesling"))
  }
  else if(wine == "sauvblanc") {
    myCorpus = tm_map(myCorpus, removeWords, c("sauvignon", "blanc"))
  }
  else if(wine == "syrah") {
    myCorpus = tm_map(myCorpus, removeWords, c("syrah", "syrahs", "shiraz"))
  }
  else if(wine == "tempranillo") {
    myCorpus = tm_map(myCorpus, removeWords, c("tempranillo"))
  }
  else if(wine == "zinfandel") {
    myCorpus = tm_map(myCorpus, removeWords, c("zinfandel"))
  }

  myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
  m = as.matrix(myDTM)
  sort(rowSums(m), decreasing = TRUE)
})