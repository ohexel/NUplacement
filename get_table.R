library(httr)
library(rvest)
library(readr)

url <- "https://www.sociology.northwestern.edu/graduate/job-placement.html"
f <- GET(url)
entries <- read_html(f) %>%
    html_nodes("tr") %>%
    html_nodes("td") %>%
    html_text(trim = TRUE)
splitindices <- which(grepl("\\d+-\\d+",entries))
startidx <- splitindices
endidx <- c(splitindices[2:length(splitindices)] - 1, length(entries))
plt <- data.frame("graduation" = character(),
                  "name" = character(),
                  "job" = character())
for(i in startidx){
    start <- i
    end <- endidx[which(startidx == start)]
    new <- entries[(start + 2):end]
    l <- length(new)/2
    splitnew <- split(new, rep(1:2, l))
    name <- as.character(splitnew$`1`)
    job <- as.character(splitnew$`2`)
    # thank you, whoever made the site, for non-ASCII characters
    job <- stringi::stri_trans_general(job, "latin-ascii")
    graduation <- rep(entries[i], l)
    newnew <- data.frame(graduation = graduation,
                         name = name,
                         job = job,
                         stringsAsFactors = FALSE)
    plt <- rbind(plt, newnew, stringsAsFactors = FALSE )
}
plt$splitjobs <- strsplit(plt$job, ";")
#plt$firstjob <- sub("([\\w\\s,&'-.]+);[\\w\\s,&'-.]*", "\\1", plt$jobs, perl = TRUE)
#jobs <- sub("([\\w\\s,&'-.]+);([\\w\\s,&'-.]+)", "\\2", plt$job, perl = TRUE)
# for some reason, unnest drops empty lists
nojob <- plt[plt$job == "",]
plt <- plt %>% tidyr::unnest(splitjobs, .drop = FALSE) %>%
    group_by(name) %>%
    mutate(count = seq(n()))
plt <- plt %>% tidyr::spread(count, splitjobs)
names(plt)[names(plt) == "1"] <- "firstjob"
names(plt)[names(plt) == "2"] <- "secondjob"
plt$firstjob <- trimws(plt$firstjob)
plt$secondjob <- trimws(plt$secondjob)
plt <- bind_rows(plt, nojob)

plt$pd2assist <- grepl("^Post[[:print:]]*", plt$firstjob, ignore.case = TRUE) &
    grepl("^Assistant[[:print:]]*", plt$secondjob, ignore.case = TRUE)
plt$pd2assoc <- grepl("^Post[[:print:]]*", plt$firstjob, ignore.case = TRUE) &
    grepl("^Associate Professor[[:print:]]*", plt$secondjob, ignore.case = TRUE)
plt$assist2assoc <- grepl("^Assistant[[:print:]]*", plt$firstjob, ignore.case = TRUE) &
    grepl("^Associate Professor[[:print:]]*", plt$secondjob, ignore.case = TRUE)
plt$pdfirst <- grepl("^Post[[:print:]]*", plt$firstjob, ignore.case = TRUE)
plt$assistfirst <- grepl("^Assistant[[:print:]]*", plt$firstjob, ignore.case = TRUE)
plt$assistsecond <- grepl("^Assistant[[:print:]]*", plt$secondjob, ignore.case = TRUE)
plt$assocfirst  <- grepl("^Associate Professor[[:print:]]*", plt$firstjob, ignore.case = TRUE)
plt$assocsecond  <- grepl("^Associate Professor[[:print:]]*", plt$secondjob, ignore.case = TRUE)

plt$academic <- plt$pd2assist | plt$pd2assoc | plt$pdfirst | plt$assist2assoc |
    plt$assistfirst | plt$assistsecond | plt$assocfirst

plt$lastname <- sub("[\\w- ]+ ([\\w-']+)$","\\1", plt$name, perl = TRUE)
plt$firstname <- sub("([\\w-]+) [\\w- ']+$","\\1", plt$name, perl = TRUE)
plt$gradyear <- sub("[\\d]+-([\\d]+)","\\1", plt$graduation, perl = TRUE)
write.csv(plt[,c("graduation", "name", "firstjob", "secondjob", "lastname",
                 "firstname", "gradyear")],
          file = "placement.csv", na = "", row.names = FALSE)
