# Mental Health Hospitals
Amy Paschal  
3/20/2018  

### Mental Health Clinics

```r
library(rvest)
```

```
## Loading required package: xml2
```

```r
library(stringr)
library(knitr)
library(kableExtra)

# Load the National Mental Health Services Survey
load("/Users/amypaschal/Documents/SMU/Term 1/Doing Data Science/Unit 10 /N-MHSS-2015-DS0001-bndl-data-r/N-MHSS-2015-DS0001-data/N-MHSS-2015-DS0001-data-r.rda")


# all US states and territories listed in the survey
states <- as.factor(sort(trimws(unique(mh2015_puf$LST))))

# Only mainland states and DC
outlying <- as.factor(c('AK', 'AS', 'GU', 'HI', 'PR', 'VI'))
mainland <- as.factor(setdiff(states, outlying))
```
#### States Surveyed
###### AK, AL, AR, AS, AZ, CA, CO, CT, DC, DE, FL, GA, GU, HI, IA, ID, IL, IN, KS, KY, LA, MA, MD, ME, MI, MN, MO, MS, MT, NC, ND, NE, NH, NJ, NM, NV, NY, OH, OK, OR, PA, PR, RI, SC, SD, TN, TX, UT, VA, VI, VT, WA, WI, WV, WY

#### Mainland States and DC
###### AL, AR, AZ, CA, CO, CT, DC, DE, FL, GA, IA, ID, IL, IN, KS, KY, LA, MA, MD, ME, MI, MN, MO, MS, MT, NC, ND, NE, NH, NJ, NM, NV, NY, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VA, VT, WA, WI, WV, WY


```r
mh2015_mainland <- mh2015_puf[mh2015_puf$LST %in% mainland,]


prefix_bank <- c('de', 'Van')
# splitName
# input: a vector of strings, each string is a word in a persons name
# output: a two dimentional vector, 
#    the first element is the a string of the first and middle names (i.e. all words not part of the last name)
#    the second element is a string of all words comprising the last name
# Properly hands multiple last names like 'de Juan' and 'Van Heusen'
splitName <- function(x) {
  num_names <- length(x)
  last_name <- x[num_names]
  leave_off <- c(num_names)
  # check for two-word last name
  if (num_names > 1 & x[num_names-1] %in% prefix_bank) {
      last_name <- paste(x[num_names-1], last_name, sep = " ")
      leave_off <- append(leave_off, num_names-1)
  }
  cbind(paste(x[-leave_off], collapse = " "), last_name)
}

# read in HMTL from the IMDB web site
IMDB_hp <- read_html("http://www.imdb.com/title/tt1201607/fullcredits?ref_=tt_ql_1")
actors_html <- html_nodes(IMDB_hp, xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "itemprop", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "itemprop", " " ))]')
characters_html <- html_nodes(IMDB_hp, xpath='//*[(@id = "fullcredits_content")]//div')

#Convert the actors and characters data to text
actor_data <- html_text(actors_html)
character_data <- html_text(characters_html)

# convert the actor data to a dataframe with FirstName and Surname columns
cast <- data.frame(t(sapply(strsplit(actor_data, " "), splitName)), stringsAsFactors = FALSE)
names(cast) <- c("FirstName","Surname")
         
# clean the character data
# remove new lines, extra white space, and anything in parentheses
character_data <- str_replace_all(html_text(characters_html), "[\r\n]" , "")
character_data <- str_replace_all(character_data, "\\s+" , " ")
character_data <- str_replace_all(character_data, "\\([^\\)]+\\)" , "")

# add the character information to the cast dataframe
cast$Character <- character_data

#display in a table
kable(cast, "markdown", row.names=FALSE, align="l", padding=2)
```



|FirstName                |Surname       |Character                                 |
|:------------------------|:-------------|:-----------------------------------------|
|Ralph                    |Fiennes       |Lord Voldemort                            |
|Michael                  |Gambon        |Professor Albus Dumbledore                |
|Alan                     |Rickman       |Professor Severus Snape                   |
|Daniel                   |Radcliffe     |Harry Potter                              |
|Rupert                   |Grint         |Ron Weasley                               |
|Emma                     |Watson        |Hermione Granger                          |
|Evanna                   |Lynch         |Luna Lovegood                             |
|Domhnall                 |Gleeson       |Bill Weasley                              |
|Clémence                 |Poésy         |Fleur Delacour                            |
|Warwick                  |Davis         |Griphook / Professor Filius Flitwick      |
|John                     |Hurt          |Ollivander                                |
|Helena Bonham            |Carter        |Bellatrix Lestrange                       |
|Graham                   |Duff          |Death Eater                               |
|Anthony                  |Allgood       |Gringotts' Guard                          |
|Rusty                    |Goffe         |Aged Gringotts' Goblin                    |
|Jon                      |Key           |Bogrod                                    |
|Kelly                    |Macdonald     |Helena Ravenclaw                          |
|Jason                    |Isaacs        |Lucius Malfoy                             |
|Helen                    |McCrory       |Narcissa Malfoy                           |
|Tom                      |Felton        |Draco Malfoy                              |
|Ian                      |Peck          |Hogsmeade Death Eater                     |
|Benn                     |Northover     |Hogsmeade Death Eater                     |
|Ciarán                   |Hinds         |Aberforth Dumbledore                      |
|Hebe                     |Beardsall     |Ariana Dumbledore                         |
|Matthew                  |Lewis         |Neville Longbottom                        |
|Devon                    |Murray        |Seamus Finnigan                           |
|Jessie                   |Cave          |Lavender Brown                            |
|Afshan                   |Azad          |Padma Patil                               |
|Isabella                 |Laughland     |Leanne                                    |
|Anna                     |Shaffer       |Romilda Vane                              |
|Georgina                 |Leonidas      |Katie Bell                                |
|Freddie                  |Stroma        |Cormac McLaggen                           |
|Alfred                   |Enoch         |Dean Thomas                               |
|Katie                    |Leung         |Cho Chang                                 |
|William                  |Melling       |Nigel                                     |
|Sian Grace               |Phillips      |Screaming Girl                            |
|Bonnie                   |Wright        |Ginny Weasley                             |
|Ralph                    |Ineson        |Amycus Carrow                             |
|Suzanne                  |Toase         |Alecto Carrow                             |
|Maggie                   |Smith         |Professor Minerva McGonagall              |
|Jim                      |Broadbent     |Professor Horace Slughorn                 |
|Scarlett                 |Byrne         |Pansy Parkinson                           |
|Josh                     |Herdman       |Gregory Goyle                             |
|Louis                    |Cordice       |Blaise Zabini                             |
|Amber                    |Evans         |Twin Girl 1                               |
|Ruby                     |Evans         |Twin Girl 2                               |
|Miriam                   |Margolyes     |Professor Pomona Sprout                   |
|Gemma                    |Jones         |Madam Pomfrey                             |
|George                   |Harris        |Kingsley Shacklebolt                      |
|David                    |Thewlis       |Remus Lupin                               |
|Julie                    |Walters       |Molly Weasley                             |
|Mark                     |Williams      |Arthur Weasley                            |
|James                    |Phelps        |Fred Weasley                              |
|Oliver                   |Phelps        |George Weasley                            |
|Chris                    |Rankin        |Percy Weasley                             |
|David                    |Bradley       |Argus Filch                               |
|Guy                      |Henry         |Pius Thicknesse                           |
|Nick                     |Moran         |Scabior                                   |
|Natalia                  |Tena          |Nymphadora Tonks                          |
|Phil                     |Wright        |Giant                                     |
|Garry                    |Sayer         |Giant                                     |
|Tony                     |Adkins        |Giant                                     |
|Dave                     |Legeno        |Fenrir Greyback                           |
|Penelope                 |McGhie        |Death Eater                               |
|Emma                     |Thompson      |Professor Sybil Trelawney                 |
|Ellie                    |Darcey-Alden  |Young Lily Potter                         |
|Ariella                  |Paradise      |Young Petunia Dursley                     |
|Benedict                 |Clarke        |Young Severus Snape                       |
|Leslie                   |Phillips      |The Sorting Hat                           |
|Alfie                    |McIlwain      |Young James Potter                        |
|Rohan                    |Gotobed       |Young Sirius Black                        |
|Geraldine                |Somerville    |Lily Potter                               |
|Adrian                   |Rawlins       |James Potter                              |
|Toby                     |Papworth      |Baby Harry Potter                         |
|Timothy                  |Spall         |Wormtail                                  |
|Robbie                   |Coltrane      |Rubeus Hagrid                             |
|Gary                     |Oldman        |Sirius Black                              |
|Peter G.                 |Reed          |Death Eater                               |
|Judith                   |Sharp         |Death Eater                               |
|Emil                     |Hostina       |Death Eater                               |
|Bob Yves Van Hellenberg  |Hubar         |Death Eater                               |
|Granville                |Saxton        |Death Eater                               |
|Tony                     |Kirwood       |Death Eater                               |
|Ashley                   |McGuire       |Death Eater                               |
|Arthur                   |Bowen         |Albus Severus Potter - 19 Years Later     |
|Daphne                   |de Beistegui  |Lily Potter - 19 Years Later              |
|Will                     |Dunn          |James Potter - 19 Years Later             |
|Jade                     |Gordon        |Astoria Malfoy - 19 Years Later           |
|Bertie                   |Gilbert       |Scorpius Malfoy - 19 Years Later          |
|Helena                   |Barlow        |Rose Weasley - 19 Years Later             |
|Ryan                     |Turner        |Hugo Weasley - 19 Years Later             |
|Jon                      |Campling      |Death Eater in Gringotts                  |
|Karen                    |Anderson      |Gringotts Goblin                          |
|Michael                  |Aston         |Wizard Parent                             |
|Michael Henbury          |Ballan        |Gringotts Goblin                          |
|Lauren                   |Barrand       |Gringotts Goblin                          |
|David                    |Barron        |Wizard with Dog in Painting               |
|Josh                     |Bennett       |Gringotts Goblin                          |
|Johann                   |Benét         |Deatheater                                |
|Sean                     |Biggerstaff   |Oliver Wood                               |
|Jada                     |Brevett       |Hogwarts Student                          |
|Ben                      |Champniss     |Parent                                    |
|Collet                   |Collins       |Snatcher                                  |
|Christoph                |Cordell       |Snatcher                                  |
|Christian                |Coulson       |Tom Marvolo Riddle                        |
|Gioacchino Jim           |Cuffaro       |Wizard Parent                             |
|Valerie                  |Dane          |Wizard Parent                             |
|Paul                     |Davies        |Deatheater                                |
|David                    |Decio         |Chief Snatcher                            |
|Ninette                  |Finch         |Augusta Longbottom                        |
|Grace Meurisse           |Francis       |Senior Gryffindor                         |
|Sean Francis             |George        |Wizard Parent                             |
|Diane                    |Gibbins       |Gringotts Goblin                          |
|Rich                     |Goble         |Death Eater                               |
|Hattie                   |Gotobed       |Young Girl in Epilogue                    |
|Melissa                  |Gotobed       |Hogwart's First Year Epilogue             |
|Ian                      |Hart          |Professor Quirinus Quirrell               |
|Stephen                  |Hawke         |Wedding Guest                             |
|David                    |Heyman        |Dining Wizard in Painting                 |
|Harper                   |Heyman        |Baby of Dining Wizard Family in Portrait  |
|Matthew                  |Hodgkin       |Hogwarts Student                          |
|Steven                   |Hopwood       |One-Legged Wizard                         |
|Joe                      |Kallis        |Death Eater                               |
|Gemma                    |Kayla         |Ravenclaw Senior                          |
|Hrvoje                   |Klecz         |Death Eater                               |
|Maxwell                  |Laird         |Gringotts Goblin                          |
|Debra                    |Leigh-Taylor  |Wizard Teacher                            |
|Christina                |Low           |Ravenclaw Student                         |
|Sarah                    |Lowe          |Ministry Wizard                           |
|Jonathan                 |Massahi       |Hogwarts Student                          |
|Tony                     |Montalbano    |Passenger                                 |
|Sha'ori                  |Morris        |Slytherin Girl                            |
|Luke                     |Newberry      |Teddy Lupin                               |
|Sarah Jane               |O'Neill       |Wizard Parent                             |
|Lisa                     |Osmond        |Gringotts Goblin                          |
|Elisabeth                |Roberts       |Death Eater                               |
|Keijo                    |Salmela       |Gringotts Goblin                          |
|Joshua                   |Savary        |Ravenclaw Student                         |
|Mark                     |Sealey        |Gringotts Goblin                          |
|Arti                     |Shah          |Gringotts Goblin                          |
|Glen                     |Stanway       |Death Eater                               |
|Albert                   |Tang          |Hogwarts Teacher                          |
|Richard                  |Trinder       |Augustus Rookwood                         |
|Nick                     |Turner        |Death Eater                               |
|Aaron                    |Virdee        |Gryffindor Senior                         |
|John                     |Warman        |Railway Station Porter                    |
|Spencer                  |Wilding       |Knight of Hogwarts                        |
|Amy                      |Wiles         |Slytherin Student                         |
|Thomas                   |Williamson    |Hogwarts Student                          |
#### Top Cast

```r
kable(head(cast,10), "markdown", row.names=FALSE, align="l", padding=2)
```



|FirstName  |Surname    |Character                             |
|:----------|:----------|:-------------------------------------|
|Ralph      |Fiennes    |Lord Voldemort                        |
|Michael    |Gambon     |Professor Albus Dumbledore            |
|Alan       |Rickman    |Professor Severus Snape               |
|Daniel     |Radcliffe  |Harry Potter                          |
|Rupert     |Grint      |Ron Weasley                           |
|Emma       |Watson     |Hermione Granger                      |
|Evanna     |Lynch      |Luna Lovegood                         |
|Domhnall   |Gleeson    |Bill Weasley                          |
|Clémence   |Poésy      |Fleur Delacour                        |
|Warwick    |Davis      |Griphook / Professor Filius Flitwick  |
### Shooting Statistics
#### San Antonio Spurs


```r
library(xml2)
library(ggplot2)

# read in HMTL from the IMDB web site
ESPN_spurs <- read_html("http://www.espn.com/nba/team/stats/_/name/sa/san-antonio-spurs")

# get the shooting stats table
stats_head_html <- xml_find_all(ESPN_spurs, xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "stathead", " " ))]')
shooting_head <- stats_head_html[2]

# break the table in to header and rows, and get their count
rows_html <- xml_siblings(shooting_head) 
headers_html <- xml_children(rows_html[1])
headers <- sapply(headers_html, function(x) html_text(x))
num_stat_cols <- length(headers)
stat_rows <- rows_html[-c(1,length(rows_html))]
num_stat_rows <- length(stat_rows)

# make header cols into acceptable variable names
headers <- str_replace_all(headers, "%" , "P")
headers <- str_replace_all(headers, "3" , "Three")
headers <- str_replace_all(headers, "2" , "Two")

# strip the statistics rows of html
stats_html <- sapply(stat_rows, function(x) xml_children(x))
stat_list <- sapply(stats_html, function(x) html_text(x))

# format information into a data frame
stats_matrix <- matrix(stat_list, nrow = num_stat_rows, ncol = num_stat_cols, byrow = TRUE)
stats_df <- data.frame(stats_matrix, row.names = NULL, stringsAsFactors = FALSE)
names(stats_df) <- headers

# split the player and position into two columns
stats_df$POS <- str_replace_all(sapply(strsplit(stats_df$PLAYER, ","), "[[", 2),"\\s+","")
stats_df$PLAYER <- sapply(strsplit(stats_df$PLAYER, ","), "[[", 1)
#reorder columns
stats_df <- stats_df[c(headers[1],"POS",headers[-1])]

# convert numeric types to numeric
stats_df$FGM <- as.numeric(stats_df$FGM)
stats_df$FGA <- as.numeric(stats_df$FGA)
stats_df$FGP <- as.numeric(stats_df$FGP)
stats_df$ThreePM <- as.numeric(stats_df$ThreePM)
stats_df$ThreePA <- as.numeric(stats_df$ThreePA)
stats_df$ThreePP <- as.numeric(stats_df$ThreePP)
stats_df$FTM <- as.numeric(stats_df$FTM)
stats_df$FTA <- as.numeric(stats_df$FTA)
stats_df$FTP <- as.numeric(stats_df$FTP)
stats_df$TwoPM <- as.numeric(stats_df$TwoPM)
stats_df$TwoPA <- as.numeric(stats_df$TwoPA)
stats_df$TwoPP <- as.numeric(stats_df$TwoPP)
stats_df$PPS <- as.numeric(stats_df$PPS)
stats_df$AFGP <- as.numeric(stats_df$AFGP)


str(stats_df)
```

```
## 'data.frame':	17 obs. of  16 variables:
##  $ PLAYER : chr  "LaMarcus Aldridge" "Kawhi Leonard" "Rudy Gay" "Pau Gasol" ...
##  $ POS    : chr  "PF" "SF" "SF" "C" ...
##  $ FGM    : num  8.9 5.8 4.3 3.8 3.3 3.1 3.2 3.7 3.1 3.3 ...
##  $ FGA    : num  17.8 12.3 9.2 8.4 8 7.2 8.1 7.7 6 7.3 ...
##  $ FGP    : num  0.502 0.468 0.468 0.457 0.413 0.433 0.397 0.479 0.515 0.446 ...
##  $ ThreePM: num  0.4 1.2 0.7 0.6 1.8 0.9 1.7 0.2 0.2 0.1 ...
##  $ ThreePA: num  1.3 3.9 2 1.7 4.8 3 4.6 0.7 0.7 0.4 ...
##  $ ThreePP: num  0.301 0.314 0.323 0.376 0.379 0.315 0.374 0.233 0.277 0.296 ...
##  $ FTM    : num  4.4 3.4 1.9 2.1 1.2 1.6 0.6 1 1.5 1.1 ...
##  $ FTA    : num  5.3 4.2 2.4 2.7 1.4 1.9 0.8 1.5 2 1.6 ...
##  $ FTP    : num  0.84 0.82 0.79 0.77 0.88 0.85 0.78 0.7 0.73 0.7 ...
##  $ TwoPM  : num  8.5 4.6 3.6 3.2 1.5 2.2 1.5 3.5 2.9 3.2 ...
##  $ TwoPA  : num  16.5 8.4 7.2 6.7 3.2 4.2 3.5 7 5.3 6.9 ...
##  $ TwoPP  : num  0.518 0.539 0.509 0.477 0.465 0.517 0.427 0.503 0.548 0.454 ...
##  $ PPS    : num  1.28 1.31 1.21 1.24 1.2 ...
##  $ AFGP   : num  0.51 0.52 0.5 0.49 0.53 0.5 0.5 0.49 0.53 0.45 ...
```

```r
#display in a table
kable(stats_df, "markdown", row.names=FALSE, align="l", padding=2)
```



|PLAYER             |POS  |FGM  |FGA   |FGP    |ThreePM  |ThreePA  |ThreePP  |FTM  |FTA  |FTP   |TwoPM  |TwoPA  |TwoPP  |PPS    |AFGP  |
|:------------------|:----|:----|:-----|:------|:--------|:--------|:--------|:----|:----|:-----|:------|:------|:------|:------|:-----|
|LaMarcus Aldridge  |PF   |8.9  |17.8  |0.502  |0.4      |1.3      |0.301    |4.4  |5.3  |0.84  |8.5    |16.5   |0.518  |1.276  |0.51  |
|Kawhi Leonard      |SF   |5.8  |12.3  |0.468  |1.2      |3.9      |0.314    |3.4  |4.2  |0.82  |4.6    |8.4    |0.539  |1.315  |0.52  |
|Rudy Gay           |SF   |4.3  |9.2   |0.468  |0.7      |2.0      |0.323    |1.9  |2.4  |0.79  |3.6    |7.2    |0.509  |1.213  |0.50  |
|Pau Gasol          |C    |3.8  |8.4   |0.457  |0.6      |1.7      |0.376    |2.1  |2.7  |0.77  |3.2    |6.7    |0.477  |1.239  |0.49  |
|Patty Mills        |PG   |3.3  |8.0   |0.413  |1.8      |4.8      |0.379    |1.2  |1.4  |0.88  |1.5    |3.2    |0.465  |1.202  |0.53  |
|Manu Ginobili      |SG   |3.1  |7.2   |0.433  |0.9      |3.0      |0.315    |1.6  |1.9  |0.85  |2.2    |4.2    |0.517  |1.224  |0.50  |
|Danny Green        |SG   |3.2  |8.1   |0.397  |1.7      |4.6      |0.374    |0.6  |0.8  |0.78  |1.5    |3.5    |0.427  |1.086  |0.50  |
|Tony Parker        |PG   |3.7  |7.7   |0.479  |0.2      |0.7      |0.233    |1.0  |1.5  |0.70  |3.5    |7.0    |0.503  |1.112  |0.49  |
|Kyle Anderson      |SF   |3.1  |6.0   |0.515  |0.2      |0.7      |0.277    |1.5  |2.0  |0.73  |2.9    |5.3    |0.548  |1.313  |0.53  |
|Dejounte Murray    |PG   |3.3  |7.3   |0.446  |0.1      |0.4      |0.296    |1.1  |1.6  |0.70  |3.2    |6.9    |0.454  |1.058  |0.45  |
|Bryn Forbes        |SG   |2.7  |6.4   |0.421  |1.2      |3.1      |0.388    |0.6  |0.9  |0.64  |1.5    |3.3    |0.451  |1.113  |0.51  |
|Davis Bertans      |C    |2.2  |5.1   |0.441  |1.3      |3.5      |0.372    |0.6  |0.7  |0.82  |0.9    |1.6    |0.594  |1.256  |0.57  |
|Joffrey Lauvergne  |C    |1.9  |3.6   |0.522  |0.0      |0.1      |0.000    |0.6  |0.9  |0.65  |1.9    |3.5    |0.538  |1.202  |0.52  |
|Derrick White      |PG   |0.9  |1.9   |0.485  |0.5      |0.8      |0.615    |0.8  |1.2  |0.70  |0.4    |1.1    |0.400  |1.636  |0.61  |
|Brandon Paul       |SG   |0.8  |2.0   |0.422  |0.2      |0.9      |0.275    |0.4  |0.6  |0.57  |0.6    |1.1    |0.538  |1.147  |0.48  |
|Darrun Hilliard    |SG   |0.4  |1.4   |0.263  |0.0      |0.4      |0.000    |0.4  |0.5  |0.86  |0.4    |1.0    |0.385  |0.842  |0.26  |
|Matt Costello      |SF   |0.5  |1.0   |0.500  |0.0      |0.0      |0.000    |0.0  |0.0  |0.00  |0.5    |1.0    |0.500  |1.000  |0.50  |

```r
# sort by FGP descending in a way that makes ggplot happy
stats_df$PLAYER <- factor(stats_df$PLAYER, levels = stats_df$PLAYER[order(-stats_df$FGP)])

ggplot(data=stats_df, aes(x=PLAYER, y=FGP, fill=POS)) + geom_bar(stat="identity", width=.7) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title="Field Goal Percentage by Player", x ="Players", y = "Field Goal Percentage") + scale_fill_manual("Positions", values = c("PF" = "steelblue2", "SF" = "darkgoldenrod2", "C" = "coral1", "PG" ="darkseagreen", "SG" ="burlywood2"))
```

![](apaschal_Livesession10assignment_MS6306_sec402_files/figure-html/BBALL-1.png)<!-- -->

```
