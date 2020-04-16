# Tom Alig
# March 31, 2020
# scripts for Covid-19 data

###############################################################
###############################################################

# Define colors
cb.blue <- "#0072B2"
cb.orange <- "#E69F00"
cb.purple <- "#CC79A7"
cb.six <- c("#000000",  
            cb.blue, 
            cb.orange, cb.purple,
            "#56B4E9", "#009E73" )
cb.seven <- c("#009E73", cb.blue, "#000000",
              "#999999", 
              cb.orange, cb.purple, "red")

cb.8 <- c("#009E73", cb.blue, "#000000",
              "#999999", 
              cb.orange, cb.purple, "red", "maroon")


clr.ww.conf = cb.orange
clr.us.conf = cb.blue
clr.ww.dead = 'darkred'
clr.us.dead = cb.purple

###############################################################
###############################################################
# hard code in 1 million so I don't fat finger it later
mill <- 1000000

###############################################################
###############################################################

# Population Statistics
michpop <- 10000000
ohpop <- 11700000
txpop <- 28700000
wipop <- 5800000
capop <- 39600000
nypop <- 19500000
wapop <- 7500000
flpop <- 21500000

## worldwide pop
italypop <- 61000000
frpop <- 65000000
gerpop <- 84000000
chpop <- 1439000000
uspop <- 327000000
skpop <- 51000000
spainpop <- 47000000

###############################################################
###############################################################

# Per 1 million population graph
#     a = df
#     type = conf, act, death, recovered
#     region = one of the following:
#             State for US plot; or Country.Reg for worldwide plot
#     typex = conft, act, death, recovered

graph.my.data.per1mm <- function(a, type, region, typex) {
  ggplot(a, aes(x = date, y = type, na.rm = TRUE,
               color = region,
               group = region
    )) +
    geom_point(size = 3) + 
    geom_line(size = 2) +
    scale_colour_manual(values = cb.seven) +
    scale_x_date(date_labels="%b %d",date_breaks  ="1 week") +
    scale_y_continuous(labels = comma) +
    #xlab("Date") +
    ylab("per 1 Million Population") +
    theme(panel.grid.minor = element_blank()) +
    guides(color = guide_legend(title = NULL)) +
    theme_bw() +
    theme(legend.text = element_text(size = 14)) +
    theme(axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          axis.title.y = element_text(size = 13)) +
    labs(title = paste("Per 1 Million Population, COVID-19",
                  typex,
                  "Cases")) +
    theme(plot.title = element_text(size = 16))
}
  

###############################################################
###############################################################  
               
# Raw data graph
#     a = df
#     type = conf, act, death, recovered
#     region = one of the following:
#             State for US plot; or Country.Reg for worldwide plot
#     typex = conft, act, death, recovered

graph.my.data.raw <- function(a, type, region, typex) {
  ggplot(a, aes(x = date, y = type, na.rm = TRUE,
                color = region,
                group = region
  )) +
    geom_point(size = 3) + 
    geom_line(size = 2) +
    scale_colour_manual(values = cb.seven) +
    scale_x_date(date_labels="%b %d",date_breaks  ="1 week") +
    scale_y_continuous(labels = comma) +
    xlab("") +
    ylab("Total") +
    theme(panel.grid.minor = element_blank()) +
    guides(color = guide_legend(title = NULL)) +
    theme_bw() +
    theme(legend.text = element_text(size = 14)) +
    theme(axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          axis.title.y = element_text(size = 13)) +
    labs(title = paste("Total Number of COVID-19",
                       typex,
                       "Cases")) +
    theme(plot.title = element_text(size = 16))
}


###############################################################
###############################################################  

# Raw data graph
#     a = df
#     type = conf, act, death, recovered
#     region = one of the following:
#             State for US plot; or Country.Reg for worldwide plot
#     typex = conft, act, death, recovered

graph.bigpic.us.raw <- function(a, type, typex) {
  a %>% 
    group_by(date) %>% 
    summarize(tot.conf = sum(us.conf),
              tot.deaths = sum(us.death),
              tot.rec = sum(us.rec),
              tot.active = sum(us.act)) %>% 
  ggplot(aes(x = date, y = type, na.rm = TRUE#,
                #color = region,
                #group = region
  )) +
    geom_point(size = 3) + 
    geom_line(size = 2) +
    #scale_colour_manual(values = cb.seven) +
    scale_x_date(date_labels="%b %d",date_breaks  ="1 week") +
    scale_y_continuous(labels = comma) +
    xlab("") +
    ylab("Total") +
    theme(panel.grid.minor = element_blank()) +
    guides(color = guide_legend(title = NULL)) +
    theme_bw() +
    theme(legend.text = element_text(size = 14)) +
    theme(axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          axis.title.y = element_text(size = 13)) +
    labs(title = paste("Total Number of COVID-19",
                       typex,
                       "Cases")) +
    theme(plot.title = element_text(size = 16))
}

###############################################################
###############################################################  
###############################################################
###############################################################  



# To insert valueBox in Shiny App
  
valueBox <- function(value, subtitle, icon, color) {
  div(class = "col-lg-3 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = "col-xs-3",
                      icon(icon, "fa-5x")
                  ),
                  div(class = ("col-xs-9 text-center"),
                      div(style = ("font-size: 40px; font-weight: bold;"),
                          textOutput(value)
                      ),
                      div(subtitle)
                  )
              )
          ),
          div(class = "panel-footer",
              div(class = "clearfix")
          )
      )
  )
}

###############################################################
###############################################################  

